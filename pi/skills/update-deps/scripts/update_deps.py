#!/usr/bin/env python3
"""Update one repository's dependencies of one type and publish them as one PR.

Repository code runs only inside the update-deps container. The host reads
back only the files the type owns, as data, and does every git and GitHub step.
"""
import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tomllib
from datetime import datetime, timedelta, timezone
from pathlib import Path

HOME = Path(os.environ.get('UPDATE_DEPS_HOME', Path.home() / '.cache' / 'update-deps'))
IMAGE = os.environ.get('UPDATE_DEPS_IMAGE', 'update-deps:1')
IMAGE_DIR = Path(__file__).resolve().parent.parent / 'image'
GH_HELPER = '!gh auth git-credential'
REPO = re.compile(r'^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$')
MAX_OWNED_BYTES = 5 * 1024 * 1024

TRUST = 'if [ -f mise.toml ]; then mise trust -q mise.toml; fi'
MIX_PREP = ('mise x -- mix local.hex --force --if-missing && mise x -- mix local.rebar --force --if-missing'
            ' && MIX_ENV=test mise x -- mix deps.get')


def hex_names(text):
    return dict(re.findall(r'^\s+"([^"]+)": \{:hex, :[^,]+, "([^"]+)"', text, re.M))


def mise_names(text, filename):
    data = tomllib.loads(text)
    if filename == 'mise.lock':
        return {k: ', '.join(e.get('version', '?') for e in v) for k, v in data.get('tools', {}).items()}
    return {k: v if isinstance(v, str) else str(v.get('version', v)) for k, v in data.get('tools', {}).items()}


# One row per type. Steps run in the container with the target names as "$@"; no names means all.
TYPES = {
    'hex': {
        'detect': ['mix.exs', 'mix.lock'],
        'owns': ['mix.lock'],
        'names': lambda files: hex_names(files.get('mix.lock', '')),
        'setup': f'{TRUST} && if [ -f mise.lock ]; then mise install --locked erlang elixir; '
                 f'else mise install erlang elixir; fi && {MIX_PREP}',
        'audit': 'mise x -- mix hex.audit',
        'update': 'if [ $# -eq 0 ]; then mise x -- mix deps.update --all; else mise x -- mix deps.update "$@"; fi',
    },
    'mise': {
        'detect': ['mise.toml'],
        'owns': ['mise.toml', 'mise.lock'],
        'names': lambda files: mise_names(files.get('mise.toml', ''), 'mise.toml'),
        'setup': f'{TRUST} && if [ -f mise.lock ]; then mise install --locked; else mise install; fi',
        'audit': None,
        # mise can install a tool before the tool it depends on; a second pass converges.
        'update': 'MISE_LOCKED=0 mise upgrade --bump "$@" || MISE_LOCKED=0 mise upgrade --bump "$@"',
    },
}


class Failed(Exception):
    pass


def run(args, **kw):
    done = subprocess.run(args, text=True, capture_output=True, **kw)
    if done.returncode:
        raise Failed(f'failed ({done.returncode}): {" ".join(args)}\n{done.stderr.strip()}')
    return done.stdout


def paths(repo, kind=None):
    base = HOME / repo.replace('/', '__')
    p = {'base': base, 'mirror': base / 'mirror.git', 'cache': base / 'cache'}
    if kind:
        p.update(run=base / kind, worktree=base / kind / 'worktree', src=base / kind / 'src', pristine=base / kind / 'base',
                 logs=base / kind / 'logs', record=base / kind / 'record.json', body=base / kind / 'pr.md')
    return p


def fetch(repo):
    mirror = paths(repo)['mirror']
    if not mirror.exists():
        mirror.parent.mkdir(parents=True, exist_ok=True)
        run(['git', '-c', f'credential.helper={GH_HELPER}', 'clone', '--bare', '--filter=blob:none',
             f'https://github.com/{repo}.git', str(mirror)])
        run(['git', '-C', str(mirror), 'config', 'remote.origin.fetch', '+refs/heads/*:refs/remotes/origin/*'])
        run(['git', '-C', str(mirror), 'config', 'credential.helper', GH_HELPER])
    run(['git', '-C', str(mirror), 'fetch', '--prune', 'origin'])
    head = run(['git', '-C', str(mirror), 'ls-remote', '--symref', 'origin', 'HEAD'])
    branch = re.search(r'^ref: refs/heads/(\S+)\tHEAD', head, re.M).group(1)
    sha = run(['git', '-C', str(mirror), 'rev-parse', f'refs/remotes/origin/{branch}']).strip()
    return branch, sha


def plan(repo):
    branch, sha = fetch(repo)
    mirror = str(paths(repo)['mirror'])
    files = set(run(['git', '-C', mirror, 'ls-tree', '--name-only', sha]).split())
    tasks = {}
    if 'mise.toml' in files:
        tasks = tomllib.loads(run(['git', '-C', mirror, 'show', f'{sha}:mise.toml'])).get('tasks', {})
    if 'test' in tasks:
        test = f'{TRUST} && mise run test'
    elif 'mix.exs' in files:
        test = f'{MIX_PREP} && MIX_ENV=test mise x -- mix test'
    else:
        test = None
    types = [k for k, t in TYPES.items() if all(f in files for f in t['detect'])]
    return {'repo': repo, 'branch': branch, 'sha': sha, 'types': types, 'test': test}


def container(repo, src, steps, log, args=()):
    """Run steps in order inside the container; a failed step stops the rest."""
    cache = paths(repo)['cache']
    cache.mkdir(parents=True, exist_ok=True)
    cmd = ['container', 'run', '--rm', '--cpus', '4', '--memory', '6g',
           '-v', f'{src}:/src', '-v', f'{cache}:/cache', IMAGE,
           'bash', '-c', 'set -o pipefail; ' + ' && '.join(f'( {s} )' for s in steps), 'step', *args]
    with open(log, 'w') as out:
        return subprocess.run(cmd, stdout=out, stderr=subprocess.STDOUT).returncode


def tests(repo, src, setup, command, log):
    if not command:
        return {'command': None, 'exit': None, 'failures': []}
    code = container(repo, src, [setup, command], log)
    text = Path(log).read_text(errors='replace')
    failures = sorted({f'{m.group(2)}: {m.group(1)}'
                       for m in re.finditer(r'^\s+\d+\) test (.+?) \((\S+)\)\s*$', text, re.M)})
    return {'command': command, 'exit': code, 'failures': failures}


def regressed(before, after):
    if after['command'] is None or after['exit'] == 0:
        return False
    if before['exit'] == 0 or not after['failures']:
        return True
    return bool(set(after['failures']) - set(before['failures']))


def advisories(log):
    """Parse `mix hex.audit`: '  pkg 1.2.3 - EEF-CVE-... (HIGH)' followed by an aka line and a title line."""
    lines = Path(log).read_text(errors='replace').splitlines()
    found = []
    for i, line in enumerate(lines):
        m = re.match(r'^  (\S+) (\S+) - (\S+) \((\w+)\)$', line)
        if m:
            title = next((l.strip() for l in lines[i + 1:i + 4] if l.strip() and not l.strip().startswith(('aka:', 'http'))), '')
            found.append({'key': f'{m[1]} {m[3]}', 'text': f'{m[1]} {m[2]} {m[3]} ({m[4]}): {title}'})
    if any('Found retired packages' in l for l in lines):
        found.append({'key': 'retired', 'text': 'Retired packages present; see the audit log.'})
    return found


def read_owned(root, owns):
    return {f: (root / f).read_text(errors='replace') for f in owns
            if (root / f).is_file() and not (root / f).is_symlink()}


def versions(kind, files):
    if kind == 'hex':
        return hex_names(files.get('mix.lock', ''))
    result = mise_names(files.get('mise.toml', ''), 'mise.toml')
    if 'mise.lock' in files:
        result.update(mise_names(files['mise.lock'], 'mise.lock'))
    return result


def copy_back(src, worktree, owns, base_files):
    """Bring only owned files out of the container copy; report other changed tracked files."""
    copied, ignored = [], []
    for rel in sorted(set(base_files) | set(owns)):
        source, target = src / rel, worktree / rel
        if source.is_symlink() or not source.is_file():
            continue
        if target.is_file() and not target.is_symlink() and source.read_bytes() == target.read_bytes():
            continue
        if rel in owns and source.stat().st_size <= MAX_OWNED_BYTES:
            shutil.copyfile(source, target)
            copied.append(rel)
        elif rel in base_files:
            ignored.append(rel)
    return copied, ignored


def export(mirror, sha, dest):
    dest.mkdir()
    archive = subprocess.run(['git', '-C', mirror, 'archive', sha], check=True, capture_output=True).stdout
    subprocess.run(['tar', '-x', '-C', str(dest)], input=archive, check=True)


def update(repo, kind, hold):
    info = plan(repo)
    if kind not in info['types']:
        raise Failed(f'{repo} has no {kind} files at {info["sha"][:7]}')
    t, p = TYPES[kind], paths(repo, kind)
    mirror = str(p['mirror'])
    if p['worktree'].exists():
        run(['git', '-C', mirror, 'worktree', 'remove', '--force', str(p['worktree'])])
    shutil.rmtree(p['run'], ignore_errors=True)
    p['logs'].mkdir(parents=True)
    run(['git', '-C', mirror, 'worktree', 'add', '-B', f'deps/{kind}', str(p['worktree']), info['sha']])
    export(mirror, info['sha'], p['src'])
    base_files = [f for f in run(['git', '-C', str(p['worktree']), 'ls-files', '-z']).split('\0') if f]
    if kind == 'hex' and not {'mise.toml', '.tool-versions'} & set(base_files):
        raise Failed(f'{repo}: no mise.toml or .tool-versions pins Erlang and Elixir, so the container cannot pick them')

    base_owned = read_owned(p['worktree'], t['owns'])
    unknown = sorted(set(hold) - set(t['names'](base_owned)))
    if unknown:
        raise Failed(f'not a {kind} dependency of {repo}: {", ".join(unknown)}')
    targets = sorted(set(t['names'](base_owned)) - set(hold)) if hold else []

    record = {'repo': repo, 'type': kind, 'branch': info['branch'], 'sha': info['sha'], 'hold': hold,
              'owns': t['owns'], 'logs': str(p['logs'])}
    if t['audit']:
        container(repo, p['src'], [t["setup"], t["audit"]], p['logs'] / 'audit-before.log')
        record['advisories_before'] = advisories(p['logs'] / 'audit-before.log')
    record['update_exit'] = container(repo, p['src'], [t["setup"], t["update"]],
                                      p['logs'] / 'update.log', targets)
    record['files'], record['ignored_changes'] = copy_back(p['src'], p['worktree'], t['owns'], base_files)
    old, new = versions(kind, base_owned), versions(kind, read_owned(p['worktree'], t['owns']))
    record['changes'] = [{'name': n, 'from': old.get(n), 'to': new[n]} for n in sorted(new) if old.get(n) != new[n]]

    # Tests run only when the update changed something: on a fresh copy of the base, then on the update.
    if record['update_exit']:
        record['verdict'] = 'update_failed'
    elif not record['files']:
        record['verdict'] = 'no_changes'
    else:
        export(mirror, info['sha'], p['pristine'])
        record['before'] = tests(repo, p['pristine'], t['setup'], info['test'], p['logs'] / 'before.log')
        record['after'] = tests(repo, p['src'], t['setup'], info['test'], p['logs'] / 'after.log')
        if t['audit']:
            container(repo, p['src'], [t["setup"], t["audit"]], p['logs'] / 'audit-after.log')
            record['advisories_after'] = advisories(p['logs'] / 'audit-after.log')
        record['verdict'] = 'regressed' if regressed(record['before'], record['after']) else 'ok'
    p['record'].write_text(json.dumps(record, indent=2))
    return record


def sync_to_src(worktree, src, base_files):
    """Copy the worktree's files over the container copy, keeping its build caches."""
    present = [f for f in run(['git', '-C', str(worktree), 'ls-files', '-co', '--exclude-standard', '-z']).split('\0')
               if f]
    for rel in present:
        source, target = worktree / rel, src / rel
        if source.is_symlink() or not source.is_file():
            continue
        if target.is_dir() and not target.is_symlink():
            shutil.rmtree(target)
        elif target.is_symlink():
            target.unlink()
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, target)
    for rel in set(base_files) - set(present):
        if (src / rel).is_file() or (src / rel).is_symlink():
            (src / rel).unlink()


def retest(repo, kind):
    p = paths(repo, kind)
    record = json.loads(p['record'].read_text())
    base_files = [f for f in run(['git', '-C', str(p['mirror']), 'ls-tree', '-r', '--name-only', '-z',
                                  record['sha']]).split('\0') if f]
    sync_to_src(p['worktree'], p['src'], base_files)
    n = len(list(p['logs'].glob('retest-*.log'))) + 1
    record['after'] = tests(repo, p['src'], TYPES[kind]['setup'], record['before']['command'],
                            p['logs'] / f'retest-{n}.log')
    record['verdict'] = 'regressed' if regressed(record['before'], record['after']) else 'ok'
    status = run(['git', '-C', str(p['worktree']), 'status', '--porcelain']).splitlines()
    record['agent_edits'] = [line[3:] for line in status if line[3:] not in record['owns']]
    p['record'].write_text(json.dumps(record, indent=2))
    return record


def fence(text):
    return '```\n' + text.replace('```', "'''") + '\n```'


def body(record, note):
    lines = [f'Routine **{record["type"]}** dependency updates for `{record["repo"]}` '
             f'at `{record["sha"][:7]}` on `{record["branch"]}`.', '']
    if record['verdict'] == 'regressed':
        lines += ['**Needs a fix.** Tests that passed before this update fail after it. Fix them on this '
                  'branch, for example with the update-deps skill. Once the branch changes more than the '
                  'lock and manifest, the nightly sweep leaves it alone.', '']
    if note:
        lines += [note, '']
    if record['changes']:
        lines += ['| Package | From | To |', '|---|---|---|']
        lines += [f'| `{c["name"]}` | {c["from"] or "new"} | {c["to"]} |' for c in record['changes']]
        lines.append('')
    if record['hold']:
        lines += ['Held back: ' + ', '.join(f'`{h}`' for h in record['hold']), '']
    if 'advisories_before' in record:
        left = record.get('advisories_after', record['advisories_before'])
        fixed = {a['key'] for a in record['advisories_before']} - {a['key'] for a in left}
        lines.append(f'**Security.** {len(fixed)} advisories fixed, {len(left)} still open.')
        if left:
            lines.append(fence('\n'.join(a['text'] for a in left)))
        lines.append('')
    before, after = record['before'], record.get('after', record['before'])
    if before['command']:
        new = sorted(set(after['failures']) - set(before['failures']))
        lines.append(f'**Tests.** Ran in a Linux container. Before the update: exit {before["exit"]}, '
                     f'{len(before["failures"])} failing. After: exit {after["exit"]}, '
                     f'{len(after["failures"])} failing, {len(new)} new.')
        if before['failures']:
            lines += ['', '<details><summary>Already failing before the update</summary>', '',
                      fence('\n'.join(before['failures'])), '', '</details>']
        if new:
            lines += ['', 'New failures:', fence('\n'.join(new))]
    else:
        lines.append('**Tests.** None found, so only the install ran.')
    if record.get('agent_edits'):
        lines += ['', '**Code changed to keep tests passing:**', fence('\n'.join(record['agent_edits']))]
    if record['ignored_changes']:
        lines += ['', 'Changed in the container but not included: ' +
                  ', '.join(f'`{f}`' for f in record['ignored_changes'])]
    lines += ['', '_Generated by the update-deps skill._']
    return '\n'.join(lines)


def publish(repo, kind, note, dry_run):
    p = paths(repo, kind)
    record = json.loads(p['record'].read_text())
    wt, head = str(p['worktree']), f'deps/{kind}'
    existing = json.loads(run(['gh', 'pr', 'list', '-R', repo, '--head', head, '--state', 'open',
                               '--json', 'number,url']) or '[]')
    uncommitted = run(['git', '-C', wt, 'status', '--porcelain']).strip()
    differs = subprocess.run(['git', '-C', wt, 'diff', '--quiet', record['sha']]).returncode != 0
    if not (uncommitted or differs):
        for pr in existing:
            close = ['gh', 'pr', 'close', str(pr['number']), '-R', repo, '--delete-branch',
                     '--comment', 'Nothing left to update.']
            print(' '.join(close), file=sys.stderr) if dry_run else run(close)
        return {'published': None, 'closed': [pr['url'] for pr in existing], 'dry_run': dry_run}
    title = f'build(deps): update {kind} dependencies'
    p['body'].write_text(body(record, note))
    draft = record['verdict'] != 'ok'
    if dry_run:
        return {'dry_run': True, 'title': title, 'body': str(p['body']), 'draft': draft,
                'would_edit': existing[0]['url'] if existing else None}
    remote = f'refs/remotes/origin/{head}'
    published = subprocess.run(['git', '-C', wt, 'rev-parse', '--verify', '-q', f'{remote}^'],
                               capture_output=True, text=True)
    unchanged = (published.stdout.strip() == record['sha'] and '??' not in uncommitted
                 and subprocess.run(['git', '-C', wt, 'diff', '--quiet', remote]).returncode == 0)
    if not unchanged:
        if uncommitted:
            run(['git', '-C', wt, 'add', '-A'])
            run(['git', '-C', wt, 'commit', '-m', title])
        run(['git', '-C', wt, 'push', '--force-with-lease', 'origin', head])
    if existing:
        number = str(existing[0]['number'])
        run(['gh', 'pr', 'edit', number, '-R', repo, '--title', title, '--body-file', str(p['body'])])
        run(['gh', 'pr', 'ready', number, '-R', repo] + (['--undo'] if draft else []))
        return {'published': existing[0]['url'], 'draft': draft}
    url = run(['gh', 'pr', 'create', '-R', repo, '--head', head, '--base', record['branch'], '--title', title,
               '--body-file', str(p['body'])] + (['--draft'] if draft else [])).strip()
    return {'published': url, 'draft': draft}


def active_repos(owners, active_days):
    since = datetime.now(timezone.utc) - timedelta(days=active_days)
    found = []
    for owner in owners:
        listed = json.loads(run(['gh', 'repo', 'list', owner, '--limit', '1000', '--no-archived', '--source',
                                 '--json', 'nameWithOwner,pushedAt,defaultBranchRef']))
        found += [r['nameWithOwner'] for r in listed
                  if (r['defaultBranchRef'] or {}).get('name') and datetime.fromisoformat(r['pushedAt']) >= since]
    return sorted(found)


def edited_by_hand(repo, kind, branch):
    """True when deps/<kind> on GitHub changes files the type does not own, so someone fixed it."""
    mirror = str(paths(repo)['mirror'])
    head = f'refs/remotes/origin/deps/{kind}'
    if subprocess.run(['git', '-C', mirror, 'rev-parse', '--verify', '-q', head], capture_output=True).returncode:
        return False
    changed = run(['git', '-C', mirror, 'diff', '--no-renames', '--name-only',
                   f'refs/remotes/origin/{branch}...{head}']).split()
    return bool(set(changed) - set(TYPES[kind]['owns']))


def sweep(owners, active_days, skip, dry_run):
    """Update every type of every active repository. A regression opens as a draft PR."""
    rows = []
    for repo in active_repos(owners, active_days):
        try:
            info = plan(repo)
        except (Failed, subprocess.CalledProcessError) as e:
            rows.append({'repo': repo, 'type': '-', 'verdict': 'error', 'error': str(e)})
            continue
        for kind in info['types']:
            row = {'repo': repo, 'type': kind, 'logs': str(paths(repo, kind)['logs'])}
            if repo in skip or f'{repo}:{kind}' in skip:
                rows.append(row | {'verdict': 'skipped'})
                continue
            try:
                if edited_by_hand(repo, kind, info['branch']):
                    rows.append(row | {'verdict': 'edited'})
                    continue
                record = update(repo, kind, [])
                row |= {'verdict': record['verdict'], 'changes': len(record['changes']),
                        'advisories': record.get('advisories_after', record.get('advisories_before', []))}
                if record['verdict'] in ('ok', 'no_changes', 'regressed'):
                    row['pr'] = publish(repo, kind, '', dry_run)
            except (Failed, subprocess.CalledProcessError) as e:
                row |= {'verdict': 'error', 'error': str(e)}
            rows.append(row)
    finished = datetime.now(timezone.utc)
    out = HOME / 'sweeps' / finished.strftime('%Y%m%dT%H%M%SZ')
    out.parent.mkdir(parents=True, exist_ok=True)
    report = {'finished_at': finished.isoformat(), 'dry_run': dry_run, 'rows': rows}
    out.with_suffix('.json').write_text(json.dumps(report, indent=2))
    out.with_suffix('.md').write_text(summary(rows, dry_run))
    return {'summary': str(out.with_suffix('.md')), 'rows': len(rows)}


def summary(rows, dry_run):
    def pr(row):
        result = row.get('pr') or {}
        if result.get('published'):
            return result['published'] + (' (draft)' if result.get('draft') else '')
        if result.get('closed'):
            return ('would close ' if result.get('dry_run') else 'closed ') + ', '.join(result['closed'])
        return 'would publish' if result.get('body') else ''

    lines = [f'# update-deps sweep{" (dry run)" if dry_run else ""}', '',
             '| Repository | Type | Verdict | Moved | PR |', '|---|---|---|---|---|']
    lines += [f'| {r["repo"]} | {r["type"]} | {r["verdict"]} | {r.get("changes", "")} | {pr(r)} |' for r in rows]
    regressed = [r for r in rows if r['verdict'] == 'regressed']
    if regressed:
        lines += ['', '## Needs a fix', '', 'Tests that passed before the update fail after it. Each is a draft PR.', '']
        lines += [f'- {r["repo"]} {r["type"]}: {pr(r)}' for r in regressed]
    edited = [r for r in rows if r['verdict'] == 'edited']
    if edited:
        lines += ['', '## Left alone', '', 'These PRs change more than the lock and manifest, so someone fixed them by hand.', '']
        lines += [f'- {r["repo"]} {r["type"]}' for r in edited]
    failed = [r for r in rows if r['verdict'] in ('error', 'update_failed')]
    if failed:
        lines += ['', '## Failed', '']
        lines += [f'- {r["repo"]} {r["type"]}: {r.get("error", "see " + r.get("logs", ""))}'.strip() for r in failed]
    open_advisories = [(r['repo'], a['text']) for r in rows for a in r.get('advisories', [])]
    if open_advisories:
        lines += ['', '## Open advisories', '']
        lines += [f'- {repo}: {text}' for repo, text in open_advisories]
    return '\n'.join(lines) + '\n'



def main():
    ap = argparse.ArgumentParser(description=__doc__)
    sub = ap.add_subparsers(dest='cmd', required=True)
    sub.add_parser('image', help='build the container image')
    s = sub.add_parser('sweep', help='update and publish every active repository of these owners')
    s.add_argument('owners', nargs='+')
    s.add_argument('--active-days', type=int, default=90, help='skip repositories with no push in this many days')
    s.add_argument('--skip', action='append', default=[], help='owner/name or owner/name:type')
    s.add_argument('--dry-run', action='store_true', help='update and test, but publish nothing')
    for name in ('plan', 'update', 'retest', 'publish'):
        s = sub.add_parser(name)
        s.add_argument('repo', help='owner/name on GitHub')
        if name != 'plan':
            s.add_argument('type', choices=TYPES)
        if name == 'update':
            s.add_argument('--hold', action='append', default=[], help='leave this dependency at its version')
        if name == 'publish':
            s.add_argument('--note', default='', help='a paragraph for the PR body')
            s.add_argument('--dry-run', action='store_true')
    a = ap.parse_args()
    if a.cmd == 'image':
        subprocess.run(['container', 'build', '--arch', 'arm64', '--tag', IMAGE, '--file',
                        str(IMAGE_DIR / 'Containerfile'), str(IMAGE_DIR)], check=True)
        return
    if hasattr(a, 'repo') and not REPO.match(a.repo):
        sys.exit('repo must be owner/name')
    try:
        result = {'plan': lambda: plan(a.repo),
                  'update': lambda: update(a.repo, a.type, a.hold),
                  'retest': lambda: retest(a.repo, a.type),
                  'publish': lambda: publish(a.repo, a.type, a.note, a.dry_run),
                  'sweep': lambda: sweep(a.owners, a.active_days, a.skip, a.dry_run)}[a.cmd]()
    except Failed as e:
        sys.exit(str(e))
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
