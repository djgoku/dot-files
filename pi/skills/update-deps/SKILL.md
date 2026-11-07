---
name: update-deps
description: Update a GitHub repository's dependencies (Hex, mise) in a container, run its tests before and after, and open or refresh one PR per dependency type for the owner to review. Use for "update deps", "dependency PRs", "security updates for <repo>", or keeping a list of repositories current.
---

# update-deps

One PR per repository per type, on branch `deps/<type>`. A rerun rebuilds the branch from the default branch and edits the same PR. Security advisories go in that PR.

Repository code runs only in the `update-deps` container. The container gets an export with no `.git` and no credentials. The host copies back only the files the type owns (`mix.lock`; `mise.toml`, `mise.lock`), and does every git and GitHub step itself. Keep it that way: never mount the worktree, a token, or `~/.ssh` into the container, and never run a command on the host inside `src/`.

## Run

```sh
UD=~/.pi/agent/skills/update-deps/scripts/ud
```

`ud` pins the Python and gh versions the script runs on.

Once per machine: `container system start`, then `$UD image` if `container image list` has no `update-deps:2`.

For each repository (`owner/name`):

1. `$UD plan owner/name` lists the types found on the default branch and the test command.
2. For each type: `$UD update owner/name <type>`. It applies updates first. Only when a file changed does it run the tests on the base and on the update. It writes `record.json`, whose `verdict` decides the next step:
   - `ok`: publish.
   - `no_changes`: publish. It closes a stale PR.
   - `update_failed`: setup or the update step failed. Do not publish. Report the tail of `logs/update.log`.
   - `regressed`: fix it (below), then publish.
3. `$UD publish owner/name <type>` commits, pushes `deps/<type>`, and opens or edits the PR. A `regressed` record opens as a draft. Add `--note "<one paragraph>"` when you changed code or held a package back.

Report each PR link with its verdict and the packages that moved. Files live in `~/.cache/update-deps/<owner>__<name>/<type>/`: `worktree/` (the PR), `src/` (the container's copy), `logs/`, `record.json`, `pr.md`.

## Fixing a regression

`regressed` means a test that passed before the update fails after it. Tests that already failed (for example, macOS-only tests in the Linux container) do not count. Leave them alone.

1. Read the new failures in `record.json` and the latest log in `logs/`.
2. Edit code in `worktree/` only. Do not skip, tag out, or delete tests. Do not add stand-ins for missing tools. Do not edit the owned lock or manifest files.
3. `$UD retest owner/name <type>` copies `worktree/` into the container and reruns the tests. Stop after three attempts.
4. If one package causes the break and the fix is not small, rerun `$UD update owner/name <type> --hold <package>`. This discards your edits. Say what was held and why in `--note`.
5. Publish whatever state you end in. A draft PR with the failure shown is a useful result.

## Many repositories

`$UD sweep djgoku hoaff-kc` runs every unarchived source repository pushed in the last 90 days (`--active-days`). It updates each type and publishes `ok`, `no_changes`, and `regressed` (as a draft PR). It leaves a `deps/<type>` branch alone once that branch changes files beyond the type's own, because someone fixed it by hand. It writes `~/.cache/update-deps/sweeps/<time>.json` and `.md`: one row per repository and type, then what needs a fix, what failed, and open advisories. uchi's dashboard shows the latest one. `--skip owner/name` or `--skip owner/name:type` leaves one out. `--dry-run` publishes nothing.

No agent is needed for a sweep. To fix a draft PR from "Needs a fix", run `$UD update` for it, fix it with the steps above, and publish.

uchi runs the sweep daily through Oban (`Uchi.Maintenance.DependencySweep`) when `UCHI_DEPENDENCY_SWEEP_ENABLED=true`.
