;;; emacs-env.el --- Manage dated Emacs environments -*- lexical-binding: t -*-

;; Author: Jonathan Carroll Otsuka
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, processes
;; URL: https://github.com/djgoku/dot-files

;;; Commentary:
;;
;; Manage versioned Emacs environments for testing emacs-overlay updates.
;; Each dated directory (dot-files-YYYY-MM-DD) is a complete, isolated config.
;;
;; Commands:
;;   M-x emacs-env-create  - Create new dated environment
;;   M-x emacs-env-launch  - Launch existing environment
;;   M-x emacs-env-status  - Show status for all environments
;;
;; Terminal/SSH selection:
;;   mise run emacs-env-select      - Select environment (GUI or terminal)
;;   mise run emacs-env-select --nw - Force terminal mode
;;
;; Customization:
;;   emacs-env-config-base  - Base directory (default: ~/.config)
;;   emacs-env-main-dir     - Fallback source dir (default: dot-files-main)
;;   emacs-env-git-branch   - Branch to track (default: build-emacs-overlay)
;;   emacs-env-git-remote   - Remote name (default: https)
;;
;; See README.org for detailed documentation, design rationale, and
;; the mise task for unified terminal/GUI selection.

;;; Code:

(require 'json)
(require 'seq)
(require 'vtable)
(require 'vc-git)

(declare-function magit-status "magit-status" (dir))

;;; Customization

;;;###autoload
(defgroup emacs-env nil
  "Manage dated Emacs environments."
  :group 'tools)

(defcustom emacs-env-config-base "~/.config"
  "Base directory where environment directories are stored."
  :type 'directory
  :group 'emacs-env)

(defcustom emacs-env-main-dir "dot-files-main"
  "Name of the fallback source directory."
  :type 'string
  :group 'emacs-env)

(defcustom emacs-env-git-branch "build-emacs-overlay"
  "Git branch to track for emacs-overlay updates."
  :type 'string
  :group 'emacs-env)

(defcustom emacs-env-git-remote "https"
  "Git remote name to fetch from."
  :type 'string
  :group 'emacs-env)

;;; Faces

;;;###autoload
(defface emacs-env-dirty-face
  '((t :inherit error))
  "Face for environments with uncommitted changes."
  :group 'emacs-env)

;;;###autoload
(defface emacs-env-clean-face
  '((t :inherit success))
  "Face for clean environments with no uncommitted changes."
  :group 'emacs-env)

;;;###autoload
(defface emacs-env-current-face
  '((t :inherit bold))
  "Face for the current environment."
  :group 'emacs-env)

;;; Helper Functions

(defmacro emacs-env--with-dir (dir &rest body)
  "Execute BODY with `default-directory' bound to expanded DIR."
  (declare (indent 1))
  `(let ((default-directory (expand-file-name ,dir)))
     ,@body))

(defun emacs-env--git-count-output (git-command &rest args)
  "Count lines of output from git GIT-COMMAND with ARGS.
Returns 0 if output is empty or command fails.
Logs a message on failure for debugging."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "git" nil t nil git-command args)))
      (if (zerop exit-code)
          (let ((output (string-trim (buffer-string))))
            (if (string-empty-p output)
                0
              (length (split-string output "\n" t))))
        (message "emacs-env: git %s failed (exit %d)" git-command exit-code)
        0))))

(defun emacs-env--proc-success-p (proc)
  "Return t if PROC exited successfully with status 0."
  (and (memq (process-status proc) '(exit signal))
       (zerop (process-exit-status proc))))

(defun emacs-env--git-staged-count ()
  "Return count of staged files in current directory."
  (emacs-env--git-count-output "diff" "--cached" "--name-only"))

(defun emacs-env--git-unstaged-count ()
  "Return count of unstaged modified files in current directory."
  (emacs-env--git-count-output "diff" "--name-only"))

(defun emacs-env--git-untracked-count ()
  "Return count of untracked files in current directory."
  (emacs-env--git-count-output "ls-files" "--others" "--exclude-standard"))

(defun emacs-env--list-dated-dirs ()
  "Return list of dot-files-YYYY-MM-DD directories, sorted newest first.
Scans `emacs-env-config-base' for directories matching the date pattern."
  (let* ((base (expand-file-name emacs-env-config-base))
         (pattern "^dot-files-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$")
         (dirs (directory-files base nil pattern)))
    (sort dirs #'string>)))

(defun emacs-env--all-dirs ()
  "Return list of all environment directories including main.
Returns dated directories (newest first) followed by main directory
if it exists."
  (let* ((base (expand-file-name emacs-env-config-base))
         (main-dir (expand-file-name emacs-env-main-dir base))
         (dated-dirs (emacs-env--list-dated-dirs)))
    (if (file-directory-p main-dir)
        (append dated-dirs (list emacs-env-main-dir))
      dated-dirs)))

(defun emacs-env--latest-source ()
  "Return the latest dated directory, or fallback to main.
Returns full path. Errors if no source can be found."
  (let ((dated-dirs (emacs-env--list-dated-dirs))
        (base (expand-file-name emacs-env-config-base)))
    (if dated-dirs
        (expand-file-name (car dated-dirs) base)
      (let ((main (expand-file-name emacs-env-main-dir base)))
        (if (file-directory-p main)
            main
          (error "No source found: need %s or a dated directory" emacs-env-main-dir))))))

(defun emacs-env--today-dirname ()
  "Return directory name for today: dot-files-YYYY-MM-DD."
  (format "dot-files-%s" (format-time-string "%Y-%m-%d")))

(defun emacs-env--update-mise-toml (file sha date-str)
  "Update mise.toml FILE with new SHA and DATE-STR in --name flags.
SHA is the new git revision to use.
DATE-STR is the date string (YYYY-MM-DD) to use in --name flags."
  (let* ((content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
         (original content))
    ;; Update the emacs-overlay package rev using regex
    ;; Format: "nix:github+djgoku/dot-files?dir=emacs-overlay&rev=SHA"
    (setq content
          (replace-regexp-in-string
           "\\(nix:github\\+djgoku/dot-files\\?dir=emacs-overlay&rev=\\)[^\"]+"
           (concat "\\1" sha)
           content))
    ;; Update --name flags: match emacs, emacs2, emacs-nw with optional date
    ;; Patterns: --name emacs, --name emacs-nw, --name emacs-2026-01-16, etc.
    (setq content
          (replace-regexp-in-string
           "--name \\(emacs2\\|emacs-nw\\|emacs\\)\\(-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)?"
           (format "--name \\1-%s" date-str)
           content))
    ;; Write back
    (with-temp-file file
      (insert content))
    ;; Log what changed
    (if (string= original content)
        (emacs-env--progress-log "WARNING: No changes made to mise.toml (regex didn't match)")
      (emacs-env--progress-log "Updated rev=%s, --name flags with %s"
                               (substring sha 0 (min 7 (length sha)))
                               date-str))))

(defun emacs-env--get-remote-sha (dir)
  "Fetch from remote and return SHA of tracked branch.
DIR is the git repository directory to operate in."
  (emacs-env--with-dir dir
    (message "Fetching from %s..." emacs-env-git-remote)
    (with-temp-buffer
      (let ((exit (call-process "git" nil t nil "fetch" emacs-env-git-remote)))
        (unless (zerop exit)
          (error "Git fetch failed (exit %d): %s" exit (string-trim (buffer-string))))))
    (with-temp-buffer
      (let ((exit (call-process "git" nil t nil "rev-parse"
                                (format "%s/%s" emacs-env-git-remote emacs-env-git-branch))))
        (let ((sha (string-trim (buffer-string))))
          (unless (and (zerop exit) (not (string-empty-p sha)))
            (error "Failed to get SHA for %s/%s: %s"
                   emacs-env-git-remote emacs-env-git-branch sha))
          (message "Got SHA: %s" (substring sha 0 (min 7 (length sha))))
          sha)))))

(defun emacs-env--copy-directory (src dest)
  "Copy directory SRC to DEST using Emacs `copy-directory'.
Signals `error' on failure."
  (message "Copying %s to %s..."
           (abbreviate-file-name src)
           (abbreviate-file-name dest))
  (condition-case err
      (progn
        (copy-directory (expand-file-name src)
                        (expand-file-name dest)
                        t nil t)
        (message "Copied to %s" (file-name-nondirectory dest)))
    (error
     (error "Failed to copy directory: %s" (error-message-string err)))))

(defun emacs-env--check-prerequisites ()
  "Verify required external tools are available.
Signals `user-error' if mise or git is not found."
  (unless (executable-find "mise")
    (user-error "mise not found in PATH. Install from https://mise.jdx.dev"))
  (unless (executable-find "git")
    (user-error "git not found in PATH")))

;;; Progress Buffer

(defvar emacs-env--progress-buffer "*emacs-env-create*"
  "Buffer name for showing environment creation progress.")

(defun emacs-env--progress-log (format-string &rest args)
  "Log message to progress buffer using FORMAT-STRING and ARGS.
Also displays the message in the echo area."
  (let ((msg (apply #'format format-string args))
        (buf (get-buffer-create emacs-env--progress-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert msg "\n")))
    (message "%s" msg)))

(defun emacs-env--log-failure (step-name event)
  "Log failure for STEP-NAME with EVENT to progress buffer."
  (emacs-env--progress-log "ERROR: %s failed: %s" step-name event)
  (emacs-env--progress-log "")
  (emacs-env--progress-log "=== CREATE FAILED ==="))

(define-derived-mode emacs-env-create-mode special-mode "Emacs-Env-Create"
  "Major mode for emacs-env environment creation progress."
  :group 'emacs-env)

(defun emacs-env--progress-run (command &optional dir)
  "Run COMMAND and log output to progress buffer.
COMMAND is a string to run via shell.
DIR is the directory to run in (defaults to `default-directory').
Returns the output as a string."
  (let* ((default-directory (or dir default-directory))
         (buf (get-buffer-create emacs-env--progress-buffer))
         (output (shell-command-to-string command)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "$ " command "\n")
        (insert output)
        (unless (string-suffix-p "\n" output)
          (insert "\n"))))
    output))

(defun emacs-env--emacs-type (arg)
  "Return Emacs type based on environment and ARG.
In terminal: always returns `emacs-nw' (ignore ARG).
In GUI: default to `emacs', prefix ARG switches to `emacs-nw'."
  (if (display-graphic-p)
      (if arg "emacs-nw" "emacs")
    "emacs-nw"))

(defun emacs-env--launch (dir emacs-type &optional capture-buffer)
  "Launch Emacs from DIR using EMACS-TYPE.
EMACS-TYPE is \"emacs\" or \"emacs-nw\".
If CAPTURE-BUFFER is non-nil, attach output filter to capture build output.
In GUI mode, launches process in background and returns the process.
In terminal mode, returns the command string to run manually."
  (if (display-graphic-p)
      (let* ((default-directory (expand-file-name dir))
             (command (list "mise" "run" emacs-type))
             (proc (make-process
                    :name "mise-emacs"
                    :buffer nil
                    :command command
                    :connection-type 'pipe)))
        (when capture-buffer
          (set-process-filter proc #'emacs-env--process-filter)
          (process-put proc 'progress-buffer capture-buffer))
        (set-process-query-on-exit-flag proc nil)
        proc)
    ;; Terminal mode: return command string
    (format "cd %s && mise run %s"
            (shell-quote-argument (expand-file-name dir))
            emacs-type)))

(defun emacs-env--launch-or-message (dir emacs-type &optional capture-buffer)
  "Launch Emacs or print command, with user feedback.
Calls `emacs-env--launch' and shows appropriate message.
Returns process in GUI mode, nil in terminal mode."
  (let ((result (emacs-env--launch dir emacs-type capture-buffer)))
    (if (processp result)
        (message "Launched from %s" (file-name-nondirectory dir))
      (message "Run: %s" result))
    result))

(defun emacs-env--current-env-p (dir-name)
  "Return non-nil if DIR-NAME matches the current Emacs environment.
Checks if `user-emacs-directory' contains DIR-NAME."
  (string-match-p (regexp-quote dir-name)
                  (expand-file-name user-emacs-directory)))

(defun emacs-env--git-repo-p (dir)
  "Return non-nil if DIR is a git repository."
  (file-directory-p (expand-file-name ".git" dir)))

(defun emacs-env--get-git-status (dir)
  "Return git status and info for DIR as a plist.
Returns (:staged N :unstaged N :untracked N :branch STR :SHA STR :date STR).
If DIR is not a git repository, returns placeholder values.
Uses vc-git for branch/SHA, raw git for file counts."
  (emacs-env--with-dir dir
    (if (emacs-env--git-repo-p default-directory)
        (let ((branch (string-trim
                       (shell-command-to-string
                        "git rev-parse --abbrev-ref HEAD 2>/dev/null")))
              (sha (or (vc-git-working-revision nil) "unknown")))
          (list :staged (emacs-env--git-staged-count)
                :unstaged (emacs-env--git-unstaged-count)
                :untracked (emacs-env--git-untracked-count)
                :branch (if (string-empty-p branch) "detached" branch)
                :sha (if (> (length sha) 7) (substring sha 0 7) sha)
                :date (string-trim
                       (shell-command-to-string
                        "git log -1 --format=%cs HEAD 2>/dev/null"))))
      ;; Not a git repo - return placeholder values
      (list :staged 0 :unstaged 0 :untracked 0
            :branch "N/A" :sha "N/A" :date "N/A"))))

(defun emacs-env--dirty-p (object)
  "Return non-nil if OBJECT has uncommitted changes.
OBJECT is a plist with :staged and :unstaged keys."
  (or (> (plist-get object :staged) 0)
      (> (plist-get object :unstaged) 0)))

(defun emacs-env--apply-faces (text object)
  "Apply appropriate faces to TEXT based on OBJECT state.
Returns TEXT with faces applied for dirty/clean and current status."
  (let* ((dirty (emacs-env--dirty-p object))
         (current (plist-get object :current))
         (face (cond
                ((and current dirty) '(emacs-env-current-face emacs-env-dirty-face))
                (current 'emacs-env-current-face)
                (dirty 'emacs-env-dirty-face)
                (t 'emacs-env-clean-face))))
    (propertize text 'face face)))

(defun emacs-env--status-data ()
  "Build status data for all dated directories plus main directory.
Returns list of plists with :index, :dir, :current, :branch, :sha, :date,
:staged, :unstaged, :untracked."
  (let ((base (expand-file-name emacs-env-config-base))
        (index 0))
    (mapcar
     (lambda (dir-name)
       (let* ((full-path (expand-file-name dir-name base))
              (status (emacs-env--get-git-status full-path)))
         (prog1
             (list :index (setq index (1+ index))
                   :dir dir-name
                   :current (emacs-env--current-env-p dir-name)
                   :branch (plist-get status :branch)
                   :sha (plist-get status :sha)
                   :date (plist-get status :date)
                   :staged (plist-get status :staged)
                   :unstaged (plist-get status :unstaged)
                   :untracked (plist-get status :untracked)))))
     (emacs-env--all-dirs))))

(defun emacs-env--status-open-magit (object)
  "Open magit-status (or vc-dir if unavailable) for the directory in OBJECT.
OBJECT is a plist with :dir key."
  (let* ((dir-name (plist-get object :dir))
         (full-path (expand-file-name dir-name emacs-env-config-base)))
    (if (fboundp 'magit-status)
        (magit-status full-path)
      (vc-dir full-path))))

(defun emacs-env--row-colors ()
  "Return row colors appropriate for current background mode.
Returns a list of two colors for alternating rows."
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      '("#2a2a2a" "#1e1e1e")
    '("#f0f0f0" "#ffffff")))

(defun emacs-env--branch-merged-p (dir)
  "Return non-nil if DIR's current branch is merged into main/master."
  (emacs-env--with-dir dir
    (let* ((current-branch (string-trim
                            (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
           (main-branch (cond
                         ((= 0 (call-process "git" nil nil nil "rev-parse" "--verify" "main"))
                          "main")
                         ((= 0 (call-process "git" nil nil nil "rev-parse" "--verify" "master"))
                          "master")
                         (t nil))))
      (when (and main-branch (not (string= current-branch main-branch)))
        (let ((merge-base (string-trim
                           (shell-command-to-string
                            (format "git merge-base %s %s"
                                    (shell-quote-argument main-branch)
                                    (shell-quote-argument current-branch)))))
              (branch-head (string-trim
                            (shell-command-to-string
                             (format "git rev-parse %s"
                                     (shell-quote-argument current-branch))))))
          (string= merge-base branch-head))))))

(defun emacs-env--status-delete-env (object)
  "Delete the environment directory in OBJECT with safety confirmations.
OBJECT is a plist with :dir, :staged, :unstaged, :untracked keys.
Requires double confirmation if there are uncommitted changes or
if the current branch hasn't been merged into main."
  (let* ((dir-name (plist-get object :dir))
         (full-path (expand-file-name dir-name emacs-env-config-base))
         (staged (plist-get object :staged))
         (unstaged (plist-get object :unstaged))
         (_untracked (plist-get object :untracked))
         (has-changes (or (> staged 0) (> unstaged 0)))
         (branch-merged (emacs-env--branch-merged-p full-path))
         (needs-double-confirm (or has-changes (not branch-merged)))
         (warnings nil))
    ;; Build warning messages
    (when has-changes
      (push (format "Has %d staged and %d unstaged changes!"
                    staged unstaged)
            warnings))
    (when (not branch-merged)
      (push "Branch has NOT been merged into main!" warnings))
    ;; First confirmation
    (when (yes-or-no-p
           (if warnings
               (format "DELETE %s?\nWARNINGS:\n  - %s\n"
                       dir-name (string-join warnings "\n  - "))
             (format "Delete %s? " dir-name)))
      ;; Second confirmation if needed
      (when (or (not needs-double-confirm)
                (yes-or-no-p
                 (format "REALLY delete %s? This cannot be undone! " dir-name)))
        (delete-directory full-path t t)
        (message "Deleted %s" dir-name)
        (emacs-env-status)))))

(defun emacs-env--status-launch-env (object)
  "Launch Emacs from the environment in OBJECT.
OBJECT is a plist with :dir key."
  (let* ((dir-name (plist-get object :dir))
         (full-path (expand-file-name dir-name emacs-env-config-base))
         (emacs-type (emacs-env--emacs-type nil)))
    (emacs-env--launch-or-message full-path emacs-type)))

(defun emacs-env-status-launch-at-point ()
  "Launch the environment at point."
  (interactive)
  (let ((object (vtable-current-object)))
    (if object
        (emacs-env--status-launch-env object)
      (user-error "No environment at point"))))

(defvar emacs-env-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'emacs-env-status)
    (define-key map "l" #'emacs-env-status-launch-at-point)
    (define-key map "D" #'emacs-env-status-delete-at-point)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `emacs-env-status' buffer.
\\<emacs-env-status-mode-map>
\\[emacs-env-status] - Refresh the status display
\\[emacs-env-status-launch-at-point] - Launch environment at point
\\[emacs-env-status-delete-at-point] - Delete environment at point (capital D)
\\[quit-window] - Close the buffer and window")

(define-derived-mode emacs-env-status-mode special-mode "Emacs-Env"
  "Major mode for emacs-env status buffer."
  :group 'emacs-env
  (display-line-numbers-mode -1))

(defun emacs-env-status-delete-at-point ()
  "Delete the environment at point."
  (interactive)
  (let ((object (vtable-current-object)))
    (if object
        (emacs-env--status-delete-env object)
      (user-error "No environment at point"))))

;;; Async Environment Creation Steps

(defun emacs-env--process-filter (proc output)
  "Filter function to insert OUTPUT from PROC into progress buffer."
  (when-let* ((buf (process-get proc 'progress-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert output))))))

(defun emacs-env--create-step-copy (source today-dir today-name date-str arg buf)
  "Step 1: Copy SOURCE to TODAY-DIR synchronously.
Chains to step 2 on completion."
  (let ((src (expand-file-name source))
        (dest (expand-file-name today-dir)))
    (emacs-env--progress-log "$ copy-directory %s %s"
                             (abbreviate-file-name src)
                             (abbreviate-file-name dest))
    (condition-case err
        (progn
          (emacs-env--copy-directory src dest)
          (emacs-env--progress-log "")
          (emacs-env--create-step-fetch today-dir today-name date-str arg buf))
      (error
       (emacs-env--log-failure "Copy" (error-message-string err))))))

(defun emacs-env--create-step-fetch (today-dir today-name date-str arg buf)
  "Step 2: Git fetch from remote asynchronously.
Chains to step 3 on completion."
  (emacs-env--progress-log "[2/4] Fetching latest emacs-overlay SHA...")
  (let* ((default-directory today-dir)
         (command (list "git" "fetch" emacs-env-git-remote)))
    (emacs-env--progress-log "$ %s" (string-join command " "))
    (let ((proc (make-process
                 :name "emacs-env-fetch"
                 :buffer nil
                 :command command
                 :filter #'emacs-env--process-filter
                 :stderr buf
                 :sentinel (lambda (proc event)
                             (when (memq (process-status proc) '(exit signal))
                               (if (zerop (process-exit-status proc))
                                   (emacs-env--create-step-sha
                                    today-dir today-name date-str arg buf)
                                 (emacs-env--log-failure "Git fetch" (string-trim event))))))))
      (process-put proc 'progress-buffer buf)
      proc)))

(defun emacs-env--create-step-sha (today-dir today-name date-str arg buf)
  "Step 3: Get SHA and update mise.toml, then trust and launch."
  (let ((default-directory today-dir))
    (with-temp-buffer
      (let ((exit (call-process "git" nil t nil "rev-parse"
                                (format "%s/%s" emacs-env-git-remote emacs-env-git-branch))))
        (let ((sha (string-trim (buffer-string))))
          (if (or (not (zerop exit)) (string-empty-p sha))
              (emacs-env--log-failure "SHA lookup" (format "exit %d: %s" exit sha))
            (emacs-env--progress-log "SHA: %s" (substring sha 0 (min 7 (length sha))))
            (emacs-env--progress-log "")
            ;; Update mise.toml
            (emacs-env--progress-log "[3/4] Updating mise.toml...")
            (emacs-env--update-mise-toml
             (expand-file-name "mise.toml" today-dir)
             sha
             date-str)
            (emacs-env--progress-log "")
            ;; Trust mise config then launch
            (emacs-env--create-step-trust today-dir today-name arg buf)))))))

(defun emacs-env--create-step-trust (today-dir today-name arg buf)
  "Step 4: Run mise trust to allow mise.toml execution."
  (emacs-env--progress-log "[4/4] Running mise trust...")
  (let* ((default-directory today-dir)
         (command (list "mise" "trust")))
    (emacs-env--progress-log "$ %s" (string-join command " "))
    (let ((proc (make-process
                 :name "emacs-env-trust"
                 :buffer nil
                 :command command
                 :filter #'emacs-env--process-filter
                 :stderr buf
                 :sentinel (lambda (proc event)
                             (when (memq (process-status proc) '(exit signal))
                               (if (zerop (process-exit-status proc))
                                   (progn
                                     (emacs-env--progress-log "")
                                     (emacs-env--create-step-launch today-dir today-name arg buf))
                                 (emacs-env--log-failure "mise trust" (string-trim event))))))))
      (process-put proc 'progress-buffer buf)
      proc)))

(defun emacs-env--create-step-launch (today-dir today-name arg buf)
  "Final step: Launch Emacs from TODAY-DIR.
TODAY-NAME is used for logging. ARG controls emacs type.
BUF is the progress buffer for output capture."
  (let ((emacs-type (emacs-env--emacs-type arg)))
    (emacs-env--progress-log "=== Launching %s ===" today-name)
    (if (display-graphic-p)
        (progn
          (emacs-env--progress-log "$ mise run %s" emacs-type)
          (emacs-env--progress-log "")
          (emacs-env--progress-log "(Output below - build may take several minutes)")
          (emacs-env--progress-log "")
          (let ((proc (emacs-env--launch today-dir emacs-type buf)))
            (set-process-sentinel
             proc
             (lambda (proc event)
               (when (memq (process-status proc) '(exit signal))
                 (emacs-env--progress-log "")
                 (emacs-env--progress-log "=== mise run finished: %s (exit %d) ==="
                                          (string-trim event)
                                          (process-exit-status proc)))))
            proc))
      ;; Terminal mode
      (let ((cmd (emacs-env--launch today-dir emacs-type)))
        (emacs-env--progress-log "Run: %s" cmd)))))

;;; Interactive Commands

;;;###autoload
(defun emacs-env-create (arg)
  "Create a new dated Emacs environment and launch it.
Copies from the latest existing dated directory, or falls back to
`emacs-env-main-dir' if no dated directories exist.
Updates mise.toml with latest emacs-overlay SHA and launches Emacs.

Runs asynchronously - progress is displayed in the *emacs-env-create* buffer."
  (interactive "P")
  (emacs-env--check-prerequisites)
  (let* ((base (expand-file-name emacs-env-config-base))
         (today-name (emacs-env--today-dirname))
         (today-dir (expand-file-name today-name base))
         (date-str (format-time-string "%Y-%m-%d"))
         (buf (get-buffer-create emacs-env--progress-buffer)))
    ;; Setup and display progress buffer
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (emacs-env-create-mode))
    (display-buffer buf)
    ;; Check if today's directory already exists
    (when (file-directory-p today-dir)
      (emacs-env--progress-log "ERROR: Environment for today already exists: %s" today-dir)
      (user-error "Environment for today already exists: %s" today-dir))
    ;; Find source and start async chain
    (let ((source (emacs-env--latest-source)))
      (emacs-env--progress-log "=== Creating %s ===" today-name)
      (emacs-env--progress-log "Source: %s" (file-name-nondirectory source))
      (emacs-env--progress-log "")
      ;; Step 1: Copy directory (async)
      (emacs-env--progress-log "[1/4] Copying directory...")
      (emacs-env--create-step-copy
       source today-dir today-name date-str arg buf))))

;;;###autoload
(defun emacs-env-launch (arg)
  "Launch Emacs from an existing dated environment.
Presents a list of available dated environments for selection
and launches Emacs using mise from the selected directory.

In GUI mode: launches in background.
In terminal mode: prints the command to run (can't spawn terminal Emacs).

With prefix ARG in GUI mode, launch terminal Emacs (-nw)."
  (interactive "P")
  (emacs-env--check-prerequisites)
  (let ((all-dirs (emacs-env--all-dirs)))
    (unless all-dirs
      (user-error "No environments found in %s. Run emacs-env-create first" emacs-env-config-base))
    (let* ((selection (completing-read "Select environment: " all-dirs nil t))
           (dir (expand-file-name selection emacs-env-config-base))
           (emacs-type (emacs-env--emacs-type arg)))
      (emacs-env--launch-or-message dir emacs-type))))

(defvar emacs-env-select-cmd-file
  (expand-file-name "emacs-env-cmd" temporary-file-directory)
  "Temp file for mise task to read the selected command.")

(defun emacs-env-unified-select (mode)
  "Select and launch an Emacs environment.
MODE is \"gui\" or \"terminal\", passed from the mise task.
The mise task handles SSH/--nw detection."
  (let ((orig-default-fg (face-attribute 'default :foreground))
        (orig-default-bg (face-attribute 'default :background))
        (orig-prompt-fg (face-attribute 'minibuffer-prompt :foreground))
        (orig-prompt-weight (face-attribute 'minibuffer-prompt :weight)))
    (unwind-protect
        (progn
          (set-face-attribute 'default nil :foreground "white" :background "black")
          (set-face-attribute 'minibuffer-prompt nil :foreground "cyan" :weight 'bold)
          (let ((all-dirs (emacs-env--all-dirs)))
            (unless all-dirs
              (delete-file emacs-env-select-cmd-file t)
              (kill-emacs 1))
            (let* ((newest (car all-dirs))
                   (selection (completing-read
                               (format "Select environment [%s]: " newest)
                               all-dirs nil t nil nil newest))
                   (dir (expand-file-name selection emacs-env-config-base)))
              ;; Write command for mise task to exec
              (let ((cmd (if (string= mode "gui")
                             (format "cd %s && mise run emacs &"
                                     (shell-quote-argument dir))
                           (format "cd %s && exec mise run emacs --nw"
                                   (shell-quote-argument dir)))))
                (with-temp-file emacs-env-select-cmd-file
                  (insert cmd))
                (kill-emacs 0)))))
      ;; Restore faces on C-g or error
      (set-face-attribute 'default nil :foreground orig-default-fg :background orig-default-bg)
      (set-face-attribute 'minibuffer-prompt nil :foreground orig-prompt-fg :weight orig-prompt-weight))))

;;;###autoload
(defun emacs-env-status ()
  "Display git status for all dated Emacs environments.
Shows a vtable with columns: Directory, current indicator, SHA, Date,
Staged, Unstaged, Untracked, and Branch.

Keybindings:
  RET - Open git status (magit if available, else vc-dir)
  l   - Launch environment at point
  g   - Refresh display (re-fetch status, update theme colors)
  D   - Delete environment (double confirm if dirty or unmerged)
  q   - Kill the buffer and close the window"
  (interactive)
  (message "Loading environment status...")
  (let* ((buf (get-buffer-create "*Emacs Environments*"))
         (data (emacs-env--status-data))
         (total (length data))
         (dirty (seq-count #'emacs-env--dirty-p data)))
    (with-current-buffer buf
      (emacs-env-status-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (make-vtable
         :use-header-line t
         :columns '((:name "#" :width 2)
                    (:name "Directory" :width 25)
                    (:name "*" :width 1)
                    (:name "SHA" :width 7)
                    (:name "Date" :width 10)
                    (:name "Staged" :width 6)
                    (:name "Unstaged" :width 8)
                    (:name "Untracked" :width 9)
                    (:name "Branch"))
         :objects data
         :getter (lambda (object column vtable)
                   (let ((value (pcase (vtable-column vtable column)
                                  ("#" (number-to-string (plist-get object :index)))
                                  ("Directory" (plist-get object :dir))
                                  ("*" (if (plist-get object :current) "*" ""))
                                  ("SHA" (plist-get object :sha))
                                  ("Date" (plist-get object :date))
                                  ("Staged" (number-to-string (plist-get object :staged)))
                                  ("Unstaged" (number-to-string (plist-get object :unstaged)))
                                  ("Untracked" (number-to-string (plist-get object :untracked)))
                                  ("Branch" (plist-get object :branch)))))
                     (emacs-env--apply-faces value object)))
         :actions '("RET" emacs-env--status-open-magit
                    "l" emacs-env--status-launch-env
                    "D" emacs-env--status-delete-env)
         :keymap emacs-env-status-mode-map
         :divider-width 1
         :row-colors (emacs-env--row-colors))
        (goto-char (point-max))
        (insert (format "\n %d environments: %d dirty, %d clean\n\n RET: git status  l: launch  D: delete  g: refresh  q: quit"
                        total dirty (- total dirty)))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(provide 'emacs-env)
;;; emacs-env.el ends here
