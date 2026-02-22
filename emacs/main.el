;;; main config  -*- lexical-binding: t; -*-
;;; create directories for auto-saves, backups and locks
(dolist (directory '("auto-saves" "backups" "locks"))
  (unless (file-directory-p (concat user-emacs-directory directory))
    (mkdir (concat user-emacs-directory directory))))
;;; functions
;;;; johnny5-magit-clone - magit-clone integration
;; Integrates with magit-clone via advice to provide:
;; - Automatic directory calculation: ~/dev/{site}/{org-or-user}/{repo}
;; - "git clone " prefix stripping when pasting full clone commands
;; - Jump to magit-status if repository already exists locally
;;
;; Set johnny5-magit-clone-default-directory-remove-domain to t to use
;; "github" instead of "github.com" in the directory path.

(setq johnny5-magit-clone-default-directory-remove-domain t)

(defun johnny5-magit-clone-default-directory (git-clone-url)
  "Calculate the parent directory for cloning a git repository.
Returns a path like ~/dev/github/djgoku/ for git@github.com:djgoku/repo.git"
  (cond ((string-prefix-p "git@" git-clone-url)
         (johnny5-git-url-parse (cadr (split-string git-clone-url "git@"))))
        ((string-prefix-p "https://" git-clone-url)
         (johnny5-git-https-url-parse git-clone-url))))

(defun johnny5-git-https-url-parse (url-string)
  "Parse HTTPS git URL and return the clone directory path.
Converts URL to a format suitable for `johnny5-git-url-parse'."
  (let* ((url (url-generic-parse-url url-string))
         (host (url-host url))
         (user-or-workspace-and-repo (substring (url-filename url) 1)))
    (johnny5-git-url-parse (format "%s:%s/" host user-or-workspace-and-repo))))

(defun johnny5-git-url-parse (git-clone-url)
  "Parse git URL (host:user/repo format) and return the clone directory path.
Returns ~/dev/{site}/{user}/ or ~/dev/{site-without-domain}/{user}/
depending on `johnny5-magit-clone-default-directory-remove-domain'."
  (let* ((git-url-path-and-repo-split (string-split git-clone-url ":"))
         (uri (car git-url-path-and-repo-split))
         (uri-without-domain (car (split-string uri "\\.")))
         (user-or-workspace-and-repo-split (split-string (cadr git-url-path-and-repo-split) "/"))
         (user-or-workspace (car user-or-workspace-and-repo-split)))
    (if johnny5-magit-clone-default-directory-remove-domain
        (format "~/dev/%s/%s/" uri-without-domain user-or-workspace)
      (format "~/dev/%s/%s/" uri user-or-workspace))))

;;;;; advice: strip "git clone " prefix and allow pasting full commands
(defun johnny5-magit-clone-clean-url (url)
  "Strip 'git clone ' prefix if present.
Allows pasting full clone commands like 'git clone git@github.com:user/repo.git'."
  (if (and url (string-prefix-p "git clone " url))
      (substring url 10)
    url))

(defun johnny5-magit-clone-read-repository ()
  "Wrap magit-clone-read-repository to allow pasting 'git clone URL' commands.
Magit's default uses magit-read-string-ns which rejects whitespace."
  (magit-read-char-case "Clone from " nil
    (?u "[u]rl or name"
        (let* ((input (read-string "Clone from url or name: "))
               (str (johnny5-magit-clone-clean-url (string-trim input))))
          (if (string-match-p "\\(://\\|@\\)" str)
              str
            (magit-clone--name-to-url str))))
    (?p "[p]ath"
        (magit-convert-filename-for-git
         (read-directory-name "Clone repository: ")))
    (?l "[l]ocal url"
        (concat "file://"
                (magit-convert-filename-for-git
                 (read-directory-name "Clone repository: file://"))))
    (?b "[b]undle"
        (magit-convert-filename-for-git
         (read-file-name "Clone from bundle: ")))))

;; :override intentionally replaces the original — our implementation is a
;; complete replacement that adds "git clone " prefix stripping.
(advice-add 'magit-clone-read-repository :override #'johnny5-magit-clone-read-repository)

;;;;; advice: jump to existing repos
(defun johnny5-magit-clone-maybe-status ()
  "If repo already exists locally, open magit-status instead of cloning."
  (let* ((repo (johnny5-magit-clone-read-repository))
         (repo-name (magit-clone--url-to-name repo))
         (default-dir (johnny5-magit-clone-default-directory repo))
         (full-path (and default-dir repo-name
                         (expand-file-name repo-name default-dir))))
    (if (and full-path (file-directory-p full-path))
        (progn
          (magit-status full-path)
          ;; user-error aborts the clone after opening magit-status
          (user-error "Repository already cloned at %s, opened magit-status" full-path))
      (list repo
            (read-directory-name "Clone to: " default-dir nil nil repo-name)
            (transient-args 'magit-clone)))))

;; :override intentionally replaces the original — adds existing-repo detection.
(advice-add 'magit-clone-read-args :override #'johnny5-magit-clone-maybe-status)

;;;;; tests
(ert-deftest test-johnny5-magit-clone-clean-url ()
  "Test that 'git clone ' prefix is stripped."
  (should (equal (johnny5-magit-clone-clean-url "git clone git@github.com:djgoku/melpa.git")
                 "git@github.com:djgoku/melpa.git"))
  (should (equal (johnny5-magit-clone-clean-url "git@github.com:djgoku/melpa.git")
                 "git@github.com:djgoku/melpa.git"))
  (should (equal (johnny5-magit-clone-clean-url "https://github.com/djgoku/melpa.git")
                 "https://github.com/djgoku/melpa.git")))

(ert-deftest test-johnny5-magit-clone-default-directory ()
  "Tests that we return the correct magit-clone-default-directory."
  (setq johnny5-magit-clone-default-directory-remove-domain nil)
  (should (equal (johnny5-magit-clone-default-directory "git@github.com:djgoku/melpa.git") "~/dev/github.com/djgoku/"))
  (should (equal (johnny5-magit-clone-default-directory "https://github.com/djgoku/melpa.git") "~/dev/github.com/djgoku/"))
  (should (equal (johnny5-magit-clone-default-directory "git@bitbucket.org:johnny-5-is-alive/dot-files.git") "~/dev/bitbucket.org/johnny-5-is-alive/"))
  (should (equal (johnny5-magit-clone-default-directory "https://djgoku@bitbucket.org/johnny-5-is-alive/dot-files.git") "~/dev/bitbucket.org/johnny-5-is-alive/")))

(ert-deftest test-johnny5-magit-clone-default-directory-remove-domain ()
  "Tests that we return the correct magit-clone-default-directory."
  (setq johnny5-magit-clone-default-directory-remove-domain t)
  (should (equal (johnny5-magit-clone-default-directory "git@github.com:djgoku/melpa.git") "~/dev/github/djgoku/"))
  (should (equal (johnny5-magit-clone-default-directory "https://github.com/djgoku/melpa.git") "~/dev/github/djgoku/"))
  (should (equal (johnny5-magit-clone-default-directory "git@bitbucket.org:johnny-5-is-alive/dot-files.git") "~/dev/bitbucket/johnny-5-is-alive/"))
  (should (equal (johnny5-magit-clone-default-directory "https://djgoku@bitbucket.org/johnny-5-is-alive/dot-files.git") "~/dev/bitbucket/johnny-5-is-alive/")))
;;;; unfill-paragraph
;; It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(keymap-set global-map "M-Q" #'unfill-paragraph)
;;;; johnny5-test-executable
(defun johnny5-test-executable (command)
  "Test we can execute a executable.
Return nil if test execution fails."
  (condition-case err
      (call-process command)
    (error (warn (format "Testing executable '%s' failed with '%s'" command err)) nil)))
;;; transient
(use-package transient)
;;; string-inflection
(use-package string-inflection)
;;; powerthesaurus
(use-package powerthesaurus)
;;; nix-mode
(use-package nix-mode)
;;; markdown-mode
(use-package markdown-mode)
;;; json-mode
(use-package json-mode)
;;; jeison
(use-package jeison)
;;; toml
(use-package toml-ts-mode
  :ensure nil
  :config
  (setq toml-ts-mode-indent-offset 0))
;;; pkl
(use-package pkl-mode)
;;; ipcalc
(use-package ipcalc)
;;; htmlize
(use-package htmlize)
;;; elfeed
(use-package elfeed)
;;; docker
(use-package docker)
;;; dired
;;;; dired-preview
(use-package dired-preview)
;;;; dired-x
;; adds F to dired buffers 'dired-do-find-marked-files'
(require 'dired-x)
;;; define-word
(use-package define-word)
;;; kubel
(use-package kubel
  :preface (setq-default kubectl-command (executable-find "kubectl"))
  :if kubectl-command)
;;; consult
(use-package consult
  :if johnny--maybe-ripgrep-executable
  :bind (("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("s-f" . consult-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq consult-ripgrep-args (concat johnny--maybe-ripgrep-executable " --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip" ripgrep--extra-args))
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        consult-async-min-input 1)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")) ;; "C-+"
;;;; consult-dir
(use-package consult-dir)
;;; marginalia
(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))
;;; embark
(use-package embark
  :bind (("M-a" . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (defun johnny5-git-link-homepage (file)
    (interactive "f")
    (let ((default-directory (project-root (project-current nil file))))
      (browse-url (call-interactively #'git-link-homepage))))
  (defvar-keymap embark-project-map
    :doc "Keymap for Embark project actions."
    :parent embark-file-map
    "RET" #'project-switch-project
    "b" #'johnny5-git-link-homepage)

  (add-to-list 'embark-keymap-alist '(project-file embark-project-map))
  (defun embark--keep-project-type(full-path)
    (cons 'project-file (cdr full-path)))

  (advice-add 'embark--project-file-full-path :filter-return #'embark--keep-project-type)
  (with-eval-after-load 'jinx
    (embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
    (add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
    (add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
    (add-to-list 'embark-repeat-actions #'jinx-next)
    (add-to-list 'embark-repeat-actions #'jinx-previous)
    (add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark--ignore-target))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))
;;; orderless
(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;;; vertico
(use-package vertico
  :init
  (vertico-mode))
;;; savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))
;;; ripgrep
(defun johnny--setup-ripgrep ()
  "Determine if ripgrep 'rg' or ripgrep-all 'rga' is installed"
  (let ((maybe-executable (or (executable-find "rga") (executable-find "rg"))))
    (if (and maybe-executable (johnny5-test-executable maybe-executable))
        (setq johnny--maybe-ripgrep-executable maybe-executable)
      (warn "ripgrep 'rg' nor ripgrep-all 'rga' were found in PATH"))))
(johnny--setup-ripgrep)
;;;; affe
(use-package affe
  :if johnny--maybe-ripgrep-executable
  :after orderless
  :config
  (setq affe-count 10000)
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight
        affe-find-command (concat johnny--maybe-ripgrep-executable " --color=never --files --hidden" ripgrep--extra-args)
        affe-grep-command (concat johnny--maybe-ripgrep-executable " --null --color=never --max-columns=1000 --no-heading --line-number -v ^$ --hidden" ripgrep--extra-args))
  :bind (("M-s a f" . affe-find)
         ("M-s a g" . affe-grep)))
;;;; deadgrep
(use-package deadgrep
  :if johnny--maybe-ripgrep-executable
  :config
  (setq deadgrep-executable johnny--maybe-ripgrep-executable))
;;;; rg
(use-package rg
  :if johnny--maybe-ripgrep-executable
  :config
  (rg-enable-default-bindings)
  (rg-enable-menu)
  (setq rg-executable johnny--maybe-ripgrep-executable))
;;;; wgrep
(use-package wgrep
  :config (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "r"))
;;; magit
(use-package magit
  :after project
  :init
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m"))
  (setq magit-clone-set-remote.pushDefault t
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1
        magit-bury-buffer-function #'magit-restore-window-configuration
        ;; setting to level 5 for gpg siging
        transient-default-level 5
        magit-revision-headers-format "Author:     %aN <%aE>\nAuthorDate: %ad\nCommit:     %cN <%cE>\nCommitDate: %cd\nSigned:\n\n%GG\n"
        magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                                 ("Version" 25 magit-repolist-column-version ((:sort magit-repolist-version<)))
                                 ("B<U" 3 magit-repolist-column-unpulled-from-upstream ((:right-align t) (:sort <)))
                                 ("B>U" 3 magit-repolist-column-unpushed-to-upstream ((:right-align t) (:sort <)))
                                 ("Path" 75 magit-repolist-column-path nil)
                                 ("Branch" 99 magit-repolist-column-branch)))
  :bind (("C-x g" . magit-project-status)))
;;;; project-populate-magit-repository-directories
(defvar project-populate-magit-repository-directories-exclude-contains
  '("elpaca/repos" "/deps/")
  "Patterns to exclude using `string-match-p'.")
(defun project-populate-magit-repository-directories ()
  "Populate `magit-repository-directories' from project.el's known projects."
  (interactive)
  (let (repository-directories)
    (dolist (dir (project-known-project-roots) repository-directories)
      (cond ((string-prefix-p "/ssh" dir) (message (format "excluded '/ssh' git dir: %s" dir)))
            ((seq-some (lambda (pattern) (string-match-p (regexp-quote pattern) dir)) project-populate-magit-repository-directories-exclude-contains) (message (format "excluded by contains git dir: %s" dir)))
            ((not (file-directory-p dir)) (message (format "excluded git dir as it doesn't exist: %s" dir)))
            (t (message (format "git dir: %s" dir)) (push (cons dir 0) repository-directories))))
    (setq magit-repository-directories repository-directories)))
;;; git-link
(use-package git-link)
;;; git-timemachine
(use-package git-timemachine)
;;; smerge
;;;; smerge-nav
;; Provides a simple navigation mode for smerge conflicts.
;; When a file with conflict markers is opened, smerge-nav-mode activates
;; automatically, providing single-key bindings for resolving conflicts.
;; Press 'q' to exit and edit manually.

(defvar-local smerge-nav-mode--old-header-line nil
  "Previous header-line-format before smerge-nav-mode was enabled.")

(defun smerge-nav-mode-quit ()
  "Exit smerge-nav-mode."
  (interactive)
  (smerge-nav-mode -1))

(defun smerge-nav--goto-first-conflict ()
  "Move to first conflict, or message if none found."
  (goto-char (point-min))
  (condition-case nil
      (smerge-next)
    (error (message "No conflicts found in buffer"))))

(defvar smerge-nav-mode-map
  (define-keymap
    :suppress t
    "n" #'smerge-next
    "p" #'smerge-prev
    "u" #'smerge-keep-upper
    "l" #'smerge-keep-lower
    "b" #'smerge-keep-base
    "a" #'smerge-keep-all
    "RET" #'smerge-keep-current
    "E" #'smerge-ediff
    "r" #'smerge-resolve
    "q" #'smerge-nav-mode-quit)
  "Keymap for `smerge-nav-mode'.")

(define-minor-mode smerge-nav-mode
  "Navigate smerge conflicts with single-key bindings.
Press 'q' to exit and edit manually."
  :lighter " SMergeNav"
  :keymap smerge-nav-mode-map
  (if smerge-nav-mode
      (progn
        (setq-local smerge-nav-mode--old-header-line header-line-format)
        (setq-local header-line-format
                    (propertize
                     " n:next  p:prev  u:upper  l:lower  b:base  a:all  RET:current  r:resolve  E:ediff  q:quit"
                     'face 'mode-line-highlight))
        ;; Defer goto so it runs after save-place restores position
        (run-with-timer 0 nil #'smerge-nav--goto-first-conflict))
    (setq-local header-line-format smerge-nav-mode--old-header-line)))

(defun smerge-nav--on-smerge-mode ()
  "Enable or disable smerge-nav-mode when smerge is enabled or disabled."
  (if smerge-mode
      (smerge-nav-mode 1)
    (smerge-nav-mode -1)))

(defun smerge-nav--maybe-enable-smerge ()
  "Enable smerge-mode if the buffer contains conflict markers."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (require 'smerge-mode)
      (smerge-mode 1))))
;;;; smerge
(use-package smerge-mode
  :ensure nil
  :hook
  (find-file . smerge-nav--maybe-enable-smerge)
  (smerge-mode . smerge-nav--on-smerge-mode))
;;; org
;; (org-narrow-to-subtree) C-x n s
;; (widen) C-x n w
;; C-u 0 M-x org-babel-remove-result-one-or-many will remove all result blocks in a buffer
;; M-x org-babel-execute-buffer will execute all src blocks in a buffer
(use-package org
  :ensure nil
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config (setq org-confirm-babel-evaluate nil
                org-src-fontify-natively t
                org-src-preserve-indentation t
                org-edit-src-content-indentation t
                org-log-into-drawer t
                ;; NOT-RETAINED - will not continue in the application process
                org-todo-keywords '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "WAIT(w!)" "APPLIED(a!)" "|" "DONE(d@)"
                                              "CANCELED(@)" "WITHDRAWN(@)" "FILLED" "HIRED" "NOT-RETAINED(n@)"))
                org-refile-targets '((org-agenda-files :maxlevel . 2))
                org-agenda-files '("~/dev/org" "~/dev/notes")
                ;; org-agenda-include-inactive-timestamps 't
                org-log-refile 'note
                org-refile-use-outline-path 'file
                org-outline-path-complete-in-steps nil
                org-refile-allow-creating-parent-nodes 'confirm
                org-startup-indented t)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (setq org-capture-templates '(("t" "Todo" entry (file "~/dev/org/inbox.org")
                                 "* TODO %? [/]\n:PROPERTIES:\n:Created: %U\nEND:\n %i\n %a")
                                ("c" "Cookbook" entry (file "~/dev/org/cookbook.org")
                                 "%(org-chef-get-recipe-from-url)"
                                 :empty-lines 1)
                                ("m" "Manual Cookbook" entry (file "~/dev/org/cookbook.org")
                                 "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))
  (require 'ox-md nil t)
  (org-babel-do-load-languages 'org-babel-load-languages '((awk . t)
                                                           (emacs-lisp . t)
                                                           (eshell . t)
                                                           ;; (elixir . t)
                                                           (perl . t)
                                                           (python . t)
                                                           ;; (mermaid . t)
                                                           (sed . t)
                                                           (shell . t)
                                                           (sql . t)
                                                           (sqlite . t)
                                                           (ruby . t)))
  :mode (("\\.org$" . org-mode)))
;;;; ob-async
;; just add :async to any org babel src blocks!
(use-package ob-async
  :after org)
;;;; org-chef
(use-package org-chef)
;;;; org-contrib
(use-package org-contrib)
;;;; ob-elixir
(use-package ob-elixir)
;;;; orgit
(use-package orgit)
;;;; org-jira
(use-package org-jira)
;;;; ox-jira
(use-package ox-jira)
;;;; org-ql
(use-package org-ql)
;;; denote
(use-package denote
  :config
  (setq denote-directory "~/dev/notes"))
;;;; consult-denote
(use-package consult-denote)
;;; programming
(use-package elixir-ts-mode)
(use-package elixir-mode)
(setq treesit-extra-load-path (list (expand-file-name "~/dev/github/casouri/tree-sitter-module/dist")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;;; schema-store
;;;; johnny5-schema-store-update
(defun johnny5-schema-store-update ()
  "Download the latest JSON Schema Store catalog asynchronously.
Saves to `user-emacs-directory'/schema-store-catalog.json.
After downloading, restart eglot in affected buffers to pick up changes."
  (interactive)
  (let ((catalog-file (locate-user-emacs-file "schema-store-catalog.json"))
        (url "https://www.schemastore.org/api/json/catalog.json"))
    (message "Downloading schema catalog from %s..." url)
    (url-retrieve url
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Schema store update failed: %s"
                                 (plist-get status :error))
                      (goto-char (point-min))
                      (re-search-forward "\n\n")
                      (write-region (point) (point-max) catalog-file)
                      (kill-buffer)
                      (message "Schema store catalog updated: %s" catalog-file)))
                  nil t)))
;;; eglot
;; M-. embark-dwim (rebound from xref-find-definitions)
;; M-, xref-go-back
;; M-? xref-find-references
(use-package eglot
  :ensure (:depth 1)
  :config
  ;; else eglot + python will not work well
  ;; https://github.com/joaotavora/eglot/discussions/1226#discussioncomment-6010670
  (add-to-list 'project-vc-ignores "./.venv/")
  (setq eldoc-echo-area-use-multiline-p t)
  ;; Schema Store: provide schema validation via LSP.
  (let ((catalog-file (locate-user-emacs-file "schema-store-catalog.json")))
    (if (file-exists-p catalog-file)
        (let* ((json-object-type 'plist)
               (json-array-type  'vector)
               (json-key-type    'keyword)
               (json-schemas     (plist-get (json-read-file catalog-file) :schemas)))
          (setq-default eglot-workspace-configuration
                        `(:json (:validate (:enable t)
                                           :schemas ,json-schemas)
                          :yaml (:schemaStore (:enable t)
                                 :validate t))))
      (setq-default eglot-workspace-configuration
                    `(:yaml (:schemaStore (:enable t)
                             :validate t)))))
  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list 'eglot-server-programs
  ;;                `((elixir-ts-mode heex-ts-mode elixir-mode) .
  ;;                  ("nextls" "--stdio=true" :initializationOptions (:experimental (:completions (:enable t)))))))
  :hook ((elixir-mode . eglot-ensure)
         (elixir-ts-mode . eglot-ensure)
         (heex-ts-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (terraform-mode . eglot-ensure)
         (toml-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure))
  :bind(:map eglot-mode-map
             ("C-c l r" . eglot-rename)
             ("C-c l a" . eglot-code-actions)
             ("M-n" . flymake-goto-next-error)
             ("M-p" . flymake-goto-prev-error))
  :custom
  ;; Shutdown server after buffer kill
  (eglot-autoshutdown t)
  ;; Enable eglot in code external to project
  (eglot-extend-to-xref t))
;;;; workaround: yaml-language-server sends scopeUri "null"
;; yaml-language-server sends scopeUri as the literal string "null" for
;; global configuration sections.  eglot-uri-to-path passes it through
;; unchanged, file-name-directory returns nil, and hack-dir-local-variables
;; crashes on (file-remote-p nil).  Guard against non-file URIs here.
(advice-add 'eglot--workspace-configuration-plist :around
            (lambda (orig server &optional path)
              (funcall orig server
                       (if (and (stringp path)
                                (not (file-name-absolute-p path)))
                           nil
                         path))))
;;; Corfu
;;
;; A modern and minimal completion UI.
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary 'separator)
  :init
  ;; Enable Corfu globally
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))
;;; which-key
(use-package which-key
  :config (which-key-mode))
;;; switch-window
(use-package switch-window
  :bind (("C-x o" . switch-window))
  :config (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?z))
;;; multiple-cursors
(use-package multiple-cursors
  :config (setq mc/always-run-for-all 1)
  (define-key mc/keymap (kbd "<return>") nil)
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/mark-all-dwim)
         ("M-s-d" . mc/edit-beginnings-of-lines)))
;;; avy
(use-package avy
  :bind (("M-j" . avy-goto-char-timer)
         ("C-c C-j" .   avy-resume)
         :map isearch-mode-map
         ("C-'" . avy-isearch)))
;;; vterm
(defun setup-vterm-init ()
  "This might be hacky, but this allows me to use mise vterm or compile with cmake."
  (setq vterm-buffer-name-string "vterm %s"
        vterm-kill-buffer-on-exit nil
        vterm-max-scrollback 1000000
        vterm-use-vterm-prompt-detection-method t
        vterm--maybe-compile nil)
  (if-let* ((vterm-base (getenv "MISE_VTERM_PATH"))
            (maybe-load-path (car (file-expand-wildcards (concat vterm-base "/share/emacs/site-lisp/elpa/vterm*")))))
      (progn
        (add-to-list 'load-path maybe-load-path)
        (message "----- using mise vterm -----"))
    (setq vterm--maybe-compile t)
    (setq vterm-always-compile-module t)
    (message "----- downloading and compiling vterm -----")))

(setup-vterm-init)

(if vterm--maybe-compile
    (use-package vterm)
  (use-package vterm
    :ensure nil))

(eval-after-load 'vterm
  '(add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path)
                                                  (setq default-directory path)))))
;;; elisp-demos
(use-package elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
;;; pdf-tools
(use-package pdf-tools
  :defer t
  :magic ("%PDF" . pdf-view-mode) ;; https://github.com/jwiegley/use-package#magic-handlers
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode 1)))
  (pdf-loader-install)
  (setq pdf-view-use-scaling t))
;;; mise
(use-package mise
  :hook ((elpaca-after-init . global-mise-mode))
  :config
  (add-to-list 'mise-auto-propagate-commands 'vterm)
  (add-to-list 'mise-auto-propagate-commands 'claude-code-ide--detect-cli)
  (add-to-list 'mise-auto-propagate-commands 'claude-code-ide-check-status))
;;; buffer-name-relative
(use-package buffer-name-relative
  :init
  (setq buffer-name-relative-prefix '("<" . ">/"))
  (buffer-name-relative-mode))
;;; sideline
(use-package sideline
  :init
  (setq sideline-flymake-display-mode 'line
        sideline-backends-right '(sideline-flymake sideline-blame)
        sideline-backends-skip-current-line t  ; don't display on current line
        sideline-format-left "%s   "           ; format for left aligment
        sideline-format-right "   %s"          ; format for right aligment
        sideline-priority 100                  ; overlays' priority
        )
  :config
  (global-sideline-mode 1))
;;;; sideline-blame
(use-package sideline-blame)
;;;; sideline-flymake
(use-package sideline-flymake)
;;; apheleia
(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (require 'cl-lib)
  ;; https://github.com/radian-software/apheleia/issues/153
  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
    (with-current-buffer scratch
      (setq-local eglot--cached-server
                  (with-current-buffer buffer
                    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
        (eglot-format-buffer))
      (funcall callback)))
  (add-to-list 'apheleia-formatters
               '(eglot-managed . apheleia-indent-eglot-managed-buffer)))
;;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))
;;; burly
(use-package burly
  :ensure (:type git :host github :repo "alphapapa/burly.el"))
;;; tab-bar-history-mode:
;; better than winnner, no tabs required
;; https://www.reddit.com/r/emacs/comments/1kz57i5/comment/mv4zgji
(defmacro johnny5-repeat-it (group cmds)
  (let ((map (intern (concat (symbol-name group) "-repeat-map"))))
    `(progn
       (defvar ,map (make-sparse-keymap))
       (cl-loop for (key def) in ,cmds do
                (define-key ,map (kbd key) def)
                (put def 'repeat-map ',map)))))
(defun johnny5-tab-bar-history-report-position ()
  (let* ((f (selected-frame))
         (back (length (gethash f tab-bar-history-back)))
         (forward (length (gethash f tab-bar-history-forward))))
    (message (concat (format "Window undo (%d / %d)" forward (+ back forward))
                     (when-let* ((msg (current-message))) (format " <%s>" msg))))))
(use-package tab-bar
  :ensure nil
  :bind (("s-[" . tab-bar-history-back)
         ("M-s-<left>" . tab-bar-history-back)
         ("s-]" . tab-bar-history-forward)
         ("M-s-<right>" . tab-bar-history-forward))
  :init
  (setq tab-bar-history-limit 200)
  :config
  (tab-bar-history-mode 1)
  (johnny5-repeat-it tab-bar-history-mode '(("<left>" tab-bar-history-back)
                                            ("M-s-<left>" tab-bar-history-back)
                                            ("<right>" tab-bar-history-forward)
                                            ("M-s-<right>" tab-bar-history-forward)))
  (advice-add 'tab-bar-history-forward :after 'johnny5-tab-bar-history-report-position)
  (advice-add 'tab-bar-history-back :after 'johnny5-tab-bar-history-report-position))
;;; ws-butler
(use-package ws-butler
  :config (ws-butler-global-mode 1))
;;; whitespace
(use-package whitespace
  :ensure nil
  :hook
  (prog-mode . whitespace-mode)
  (text-mode . whitespace-mode))
;;; helpful
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))
;;; indent-bars
(use-package indent-bars
  :ensure (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :init
  (setq
   indent-bars-pattern "."
   indent-bars-width-frac 0.5
   indent-bars-pad-frac 0.25
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth '(:face default :blend 0.4))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;                                  list list_comprehension
  ;;                                  dictionary dictionary_comprehension
  ;;                                  parenthesized_expression subscript)))
  :config
  (require 'indent-bars-ts)             ; not needed with straight
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local
               indent-tabs-mode t ; make sure tabs-based indenting is on, even if we disable it globally
               indent-bars-no-descend-lists nil) ; elisp is mostly continued lists!  allow bars to descend inside
              (indent-bars-mode 1)))
  :hook ((python-base-mode yaml-mode elixir-ts-mode) . indent-bars-mode))
;;; jinx
;; If this errors when loading and enchant is installed try
;; elpaca-delete jinx
(use-package jinx
  :hook (elpaca-after-init . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-,"   . jinx-previous)
         ("C-."   . jinx-next)
         ("C-M-$" . jinx-languages))
  :config
  (defun johnny5-jinx-get-paths ()
    "Get paths to configure jinx from mise environment."
    (let ((enchant-include-path (getenv "MISE_ENCHANT_INCLUDE_PATH"))
          (enchant-lib-path (getenv "MISE_ENCHANT_LIB_PATH")))
      (when (and enchant-include-path (file-directory-p enchant-include-path)
                 enchant-lib-path (file-directory-p enchant-lib-path))
        (setq jinx--compile-flags
              (append jinx--compile-flags
                      (list (format "-I%s/include/enchant-2" enchant-include-path)
                            (format "-L%s/lib" enchant-lib-path)))))))
  (johnny5-jinx-get-paths)
  (keymap-set jinx-repeat-map "RET" 'jinx-correct))
;;; emacs-env
;; Manage dated Emacs environments for testing emacs-overlay updates
(let ((env-file (locate-user-emacs-file "emacs-env/emacs-env.el")))
  (when (file-exists-p env-file)
    (load env-file)))

;;; load custom.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
;;; local variables
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; eval: (outline-hide-sublevels 3)
;; End:
