;;; main config  -*- lexical-binding: t; -*-
;;; create directories for auto-saves, backups and locks
(dolist (directory '("auto-saves" "backups" "locks"))
  (unless (file-directory-p (concat user-emacs-directory directory))
    (mkdir (concat user-emacs-directory directory))))
;;; functions
;;;; johnny5-magit-clone
;; This function has evolved a little after the initial need.

;; Features:
;; - If the repository is already clone switch to the repository
;;   directory and run ~magit-status~
;; - The behavior is the same if you input ~git clone
;;   git@bitbucket.org:johnny-5-is-alive/dot-files.git~ or
;;   ~git@bitbucket.org:johnny-5-is-alive/dot-files.git~ with
;;   ~johnny5-magit-clone~
;; - Clone to a consistent directory. The directory will contain the site
;;   ~github~, ~github.com~ ~bitbucket~, or ~bitbucket.org~ (~(setq
;;   johnny5-magit-clone-default-directory-remove-domain t)~ to remove
;;   the domain name from the directory path); Next part of the directory
;;   path will have the GitHub/Bitbucket org or GitHub/Bitbucket
;;   username; With the last part of the directory is the repository
;;   name; As an example input to ~johnny5-magit-clone~ of ~git clone
;;   git@bitbucket.org:johnny-5-is-alive/dot-files.git~. The directory
;;   path would be =~/dev/bitbucket.org/johnny-5-is-alive/dot-files=
;;;;; functions
;; if you want to remove the domain from say github.com and use github as the directory, set the following:
;; (setq johnny5-magit-clone-default-directory-remove-domain t)

(setq johnny5-magit-clone-default-directory-remove-domain t)

(defun johnny5-magit-clone-default-directory (git-clone-url)
  (let* ((parsed-git-clone-url (johnny5-magit-clone-parse git-clone-url)))
    (cond ((string-prefix-p "git@" parsed-git-clone-url)
           (johnny5-git-url-parse (cadr (split-string parsed-git-clone-url "git@"))))
          ((string-prefix-p "https://" parsed-git-clone-url)
           (johnny5-git-https-url-parse parsed-git-clone-url)))))

(defun johnny5-git-https-url-parse (git-https-url-parse)
  (let* ((url (url-generic-parse-url git-https-url-parse))
         (host (url-host url))
         (user-or-workspace-and-repo (substring (url-filename url) 1)))
    (johnny5-git-url-parse (format "%s:%s/" host user-or-workspace-and-repo))))

(defun johnny5-git-url-parse (git-clone-url)
  (let* ((git-url-path-and-repo-split (string-split git-clone-url ":"))
         (uri (car git-url-path-and-repo-split))
         (uri-without-domain (car (split-string uri "\\.")))
         (user-or-workspace-and-repo-split (split-string (cadr git-url-path-and-repo-split) "/"))
         (user-or-workspace (car user-or-workspace-and-repo-split))
         (repository_name (car(split-string (cadr user-or-workspace-and-repo-split) ".git"))))
    (if (and (boundp 'johnny5-magit-clone-default-directory-remove-domain) johnny5-magit-clone-default-directory-remove-domain)
        (format "~/dev/%s/%s/" uri-without-domain user-or-workspace)
      (format "~/dev/%s/%s/" uri user-or-workspace))))

(defun johnny5-magit-clone ()
  (interactive)
  (unless (featurep 'magit)
    (require 'magit))
  (let* ((repo (magit-read-string "clone repo"))
         (repo-url (johnny5-magit-clone-parse repo))
         (repo-name (johnny5-magit-clone-url-to-name repo))
         (clone-directory (johnny5-magit-clone-default-directory repo-url))
         (clone-directory-with-repo-name (format "%s/%s" clone-directory repo-name)))
    (if (file-directory-p clone-directory-with-repo-name)
        (magit-status clone-directory-with-repo-name)
      (magit-clone-internal repo-url clone-directory-with-repo-name nil))
    (message "repo %s, repo-name %s, clone-directory %s, repo-url %s, clone-directory-with-repo-name %s" repo repo-name clone-directory repo-url clone-directory-with-repo-name)))

(defun johnny5-magit-clone-url-to-name (url)
  (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
       (match-string 1 url)))

(defun johnny5-magit-clone-parse (git-clone-url)
  (cond
   ((string-prefix-p "git@" git-clone-url) git-clone-url)
   ((string-prefix-p "https://" git-clone-url) git-clone-url)
   ((cadr (split-string git-clone-url "git clone ")))))
;;;;; tests
(ert-deftest test-johnny5-magit-clone-parse ()
  "test that magit-clone parse works"
  (should (equal (johnny5-magit-clone-parse "git@github.com.com:djgoku/melpa.git") "git@github.com.com:djgoku/melpa.git"))
  (should (equal (johnny5-magit-clone-parse "https://github.com/djgoku/melpa.git") "https://github.com/djgoku/melpa.git"))
  (should (equal (johnny5-magit-clone-parse "git clone git@bitbucket.org:johnny-5-is-alive/dot-files.git") "git@bitbucket.org:johnny-5-is-alive/dot-files.git")))

(ert-deftest test-johnny5-magit-clone-default-directory ()
  "Tests that we return the correct magit-clone-default-directory."
  (setq johnny5-magit-clone-default-directory-remove-domain nil)
  (should (equal (johnny5-magit-clone-default-directory "git@github.com:djgoku/melpa.git") "~/dev/github.com/djgoku/"))
  (should (equal (johnny5-magit-clone-default-directory "https://github.com/djgoku/melpa.git") "~/dev/github.com/djgoku/"))
  (should (equal (johnny5-magit-clone-default-directory "git clone git@bitbucket.org:johnny-5-is-alive/dot-files.git") "~/dev/bitbucket.org/johnny-5-is-alive/"))
  (should (equal (johnny5-magit-clone-default-directory "git clone https://djgoku@bitbucket.org/johnny-5-is-alive/dot-files.git") "~/dev/bitbucket.org/johnny-5-is-alive/")))

(ert-deftest test-johnny5-magit-clone-default-directory-remove-domain ()
  "Tests that we return the correct magit-clone-default-directory."
  (setq johnny5-magit-clone-default-directory-remove-domain t)
  (should (equal (johnny5-magit-clone-default-directory "git@github.com:djgoku/melpa.git") "~/dev/github/djgoku/"))
  (should (equal (johnny5-magit-clone-default-directory "https://github.com/djgoku/melpa.git") "~/dev/github/djgoku/"))
  (should (equal (johnny5-magit-clone-default-directory "git clone git@bitbucket.org:johnny-5-is-alive/dot-files.git") "~/dev/bitbucket/johnny-5-is-alive/"))
  (should (equal (johnny5-magit-clone-default-directory "git clone https://djgoku@bitbucket.org/johnny-5-is-alive/dot-files.git") "~/dev/bitbucket/johnny-5-is-alive/")))
;;;; unfill-paragraph
;; It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(define-key global-map "\M-Q" #'unfill-paragraph)
;;;; crontab-edit
(defun crontab-edit ()
  "Run `crontab-edit' in a emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))
;;;; johnny-test-executable
(defun johnny-test-executable (command)
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
(unless (featurep 'dired-x)
  (require 'dired-x))
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
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")) ;; "C-+"
;;;; consult-dir
(use-package consult-dir)
;;; marginalia
(use-package marginalia
  :config (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))
;;; embark
(use-package embark
  :bind ("M-a" . embark-act)
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
  (embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
  (add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
  (add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
  (add-to-list 'embark-repeat-actions #'jinx-next)
  (add-to-list 'embark-repeat-actions #'jinx-previous)
  (add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark--ignore-target)))

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
(defun johnny--maybe-ripgrep-executable ()
  "Determine if ripgrep 'rg' or ripgrep-all 'rga' is installed"
  (let* ((maybe-executable (or (executable-find "rga") (executable-find "rg"))))
    (if (and maybe-executable (johnny-test-executable maybe-executable))
        (setq johnny--maybe-ripgrep-executable maybe-executable)
      (warn "ripgrep 'rg' nor ripgrep-all 'rga' were found in PATH"))))
(johnny--maybe-ripgrep-executable)
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
  :config (rg-enable-default-bindings)
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
  (setq magit-clone-set-remote.pushDefault t)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; setting to level 5 for gpg siging
  (setq transient-default-level 5)
  (setq magit-revision-headers-format "Author:     %aN <%aE>\nAuthorDate: %ad\nCommit:     %cN <%cE>\nCommitDate: %cd\nSigned:\n\n%GG\n")
  :bind (("C-x g" . magit-project-status)))
;;; git-identity
(use-package git-identity
  :after magit
  :config (git-identity-magit-mode 1)
  (define-key magit-status-mode-map (kbd "I") #'git-identity-info)
  :custom (git-identity-verify t))
;;; git-link
(use-package git-link)
;;; git-timemachine
(use-package git-timemachine)
;;; org
;; (org-narrow-to-subtree) C-x n s
;; (widen) C-x n w
;; C-u 0 M-x org-babel-remove-result-one-or-many will remove all result blocks in a buffer
;; M-x org-babel-execute-buffer will execute all src blocks in a buffer
(use-package org
  :ensure nil
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation t)
  (setq org-log-into-drawer t)
  ;; NOT-RETAINED - will not continue in the application process
  (setq org-todo-keywords '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "WAIT(w!)" "APPLIED(a!)" "|" "DONE(d@)"
                                      "CANCELED(@)" "WITHDRAWN(@)" "FILLED" "HIRED" "NOT-RETAINED(n@)")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (setq org-agenda-files '("~/dev/org" "~/dev/notes"))
  ;; (setq org-agenda-include-inactive-timestamps 't)
  (setq org-log-refile 'note)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-startup-indented t)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (progn
    (defun imalison:org-inline-css-hook (exporter)
      "Insert custom inline css to automatically set the
  background of code to whatever theme I'm using's background"
      (when (eq exporter 'html)
        (let* ((my-pre-bg (face-background 'default))
               (my-pre-fg (face-foreground 'default)))
          (setq org-html-head-extra (concat org-html-head-extra (format
                                                                 "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                                                                 my-pre-bg my-pre-fg))))))
    (add-hook 'org-export-before-processing-hook 'imalison:org-inline-css-hook))
  :mode (("\\.org$" . org-mode)))
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
;; just add :async to any org babel src blocks!
;;;; ob-async
(use-package ob-async
  :config (require 'ob-async))
;;;; org-chef
(use-package org-chef)
;;;; org-contrib
(use-package org-contrib)
;;;; ob-elixir
(use-package ob-elixir
  :ensure t)
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
;; M-. xref-find-definitions
;; M-, xref-go-back
;; M-? xref-find-references
(use-package eglot
  :ensure (:depth 1)
  :config
  ;; else eglot + python will not work well
  ;; https://github.com/joaotavora/eglot/discussions/1226#discussioncomment-6010670
  (add-to-list 'project-vc-ignores "./.venv/")
  (setq eldoc-echo-area-use-multiline-p t)
  (add-to-list 'eglot-server-programs `((elixir-mode elixir-ts-mode heex-ts-mode) . ("devbox" "run" ".devbox/nix/profile/default/lib/language_server.sh")))
  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list 'eglot-server-programs
  ;;                `((elixir-ts-mode heex-ts-mode elixir-mode) .
  ;;                  ("nextls" "--stdio=true" :initializationOptions (:experimental (:completions (:enable t)))))))
  ;; (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) "pyright-langserver" "--stdio"))
  (add-to-list 'eglot-server-programs '(terraform-mode "terraform-ls" "serve"))
  :hook ((elixir-mode . eglot-ensure)
         (elixir-ts-mode . eglot-ensure)
         (heex-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (terraform-mode . eglot-ensure))
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
(use-package company
  :ensure t
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0
        company-backends '((company-capf company-dabbrev-code))
        company-dabbrev-minimum-length 2
        company-occurrence-weight-function #'company-occurrence-prefer-any-closest))
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
  "This might be hacky, but this allows me to use devbox vterm or compile
with cmake."
  (setq vterm-buffer-name-string "vterm %s")
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-max-scrollback 1000000)
  (setq vterm-use-vterm-prompt-detection-method t)
  (setq vterm--maybe-compile nil)
  (let* ((maybe-load-path (car (file-expand-wildcards (concat (getenv "DEVBOX_PACKAGES_DIR") "/share/emacs/site-lisp/elpa/vterm*")))))
    (if maybe-load-path
        (progn
          (add-to-list 'load-path maybe-load-path)
          (message "----- using devbox vterm -----"))
      (setq vterm--maybe-compile t)
      (setq vterm-always-compile-module t)
      (message "----- downloading and compiling vterm -----"))))
(setup-vterm-init)
(if vterm--maybe-compile
    (use-package vterm)
  (use-package vterm
    :ensure nil))
;; add-to-list after vterm is installed so we don't duplicate this
(add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path)
                                               (setq default-directory path))))
;;; elisp-demos
(use-package elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
;;; envrc
(use-package envrc
  :config (envrc-global-mode))
;;; pdf-tools
(use-package pdf-tools
  :defer t
  :magic ("%PDF" . pdf-view-mode) ;; https://github.com/jwiegley/use-package#magic-handlers
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode 1)))
  (pdf-loader-install)
  (setq pdf-view-use-scaling t))
;;; inheritenv
(use-package inheritenv
  :ensure (:type git :host github :repo "purcell/inheritenv"))
;;; buffer-env
(use-package buffer-env
  :hook (hack-local-variables . buffer-env-update)
  :config
  (add-to-list 'buffer-env-command-alist '("/devbox\\.json\\'" . "devbox run  -- \"env -0\""))
  (setq buffer-env-script-name '(".envrc" ;; ".venv/bin/activate"
                                 )))
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
(defmacro my/repeat-it (group cmds)
  (let ((map (intern (concat (symbol-name group) "-repeat-map"))))
    `(progn
       (defvar ,map (make-sparse-keymap))
       (cl-loop for (key def) in ,cmds do
                (define-key ,map (kbd key) def)
                (put def 'repeat-map ',map)))))
(defun my/tab-bar-history-report-position ()
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
  (my/repeat-it tab-bar-history-mode '(("<left>" tab-bar-history-back)
                                       ("M-s-<left>" tab-bar-history-back)
                                       ("<right>" tab-bar-history-forward)
                                       ("M-s-<right>" tab-bar-history-forward)))
  (advice-add 'tab-bar-history-forward :after 'my/tab-bar-history-report-position)
  (advice-add 'tab-bar-history-back :after 'my/tab-bar-history-report-position))
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
  :config
  (require 'indent-bars-ts)             ; not needed with straight
  :custom
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local
               indent-tabs-mode t ; make sure tabs-based indenting is on, even if we disable it globally
               indent-bars-no-descend-lists nil) ; elisp is mostly continued lists!  allow bars to descend inside
              (indent-bars-mode 1)))
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
  :hook ((python-base-mode yaml-mode elixir-ts-mode) . indent-bars-mode))
;;; jinx
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-,"   . jinx-previous)
         ("C-."   . jinx-next)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx--compile-flags (append jinx--compile-flags (list (format "-I%s/include/enchant-2" (getenv "DEVBOX_PACKAGES_DIR")) (format "-L%s/lib"  (getenv "DEVBOX_PACKAGES_DIR")))))
  (unless (featurep 'jinx)
    (require 'jinx)
    (keymap-set jinx-repeat-map "RET" 'jinx-correct)
    (embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
    (add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
    (add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
    (add-to-list 'embark-repeat-actions #'jinx-next)
    (add-to-list 'embark-repeat-actions #'jinx-previous)
    (add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark--ignore-target))))
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
