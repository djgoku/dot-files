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
(define-key global-map "\M-Q" 'unfill-paragraph)
;;;; crontab-edit
(defun crontab-edit ()
  "Run `crontab-edit' in a emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))
;;;; setup-browse-url-browser-function
(defun setup-browse-url-browser-function ()
  (if (or (string-equal system-type "berkley-unix") (string-equal system-type "gnu/linux"))
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "nyxt")
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)))
;;;; scale-default-text-scale
(defun scale-default-text-scale ()
  (when (and (eql (display-pixel-height) 1934) (eql (display-pixel-width) 2992))
    (default-text-scale-increment 50)))
;;; emacs
(use-package emacs
  :ensure nil
  :config (recentf-mode t)
  (setq history-length t)
  (setq recentf-max-menu-items 10000)
  (setq recentf-max-saved-items 10000)
  (show-paren-mode 1)
  (setq show-paren-style 'expression)
  (menu-bar-mode -1)
  (if (fboundp 'toggle-scroll-bar)
      (toggle-scroll-bar nil))
  (tool-bar-mode -1)
  (ido-mode -1)
  (setq auto-revert-interval 1)
  (global-auto-revert-mode 1)
  (setq make-backup-files nil)
  (setq markdown-command "pandoc")
  (setq-default indent-tabs-mode nil)
  (setq auto-save-no-message t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (electric-pair-mode t)
  (display-time-mode 1)
  (setq display-time-default-load-average nil)
  (setq display-time-format "%F %H:%M - %a")
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  (setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-saves/") t)))
  (setq lock-file-name-transforms `((".*" ,(concat user-emacs-directory "locks/") t)))
  (setq delete-by-moving-to-trash t)
  (setq initial-major-mode #'org-mode)
  (setq initial-scratch-message "* test\n#+begin_src emacs-lisp\n#+end_src")
  (setq window-combination-resize t)
  (setq use-short-answers t)
  (delete-selection-mode 1)
  (setq-default mode-line-buffer-identification
                (list
                 'buffer-file-name
                 (propertized-buffer-identification "%12f")
                 (propertized-buffer-identification "%12b")))
  ;; this allows for Copying or Renaming across dired buffers
  (setq dired-dwim-target t)
  (setq native-comp-async-report-warnings-errors 'silent)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-use-momentum t)
  (setq native-comp-jit-compilation-deny-list '("git-timemachine"))
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode 1)
  (setq ring-bell-function 'ignore)
  (global-visual-wrap-prefix-mode 1)
  (setq which-func-update-delay 1.0)
  (setq use-package-compute-statistics t)
  :bind (;; Better than default - from /u/zck
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ("s-a" . mark-whole-buffer)
         ("s-u" . revert-buffer))
  :hook
  (emacs-startup . toggle-frame-maximized)
  ;; only scale when we are on the laptop
  (emacs-startup . scale-default-text-scale)
  (emacs-startup . setup-browse-url-browser-function))
;;; package installs
(setq johnny5-package-list
      '(consult-denote
        deadgrep
        define-word
        dired-preview
        docker
        elfeed
        git-link
        git-timemachine
        htmlize
        ipcalc
        jeison
        json-mode
        kubel
        markdown-mode
        nix-mode
        ob-elixir
        ;; ob-mermaid
        org-chef
        org-contrib
        org-jira
        org-ql
        orgit
        ox-jira
        powerthesaurus
        sideline-blame
        sideline-flymake
        string-inflection
        transient))

(dolist (package johnny5-package-list)
  (eval `(use-package ,package :ensure t) t))
;;; affe
(use-package affe
  :ensure t
  :after orderless
  :config
  (setq affe-count 10000)
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight
        affe-find-command "rga --color=never --files --hidden -g !.git -g !.venv"
        affe-grep-command "rga --null --color=never --max-columns=1000 --no-heading --line-number -v ^$ --hidden -g !.git -g !.venv")
  :bind (("M-s a f" . affe-find)
         ("M-s a g" . affe-grep)))
;;; consult
(use-package consult
  :ensure t
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
  (setq consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")
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
;;; marginalia
(use-package marginalia
  :ensure t
  :config (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))
;;; embark
(use-package embark
  :ensure t
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
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))
;;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;;; vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
;;; savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))
;;; wgrep
(use-package wgrep
  :ensure t
  :config (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "r"))
;;; rg
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings)
  (rg-enable-menu))
;;; magit
(use-package magit
  :ensure t
  :after project
  :init
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m"))
  (setq magit-clone-set-remote.pushDefault t)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  ;; setting to level 5 for gpg siging
  (setq transient-default-level 5)
  (setq magit-revision-headers-format "Author:     %aN <%aE>\nAuthorDate: %ad\nCommit:     %cN <%cE>\nCommitDate: %cd\nSigned:\n\n%GG\n")
  :bind (("C-x g" . magit-project-status)))
;;; git-identity
(use-package git-identity
  :ensure t
  :after magit
  :config (git-identity-magit-mode 1)
  (define-key magit-status-mode-map (kbd "I") 'git-identity-info)
  :custom (git-identity-verify t))
;;; org
;; (org-narrow-to-subtree) C-x n s
;; (widen) C-x n w
;; C-u 0 M-x org-babel-remove-result-one-or-many will remove all result blocks in a buffer
;; M-x org-babel-execute-buffer will execute all src blocks in a buffer
(use-package org
  :ensure nil
  :bind (("C-c c" . 'org-capture)
         ("C-c a" . 'org-agenda))
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
(use-package ob-async
  :ensure t
  :config (require 'ob-async))
;;; denote
(use-package denote
  :ensure t
  :config
  (setq denote-directory "~/dev/notes"))
;;; programming
(use-package elixir-ts-mode
  :ensure t)
(use-package elixir-mode
  :ensure t)
(setq treesit-extra-load-path (list (expand-file-name "~/dev/github/casouri/tree-sitter-module/dist")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(use-package treesit-auto
  :ensure t
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
  :ensure t
  :config (which-key-mode))
;;; switch-window
(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window))
  :config (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?z))
;;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config (setq mc/always-run-for-all 1)
  (define-key mc/keymap (kbd "<return>") nil)
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/mark-all-dwim)
         ("M-s-d" . mc/edit-beginnings-of-lines)))
;;; avy
(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer)))
;;; vterm
(use-package vterm
  :ensure t
  :config (setq vterm-buffer-name-string "vterm %s")
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-max-scrollback 100000)
  (setq vterm-use-vterm-prompt-detection-method t)
  (setq vterm-always-compile-module t)
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path)
                                                 (setq default-directory path)))))
;;; elisp-demos
(use-package elisp-demos
  :ensure t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
;;; envrc
(use-package envrc
  :ensure t
  :config (envrc-global-mode))
;;; pdf-tools
(use-package pdf-tools
  :ensure t
  :defer t
  :magic ("%PDF" . pdf-view-mode) ;; https://github.com/jwiegley/use-package#magic-handlers
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (pdf-loader-install)
  (setq pdf-view-use-scaling t))
;;; inheritenv
(use-package inheritenv
  :ensure (:type git :host github :repo "purcell/inheritenv"))
;;; buffer-env
(use-package buffer-env
  :ensure t
  :hook (hack-local-variables . buffer-env-update)
  :config
  (add-to-list 'buffer-env-command-alist '("/devbox\\.json\\'" . "devbox run  -- \"env -0\""))
  (setq buffer-env-script-name '(".envrc" ;; ".venv/bin/activate"
                                 )))
;;; buffer-name-relative
(use-package buffer-name-relative
  :ensure t
  :init
  (setq buffer-name-relative-prefix '("<" . ">/"))
  (buffer-name-relative-mode))
;;; sideline
(use-package sideline
  :ensure t
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
;;; apheleia
(use-package apheleia
  :ensure t
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
  :ensure t
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
                     (when-let ((msg (current-message))) (format " <%s>" msg))))))
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
  :ensure t
  :config (ws-butler-global-mode 1))
;;; whitespace
(use-package whitespace
  :ensure nil
  :hook
  (prog-mode . whitespace-mode)
  (text-mode . whitespace-mode))
;;; helpful
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))
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
