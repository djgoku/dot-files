;; cp dot-files/nix/shell.nix ~/shell.nix
;; install nix; nix-shell;
;; asdf plugin add ripgrep
;; asdf install ripgrep 13.0.0
;; asdf global ripgrep 13.0.0
;; asdf plugin add nodejs
;; asdf install nodejs 15.14.0
;; asdf global nodejs 15.14.0
;; ln -s ~/dot-files/emacs/init.el ~/.config/emacs/init.el

(defvar bootstrap-version)
(setq straight-repository-branch "develop")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-current-profile 'testing)
(setq straight-profiles
      '(
        (testing . "testing.el")))

(use-package emacs
  :config
  (recentf-mode t)
  (setq recentf-max-menu-items 10000)
  (setq recentf-max-saved-items 10000)
  (show-paren-mode 1)
  (setq show-paren-style 'expression)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (global-auto-revert-mode 1)
  (setq make-backup-files nil)
  (setq markdown-command "pandoc")
  (setq-default indent-tabs-mode nil)
  (setq auto-save-no-message t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq electric-pair-mode t)
  (add-to-list 'default-frame-alist '(font . "Menlo-10"))
  (when (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))
  :bind (;; Better than default - from /u/zck
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)))

(use-package modus-themes
  :straight t
  :init
  (modus-themes-load-themes)
  :config
  (setq modus-themes-org-blocks 'greyscale)
  (modus-themes-load-vivendi))

(use-package undo-fu
  :straight t
  :bind (("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :straight t
  :config
  (global-undo-fu-session-mode)
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package wgrep
  :straight t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "r"))

(use-package consult
  :straight t
  :bind (("C-c k" . consult-keep-lines)
         ("C-c C-k" . consult-focus-lines)
         ("C-x b" . consult-buffer)
         ("C-c r" . consult-ripgrep)
         ("M-g g" . consult-goto-line)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
	 ("s-f" . consult-line))
  :config
  (defun find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))
  (fset 'multi-occur #'consult-multi-occur)
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)
  (setq consult-project-root-function (lambda () (let ((p (project-current))) (when p (project-root p)))))
  (setq consult-ripgrep-command "rg -i --no-ignore --null --line-buffered --color=ansi --max-columns=250   --no-heading --line-number . -e ARG OPTS")
  (setq consult-async-min-input 2))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package embark
  :straight t
  :bind
  ("M-a" . embark-act))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package orderless
  :straight t
  :init (icomplete-mode)
  :custom (completion-styles '(orderless)))

(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :straight t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package default-text-scale
  :straight t
  :config
  :hook (after-init . default-text-scale-mode)
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("C-x C-0" . default-text-scale-reset)))

(use-package rg
  :straight t
  :config
  (rg-enable-default-bindings)
  (rg-enable-menu))

(defun johnny5-split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (balance-windows))

(defun johnny5-delete-window-evenly ()
  (interactive)
  (delete-window)
  (balance-windows))

(defun johnny5-split-window-below ()
  (interactive)
  (split-window-below)
  (balance-windows))

(global-set-key "\C-x3" 'johnny5-split-window-horizontally)
(global-set-key "\C-x2" 'johnny5-split-window-below)
(global-set-key "\C-x0" 'johnny5-delete-window-evenly)

;;; It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(define-key global-map "\M-Q" 'unfill-paragraph)

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; ;;http://www.emacswiki.org/emacs/WhiteSpace
;; ;; (require 'whitespace)
;; ;; (global-whitespace-mode 0)
;; ;; (global-whitespace-mode t)
;; ;; (global-whitespace-newline-mode t)
;; ;; (add-hook 'text-mode-hook 'whitespace-newline-mode)

(use-package spell-fu
  :straight (:host gitlab :repo "ideasman42/emacs-spell-fu")
  :config
  (setq ispell-personal-dictionary (expand-file-name (concat startup--xdg-config-home-emacs "hunspell_en_US")))
  (global-spell-fu-mode))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(setq johnny5-package-list '(docker
                             elfeed
                             elixir-mode
                             flycheck-yamllint
                             git-link
                             git-timemachine
                             go-mode
                             groovy-mode
                             htmlize
                             ipcalc
                             jeison
                             kubel
                             markdown-mode
                             ob-elixir
                             org
                             org-contrib
                             org-ql
                             ox-jira
                             pdf-tools
                             ruby-mode
                             speed-type
                             string-inflection
                             tree-sitter
                             tree-sitter-langs))

(defun johnny5-install-package-list (package-list)
  "Install johnny5 PACKAGE-LIST."
  (while package-list
    (straight-use-package (car package-list))
    (setq package-list (cdr package-list))))

(johnny5-install-package-list johnny5-package-list)

(use-package git-identity
  :straight t
  :after magit
  :config
  (git-identity-magit-mode 1)
  (define-key magit-status-mode-map (kbd "I") 'git-identity-info)
  :custom
  (git-identity-verify t))

(use-package project
  :straight t
  :config
  (add-to-list 'project-switch-commands '(?m "Magit" magit-status)))

(use-package org
  :straight t
  :bind (("C-c c" . 'org-capture))
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "INPROGRESS(i@)" "WAIT(w@)" "|" "DONE(d@)" "CANCELED(c@)")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (setq org-agenda-files '("~/org/test"))
  ;; (setq org-agenda-include-inactive-timestamps 't)
  (setq org-log-refile 'note)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (progn
    (defun imalison:org-inline-css-hook (exporter)
      "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
      (when (eq exporter 'html)
        (let* ((my-pre-bg (face-background 'default))
               (my-pre-fg (face-foreground 'default)))
          (setq
           org-html-head-extra
           (concat
            org-html-head-extra
            (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                    my-pre-bg my-pre-fg))))))

    (add-hook 'org-export-before-processing-hook 'imalison:org-inline-css-hook))
  :mode (("\\.org$" . org-mode)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/test/inbox.org" "Inbox")
         "** %? [/]\n   :PROPERTIES:\n   :Created: %U\n   :END:\n %i\n %a")
        ("j" "Jira" entry (file+headline "~/org/test/inbox.org" "Inbox")
         "** %(johnny5-prompt-org-capture \"Jira URL\" 'org-capture-jira-url 'johnny5-parse-jira-url-for-task-id) [/]\n:PROPERTIES:\n:Created: %U\n:Jira-Summary: %^{Jira-Summary} \n:Jira-URL: %(progn org-capture-jira-url)\n:END:\n%?")))

(defvar johnny5-org-capture-jira-url nil
  "stuff")

(defvar johnny5-org-capture-prompt-history nil
  "History of prompt answers for org-capture.")

(defun johnny5-prompt-org-capture (prompt variable &optional parse-function)
    "PROMPT for string save it to VARIABLE and insert it and optionally PARSE-FUNCTION to parse the VARIABLE."
    (let ((prompt-string (read-string (concat prompt ": "))))
      (make-local-variable variable)
      (set variable prompt-string)
      (setq johnny5-org-capture-prompt-history (cons prompt-string johnny5-org-capture-prompt-history))
      (if parse-function
          (funcall parse-function prompt-string)
         prompt-string)))

(defun johnny5-parse-jira-url-for-task-id (url)
  "URL will be parsed."
  (car (last (split-string url "/"))))

(require 'ox-md nil t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (emacs-lisp . t)
   (elixir . t)
   (perl . t)
   (python . t)
   (sed . t)
   (shell . t)
   (ruby . t)))

;; just add :async to any org babel src blocks!
(use-package ob-async
  :straight t
  :config
  (require 'ob-async))

(use-package org-download
  :straight t
  :after org
  :defer nil
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
  :bind
  ("C-M-y" . org-download-screenshot)
  :config
  (require 'org-download))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package flycheck-mypy
  :straight t
  :config
  (flycheck-add-next-checker 'python-flake8 'python-mypy))

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (global-company-mode t)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package lsp-pyright
  :straight (:host github :repo "emacs-lsp/lsp-pyright")
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package lsp-mode
  :straight t
  :hook (
         (ruby-mode . lsp)
         (python-mode . lsp)
         (elixir-mode . lsp)
         (sqls . lsp)
         (yamlls . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-keymap-prefix "s-l")
  (add-to-list 'exec-path "/Users/jonathanotsuka/development/github/elixir-ls/release"))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package terraform-mode
  :straight t
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package company-terraform
  :straight t
  :config
  (company-terraform-init))

(use-package kubernetes
  :straight t
  :config
  (setq kubernetes-poll-frequency 3600)
  (setq kubernetes-redraw-frequency 3600))

(use-package yaml-mode
  :straight t
  :custom
  (yaml-indent-offset 0))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package switch-window
  :straight t
  :bind (("C-x o" . switch-window))
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?z))

(use-package tramp
  :straight (tramp :host nil :repo "https://git.savannah.gnu.org/r/tramp.git"))

(defun add-ssh-agent-to-tramp ()
  (cl-pushnew '("-A")
              (cadr (assoc 'tramp-login-args
                           (assoc "ssh" tramp-methods)))
              :test #'equal))
(add-ssh-agent-to-tramp)

(defun crontab-e ()
    "Run `crontab -e' in a emacs buffer."
    (interactive)
    (with-editor-async-shell-command "crontab -e"))

(use-package multiple-cursors
  :straight t
  :config
  (setq mc/always-run-for-all 1)
  (define-key mc/keymap (kbd "<return>") nil)
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/mark-all-dwim)
         ("M-s-d" . mc/edit-beginnings-of-lines)))

;; (use-package avy
;;   :straight t
;;   :bind (("C-:" . avy-goto-char)))

(use-package bufler
  :straight (:host github :repo "alphapapa/bufler.el")
  :bind (("C-x C-b" . bufler)))

(use-package vterm
  :straight t
  :config
  (setq vterm-buffer-name-string "vterm %s")
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-max-scrollback 100000)
  (setq vterm-use-vterm-prompt-detection-method t)
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path)))))

(use-package json-to-org-table
  :straight (:host github :repo "noonker/json-to-org-table"))

(when (file-exists-p "~/.config/emacs/custom.el")
  (setq custom-file "~/.config/emacs/custom.el")
  (load custom-file))
