;;; Example Elpaca configuration -*- lexical-binding: t; -*-
;;; elpaca
(setq elpaca-core-date '(20250602))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
;;; modus-themes
(use-package modus-themes
  :ensure t
  :config
  (setq modus-vivendi-tritanopia-palette-overrides
        '((bg-paren-match unspecified) (fg-paren-match unspecified) (bg-paren-expression "#0000ff")))
  (modus-themes-load-theme 'modus-vivendi-tritanopia))
;;; undo-fu
(use-package undo-fu
  :ensure t
  :bind (("C-z" . undo-fu-only-undo) ("s-z" . undo-fu-only-undo) ("C-S-z" . undo-fu-only-redo)))

(use-package
  undo-fu-session
  :ensure t
  :config
  (global-undo-fu-session-mode)
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
;;; outline
;;;; outline
(use-package outline
  :ensure nil
  :hook
  (prog-mode-hook . outline-minor-mode)
  :bind
  (:map outline-minor-mode-map
        ("C-<tab>" . outline-cycle-buffer)
        ("M-p" . outline-previous-heading)
        ("M-n" . outline-next-heading)))
;;;; outli
(use-package outli
  :ensure (:host github :repo "jdtsmith/outli")
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
              ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode)
  :init (outli-mode 1))
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
  (setq ripgrep--extra-args " -g !.venv -g !.pre-commit -g !.terraform")
  (setq johnny--maybe-ripgrep-executable nil)
  :bind (;; Better than default - from /u/zck
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ("s-a" . mark-whole-buffer)
         ("s-u" . revert-buffer)
         ("s--" . global-text-scale-adjust)
         ("s-=" . global-text-scale-adjust)
         ("s-0" . global-text-scale-adjust))
  :hook
  (emacs-startup . toggle-frame-maximized)
  ;; only scale when we are on the laptop
  (emacs-startup . scale-default-text-scale)
  (emacs-startup . setup-browse-url-browser-function))
;;;; unset keys that are related to suspending
;; Unset suspend-frame and suspend-frame
(global-unset-key (kbd "C-x C-z"))
;;;; unset keys that are related to default-text-scale
;; Unset mouse-wheel-text-scale
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))
;; text-scale-adjust
(global-unset-key (kbd "s-+"))
(global-unset-key (kbd "C-x C--"))
(global-unset-key (kbd "C-x C-0"))
(global-unset-key (kbd "C-x C-="))
;; Unset more things that scale up/down text
(global-unset-key (kbd "<wheel-down>"))
(global-unset-key (kbd "<wheel-up>"))
(global-unset-key (kbd "<pinch>"))
;;;; setup-browse-url-browser-function
(defun setup-browse-url-browser-function ()
  (if (or (string-equal system-type "berkley-unix") (string-equal system-type "gnu/linux"))
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "nyxt")
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)))
;;;; scale-default-text-scale
(defun scale-default-text-scale ()
  (when (and (eql (display-pixel-height) 1934) (eql (display-pixel-width) 2992))
    (global-text-scale-adjust 5)))
;;; init-file-debug
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))
;;; load main config
(when (file-readable-p (locate-user-emacs-file "main.el"))
  (load (locate-user-emacs-file "main.el")))
;;; local variables
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; eval: (outline-hide-sublevels 3)
;; End:
