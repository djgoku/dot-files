(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(straight-use-package 'org)

(use-package
  modus-themes
  :straight t
  :init (setq modus-themes-org-blocks 'gray-background)
  :config (modus-themes-load-theme 'modus-vivendi-deuteranopia))

(use-package undo-fu
  :straight t
  :bind
  (("C-z" . undo-fu-only-undo)
   ("C-S-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :straight t
  :config
  (global-undo-fu-session-mode)
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
