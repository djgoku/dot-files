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
;;; default-text-scale
;;;; default-text-scale
(use-package default-text-scale
  :ensure t
  :config
  :hook (emacs-startup . default-text-scale-mode)
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("C-x C-0" . default-text-scale-reset)))
;;;; unset keys that are related to suspending
;; Unset suspend-frame and suspend-frame
(global-unset-key (kbd "C-x C-z"))
;;;; unset keys that are related to default-text-scale
;; Unset mouse-wheel-text-scale
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))
;; Unset more things that scale up/down text
(global-unset-key (kbd "<wheel-down>"))
(global-unset-key (kbd "<wheel-up>"))
(global-unset-key (kbd "<pinch>"))
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
;;; load main config
(when (file-readable-p (locate-user-emacs-file "main.el"))
  (load (locate-user-emacs-file "main.el")))
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
