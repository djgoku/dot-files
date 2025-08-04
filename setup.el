;; -*- coding: utf-8; lexical-binding: t; -*-

(defun print-listing-of-stow-target (target)
  ""
  (princ (format "Directory Listing %s\n%s" target (shell-command-to-string (concat "ls -la " target)))))

(defun stow (things-to-stow &optional stow-source-switch)
  ""
  (dolist (elt things-to-stow)
    (let* ((directory (car elt))
           (target (cdr elt)))
      (mkdir target t)
      ;; (print-listing-of-stow-target target)
      (cd directory)
      (shell-command-to-string (concat "stow " (or stow-source-switch "") " . --target " target))
      (print-listing-of-stow-target target)
      (cd ".."))))

;; turn off natural scrolling
(shell-command-to-string "defaults write -g com.apple.swipescrolldirection -boolean NO")

;; dark mode

(shell-command-to-string "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'")

(stow '(("devbox" . "~/.local/share/devbox/global/default")
        ("mise" . "~/.config/mise")
        ("emacs" . "~/.config/emacs")) ;; "-D"
      )

;; install mise
;;; add mise to .zshrc
(if (file-exists-p "~/.local/bin/mise")
    (princ "\n~/.local/bin/mise already exists nothing to do here.\n\n")
  (mkdir "~/.local/bin" t)
  (princ (shell-command-to-string "curl -s https://api.github.com/repos/jdx/mise/releases/latest | jq -r '.assets[] | select (.name | endswith(\"macos-arm64\")) | .browser_download_url' | xargs -I{} sh -c 'echo \"downloading {}\n\n\" && curl -L {} > ~/.local/bin/mise'"))
  (shell-command-to-string "chmod +x ~/.local/bin/mise"))

;; install devbox
;;; add devbox global to .zshrc
;; curl -fsSL https://get.jetify.com/devbox | bash
