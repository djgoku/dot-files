(condition-case e
    (progn
      (load "/.config/emacs/early-init.el")
      (when (file-readable-p "/.config/emacs/config.org")
        (org-babel-load-file (expand-file-name "/.config/emacs/config.org")))
      (message "-OK-"))
  (error
   (message "ERROR!")
   (signal (car e) (cdr e))))
