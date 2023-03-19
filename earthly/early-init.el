(condition-case e
    (progn
      (load "/.config/emacs/early-init.el")
      (message "-OK-"))
  (error
   (message "ERROR!")
   (signal (car e) (cdr e))))
