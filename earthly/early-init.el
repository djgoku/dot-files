(condition-case e
    (progn
      (load "/root/.config/emacs/early-init.el")
      (message "-OK-"))
  (error
   (message "ERROR!")
   (signal (car e) (cdr e))))
