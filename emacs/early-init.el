;; Example Elpaca early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(let ((threshold gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'elpaca-after-init-hook (lambda () (setq gc-cons-threshold threshold))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
