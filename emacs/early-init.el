;; Example Elpaca early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq load-path-filter-function #'load-path-filter-cache-directory-files)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
