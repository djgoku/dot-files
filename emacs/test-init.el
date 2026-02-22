;;; test-init.el --- Emacs config smoke tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded via: emacs --init-directory=./emacs -l ./emacs/test-init.el
;;
;; Two scenarios:
;; - Cached packages (local): elpaca finishes before -l runs, tests run immediately
;; - Fresh install (CI): elpaca still processing, we hook elpaca-after-init-hook

;;; Code:

(defvar test-init--results nil
  "List of (NAME PASS-P MESSAGE) test results.")

(defvar test-init--timeout 300
  "Seconds to wait before declaring timeout failure.")

;;; Individual tests

(defun test-init--check-init ()
  "Verify core init loaded: elpaca, use-package, main.el symbols."
  (let ((failures nil))
    (unless (featurep 'elpaca)
      (push "elpaca feature not loaded" failures))
    (unless (featurep 'use-package)
      (push "use-package feature not loaded" failures))
    (unless (fboundp 'johnny--setup-ripgrep)
      (push "johnny--setup-ripgrep not defined (main.el not loaded)" failures))
    (unless (bound-and-true-p elpaca-after-init-time)
      (push "elpaca-after-init-time not set" failures))
    (if failures
        (push (list "init" nil (mapconcat #'identity failures "; ")) test-init--results)
      (push (list "init" t "elpaca, use-package, and main.el loaded") test-init--results))))

(defun test-init--check-ripgrep ()
  "Verify ripgrep executable is detected and executable."
  (cond
   ((not (boundp 'johnny--maybe-ripgrep-executable))
    (push (list "ripgrep" nil "johnny--maybe-ripgrep-executable not defined") test-init--results))
   ((null johnny--maybe-ripgrep-executable)
    (push (list "ripgrep" nil "johnny--maybe-ripgrep-executable is nil") test-init--results))
   ((not (file-executable-p johnny--maybe-ripgrep-executable))
    (push (list "ripgrep" nil (format "%s is not executable" johnny--maybe-ripgrep-executable)) test-init--results))
   (t
    (push (list "ripgrep" t (format "found %s" johnny--maybe-ripgrep-executable)) test-init--results))))

(defun test-init--check-vterm ()
  "Verify vterm loads and can create a buffer."
  (condition-case err
      (progn
        (require 'vterm)
        (let ((buf (vterm "test-init-vterm")))
          (unwind-protect
              (with-current-buffer buf
                (if (eq major-mode 'vterm-mode)
                    (push (list "vterm" t "vterm buffer created and cleaned up") test-init--results)
                  (push (list "vterm" nil (format "expected vterm-mode, got %s" major-mode)) test-init--results)))
            (let ((proc (get-buffer-process buf)))
              (when proc (delete-process proc)))
            (kill-buffer buf))))
    (error
     (push (list "vterm" nil (format "error: %s" (error-message-string err))) test-init--results))))

;;; Report and exit

(defun test-init--report-and-exit ()
  "Print results, write results file, and exit."
  (let* ((results (reverse test-init--results))
         (all-pass (cl-every (lambda (r) (nth 1 r)) results))
         (exit-code (if all-pass 0 1))
         (results-file (expand-file-name "emacs-test-results.txt"
                                         (or (getenv "TMPDIR") "/tmp"))))
    (message "")
    (message "=== Emacs Config Smoke Test Results ===")
    (dolist (r results)
      (message "  %s %s: %s"
               (if (nth 1 r) "PASS" "FAIL")
               (nth 0 r)
               (nth 2 r)))
    (message "===")
    (message "Overall: %s" (if all-pass "ALL PASSED" "SOME FAILED"))
    (message "")

    (with-temp-file results-file
      (dolist (r results)
        (insert (format "%s\t%s\t%s\n"
                        (if (nth 1 r) "PASS" "FAIL")
                        (nth 0 r)
                        (nth 2 r))))
      (insert (format "EXIT\t%d\n" exit-code)))

    (kill-emacs exit-code)))

;;; Run tests

(defun test-init--run-tests ()
  "Run all smoke tests and exit."
  (test-init--check-init)
  (test-init--check-ripgrep)
  (test-init--check-vterm)
  (test-init--report-and-exit))

;;; Entry point — handle both cached and fresh-install scenarios

(if (bound-and-true-p elpaca-after-init-time)
    ;; Cached: elpaca already finished before -l ran
    (progn
      (message "test-init: elpaca already done, running tests immediately")
      (test-init--run-tests))
  ;; Fresh install: elpaca still processing, wait for it
  (message "test-init: waiting for elpaca to finish (timeout: %ds)..." test-init--timeout)
  (add-hook 'elpaca-after-init-hook #'test-init--run-tests)
  (run-with-timer test-init--timeout nil
                  (lambda ()
                    (push (list "timeout" nil
                                (format "elpaca did not finish within %ds" test-init--timeout))
                          test-init--results)
                    (test-init--report-and-exit))))

(provide 'test-init)
;;; test-init.el ends here
