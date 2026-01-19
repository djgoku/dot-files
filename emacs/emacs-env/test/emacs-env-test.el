;;; emacs-env-test.el --- Tests for emacs-env -*- lexical-binding: t -*-

;; Author: Jonathan Carroll Otsuka
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools

;;; Commentary:
;; ERT tests for emacs-env.el

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the file under test
(require 'emacs-env)

;;; Test Infrastructure

(defvar emacs-env-test-temp-dir nil
  "Temporary directory for tests.")

(defvar emacs-env-test--saved-config nil
  "Saved configuration to restore after tests.")

(defun emacs-env-test-setup ()
  "Create temporary test directory structure and save config."
  (setq emacs-env-test--saved-config
        (list :config-base emacs-env-config-base
              :main-dir emacs-env-main-dir
              :git-branch emacs-env-git-branch
              :git-remote emacs-env-git-remote))
  (setq emacs-env-test-temp-dir (make-temp-file "emacs-env-test-" t))
  (setq emacs-env-config-base emacs-env-test-temp-dir))

(defun emacs-env-test-teardown ()
  "Clean up temporary test directory and restore config."
  (when (and emacs-env-test-temp-dir
             (file-directory-p emacs-env-test-temp-dir))
    (delete-directory emacs-env-test-temp-dir t))
  (setq emacs-env-test-temp-dir nil)
  (when emacs-env-test--saved-config
    (setq emacs-env-config-base (plist-get emacs-env-test--saved-config :config-base))
    (setq emacs-env-main-dir (plist-get emacs-env-test--saved-config :main-dir))
    (setq emacs-env-git-branch (plist-get emacs-env-test--saved-config :git-branch))
    (setq emacs-env-git-remote (plist-get emacs-env-test--saved-config :git-remote))))

;;; Directory Listing Tests

(ert-deftest emacs-env-test-list-dated-dirs-empty ()
  "Test listing dated dirs when none exist."
  (emacs-env-test-setup)
  (unwind-protect
      (should (null (emacs-env--list-dated-dirs)))
    (emacs-env-test-teardown)))

(ert-deftest emacs-env-test-list-dated-dirs-sorted ()
  "Test listing dated dirs returns sorted list (newest first)."
  (emacs-env-test-setup)
  (unwind-protect
      (progn
        (make-directory (expand-file-name "dot-files-2025-12-01" emacs-env-test-temp-dir))
        (make-directory (expand-file-name "dot-files-2025-12-14" emacs-env-test-temp-dir))
        (make-directory (expand-file-name "dot-files-2025-12-07" emacs-env-test-temp-dir))
        (make-directory (expand-file-name "dot-files-main" emacs-env-test-temp-dir))
        (make-directory (expand-file-name "other-stuff" emacs-env-test-temp-dir))
        (let ((result (emacs-env--list-dated-dirs)))
          (should (equal result '("dot-files-2025-12-14"
                                  "dot-files-2025-12-07"
                                  "dot-files-2025-12-01")))))
    (emacs-env-test-teardown)))

(ert-deftest emacs-env-test-latest-source-fallback-to-main ()
  "Test fallback to main dir when no dated dirs exist."
  (emacs-env-test-setup)
  (unwind-protect
      (progn
        (make-directory (expand-file-name "dot-files-main" emacs-env-test-temp-dir))
        (let ((result (emacs-env--latest-source)))
          (should (string-suffix-p "dot-files-main" result))))
    (emacs-env-test-teardown)))

(ert-deftest emacs-env-test-latest-source-uses-newest ()
  "Test that latest dated dir is used when available."
  (emacs-env-test-setup)
  (unwind-protect
      (progn
        (make-directory (expand-file-name "dot-files-main" emacs-env-test-temp-dir))
        (make-directory (expand-file-name "dot-files-2025-12-01" emacs-env-test-temp-dir))
        (make-directory (expand-file-name "dot-files-2025-12-14" emacs-env-test-temp-dir))
        (let ((result (emacs-env--latest-source)))
          (should (string-suffix-p "dot-files-2025-12-14" result))))
    (emacs-env-test-teardown)))

(ert-deftest emacs-env-test-today-dirname-format ()
  "Test that today dirname has correct format."
  (let ((result (emacs-env--today-dirname)))
    (should (string-match-p "^dot-files-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" result))
    (should (string-match-p (format-time-string "%Y-%m-%d") result))))

;;; mise.toml Update Tests

(defconst emacs-env-test-mise-toml
  "[tools]
\"nix:github+djgoku/dot-files?dir=emacs-overlay&rev=abc123def456\" = \"latest\"

[tasks.emacs]
run = \"`mise which Emacs` --name emacs-2025-12-01 &\"

[tasks.emacs2]
run = \"`mise which Emacs` --name emacs2-2025-12-01 &\"

[tasks.emacs-nw]
run = \"`mise which Emacs` --name emacs-nw-2025-12-01 -nw\""
  "Sample mise.toml for testing.")

(ert-deftest emacs-env-test-update-mise-toml-rev ()
  "Test that SHA rev is updated in mise.toml."
  (emacs-env-test-setup)
  (unwind-protect
      (let ((test-file (expand-file-name "mise.toml" emacs-env-test-temp-dir)))
        (with-temp-file test-file
          (insert emacs-env-test-mise-toml))
        (emacs-env--update-mise-toml test-file "newsha789xyz" "2026-01-18")
        (let ((content (with-temp-buffer
                         (insert-file-contents test-file)
                         (buffer-string))))
          (should (string-match-p "rev=newsha789xyz" content))
          (should-not (string-match-p "rev=abc123def456" content))))
    (emacs-env-test-teardown)))

(ert-deftest emacs-env-test-update-mise-toml-names ()
  "Test that --name flags are updated with date."
  (emacs-env-test-setup)
  (unwind-protect
      (let ((test-file (expand-file-name "mise.toml" emacs-env-test-temp-dir)))
        (with-temp-file test-file
          (insert emacs-env-test-mise-toml))
        (emacs-env--update-mise-toml test-file "abc123" "2026-01-18")
        (let ((content (with-temp-buffer
                         (insert-file-contents test-file)
                         (buffer-string))))
          (should (string-match-p "--name emacs-2026-01-18" content))
          (should (string-match-p "--name emacs2-2026-01-18" content))
          (should (string-match-p "--name emacs-nw-2026-01-18" content))
          (should-not (string-match-p "2025-12-01" content))))
    (emacs-env-test-teardown)))

;;; Git Helper Tests

(ert-deftest emacs-env-test-git-count-output-empty ()
  "Test git count returns 0 for empty output."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile _destination _display &rest _args)
               0)))  ; exit code 0, empty buffer
    (should (= 0 (emacs-env--git-count-output "diff" "--name-only")))))

(ert-deftest emacs-env-test-git-count-output-with-files ()
  "Test git count returns correct count."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile _destination _display &rest _args)
               (insert "file1.el\nfile2.el\nfile3.el\n")
               0)))  ; exit code 0
    (should (= 3 (emacs-env--git-count-output "diff" "--name-only")))))

(ert-deftest emacs-env-test-emacs-type-gui-no-arg ()
  "Test emacs type returns 'emacs' in GUI mode without arg."
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda () t)))
    (should (equal "emacs" (emacs-env--emacs-type nil)))))

(ert-deftest emacs-env-test-emacs-type-gui-with-arg ()
  "Test emacs type returns 'emacs-nw' in GUI mode with arg."
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda () t)))
    (should (equal "emacs-nw" (emacs-env--emacs-type t)))))

(ert-deftest emacs-env-test-emacs-type-terminal ()
  "Test emacs type returns 'emacs-nw' in terminal mode."
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda () nil)))
    (should (equal "emacs-nw" (emacs-env--emacs-type nil)))
    (should (equal "emacs-nw" (emacs-env--emacs-type t)))))

;;; Prerequisite Check Tests

(ert-deftest emacs-env-test-check-prerequisites-missing-mise ()
  "Test error when mise is not found."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (not (string= cmd "mise")))))
    (should-error (emacs-env--check-prerequisites) :type 'user-error)))

(ert-deftest emacs-env-test-check-prerequisites-missing-git ()
  "Test error when git is not found."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd)
               (cond ((string= cmd "mise") "/usr/bin/mise")
                     ((string= cmd "git") nil)))))
    (should-error (emacs-env--check-prerequisites) :type 'user-error)))

(ert-deftest emacs-env-test-check-prerequisites-all-found ()
  "Test no error when all prerequisites found."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/something")))
    (should (eq nil (emacs-env--check-prerequisites)))))

;;; Launch Tests

(ert-deftest emacs-env-test-launch-errors-if-no-dirs ()
  "Test that launch errors when no directories exist."
  (emacs-env-test-setup)
  (unwind-protect
      (let ((err (should-error (emacs-env-launch nil) :type 'user-error)))
        (should (string-match-p "No environments found" (error-message-string err))))
    (emacs-env-test-teardown)))

(provide 'emacs-env-test)
;;; emacs-env-test.el ends here
