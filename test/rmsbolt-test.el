;;; rmsbolt-test.el --- Tests for rmsbolt  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for rmsbolt

;;; Code:

(require 'el-mock nil t)
(require 'rmsbolt)

(ert-deftest sanity-check-ert ()
  "Check if ERT is working. :)"
  (should t))

(defun test-asm-preprocessor (pre post)
  "Tests the asm preprocessor on the current buffer."
  (insert-file-contents pre)
  (setq-local buffer-file-name (expand-file-name default-directory))
  (let
      ((source
        (string-trim
         (string-join
          (rmsbolt--process-asm-lines (current-buffer)
                                      (split-string (buffer-string) "\n" t))
          "\n")))
       (target
        (with-temp-buffer
          (insert-file-contents post)
          (string-trim
           (buffer-string)))))
    (should (string= source target))))

;;;; Filtration tests

(ert-deftest filter-tests-all-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-disassemble nil)
    (setq-local rmsbolt-filter-comment-only t)
    (setq-local rmsbolt-filter-directives t)
    (setq-local rmsbolt-filter-labels t)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post1.s")))
(ert-deftest filter-tests-none-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-disassemble nil)
    (setq-local rmsbolt-filter-comment-only nil)
    (setq-local rmsbolt-filter-directives nil)
    (setq-local rmsbolt-filter-labels nil)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post2.s")))
(ert-deftest filter-tests-dir-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-disassemble nil)
    (setq-local rmsbolt-filter-comment-only nil)
    (setq-local rmsbolt-filter-directives t)
    (setq-local rmsbolt-filter-labels nil)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post3.s")))
(ert-deftest filter-tests-weak-ref-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-disassemble nil)
    (setq-local rmsbolt-filter-comment-only nil)
    (setq-local rmsbolt-filter-directives t)
    (setq-local rmsbolt-filter-labels t)
    (test-asm-preprocessor "test/rmsbolt-c-pre2.s" "test/rmsbolt-c-post4.s")))

;;;; Demangler tests

(ert-deftest demangler-test-disabled ()
  (with-temp-buffer
    (setq-local rmsbolt-demangle nil)
    (should
     (string-empty-p
      (rmsbolt--demangle-command
       ""
       (make-rmsbolt-lang :demangler nil)
       (current-buffer))))))

(ert-deftest demangler-test-invalid-demangler ()
  (with-temp-buffer
    (setq-local rmsbolt-demangle t)
    (should
     (string-empty-p
      (rmsbolt--demangle-command
       ""
       (make-rmsbolt-lang :demangler nil)
       (current-buffer))))))

(ert-deftest demangler-test-not-path ()
  (with-temp-buffer
    (setq-local rmsbolt-demangle t)
    (should
     (string-empty-p
      (rmsbolt--demangle-command
       ""
       (make-rmsbolt-lang :demangler "nonsense-binary-name-not-on-path")
       (current-buffer))))))

(ert-deftest demangler-test-valid-demangler ()
  ;; Assumes test is on the path!
  (with-temp-buffer
    (setq-local rmsbolt-demangle t)
    (should
     (string-match-p
      (regexp-opt '("test"))
      (rmsbolt--demangle-command
       ""
       (make-rmsbolt-lang :demangler "test")
       (current-buffer))))))


;;; rmsbolt-test.el ends here
(provide 'rmsbolt-test)
