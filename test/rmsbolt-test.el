;;; rmsbolt-test.el --- Tests for rmsbolt

;;; Commentary:
;; Tests for rmsbolt

;;; Code:

(require 'el-mock)
(require 'rmsbolt)

(ert-deftest sanity-check-ert ()
  "Check if ERT is working. :)"
  (should t))

(defun test-asm-preprocessor (pre post)
  "Tests the asm preprocessor on the current buffer."
  (insert-file-contents pre)
  (should
   (string=
    (string-trim
     (mapconcat 'identity
                (rmsbolt--process-asm-lines (current-buffer)
                                            (split-string (buffer-string) "\n" t))
                "\n"))
    (with-temp-buffer
      (insert-file-contents post)
      (string-trim
       (buffer-string))))))

(ert-deftest filter-tests-all-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-dissasemble nil)
    (setq-local rmsbolt-filter-comment-only t)
    (setq-local rmsbolt-filter-directives t)
    (setq-local rmsbolt-filter-labels t)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post1.s")))
(ert-deftest filter-tests-none-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-dissasemble nil)
    (setq-local rmsbolt-filter-comment-only nil)
    (setq-local rmsbolt-filter-directives nil)
    (setq-local rmsbolt-filter-labels nil)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post2.s")))
(ert-deftest filter-tests-dir-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-dissasemble nil)
    (setq-local rmsbolt-filter-comment-only nil)
    (setq-local rmsbolt-filter-directives t)
    (setq-local rmsbolt-filter-labels nil)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post3.s")))

;;;; Filtration tests

;;; rmsbolt-test.el ends here
