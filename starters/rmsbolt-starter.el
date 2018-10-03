;;; rmsbolt-starter.el --- Starter file for rmsbolt -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple starter!

;;; Code:

(defun my-apply (fn &rest args)
  "`apply's FN to ARGS."
  (apply fn args))

(defun is-rms (letter)
  "Check to see if a LETTER is RMS."
  (pcase letter
    ((or "R" "M" "S") t)
    (_ nil)
    (_ "I will never run!")))

(defun main ()
  "Main entrypoint."
  (let* ((a (my-apply (lambda (a) (- a (+ 20 21)))
                      999))
         (a (+ 1 1 1 a)))
    (message (is-rms a))))

(main)

;;; rmsbolt-starter.el ends here
