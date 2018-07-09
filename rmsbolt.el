;;; rmsbolt.el --- A compiler output viewer for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jay Kamat
;; Author: Jay Kamat <jaygkamat@gmail.com>
;; Version: 0.0.1
;; Keywords: compilation
;; URL: http://gitlab.com/jgkamat/rmsbolt
;; Package-Requires: ((emacs "25.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO create commentary

;;; Constants:

(require 'cl-lib)

(defconst +rmsbolt-compile-name+ "rmsbolt-compile")

(defconst +rmsbolt-assembler-pattern+ (rx bol (1+ space)
                                          "." (1+ (not (any ";")))
                                          (0+ space) eol))

;;; Code:
;;;; Variables:
(defvar rmsbolt-temp-dir nil
  "Temporary directory to use for compilation and other reasons.")
(defvar rmsbolt-shell "bash"
  "Shell rmsbolt will use to split paths.")
(defvar rmsbolt-output-buffer "*rmsbolt-output*")

(defvar rmsbolt-output-filename "rmsbolt.s")
(defvar rmsbolt-hide-compile t)
(defvar rmsbolt-intel-x86 t)
(defvar rmsbolt-filter-asm-directives t)

;;;; Macros

(defmacro rmsbolt-with-display-buffer-no-window (&rest body)
  ;; See http://debbugs.gnu.org/13594
  `(let ((display-buffer-overriding-action
          (if rmsbolt-hide-compile
              (list #'display-buffer-no-window)
            display-buffer-overriding-action)))
     ,@body))


;;;; Functions

(defun rmsbolt--process-asm-lines (asm-lines)
  "Process and filter a set of asm lines."
  (when rmsbolt-filter-asm-directives
    (setq asm-lines
          (cl-remove-if
           (apply-partially #'string-match-p +rmsbolt-assembler-pattern+)
           asm-lines)))
  (mapconcat 'identity
             asm-lines
             "\n"))

(defun rmsbolt--handle-finish-compile (buffer _str)
  "Finish hook for compilations."
  (let ((compilation-fail
         (with-current-buffer buffer
           (eq 'compilation-mode-line-fail
               (get-text-property 0 'face (car mode-line-process)))))
        (default-directory (buffer-local-value 'default-directory buffer)))

    (with-current-buffer (get-buffer-create rmsbolt-output-buffer)
      (cond ((not compilation-fail)
             (if (not (file-exists-p rmsbolt-output-filename))
                 (message "Error reading from output file.")
               (delete-region (point-min) (point-max))
               (insert
                (rmsbolt--process-asm-lines
                 (with-temp-buffer
                   (insert-file-contents rmsbolt-output-filename)
                   (split-string (buffer-string) "\n" t))))
               (asm-mode)
               (display-buffer (current-buffer))))
            (t
             ;; Display compilation output
             (display-buffer buffer))))))

(defun rmsbolt-compile ()
  "Compile the current rmsbolt buffer."
  (interactive)
  (save-some-buffers nil (lambda () rmsbolt-mode))
  (let* ((cmd "gcc -O0")
         (cmd (mapconcat 'identity
                         (list cmd
                               "-S" (buffer-file-name)
                               "-o" rmsbolt-output-filename
                               (when rmsbolt-intel-x86
                                 "-masm=intel"))
                         " ")))

    (rmsbolt-with-display-buffer-no-window
     (with-current-buffer (compilation-start cmd)
       (add-hook 'compilation-finish-functions
                 #'rmsbolt--handle-finish-compile nil t))))

  ;; TODO
  )

;;;; Alda Keymap
(defvar rmsbolt-mode-map nil "Keymap for `alda-mode'.")
(when (not rmsbolt-mode-map) ; if it is not already defined

  ;; assign command to keys
  (setq rmsbolt-mode-map (make-sparse-keymap))
  (define-key rmsbolt-mode-map (kbd "C-c C-c") #'rmsbolt-compile))

;;;; Init commands

(defun rmsbolt-c ()
  (interactive)
  (let* ((file-name (expand-file-name "rmsbolt.c" rmsbolt-temp-dir))
         (exists (file-exists-p file-name)))
    (find-file file-name)
    (unless exists
      (insert
       "#include <stdio.h>

// RMS: gcc -O0

int isRMS(int a) {
	 switch (a) {
	 case 'R':
		  return 1;
	 case 'M':
		  return 2;
	 case 'S':
		  return 3;
	 default:
		  return 0;
	 }
}

int main() {
		char a = 1 + 1;
		if (isRMS(a))
			 printf(\"%c\\n\", a);
}
")
      (save-buffer))

    (unless rmsbolt-mode
      (rmsbolt-mode 1))
    )
  )

;;;; Mode Definition:

;;;###autoload
;; TODO handle more modes than c-mode
(define-minor-mode rmsbolt-mode
  "RMSbolt"
  nil "RMSBolt" rmsbolt-mode-map
  (unless rmsbolt-temp-dir
    (setq rmsbolt-temp-dir
          (make-temp-file "rmsbolt-" t))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (delete-directory rmsbolt-temp-dir t)
                (setq rmsbolt-temp-dir nil))))

  )

(provide 'rmsbolt)

;;; rmsbolt.el ends here
