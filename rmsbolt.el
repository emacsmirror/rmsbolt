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
(require 'subr-x)
(require 'map)

;;; Code:
;;;; Customize:
(defgroup rmsbolt nil
  "rmsbolt customization options"
  :group 'applications)

(defcustom rmsbolt-dissasemble nil
  "Whether we should dissasemble an output binary."
  :type 'boolean
  :safe 'booleanp)
(defcustom rmsbolt-command nil
  "The base command to run rmsbolt from."
  :type 'string
  :safe 'stringp)


;;;; Variables:
(defvar rmsbolt-temp-dir nil
  "Temporary directory to use for compilation and other reasons.")
(defvar rmsbolt-shell "bash"
  "Shell rmsbolt will use to split paths.")
(defvar rmsbolt-output-buffer "*rmsbolt-output*")

(defvar rmsbolt-hide-compile t)
(defvar rmsbolt-intel-x86 t)
(defvar rmsbolt-filter-asm-directives t)
(defvar rmsbolt-filter-unused-labels t)
(defvar rmsbolt-filter-comment-only t)
(defvar rmsbolt-binary-asm-limit 5000)
(defun rmsbolt-output-filename (options &optional asm)
  (if (and (not asm)
           (rmsbolt-o-dissasemble options))
      (expand-file-name "rmsbolt.out" rmsbolt-temp-dir)
    (expand-file-name "rmsbolt.s" rmsbolt-temp-dir)))

(defvar-local rmsbolt-dissasemble nil)


;;;; Regexes

(defvar rmsbolt-label-def  (rx bol (group (any ".a-zA-Z_$@")
                                          (0+ (any "a-zA-Z0-9$_@.")))
                               ":"))
(defvar rmsbolt-defines-global (rx bol (0+ space) ".glob"
                                   (opt "a") "l" (0+ space)
                                   (group (any ".a-zA-Z_")
                                          (0+ (any "a-zA-Z0-9$_.")))))
(defvar rmsbolt-label-find (rx (any ".a-zA-Z_")
                               (0+
                                (any "a-zA-Z0-9$_."))))
(defvar rmsbolt-assignment-def (rx bol (0+ space)
                                   (group (any ".a-zA-Z_$")
                                          (1+ (any "a-zA-Z0-9$_.")))
                                   (0+ space) "="))
(defvar rmsbolt-has-opcode (rx bol (0+ space)
                               (any "a-zA-Z")))

(defvar rmsbolt-defines-function (rx bol (0+ space) ".type"
                                     (0+ any) "," (0+ space) (any "@%")
                                     "function" eol))
(defvar rmsbolt-data-defn (rx bol (0+ space) "."
                              (group (or "string" "asciz" "ascii"
                                         (and
                                          (optional (any "1248")) "byte")
                                         "short" "word" "long" "quad" "value" "zero"))))
(defvar rmsbolt-directive (rx bol (0+ space) "." (0+ any) eol))
(defvar rmsbolt-endblock (rx "." (or "cfi_endproc" "data" "text" "section")))
(defvar rmsbolt-comment-only (rx bol (0+ space) (or (and (or (any "#@;") "//"))
                                                    (and "/*" (0+ any) "*/"))
                                 (0+ any) eol))
(defvar rmsbolt-dissas-line (rx bol
                                (group "/" (1+ (not (any ":")))) ":"
                                (group (1+ num))
                                (0+ any)))
(defvar rmsbolt-dissas-label (rx bol (group (1+ (any digit "a-f")))
                                 (1+ space) "<"
                                 (group (1+ (not (any ">")))) ">:" eol))
(defvar rmsbolt-dissas-dest (rx (0+ any) (group (1+ (any digit "a-f")))
                                (1+ space) "<" (group (1+ (not (any ">")))) ">" eol))

(defvar rmsbolt-dissas-opcode (rx bol (0+ space) (group (1+ (any digit "a-f")))
                                  ":" (0+ space)
                                  (group (1+
                                          (repeat 2
                                                  (any digit "a-f"))
                                          (opt " ")))
                                  (0+ space)
                                  (group (0+ any))))

;;;; Classes

(cl-defstruct (rmsbolt-options
               (:conc-name rmsbolt-o-))
  (compile-cmd
   ""
   :type 'string
   :documentation "The command used to compile this file")
  (lang
   nil
   :type 'symbol
   :documentation "The major mode language we are compiling in."
   )
  (dissasemble
   nil
   :type 'bool
   :documentation "Whether we should compile to binary and dissasemble that."))

(cl-defstruct (rmsbolt-lang
               (:conc-name rmsbolt-l-))
  (options
   nil
   :type 'rmsbolt-options
   :documentation "The default options object to use.")
  (mode
   'fundamental-mode
   :type 'symbol
   :documentation "The mode to activate this language in.")
  (supports-binary
   nil
   :type 'bool
   :documentation "If we support binary dumping with this language.")
  (objdumper
   'objdump
   :type symbol
   :documentation "The object dumper to use if dissasembling binary.")
  (starter-file
   nil
   :type 'string
   :documentation "Some starter code to jump off of, if not supplying your own.")
  (starter-file-name
   nil
   :type 'string
   :documentation "The starter filename to use")
  (dissas-hidden-funcs
   nil
   :type 'string
   :documentation "Functions that are hidden when dissasembling.")
  (compile-cmd-function
   nil
   :type 'function
   :documentation "A function which takes in a compile command (could be the default) and adds needed args to it."))


(defun rmsbolt--c-compile-cmd (options)
  "Process a compile command for gcc/clang."
  (let* ((cmd (rmsbolt-o-compile-cmd options))
         (cmd (mapconcat 'identity
                         (list cmd
                               "-g"
                               (if (rmsbolt-o-dissasemble options)
                                   ""
                                 "-S")
                               (buffer-file-name)
                               "-o" (rmsbolt-output-filename options)
                               (when rmsbolt-intel-x86
                                 "-masm=intel"))
                         " ")))
    cmd))
(defvar rmsbolt--hidden-func-c (rx bol (or (and "__" (0+ any))
                                           (and "_" (or "init" "start" "fini"))
                                           (and (opt "de") "register_tm_clones")
                                           "call_gmon_start"
                                           "frame_dummy"
                                           (and ".plt" (0+ any)))
                                   eol))
(defvar rmsbolt-languages)
(setq
 rmsbolt-languages
 `((c-mode
    . ,(make-rmsbolt-lang :mode 'c
                          :options (make-rmsbolt-options
                                    :compile-cmd "gcc"
                                    :lang 'c-mode)
                          :supports-binary t
                          :starter-file-name "rmsbolt.c"
                          :compile-cmd-function #'rmsbolt--c-compile-cmd
                          :dissas-hidden-funcs rmsbolt--hidden-func-c
                          :starter-file
                          "#include <stdio.h>

// Local Variables:
// rmsbolt-command: \"gcc -O3\"
// rmsbolt-dissasemble: nil
// End:

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
"
                          ))
   (c++-mode
    . ,(make-rmsbolt-lang :mode 'c++-mode
                          :options (make-rmsbolt-options
                                    :compile-cmd "g++"
                                    :lang 'c++-mode)
                          :supports-binary t
                          :starter-file-name "rmsbolt.cpp"
                          :compile-cmd-function #'rmsbolt--c-compile-cmd
                          :dissas-hidden-funcs rmsbolt--hidden-func-c
                          :starter-file
                          "#include <iostream>

// Local Variables:
// rmsbolt-command: \"g++ -O3\"
// rmsbolt-dissasemble: nil
// End:

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
			 std::cout << a << std::endl;
}
"
                          ))))


;;;; Macros

(defmacro rmsbolt-with-display-buffer-no-window (&rest body)
  ;; See http://debbugs.gnu.org/13594
  `(let ((display-buffer-overriding-action
          (if rmsbolt-hide-compile
              (list #'display-buffer-no-window)
            display-buffer-overriding-action)))
     ,@body))


;;;; Functions
;; Functions to parse and lint assembly were lifted almost directly from the compiler-exporter

(defun rmsbolt-re-seq (regexp string)
  "Get list of all REGEXP match in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

;;;;; Filter Functions
(defun rmsbolt--has-opcode-p (line)
  "Check if LINE has opcodes."
  (save-match-data
    (let* ((match (string-match rmsbolt-label-def line))
           (line (if match
                     (substring line (match-end 0))
                   line))
           (line (cl-first (split-string line (rx (1+ (any ";#")))))))
      (if (string-match-p rmsbolt-assignment-def line)
          nil
        (string-match-p rmsbolt-has-opcode line)))))

(defun rmsbolt--find-used-labels (asm-lines)
  "Find used labels in asm-lines."
  (let ((match nil)
        (current-label nil)
        (labels-used nil)
        (trimmed-line nil)
        (weak-usages (make-hash-table :test #'equal)))
    (dolist (line asm-lines)
      (setq trimmed-line (string-trim line))

      (setq match (and
                   (string-match rmsbolt-label-def line)
                   (match-string 1 line)))
      (when match
        (setq current-label match))
      (setq match (and (string-match rmsbolt-defines-global line)
                       (match-string 1 line)))
      (when match
        (cl-pushnew match labels-used :test #'equal))
      ;; When we have no line or a period started line, skip
      (unless (or (= 0 (length line))
                  (string-prefix-p "." line)
                  (not (string-match-p rmsbolt-label-find line)))
        (if (or (not rmsbolt-filter-asm-directives)
                (rmsbolt--has-opcode-p line)
                (string-match-p rmsbolt-defines-function line))
            ;; Add labels indescriminantly
            (dolist (l (rmsbolt-re-seq rmsbolt-label-find line))
              (cl-pushnew l labels-used :test #'equal))

          (when (and current-label
                     (or (string-match-p rmsbolt-data-defn line)
                         (rmsbolt--has-opcode-p line)))
            (dolist (l (rmsbolt-re-seq rmsbolt-label-find line))
              (cl-pushnew l (gethash current-label weak-usages) :test #'equal))))))

    (let* ((max-label-iter 10)
           (label-iter 0)
           (completed nil))

      (while (and (<= (incf label-iter)
                      max-label-iter)
                  (not completed))
        (let ((to-add nil))
          (mapc
           (lambda (label)
             (mapc
              (lambda(now-used)
                (when (not (cl-find now-used labels-used :test #'equal))
                  (cl-pushnew now-used to-add :test #'equal)))
              (gethash label weak-usages)))
           labels-used)
          (if to-add
              (mapc (lambda (l) (cl-pushnew l labels-used :test #'equal)) to-add)
            (setq completed t))))
      labels-used)))

(defun rmsbolt--user-func-p (opts func)
  "Return t if FUNC is a user function."
  (let* ((lang (rmsbolt--get-lang (rmsbolt-o-lang opts)))
         (regexp (rmsbolt-l-dissas-hidden-funcs lang)))
    (if regexp
        (not (string-match-p regexp func))
      t)))

(cl-defun rmsbolt--process-dissasembled-lines (opts asm-lines)
  "Process and filter dissasembled ASM-LINES from OPTS."
  (let* ((result nil)
         (func nil)
         (match nil))
    (dolist (line asm-lines)
      (cl-tagbody
       (when (> (length result) rmsbolt-binary-asm-limit)
         (return-from rmsbolt--process-dissasembled-lines
           '("Aborting processing due to exceeding the binary limit.")))
       ;; TODO process line numbers
       (when (string-match rmsbolt-dissas-label line)
         (setq func (match-string 2 line))
         (when (rmsbolt--user-func-p opts func)
           (push (concat func ":") result))
         (go continue))
       (unless (and func
                    (rmsbolt--user-func-p opts func))
         (go continue))
       (when (string-match rmsbolt-dissas-opcode line)
         (push (concat "\t" (match-string 3 line)) result)
         (go continue))
       continue))
    (mapconcat 'identity
               (nreverse result)
               "\n")))

(cl-defun rmsbolt--process-asm-lines (opts asm-lines)
  "Process and filter a set of asm lines."
  (if (rmsbolt-o-dissasemble opts)
      (rmsbolt--process-dissasembled-lines opts asm-lines)
    (let ((used-labels (rmsbolt--find-used-labels asm-lines))
          (result nil)
          (prev-label nil))
      (dolist (line asm-lines)
        (let* ((raw-match (or (string-match rmsbolt-label-def line)
                              (string-match rmsbolt-assignment-def line)))
               (match (when raw-match
                        (match-string 1 line)))
               (used-label (cl-find match used-labels :test #'equal)))
          (cl-tagbody
           ;; TODO process line numbers
           ;; End block, reset prev-label and source
           (when (string-match-p rmsbolt-endblock line)
             (setq prev-label nil))

           (when (and rmsbolt-filter-comment-only
                      (string-match-p rmsbolt-comment-only line))
             (go continue))

           ;; continue means we don't add to the ouptut
           (when match
             (if (not used-label)
                 ;; Unused label
                 (when rmsbolt-filter-unused-labels
                   (go continue))
               ;; Real label, set prev-label
               (setq prev-label raw-match)))
           (when (and rmsbolt-filter-asm-directives
                      (not match))
             (if  (and (string-match-p rmsbolt-data-defn line)
                       prev-label)
                 ;; data is being used
                 nil
               (when (string-match-p rmsbolt-directive line)
                 (go continue))))
           (push line result)
           continue)))
      (mapconcat 'identity
                 (nreverse result)
                 "\n"))))

;;;;; Handlers
(defun rmsbolt--handle-finish-compile (buffer _str)
  "Finish hook for compilations."
  (let ((compilation-fail
         (with-current-buffer buffer
           (eq 'compilation-mode-line-fail
               (get-text-property 0 'face (car mode-line-process)))))
        (default-directory (buffer-local-value 'default-directory buffer))
        (opts (buffer-local-value 'rmsbolt-current-options buffer)))

    (with-current-buffer (get-buffer-create rmsbolt-output-buffer)
      (cond ((not compilation-fail)
             (if (not (file-exists-p (rmsbolt-output-filename opts t)))
                 (message "Error reading from output file.")
               (delete-region (point-min) (point-max))
               (insert
                (rmsbolt--process-asm-lines
                 opts
                 (with-temp-buffer
                   (insert-file-contents (rmsbolt-output-filename opts t))
                   (split-string (buffer-string) "\n" t))))
               (asm-mode)
               (display-buffer (current-buffer))))
            (t
             ;; Display compilation output
             (display-buffer buffer))))))

;;;;; Parsing Options
(defun rmsbolt--get-lang (&optional language)
  (cdr-safe (assoc (or language major-mode) rmsbolt-languages)))
(defun rmsbolt--parse-options ()
  "Parse RMS options from file."
  (let* ((lang (rmsbolt--get-lang))
         (options (copy-rmsbolt-options
                   (rmsbolt-l-options lang)))
         (cmd rmsbolt-command))
    (when cmd
      (setf (rmsbolt-o-compile-cmd options) cmd))
    (setf (rmsbolt-o-dissasemble options)
          rmsbolt-dissasemble)
    (prin1 (rmsbolt-o-dissasemble options))
    options))

;;;;; UI Functions
(defvar-local rmsbolt-current-options nil)
(defun rmsbolt-compile ()
  "Compile the current rmsbolt buffer."
  (interactive)
  (save-some-buffers nil (lambda () rmsbolt-mode))
  (hack-local-variables)
  (let* ((options (rmsbolt--parse-options))
         (lang (rmsbolt--get-lang))
         (func (rmsbolt-l-compile-cmd-function lang))
         (cmd (funcall func options)))

    (when (rmsbolt-o-dissasemble options)
      (pcase
          (rmsbolt-l-objdumper lang)
        ('objdump
         (setq cmd
               (mapconcat 'identity
                          (list cmd
                                "&&"
                                "objdump" "-d" (rmsbolt-output-filename options)
                                "-C" "--insn-width=16" "-l"
                                "-M" (if rmsbolt-intel-x86
                                         "intel"
                                       "att")
                                ">" (rmsbolt-output-filename options t))
                          " ")))
        (_
         (error "Objdumper not recognized"))))
    (rmsbolt-with-display-buffer-no-window
     (with-current-buffer (compilation-start cmd)
       (add-hook 'compilation-finish-functions
                 #'rmsbolt--handle-finish-compile nil t)
       (setq-local rmsbolt-current-options options)))))

;;;; Keymap
(defvar rmsbolt-mode-map nil "Keymap for `rmsbolt-mode'.")
(when (not rmsbolt-mode-map) ; if it is not already defined
  ;; assign command to keys
  (setq rmsbolt-mode-map (make-sparse-keymap))
  (define-key rmsbolt-mode-map (kbd "C-c C-c") #'rmsbolt-compile))

;;;; Init commands


(defun rmsbolt-starter (lang-mode)
  "Code for fully setting up a language from LANG-MODE."
  (let* ((lang-def (rmsbolt--get-lang lang-mode))
         (file-name
          (expand-file-name (rmsbolt-l-starter-file-name lang-def) rmsbolt-temp-dir))
         (exists (file-exists-p file-name)))
    (find-file file-name)
    (unless exists
      (insert
       (rmsbolt-l-starter-file lang-def))
      (save-buffer))

    (unless rmsbolt-mode
      (rmsbolt-mode 1)))
  )
(defun rmsbolt-c ()
  (interactive)
  (rmsbolt-starter 'c-mode))
(defun rmsbolt-c++ ()
  (interactive)
  (rmsbolt-starter 'c++-mode))

;;;; Mode Definition:

;;;###autoload
;; TODO handle more modes than c-mode
(define-minor-mode rmsbolt-mode
  "RMSbolt"
  nil "RMSBolt" rmsbolt-mode-map
  (unless (and rmsbolt-temp-dir
               (file-exists-p rmsbolt-temp-dir))
    (setq rmsbolt-temp-dir
          (make-temp-file "rmsbolt-" t))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (delete-directory rmsbolt-temp-dir t)
                (setq rmsbolt-temp-dir nil)))))

(provide 'rmsbolt)

;;; rmsbolt.el ends here
