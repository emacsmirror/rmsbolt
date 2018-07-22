;;; rmsbolt.el --- A compiler output viewer for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jay Kamat
;; Author: Jay Kamat <jaygkamat@gmail.com>
;; Version: 0.0.1
;; Keywords: compilation
;; URL: http://gitlab.com/jgkamat/rmsbolt
;; Package-Requires: ((emacs "25.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
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

(defcustom rmsbolt-use-overlays t
  "Whether we should use overlays to show matching code."
  :type 'boolean
  :group 'rmsbolt)
(defcustom rmsbolt-goto-match t
  "Whether we should goto the match in the other buffer if it is non visible."
  :type 'boolean
  :group 'rmsbolt)

;;;;; Buffer Local Tweakables
(defcustom rmsbolt-dissasemble nil
  "Whether we should dissasemble an output binary."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-command nil
  "The base command to run rmsbolt from."
  :type 'string
  ;; nil means use default command
  :safe (lambda (v) (or (booleanp v) (stringp v)))
  :group 'rmsbolt)
(defcustom rmsbolt-intel-x86 t
  "Whether to use intel x86 format or att."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-filter-directives t
  "Whether to filter assembly directives."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-filter-labels t
  "Whether to filter unused labels."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-filter-comment-only t
  "Whether to filter comment-only lines."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)
(defcustom rmsbolt-ignore-binary-limit nil
  "Whether to ignore the binary limit. Could hang emacs..."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)

;;;; Faces

(defface rmsbolt-current-line-face
  '((t (:weight bold)))
  "Face to fontify the current line for showing matches."
  :group 'fic-mode)

;;;; Variables:
(defvar rmsbolt-temp-dir nil
  "Temporary directory to use for compilation and other reasons.")
(defvar rmsbolt-shell "bash"
  "Shell rmsbolt will use to split paths.")
(defvar rmsbolt-output-buffer "*rmsbolt-output*")
;; whether rmsbolt-mode is enabled.
(defvar rmsbolt-mode)

(defvar rmsbolt-hide-compile t)
(defvar rmsbolt-binary-asm-limit 10000)
(defun rmsbolt-output-filename (src-buffer &optional asm)
  "Function for generating an output filename for SRC-BUFFER.

Outputs assembly file if ASM."
  (if (and (not asm)
           (buffer-local-value 'rmsbolt-dissasemble src-buffer))
      (expand-file-name "rmsbolt.out" rmsbolt-temp-dir)
    (expand-file-name "rmsbolt.s" rmsbolt-temp-dir)))

(defvar-local rmsbolt-line-mapping nil
  "Line mapping hashtable from source lines -> asm lines")
(defvar-local rmsbolt-current-line nil
  "Current line for fontifier.")

(defvar rmsbolt-overlays nil
  "List of overlays to use.")
(defvar rmsbolt-overlay-delay 0.125
  "Time in seconds to delay before showing overlays.")

(defvar rmsbolt--idle-timer nil
  "Idle timer for rmsbolt overlays.")

(defvar rmsbolt-dir (when load-file-name
                      (file-name-directory load-file-name))
  "The directory which rmsbolt is installed to.")

(defvar-local rmsbolt-src-buffer nil)

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
(defvar rmsbolt-source-tag (rx bol (0+ space) ".loc" (1+ space)
                               (group (1+ digit)) (1+ space)
                               (group (1+ digit))
                               (0+ any)))
(defvar rmsbolt-source-stab (rx bol (0+ any) ".stabn" (1+ space)
                                (group (1+ digit)) ",0,"
                                (group (1+ digit)) "," (0+ any)))

;;;; Classes

(cl-defstruct (rmsbolt-lang
               (:conc-name rmsbolt-l-))
  (mode
   'fundamental-mode
   :type 'symbol
   :documentation "The mode to activate this language in.")
  (supports-asm
   nil
   :type 'bool
   :documentation "If we support assembly directly. If nil, we must dissasemble.")
  (objdumper
   'objdump
   :type symbol
   :documentation "The object dumper to use if dissasembling binary.")
  (starter-file-name
   nil
   :type 'string
   :documentation "The starter filename to use")
  (dissas-hidden-funcs
   nil
   :type 'string
   :documentation "Functions that are hidden when dissasembling.")
  (compile-cmd
   nil
   :type 'string
   :documentation "Default compilation command to use if none is provided.")
  (compile-cmd-function
   nil
   :type 'function
   :documentation "A function which takes in a compile command (could be the default) and adds needed args to it."))


(cl-defun rmsbolt--c-compile-cmd (&key src-buffer)
  "Process a compile command for gcc/clang."
  (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (cmd (mapconcat 'identity
                         (list cmd
                               "-g"
                               (if (buffer-local-value 'rmsbolt-dissasemble src-buffer)
                                   ""
                                 "-S")
                               (buffer-file-name)
                               "-o" (rmsbolt-output-filename src-buffer)
                               (when (buffer-local-value 'rmsbolt-intel-x86 src-buffer)
                                 "-masm=intel"))
                         " ")))
    cmd))
(cl-defun rmsbolt--ocaml-compile-cmd (&key src-buffer)
  "Process a compile command for gcc/clang.

Needed as ocaml cannot output asm to a non-hardcoded file"
  (let* ((diss (buffer-local-value 'rmsbolt-dissasemble src-buffer))
         (output-filename (rmsbolt-output-filename src-buffer))
         (predicted-asm-filename (concat (file-name-sans-extension (buffer-file-name)) ".s"))
         (cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (cmd (mapconcat 'identity
                         (list cmd
                               "-g"
                               (if (buffer-local-value 'rmsbolt-dissasemble src-buffer)
                                   ""
                                 "-S")
                               (buffer-file-name)
                               (mapconcat #'identity
                                          (cond
                                           (diss
                                            (list "-o" output-filename))
                                           ((equal predicted-asm-filename output-filename)
                                            nil)
                                           (t
                                            (list "&&" "mv"
                                                  (concat (file-name-sans-extension (buffer-file-name))
                                                          ".s")
                                                  output-filename)))
                                          " "))
                         " ")))
    cmd))
(defvar rmsbolt--hidden-func-c (rx bol (or (and "__" (0+ any))
                                           (and "_" (or "init" "start" "fini"))
                                           (and (opt "de") "register_tm_clones")
                                           "call_gmon_start"
                                           "frame_dummy"
                                           (and ".plt" (0+ any)))
                                   eol))
(defvar rmsbolt--hidden-func-ocaml)
(setq rmsbolt--hidden-func-ocaml (rx bol
                                     (or
                                      (and "camlCamlinternalFormat__" (0+ any))
                                      ;; (0+ any)
                                      )
                                     eol))
;;;; Language Definitions
(defvar rmsbolt-languages)
(setq
 rmsbolt-languages
 `((c-mode
    . ,(make-rmsbolt-lang :mode 'c
                          :compile-cmd "gcc"
                          :supports-asm t
                          :starter-file-name "rmsbolt.c"
                          :compile-cmd-function #'rmsbolt--c-compile-cmd
                          :dissas-hidden-funcs rmsbolt--hidden-func-c))
   (c++-mode
    . ,(make-rmsbolt-lang :mode 'c++-mode
                          :compile-cmd "g++"
                          :supports-asm t
                          :starter-file-name "rmsbolt.cpp"
                          :compile-cmd-function #'rmsbolt--c-compile-cmd
                          :dissas-hidden-funcs rmsbolt--hidden-func-c))
   ;; In order to parse ocaml files, you need the emacs ocaml mode, tuareg
   (tuareg-mode
    . ,(make-rmsbolt-lang :mode 'tuareg-mode
                          :compile-cmd "ocamlopt"
                          :supports-asm t
                          :starter-file-name "rmsbolt.ml"
                          :compile-cmd-function #'rmsbolt--ocaml-compile-cmd
                          :dissas-hidden-funcs rmsbolt--hidden-func-ocaml))))

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

;; Filtering functions were more or less lifted from the godbolt compiler exporter to maintain compatiblity.
;; https://github.com/mattgodbolt/compiler-explorer/blob/master/lib/asm.js

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

(defun rmsbolt--find-used-labels (src-buffer asm-lines)
  "Find used labels in asm-lines."
  (let ((match nil)
        (current-label nil)
        (labels-used nil)
        (weak-usages (make-hash-table :test #'equal)))
    (dolist (line asm-lines)
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
        (if (or (not (buffer-local-value 'rmsbolt-filter-directives src-buffer))
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

      (while (and (<= (cl-incf label-iter)
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

(defun rmsbolt--user-func-p (src-buffer func)
  "Return t if FUNC is a user function."
  (let* ((lang (rmsbolt--get-lang
                (buffer-local-value 'major-mode src-buffer)))
         (regexp (rmsbolt-l-dissas-hidden-funcs lang)))
    (if regexp
        (not (string-match-p regexp func))
      t)))

;; TODO godbolt does not handle dissasembly with filter=off, but we should.
(cl-defun rmsbolt--process-dissasembled-lines (src-buffer asm-lines)
  "Process and filter dissasembled ASM-LINES from SRC-BUFFER."
  (let* ((result nil)
         (func nil)
         (source-linum nil))
    (dolist (line asm-lines)
      (cl-tagbody
       (when (and (> (length result) rmsbolt-binary-asm-limit)
                  (not (buffer-local-value 'rmsbolt-ignore-binary-limit src-buffer)))
         (cl-return-from rmsbolt--process-dissasembled-lines
           '("Aborting processing due to exceeding the binary limit.")))
       (when (string-match rmsbolt-dissas-line line)
         (setq source-linum (string-to-number (match-string 2 line)))
         ;; We are just setting a linum, no data here.
         (go continue))

       (when (string-match rmsbolt-dissas-label line)
         (setq func (match-string 2 line))
         (when (rmsbolt--user-func-p src-buffer func)
           (push (concat func ":") result))
         (go continue))
       (unless (and func
                    (rmsbolt--user-func-p src-buffer func))
         (go continue))
       (when (string-match rmsbolt-dissas-opcode line)
         (let ((line (concat "\t" (match-string 3 line))))
           ;; Add line text property if available
           (when source-linum
             (add-text-properties 0 (length line)
                                  `(rmsbolt-src-line ,source-linum) line))
           (push line result))
         (go continue))
       continue))
    (nreverse result)))

(cl-defun rmsbolt--process-asm-lines (src-buffer asm-lines)
  "Process and filter a set of asm lines."
  (if (buffer-local-value 'rmsbolt-dissasemble src-buffer)
      (rmsbolt--process-dissasembled-lines src-buffer asm-lines)
    (let ((used-labels (rmsbolt--find-used-labels src-buffer asm-lines))
          (result nil)
          (prev-label nil)
          (source-linum nil))
      (dolist (line asm-lines)
        (let* ((raw-match (or (string-match rmsbolt-label-def line)
                              (string-match rmsbolt-assignment-def line)))
               (match (when raw-match
                        (match-string 1 line)))
               (used-label (cl-find match used-labels :test #'equal)))
          (cl-tagbody
           ;; Process any line number hints
           (when (string-match rmsbolt-source-tag line)
             (setq source-linum
                   (string-to-number
                    (match-string 2 line))))
           (when (string-match rmsbolt-source-stab line)
             (pcase (string-to-number (match-string 1 line))
               ;; http://www.math.utah.edu/docs/info/stabs_11.html
               (68
                (setq source-linum (match-string 2 line)))
               ((or 100 132)
                (setq source-linum nil))))

           ;; End block, reset prev-label and source
           (when (string-match-p rmsbolt-endblock line)
             (setq prev-label nil))

           (when (and (buffer-local-value 'rmsbolt-filter-comment-only src-buffer)
                      (string-match-p rmsbolt-comment-only line))
             (go continue))

           ;; continue means we don't add to the ouptut
           (when match
             (if (not used-label)
                 ;; Unused label
                 (when (buffer-local-value 'rmsbolt-filter-labels src-buffer)
                   (go continue))
               ;; Real label, set prev-label
               (setq prev-label raw-match)))
           (when (and (buffer-local-value 'rmsbolt-filter-directives src-buffer)
                      (not match))
             (if  (and (string-match-p rmsbolt-data-defn line)
                       prev-label)
                 ;; data is being used
                 nil
               (when (string-match-p rmsbolt-directive line)
                 (go continue))))
           ;; Add line numbers to mapping
           (when (and source-linum
                      (rmsbolt--has-opcode-p line))
             (add-text-properties 0 (length line)
                                  `(rmsbolt-src-line ,source-linum) line))
           ;; Add line
           (push line result)

           continue)))
      (nreverse result))))

;;;;; Handlers
(defun rmsbolt--handle-finish-compile (buffer _str)
  "Finish hook for compilations."
  (let ((compilation-fail
         (with-current-buffer buffer
           (eq 'compilation-mode-line-fail
               (get-text-property 0 'face (car mode-line-process)))))
        (default-directory (buffer-local-value 'default-directory buffer))
        (src-buffer (buffer-local-value 'rmsbolt-src-buffer buffer)))

    (with-current-buffer (get-buffer-create rmsbolt-output-buffer)
      ;; Store src buffer value for later linking
      (cond ((not compilation-fail)
             (if (not (file-exists-p (rmsbolt-output-filename src-buffer t)))
                 (message "Error reading from output file.")
               (let ((lines
                      (rmsbolt--process-asm-lines
                       src-buffer
                       (with-temp-buffer
                         (insert-file-contents (rmsbolt-output-filename src-buffer t))
                         (split-string (buffer-string) "\n" t))))
                     (ht (make-hash-table))
                     (linum 0))
                 ;; Add lines to hashtable
                 (dolist (line lines)
                   (when-let ((property
                               (get-text-property
                                0 'rmsbolt-src-line line)))
                     (cl-pushnew
                      ;; These numbers are 0 indexed, but we want 1 indexed
                      (1+ linum)
                      (gethash property ht)))
                   (incf linum))

                 (with-current-buffer src-buffer
                   (setq-local rmsbolt-line-mapping ht))
                 (delete-region (point-min) (point-max))
                 (insert
                  (mapconcat 'identity lines "\n"))
                 (asm-mode)
                 (rmsbolt-mode 1)
                 (setq-local rmsbolt-src-buffer src-buffer)
                 (display-buffer (current-buffer)))))
            (t
             ;; Display compilation output
             (display-buffer buffer))))))

;;;;; Parsing Options
(defun rmsbolt--get-lang (&optional language)
  (cdr-safe (assoc (or language major-mode) rmsbolt-languages)))
(defun rmsbolt--parse-options ()
  "Parse RMS options from file."
  (hack-local-variables)
  (let* ((lang (rmsbolt--get-lang))
         (src-buffer (current-buffer))
         (cmd rmsbolt-command))
    (when (not cmd)
      (setq-local rmsbolt-command (rmsbolt-l-compile-cmd lang)))
    (when (not (rmsbolt-l-supports-asm lang))
      (setq-local rmsbolt-dissasemble t))
    src-buffer))

;;;;; UI Functions
(defun rmsbolt-compile ()
  "Compile the current rmsbolt buffer."
  (interactive)
  (save-some-buffers nil (lambda () rmsbolt-mode))
  (if (eq major-mode 'asm-mode)
      ;; We cannot compile asm-mode files
      (message "Cannot compile this file. Are you sure you are not in the output buffer?")
    (rmsbolt--parse-options)
    (let* ((src-buffer (current-buffer))
           (lang (rmsbolt--get-lang))
           (func (rmsbolt-l-compile-cmd-function lang))
           (cmd (funcall func :src-buffer src-buffer)))

      (when (buffer-local-value 'rmsbolt-dissasemble src-buffer)
        (pcase
            (rmsbolt-l-objdumper lang)
          ('objdump
           (setq cmd
                 (mapconcat 'identity
                            (list cmd
                                  "&&"
                                  "objdump" "-d" (rmsbolt-output-filename src-buffer)
                                  "-C" "--insn-width=16" "-l"
                                  "-M" (if (buffer-local-value 'rmsbolt-intel-x86 src-buffer)
                                           "intel"
                                         "att")
                                  ">" (rmsbolt-output-filename src-buffer t))
                            " ")))
          (_
           (error "Objdumper not recognized"))))
      (setq-local rmsbolt-src-buffer src-buffer)
      (rmsbolt-with-display-buffer-no-window
       (with-current-buffer (compilation-start cmd)
         (add-hook 'compilation-finish-functions
                   #'rmsbolt--handle-finish-compile nil t)
         (setq-local rmsbolt-src-buffer src-buffer))))))

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
         (exists (file-exists-p file-name))
         (src-file-name
          (when rmsbolt-dir
            (expand-file-name (rmsbolt-l-starter-file-name lang-def) (concat rmsbolt-dir "starters/"))))
         (src-file-exists (file-exists-p src-file-name)))
    (if (not src-file-exists)
        (error "Could not find starter files! Are you sure the starter/ folder is available?")
      (find-file file-name)
      (unless exists
        (insert-file-contents
         src-file-name)
        (save-buffer))
      (unless rmsbolt-mode
        (rmsbolt-mode 1)))))
(defmacro rmsbolt-defstarter (lang mode)
  "Defines a starter for LANG and MODE."
  `(defun ,(intern (concat "rmsbolt-" lang)) ()
     ,(concat "Open a rmsbolt starter file for " lang ".")
     (interactive)
     (rmsbolt-starter ,mode)))
(rmsbolt-defstarter "c" 'c-mode)
(rmsbolt-defstarter "c++" 'c++-mode)
(rmsbolt-defstarter "ocaml" 'tuareg-mode)

;;;; Font lock matcher
(defun rmsbolt--goto-line (line)
  "Goto a certain LINE."
  (when line
    (let ((cur (line-number-at-pos)))
      (forward-line (- line cur)))))
(defun rmsbolt--setup-overlay (start end buf)
  "Setup overlay with START and END in BUF."
  (let ((o (make-overlay start end buf)))
    (overlay-put o 'face 'rmsbolt-current-line-face)
    o))
(cl-defun rmsbolt--point-visible (point)
  "Check if the current point is visible in a winodw in the current buffer."
  (dolist (w (get-buffer-window-list))
    (when (pos-visible-in-window-p point w)
      (cl-return-from rmsbolt--point-visible t)))
  nil)

(defun rmsbolt-move-overlays ()
  "Function for moving overlays for rmsbolt."

  (if-let* ((should-run
             (and rmsbolt-mode rmsbolt-use-overlays))
            (src-buffer
             (buffer-local-value 'rmsbolt-src-buffer (current-buffer)))
            (output-buffer (get-buffer-create rmsbolt-output-buffer))
            (current-line (line-number-at-pos))
            (src-current-line
             (if (eq (current-buffer) src-buffer)
                 current-line
               (get-text-property (point) 'rmsbolt-src-line)))
            (hash-table (buffer-local-value 'rmsbolt-line-mapping src-buffer))
            (asm-lines (gethash src-current-line hash-table))
            ;; TODO also consider asm
            (src-pts
             (with-current-buffer src-buffer
               (save-excursion
                 (rmsbolt--goto-line src-current-line)
                 (values (c-point 'bol) (c-point 'eol))))))
      (let ((line-visible (not rmsbolt-goto-match))
            (src-buffer-selected (eq (current-buffer) src-buffer)))
        (mapc #'delete-overlay rmsbolt-overlays)
        (setq rmsbolt-overlays nil)
        (push (rmsbolt--setup-overlay (first src-pts) (second src-pts) src-buffer)
              rmsbolt-overlays)
        (unless src-buffer-selected
          (with-current-buffer src-buffer
            (setq line-visible (rmsbolt--point-visible (first src-pts)))))
        (with-current-buffer output-buffer
          (save-excursion
            (dolist (l asm-lines)
              (rmsbolt--goto-line l)
              ;; check if line is visible and set line-visible
              (unless (or line-visible (not src-buffer-selected))
                (setq line-visible (rmsbolt--point-visible (c-point 'bol))))

              (push (rmsbolt--setup-overlay (c-point 'bol) (c-point 'eol) output-buffer)
                    rmsbolt-overlays)))
          (unless line-visible
            ;; Scroll buffer to first line
            (when-let
                ((scroll-buffer (if src-buffer-selected
                                    output-buffer
                                  src-buffer))
                 (line-scroll (if src-buffer-selected
                                  (first asm-lines)
                                src-current-line))
                 (window (get-buffer-window scroll-buffer)))
              (with-selected-window window
                (rmsbolt--goto-line line-scroll))))))
    (mapc #'delete-overlay rmsbolt-overlays)
    (setq rmsbolt-overlays nil)))

;;;; Mode Definition:

;;;###autoload
;; TODO handle more modes than c-mode
(define-minor-mode rmsbolt-mode
  "RMSbolt"
  nil "RMSBolt" rmsbolt-mode-map
  ;; This idle timer always runs, even when we aren't in rmsbolt-mode
  (unless rmsbolt--idle-timer
    (setq rmsbolt--idle-timer (run-with-idle-timer
                               rmsbolt-overlay-delay t
                               #'rmsbolt-move-overlays)))

  (unless (and rmsbolt-temp-dir
               (file-exists-p rmsbolt-temp-dir))
    (setq rmsbolt-temp-dir
          (make-temp-file "rmsbolt-" t))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (when (and (boundp 'rmsbolt-temp-dir)
                           rmsbolt-temp-dir
                           (file-directory-p rmsbolt-temp-dir))
                  (delete-directory rmsbolt-temp-dir t))
                (setq rmsbolt-temp-dir nil)))))

(provide 'rmsbolt)

;;; rmsbolt.el ends here
