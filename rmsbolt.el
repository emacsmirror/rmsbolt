;;; rmsbolt.el --- A compiler output viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jay Kamat
;; Author: Jay Kamat <jaygkamat@gmail.com>
;; Version: 0.1.0
;; Keywords: compilation, tools
;; URL: http://gitlab.com/jgkamat/rmsbolt
;; Package-Requires: ((emacs "25.1"))

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

;; RMSBolt is a package to provide assembly or bytecode output for a source
;; code input file.
;;
;; It currently supports: C/C++, OCaml, Haskell, Python, Java, and (limited)
;; Common Lisp.
;;
;; Adding support for more languages, if they have an easy manual compilation
;; path from source->assembly/bytecode with debug information, should be much
;; easier than in other alternatives.
;;
;; It's able to do this by:
;; 1. Compiling changes automatically, adding options which cause the compiler
;; to output assembly/bytecode with debug information (or by using objdump)
;; 2. Parse assembly/bytecode to create a map from it to the original source
;; 3. Strip out unneeded information from bytecode to only show useful code
;; 4. Provide an interface for highlighting the matched assembly/bytecode line
;; to the source and vice versa
;;
;; Tweakables:
;; RMSBolt is primarily configured with Emacs local variables. This lets you
;; change compiler and rmsbolt options simply by editing a local variable block.
;;
;; Notable options:
;; `rmsbolt-command': determines the prefix of the compilation command to use
;; `rmsbolt-disassemble': disassemble from a compiled binary with objdump, if supported.
;; `rmsbolt-filter-*': Tweak filtering of binary output
;; `rmsbolt-intel-x86': Toggle between intel and att syntax if supported
;; `rmsbolt-demangle': Demangle the output, if supported.
;;
;; Please see the readme at https://gitlab.com/jgkamat/rmsbolt for
;; more information!
;;
;; Thanks:
;; Inspiration and some assembly parsing logic was adapted from Matt Godbolt's
;; compiler-explorer: https://github.com/mattgodbolt/compiler-explorer and
;; Jonas Konrad's javap: https://github.com/yawkat/javap.

;;; Requires:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(require 'map)
(require 'cc-defs)

(require 'rmsbolt-java)

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
(defcustom rmsbolt-mode-lighter " RMS🗲"
  "Lighter displayed in mode line when function `rmsbolt-mode' is active."
  :type 'string
  :group 'rmsbolt)
(defcustom rmsbolt-automatic-recompile t
  "Whether to automatically recompile on source buffer changes."
  :type 'boolean
  :group 'rmsbolt)

;;;;; Buffer Local Tweakables
(defcustom rmsbolt-disassemble nil
  "Whether we should disassemble an output binary."
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
(defcustom rmsbolt-demangle t
  "Whether to attempt to demangle the resulting assembly."
  :type 'boolean
  :safe 'booleanp
  :group 'rmsbolt)

;;;; Faces

(defface rmsbolt-current-line-face
  '((t (:weight bold :inherit highlight)))
  "Face to fontify the current line for showing matches."
  :group 'rmsbolt)

;;;; Variables:
(defvar rmsbolt-output-buffer "*rmsbolt-output*")
;; whether rmsbolt-mode is enabled.
(defvar rmsbolt-mode)

(defvar rmsbolt-hide-compile t)
(defvar rmsbolt-binary-asm-limit 10000)
(defvar-local rmsbolt-line-mapping nil
  "Line mapping hashtable from source lines -> asm lines")
(defvar-local rmsbolt-current-line nil
  "Current line for fontifier.")

(defvar rmsbolt-overlays nil
  "List of overlays to use.")
(defvar rmsbolt-overlay-delay 0.125
  "Time in seconds to delay before showing overlays.")
(defvar rmsbolt-compile-delay 1
  "Time in seconds to delay before recompiling if there is a change.")
(defvar rmsbolt--automated-compile nil
  "Whether this compile was automated or not.")

(defvar rmsbolt--idle-timer nil
  "Idle timer for rmsbolt overlays.")
(defvar rmsbolt--compile-idle-timer nil
  "Idle timer for rmsbolt overlays.")
(defvar rmsbolt--temp-dir nil
  "Temporary directory to use for compilation and other reasons.

Please DO NOT modify this blindly, as this directory will get deleted on Emacs exit.")

(defvar rmsbolt-dir (when load-file-name
                      (file-name-directory load-file-name))
  "The directory which rmsbolt is installed to.")

(defvar-local rmsbolt-src-buffer nil)

;;;; Variable-like funcs
(defun rmsbolt-output-filename (src-buffer &optional asm)
  "Function for generating an output filename for SRC-BUFFER.

Outputs assembly file if ASM."
  (if (and (not asm)
           (buffer-local-value 'rmsbolt-disassemble src-buffer))
      (expand-file-name "rmsbolt.out" rmsbolt--temp-dir)
    (expand-file-name "rmsbolt.s" rmsbolt--temp-dir)))

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
(defvar rmsbolt-disass-line (rx bol
                                (group "/" (1+ (not (any ":")))) ":"
                                (group (1+ num))
                                (0+ any)))
(defvar rmsbolt-disass-label (rx bol (group (1+ (any digit "a-f")))
                                 (1+ space) "<"
                                 (group (1+ (not (any ">")))) ">:" eol))
(defvar rmsbolt-disass-dest (rx (0+ any) (group (1+ (any digit "a-f")))
                                (1+ space) "<" (group (1+ (not (any ">")))) ">" eol))

(defvar rmsbolt-disass-opcode (rx bol (0+ space) (group (1+ (any digit "a-f")))
                                  ":" (0+ space)
                                  (group (1+
                                          (repeat 2
                                                  (any digit "a-f"))
                                          (opt " ")))
                                  (0+ space)
                                  (group (0+ any))))
(defvar rmsbolt-source-file (rx bol (0+ space) ".file" (1+ space)
                                (group (1+ digit)) (1+ space) ?\"
                                (group (1+ (not (any ?\")))) ?\"
                                (opt (1+ space) ?\"
                                     (group (1+ (not (any ?\")))) ?\")
                                (0+ any)))
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
  (supports-disass
   nil
   :type 'bool
   :documentation "If we support assembly directly. If nil, we must disassemble.")
  (supports-asm
   nil
   :type 'bool
   :documentation "If we support assembly directly. If nil, we must disassemble.")
  (objdumper
   'objdump
   :type 'symbol
   :documentation "The object dumper to use if disassembling binary.")
  (demangler
   nil
   :type 'string
   :documentation "The command of the demangler to use for this source code.")
  (disass-hidden-funcs
   nil
   :type 'string
   :documentation "Functions that are hidden when disassembling.")
  (compile-cmd
   nil
   :type 'string
   :documentation "Default compilation command to use if none is provided.")
  (compile-cmd-function
   nil
   :type 'function
   :documentation "A function which takes in a compile command
(could be the default) and adds needed args to it.")
  (process-asm-custom-fn
   nil
   :type 'function
   :documentation "A custom function used for parsing asm lines
   instead of the default assembly one." ))


(cl-defun rmsbolt--c-compile-cmd (&key src-buffer)
  "Process a compile command for gcc/clang."
  (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (cmd (mapconcat #'identity
                         (list cmd
                               "-g"
                               (if (buffer-local-value 'rmsbolt-disassemble src-buffer)
                                   ""
                                 "-S")
                               (buffer-file-name)
                               "-o" (rmsbolt-output-filename src-buffer)
                               (when (buffer-local-value 'rmsbolt-intel-x86 src-buffer)
                                 "-masm=intel"))
                         " ")))
    cmd))
(cl-defun rmsbolt--ocaml-compile-cmd (&key src-buffer)
  "Process a compile command for ocaml.

  Needed as ocaml cannot output asm to a non-hardcoded file"
  (let* ((diss (buffer-local-value 'rmsbolt-disassemble src-buffer))
         (output-filename (rmsbolt-output-filename src-buffer))
         (predicted-asm-filename (concat (file-name-sans-extension (buffer-file-name)) ".s"))
         (cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (cmd (mapconcat #'identity
                         (list cmd
                               "-g"
                               (if (buffer-local-value 'rmsbolt-disassemble src-buffer)
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
(cl-defun rmsbolt--lisp-compile-cmd (&key src-buffer)
  "Process a compile command for common lisp.

   Assumes function name to disassemble is 'main'."
  (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (interpreter (cl-first (split-string cmd nil t)))
         (disass-eval "\"(disassemble 'main)\"")
         (disass-eval-unquoted "(disassemble 'main)"))
    (pcase interpreter
      ("sbcl"
       (mapconcat #'identity
                  (list cmd "--noinform" "--load"
                        (buffer-file-name)
                        "--eval" disass-eval "--non-interactive"
                        ;; Remove leading comments
                        "|" "sed" "'s/^;\s//'" ">"
                        (rmsbolt-output-filename src-buffer))
                  " "))
      ("clisp"
       (mapconcat #'identity
                  (list cmd "-q" "-x"
                        (concat
                         "\"(load \\\"" (buffer-file-name) "\\\") " disass-eval-unquoted "\"")
                        ">" (rmsbolt-output-filename src-buffer))
                  " "))
      (_
       (error "This Common Lisp interpreter is not supported")))))
(cl-defun rmsbolt--rust-compile-cmd (&key src-buffer)
  "Process a compile command for rustc."
  (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (cmd (mapconcat #'identity
                         (list cmd
                               "-g"
                               "--emit"
                               (if (buffer-local-value 'rmsbolt-disassemble src-buffer)
                                   "link"
                                 "asm")
                               (buffer-file-name)
                               "-o" (rmsbolt-output-filename src-buffer)
                               (when (buffer-local-value 'rmsbolt-intel-x86 src-buffer)
                                 "-Cllvm-args=--x86-asm-syntax=intel"))
                         " ")))
    cmd))
(cl-defun rmsbolt--py-compile-cmd (&key src-buffer)
  "Process a compile command for python3."
  (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer)))
    (mapconcat #'identity
               (list cmd "-m" "dis" (buffer-file-name)
                     ">" (rmsbolt-output-filename src-buffer))
               " ")))

(cl-defun rmsbolt--hs-compile-cmd (&key src-buffer)
  "Process a compile command for ghc."
  (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (cmd (mapconcat #'identity
                         (list cmd
                               "-g"
                               (if (buffer-local-value 'rmsbolt-disassemble src-buffer)
                                   ""
                                 "-S")
                               (buffer-file-name)
                               "-o" (rmsbolt-output-filename src-buffer))
                         " ")))
    cmd))
(cl-defun rmsbolt--java-compile-cmd (&key src-buffer)
  "Process a compile command for ocaml.

  Needed as ocaml cannot output asm to a non-hardcoded file"
  (let* ((output-filename (rmsbolt-output-filename src-buffer))
         (class-filename (concat (file-name-sans-extension (buffer-file-name)) ".class"))
         (cmd (buffer-local-value 'rmsbolt-command src-buffer))
         (cmd (mapconcat #'identity
                         (list cmd
                               "-g"
                               (buffer-file-name)
                               "&&"
                               "javap"
                               "-c" "-l"
                               class-filename
                               ">"
                               output-filename)
                         " ")))
    cmd))

(defvar rmsbolt--hidden-func-c
  (rx bol (or (and "__" (0+ any))
              (and "_" (or "init" "start" "fini"))
              (and (opt "de") "register_tm_clones")
              "call_gmon_start"
              "frame_dummy"
              (and ".plt" (0+ any)))
      eol))
(defvar rmsbolt--hidden-func-ocaml
  (rx bol
      (or (and "__" (0+ any))
          (and "_" (or "init" "start" "fini"))
          (and (opt "de") "register_tm_clones")
          (and ".plt" (0+ any))
          (and "camlCamlinternalFormat" (0+ any))
          (and (1+ (not (any "@"))) "@plt")
          (and (or "caml_" "camlStd_") (0+ any))
          (and "caml" (or "Pervasives" "List" "Bytes"
                          "String" "Buffer" "Printf"
                          "Char" "Sys") "__" (0+ any))
          ;; Ocaml likes to make labels following camlModule__,
          ;; filter out any lowercase
          (and (1+ (1+ lower) (opt (or "64" "32" "8" "16")) (opt "_"))))
      eol))
;;;; Language Definitions
(defvar rmsbolt-languages)
(setq
 rmsbolt-languages
 `((c-mode
    . ,(make-rmsbolt-lang :compile-cmd "gcc"
                          :supports-asm t
                          :supports-disass t
                          :demangler "c++filt"
                          :compile-cmd-function #'rmsbolt--c-compile-cmd
                          :disass-hidden-funcs rmsbolt--hidden-func-c))
   (c++-mode
    . ,(make-rmsbolt-lang :compile-cmd "g++"
                          :supports-asm t
                          :supports-disass t
                          :demangler "c++filt"
                          :compile-cmd-function #'rmsbolt--c-compile-cmd
                          :disass-hidden-funcs rmsbolt--hidden-func-c))
   ;; In order to parse ocaml files, you need the emacs ocaml mode, tuareg
   (tuareg-mode
    . ,(make-rmsbolt-lang :compile-cmd "ocamlopt"
                          :supports-asm t
                          :supports-disass t
                          :compile-cmd-function #'rmsbolt--ocaml-compile-cmd
                          :disass-hidden-funcs rmsbolt--hidden-func-ocaml))
   (lisp-mode
    . ,(make-rmsbolt-lang :compile-cmd "sbcl"
                          :supports-asm t
                          :supports-disass nil
                          :objdumper 'cat
                          :compile-cmd-function #'rmsbolt--lisp-compile-cmd
                          :disass-hidden-funcs nil))
   (rust-mode
    . ,(make-rmsbolt-lang :compile-cmd "rustc"
                          :supports-asm t
                          :supports-disass nil
                          :objdumper 'objdump
                          :demangler "rustfilt"
                          :compile-cmd-function #'rmsbolt--rust-compile-cmd
                          :disass-hidden-funcs nil))
   ;; ONLY SUPPORTS PYTHON 3
   (python-mode
    . ,(make-rmsbolt-lang :compile-cmd "python3"
                          :supports-asm t
                          :supports-disass nil
                          :compile-cmd-function #'rmsbolt--py-compile-cmd
                          :disass-hidden-funcs nil
                          :process-asm-custom-fn #'rmsbolt--process-python-bytecode))
   (haskell-mode
    . ,(make-rmsbolt-lang :compile-cmd "ghc"
                          :supports-asm t
                          :supports-disass nil
                          :demangler "haskell-demangler"
                          :compile-cmd-function #'rmsbolt--hs-compile-cmd
                          :disass-hidden-funcs nil))
   (java-mode
    . ,(make-rmsbolt-lang :compile-cmd "javac"
                          :supports-asm t
                          :supports-disass nil
                          :objdumper 'cat
                          :compile-cmd-function #'rmsbolt--java-compile-cmd
                          :process-asm-custom-fn #'rmsbolt--process-java-bytecode
                          :disass-hidden-funcs nil))
   ))
(make-obsolete-variable 'rmsbolt-languages
                        'rmsbolt-language-descriptor "RMSBolt-0.2")

(defvar-local rmsbolt-language-descriptor nil
  ;; FIXME: Major modes can't set this without calling `make-rmsbolt-lang',
  ;; so it forces them to require `rmsbolt', which is a bummer.
  "Description of the language tools of current buffer for use by RMSBolt.
This should be an object of type `rmsbolt-lang', normally set by the major mode")

;;;; Macros

(defmacro rmsbolt-with-display-buffer-no-window (&rest body)
  "Run BODY without displaying any window."
  ;; See http://debbugs.gnu.org/13594
  `(let ((display-buffer-overriding-action
          (if rmsbolt-hide-compile
              (list #'display-buffer-no-window)
            display-buffer-overriding-action)))
     ,@body))


;;;; Functions
;; Functions to parse and lint assembly were lifted almost directly from the compiler-explorer

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

;; Filtering functions were more or less lifted from the godbolt compiler explorer to maintain compatiblity.
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
  "Find used labels in ASM-LINES generated from SRC-BUFFER."
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
  "Return t if FUNC is a user function.
Argument SRC-BUFFER source buffer."
  (let* ((lang (with-current-buffer src-buffer
                 (rmsbolt--get-lang)))
         (regexp (rmsbolt-l-disass-hidden-funcs lang)))
    (if regexp
        (not (string-match-p regexp func))
      t)))

;; TODO godbolt does not handle disassembly with filter=off, but we should.
(cl-defun rmsbolt--process-disassembled-lines (src-buffer asm-lines)
  "Process and filter disassembled ASM-LINES from SRC-BUFFER."
  (let* ((result nil)
         (func nil)
         (source-linum nil))
    (dolist (line asm-lines)
      (cl-tagbody
       (when (and (> (length result) rmsbolt-binary-asm-limit)
                  (not (buffer-local-value 'rmsbolt-ignore-binary-limit src-buffer)))
         (cl-return-from rmsbolt--process-disassembled-lines
           '("Aborting processing due to exceeding the binary limit.")))
       (when (string-match rmsbolt-disass-line line)
         ;; Don't add linums from files which we aren't inspecting
         (if (file-equal-p (buffer-file-name src-buffer)
                           (match-string 1 line))
             (setq source-linum (string-to-number (match-string 2 line)))
           (setq source-linum nil))
         ;; We are just setting a linum, no data here.
         (go continue))

       (when (string-match rmsbolt-disass-label line)
         (setq func (match-string 2 line))
         (when (rmsbolt--user-func-p src-buffer func)
           (push (concat func ":") result))
         (go continue))
       (unless (and func
                    (rmsbolt--user-func-p src-buffer func))
         (go continue))
       (when (string-match rmsbolt-disass-opcode line)
         (let ((line (concat "\t" (match-string 3 line))))
           ;; Add line text property if available
           (when source-linum
             (add-text-properties 0 (length line)
                                  `(rmsbolt-src-line ,source-linum) line))
           (push line result))
         (go continue))
       continue))
    (nreverse result)))

(cl-defun rmsbolt--process-src-asm-lines (src-buffer asm-lines)
  (let ((used-labels (rmsbolt--find-used-labels src-buffer asm-lines))
        (result nil)
        (prev-label nil)
        (source-linum nil)
        (source-file nil)
        (skip-file-match
         ;; Skip file match if we don't have a current filename
         (not (buffer-file-name src-buffer))))
    (dolist (line asm-lines)
      (let* ((raw-match (or (string-match rmsbolt-label-def line)
                            (string-match rmsbolt-assignment-def line)))
             (match (when raw-match
                      (match-string 1 line)))
             (used-label (cl-find match used-labels :test #'equal)))
        (cl-tagbody
         ;; Process file name hints
         (when (string-match rmsbolt-source-file line)
           (if (match-string 3 line)
               ;; Clang style match
               (setq source-file (expand-file-name
                                  (match-string 3 line)
                                  (match-string 2 line)))
             (setq source-file (match-string 2 line))))
         ;; Process any line number hints
         (when (string-match rmsbolt-source-tag line)
           (if (or skip-file-match
                   (file-equal-p (buffer-file-name src-buffer) source-file))
               (setq source-linum (string-to-number
                                   (match-string 2 line)))
             (setq source-linum nil)))
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
    (nreverse result)))

(cl-defun rmsbolt--process-python-bytecode (_src-buffer asm-lines)
  (let ((source-linum nil)
        (result nil))
    (dolist (line asm-lines)
      (if (not (string-match (rx bol (repeat 3 (opt space))
                                 (group (opt (1+ digit))) (0+ space)
                                 (group (opt "-->")) (0+ space)
                                 (group (opt ">>")) (0+ space)
                                 (group (1+ digit)) (0+ space)
                                 (group (1+ (or letter "_"))) (0+ space)
                                 (group (opt (1+ digit))) (0+ space)
                                 (group (opt (0+ any))))
                             line))
          ;; just push the var with no linum
          (push line result)
        ;; Grab line numbers
        (unless (string-empty-p (match-string 1 line))
          (setq source-linum
                (string-to-number (match-string 1 line))))
        ;; Reformat line to be more like assembly
        (setq line (mapconcat #'identity
                              (list (match-string 5 line)
                                    (match-string 6 line)
                                    (match-string 7 line))
                              "\t"))
        (when source-linum
          (add-text-properties 0 (length line)
                               `(rmsbolt-src-line ,source-linum) line))
        ;; Add line
        (push line result)))
    (nreverse result)))

(defun rmsbolt--process-java-bytecode (src-buffer asm-lines)
  "Wrapper for easy integration into rmsbolt.
Argument SRC-BUFFER source buffer.
Argument ASM-LINES input lines."
  (rmsbolt-java-process-bytecode
   asm-lines
   (buffer-local-value 'rmsbolt-filter-directives src-buffer)))

(cl-defun rmsbolt--process-asm-lines (src-buffer asm-lines)
  "Process and filter a set of asm lines."
  (let* ((lang (with-current-buffer src-buffer
                 (rmsbolt--get-lang)))
         (process-asm-fn (when lang
                           (rmsbolt-l-process-asm-custom-fn lang))))
    (cond
     (process-asm-fn
      (funcall process-asm-fn src-buffer asm-lines))
     ((buffer-local-value 'rmsbolt-disassemble src-buffer)
      (rmsbolt--process-disassembled-lines src-buffer asm-lines))
     (t
      (rmsbolt--process-src-asm-lines src-buffer asm-lines)))))

;;;;; Handlers
(defun rmsbolt--handle-finish-compile (buffer str)
  "Finish hook for compilations.
Argument BUFFER compilation buffer.
Argument STR compilation finish status."
  (let ((compilation-fail
         (not (string-match "^finished" str)))
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
                         (split-string (buffer-string) "\n" nil))))
                     (ht (make-hash-table))
                     (linum 1)
                     (start-match nil)
                     (in-match nil)
                     (output-buffer (current-buffer)))
                 ;; Add lines to hashtable
                 (dolist (line lines)
                   (let ((property
                          (get-text-property
                           0 'rmsbolt-src-line line)))
                     (progn
                       (cl-tagbody
                        run-conditional
                        (cond
                         ((and in-match (eq in-match property))
                          ;; We are continuing an existing match
                          nil)
                         (in-match
                          ;; We are in a match that has just expired
                          (push (cons start-match (1- linum))
                                (gethash in-match ht))
                          (setq in-match nil
                                start-match nil)
                          (go run-conditional))
                         (property
                          (setq in-match property
                                start-match linum))))))
                   (cl-incf linum))

                 (with-current-buffer src-buffer
                   (setq-local rmsbolt-line-mapping ht))
                 ;; Replace buffer contents non-destructively if possible
                 (if (functionp #'replace-buffer-contents)
                     (with-temp-buffer
                       (insert (mapconcat #'identity lines "\n"))
                       (let ((tmp-buffer (current-buffer)))
                         (with-current-buffer output-buffer
                           (replace-buffer-contents tmp-buffer))))
                   (with-current-buffer output-buffer
                     (let ((old-point (point)))
                       (erase-buffer)
                       (insert (mapconcat #'identity lines "\n"))
                       (goto-char old-point))))
                 (asm-mode)
                 (rmsbolt-mode 1)
                 (setq-local rmsbolt-src-buffer src-buffer)
                 (display-buffer (current-buffer))
                 ;; Attempt to replace overlays
                 (with-current-buffer src-buffer
                   (rmsbolt-move-overlays)))))
            ((and t
                  (not rmsbolt--automated-compile))
             ;; Display compilation output
             (display-buffer buffer)))
      ;; Reset automated recompile
      (setq rmsbolt--automated-compile nil))))

;;;;; Parsing Options
(defun rmsbolt--get-lang ()
  "Helper function to get lang def for LANGUAGE."
  (or rmsbolt-language-descriptor
      (cdr-safe (assoc major-mode rmsbolt-languages))))
(defun rmsbolt--parse-options ()
  "Parse RMS options from file."
  (hack-local-variables)
  (let* ((lang (rmsbolt--get-lang))
         (src-buffer (current-buffer))
         (cmd rmsbolt-command)
         (force-disass (not (rmsbolt-l-supports-asm lang)))
         (force-asm (not (rmsbolt-l-supports-disass lang))))
    (when (not cmd)
      (setq-local rmsbolt-command (rmsbolt-l-compile-cmd lang)))
    (when (and force-disass force-asm)
      (error "No disassemble method found for this langauge, please double check spec"))
    (when force-disass
      (setq-local rmsbolt-disassemble t))
    (when force-asm
      (setq-local rmsbolt-disassemble nil))
    src-buffer))

(defun rmsbolt--demangle-command (existing-cmd lang src-buffer)
  "Append a demangler routine to EXISTING-CMD with LANG and SRC-BUFFER and return it."
  (if-let ((to-demangle (buffer-local-value 'rmsbolt-demangle src-buffer))
           (demangler (rmsbolt-l-demangler lang))
           (demangler-exists (executable-find demangler)))
      (concat existing-cmd " "
              (mapconcat
               #'identity
               (list "&&" demangler
                     "<" (rmsbolt-output-filename src-buffer t)
                     ">" (expand-file-name "tmp.s" rmsbolt--temp-dir)
                     "&&" "mv"
                     (expand-file-name "tmp.s" rmsbolt--temp-dir)
                     (rmsbolt-output-filename src-buffer t))
               " "))
    existing-cmd))

;;;;; UI Functions
(defun rmsbolt-compile ()
  "Compile the current rmsbolt buffer."
  (interactive)
  (save-some-buffers nil (lambda () rmsbolt-mode))
  (if (eq major-mode 'asm-mode)
      ;; We cannot compile asm-mode files
      (message "Cannot compile assembly files. Are you sure you are not in the output buffer?")
    (rmsbolt--parse-options)
    (rmsbolt--gen-temp)
    (let* ((src-buffer (current-buffer))
           (lang (rmsbolt--get-lang))
           (func (rmsbolt-l-compile-cmd-function lang))
           ;; Generate command
           (cmd (funcall func :src-buffer src-buffer))
           ;; Convert to demangle if we need to
           (cmd (rmsbolt--demangle-command cmd lang src-buffer))
           (default-directory rmsbolt--temp-dir))

      (when (buffer-local-value 'rmsbolt-disassemble src-buffer)
        (pcase
            (rmsbolt-l-objdumper lang)
          ('objdump
           (setq cmd
                 (mapconcat #'identity
                            (list cmd
                                  "&&"
                                  "objdump" "-d" (rmsbolt-output-filename src-buffer)
                                  "-C" "--insn-width=16" "-l"
                                  "-M" (if (buffer-local-value 'rmsbolt-intel-x86 src-buffer)
                                           "intel"
                                         "att")
                                  ">" (rmsbolt-output-filename src-buffer t))
                            " ")))
          ('cat
           (setq cmd
                 (mapconcat #'identity
                            (list cmd
                                  "&&" "mv"
                                  (rmsbolt-output-filename src-buffer)
                                  (rmsbolt-output-filename src-buffer t))
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
(defvar rmsbolt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'rmsbolt-compile)
    map)
  "Keymap for function `rmsbolt-mode'.")

;;;; Init commands

(defun rmsbolt--gen-temp ()
  "Generate rmsbolt temp dir if needed."
  (unless (and rmsbolt--temp-dir
               (file-exists-p rmsbolt--temp-dir))
    (setq rmsbolt--temp-dir
          (make-temp-file "rmsbolt-" t))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (when (and (boundp 'rmsbolt--temp-dir)
                           rmsbolt--temp-dir
                           (file-directory-p rmsbolt--temp-dir))
                  (delete-directory rmsbolt--temp-dir t))
                (setq rmsbolt--temp-dir nil)))))

;;;;; Starter Definitions

;; IIUC, this "starter" business is not a necessary part of RMSBolt, but is
;; a way to provide sample files with which users can try out RMSBolt.

(defvar rmsbolt-starter-files
  '(("c" . "rmsbolt.c")
    ("c++" . "rmsbolt.cpp")
    ("ocaml" . "rmsbolt.ml")
    ("cl" . "rmsbolt.lisp")
    ("rust " . "rmsbolt.rs")
    ("python" . "rmsbolt.py")
    ("haskell" . "rmsbolt.hs")
    ;; FIXME: Why capital letter?
    ("java" . "Rmsbolt.java")))

;;;###autoload
(defun rmsbolt-starter (lang-name)
  "Setup new file based on the sample STARTER-FILE-NAME."
  (interactive
   (list (completing-read "Language: " rmsbolt-starter-files nil t)))
  (rmsbolt--gen-temp)
  (let* ((starter-file-name (cdr (assoc lang-name rmsbolt-starter-files)))
         (file-name
          (expand-file-name starter-file-name rmsbolt--temp-dir))
         (exists (file-exists-p file-name))
         (src-file-name
          (when rmsbolt-dir
            (expand-file-name starter-file-name
                              (expand-file-name "starters/" rmsbolt-dir))))
         (src-file-exists (when src-file-name
                            (file-exists-p src-file-name))))
    (if (not src-file-exists)
        (error "Could not find starter files! Are you sure the starter/ folder is available? If you want to overide, set `rmsbolt-dir' to your install path")
      (find-file file-name)
      (unless exists
        (insert-file-contents
         src-file-name)
        (save-buffer))
      (unless rmsbolt-mode
        (rmsbolt-mode 1)))))

;;;; Overlay Commands
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
  "Check if the current point is visible in a window in the current buffer."
  (dolist (w (get-buffer-window-list))
    (when (pos-visible-in-window-p point w)
      (cl-return-from rmsbolt--point-visible t)))
  nil)

(defun rmsbolt-move-overlays ()
  "Function for moving overlays for rmsbolt."
  (when rmsbolt-mode
    (if-let ((should-run rmsbolt-use-overlays)
             (src-buffer
              (buffer-local-value 'rmsbolt-src-buffer (current-buffer)))
             ;; Don't run on unsaved buffers
             (should-run (and (not (buffer-modified-p src-buffer))
                              (buffer-local-value 'rmsbolt-mode src-buffer)))
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
                  (cl-values (c-point 'bol) (c-point 'bonl))))))
        (let ((line-visible (not rmsbolt-goto-match))
              (src-buffer-selected (eq (current-buffer) src-buffer)))
          ;; Clear out overlays in case they are used
          (mapc #'delete-overlay rmsbolt-overlays)
          (setq rmsbolt-overlays nil)
          (push (rmsbolt--setup-overlay (cl-first src-pts) (cl-second src-pts) src-buffer)
                rmsbolt-overlays)
          (unless src-buffer-selected
            (with-current-buffer src-buffer
              (setq line-visible (rmsbolt--point-visible (cl-first src-pts)))))
          (with-current-buffer output-buffer
            (let ((saved-pt (point)))
              (save-excursion
                (dolist (l asm-lines)
                  (let* ((start (car l))
                         (end (cdr l))
                         (start-pt (progn (rmsbolt--goto-line start)
                                          (c-point 'bol)))
                         (end-pt (progn (rmsbolt--goto-line end)
                                        (c-point 'bonl)))
                         (visible (or line-visible
                                      (rmsbolt--point-visible start-pt)
                                      (rmsbolt--point-visible end-pt)
                                      (and (> saved-pt start-pt)
                                           (< saved-pt end-pt)))))
                    ;; check if line is visible and set line-visible
                    (unless (or line-visible (not src-buffer-selected))
                      (setq line-visible visible))
                    (push (rmsbolt--setup-overlay start-pt end-pt output-buffer)
                          rmsbolt-overlays)))))
            (unless line-visible
              ;; Scroll buffer to first line
              (when-let ((scroll-buffer (if src-buffer-selected
                                            output-buffer
                                          src-buffer))
                         (line-scroll (if src-buffer-selected
                                          (car-safe
                                           (cl-first asm-lines))
                                        src-current-line))
                         (window (get-buffer-window scroll-buffer)))
                (with-selected-window window
                  (rmsbolt--goto-line line-scroll)
                  ;; If we scrolled, recenter
                  (recenter))))))
      (mapc #'delete-overlay rmsbolt-overlays)
      (setq rmsbolt-overlays nil))
    ;; If not in rmsbolt-mode, don't do anything
    ))

(defun rmsbolt-hot-recompile ()
  "Recompile source buffer if we need to."
  (when-let ((should-hot-compile rmsbolt-mode)
             (should-hot-recompile rmsbolt-automatic-recompile)
             (output-buffer (get-buffer rmsbolt-output-buffer))
             (src-buffer (buffer-local-value 'rmsbolt-src-buffer output-buffer))
             (modified (buffer-modified-p src-buffer)))
    (with-current-buffer src-buffer
      ;; Write to disk
      (save-buffer)
      ;; Recompile
      (setq rmsbolt--automated-compile t)
      (rmsbolt-compile))))

;;;; Mode Definition:

;;;###autoload
;; TODO handle more modes than c-mode
(define-minor-mode rmsbolt-mode
  "Toggle rmsbolt-mode.

This mode is enabled both in modes to be compiled and output buffers."
  :global nil
  :lighter rmsbolt-mode-lighter rmsbolt-mode-map
  ;; Init
  (cond
   (rmsbolt-mode
    ;; This idle timer always runs, even when we aren't in rmsbolt-mode
    ;; It won't do anything unless we are in rmsbolt-mode
    (unless rmsbolt--idle-timer
      (setq rmsbolt--idle-timer (run-with-idle-timer
                                 rmsbolt-overlay-delay t
                                 #'rmsbolt-move-overlays)))
    (unless (or rmsbolt--compile-idle-timer
                (not rmsbolt-automatic-recompile))
      (setq rmsbolt--compile-idle-timer (run-with-idle-timer
                                         rmsbolt-compile-delay t
                                         #'rmsbolt-hot-recompile)))
    (rmsbolt--gen-temp))
   (t ;; Cleanup
    (mapc #'delete-overlay rmsbolt-overlays))))

(provide 'rmsbolt)

;;; rmsbolt.el ends here
