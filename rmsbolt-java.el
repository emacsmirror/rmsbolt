;;; rmsbolt-java.el --- A elisp library to parse javap output -*- lexical-binding: t; -*-

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
;; The java bytecode dissasembler format is rather obtuse. This library tries
;; to make a programatic layer for interacting with it. It's main aim is
;; correlating lines in source code to the generated output.
;;
;; This library takes in the output of javap -c -l split into a list by lines,
;; which is the same format rmsbolt uses.

;;; Requires:

(require 'cl-lib)

;;; Code:

(defun rmsbolt-java-process-bytecode (asm-lines)
  "Process ASM-LINES to add properties refrencing the source code.
Also filters \"useless\" lines out."
  )

(defun rmsbolt--process-java-bytecode (_src_buffer asm-lines)
  "Wrapper for easy integration into rmsbolt."
  (rmsbolt-java-process-bytecode asm-lines))

(provide 'rmsbolt-java)

;;; rmsbolt-java.el ends here
