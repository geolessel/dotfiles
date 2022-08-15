;;; kickasm-mode.el --- major mode for editing Kick Assembler code

;; Copyright (C) 2016 Mattias Weidhagen.

;; Author: Mattias Weidhagen <mattias.weidhagen@gmail.com>
;; Created: 15 December 2018
;; URL: https: //github.com/mweidhagen/kickasm-mode
;; Keywords: languages
;; Version: 1.0.18
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a major mode for editing Mads Nielsen's
;; Kick Assembler.  The keywords and syntax are up to date as of
;; Kick Assembler 5.21

;; Kick Assembler Home: http://theweb.dk/KickAssembler

;; The aim of this package is to provide font lock support as well as
;; indentation for Kick Assembler code.  The indentation can be
;; customizable in various ways.  It also contains an assemble command
;; to assemble a buffer using Kick Assembler and then to test the
;; assembled program in VICE or C64 Debugger.

;; For the font lock faces to work properly you should only use capital
;; letters, numbers and underscore for constants and use lower case
;; (or mixed) letters for variables, function names etc.

;; There are three flavors of indentation style which you specify by
;; setting the variable `kickasm-mnemonic-indentation-mode'.
;; If setting it to nil then all mnemonics will be strictly indented
;; to the value specified in variable `kickasm-mnemonic-indent'
;; If setting it to 'min then all mnemonics will be indented to the
;; value specified in variable `kickasm-mnemonic-indent' unless the
;; surrounding code demands that it should be indented further to
;; the right.
;; If setting it to 'depth then all mnemonics will be indented just
;; like any other command (like .if/.for/.while etc).
;; If you prefer any of the two latter styles then you might want to
;; change the value of `kickasm-command-start-indent' to something larger
;; than the default value.
;; It is also possible to turn off all automatic indentation by setting
;; variable `kickasm-use-automatic-indentation' to nil.  In that case
;; pressing TAB will insert a tab character instead.

;; When editing a Kick Assembler file you normally have script code
;; mixed with assembler code.  To make editing easy then the TAB key,
;; apart from indenting a line, also cycles through a list of tab stops
;; depending on the line type.

;; If you want to have comments starting in column 0, e.g. at the top of
;; the file, you will have to use /* (c-style comment starter) and
;; not // (c++-style comment starter).

;; kickasm mode is not automatically linked to any specific file
;; extension.  To do that you should put the following lines in your
;; init.el file:
;;
;; (require 'kickasm-mode)
;; (add-to-list 'auto-mode-alist '("\\.asm" . kickasm-mode))
;;
;; You should replace .asm with the extension you are using for your
;; assembler files.
;;
;; Use 'M-x customize-group RET kickasm RET' to adapt kickasm to your
;; needs.

;; TODO: Fix so that predefined functions need a parenthesis before
;;       changing color
;;       Add faces to keywords: true, false

;;; Code:

(require 'compile)

(defgroup kickasm nil
  "Options for `kickasm-mode'"
  :prefix "kickasm-"
  :group 'languages)

(defgroup kickasm-faces nil
  "Faces used by `kickasm-mode'"
  :group 'kickasm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom kickasm-mnemonic-indent 16
  "Column for 6502 mnemonic."
  :type 'integer
  :group 'kickasm)

(defcustom kickasm-mnemonic-comment-indent 40
  "Column for 6502 mnemonic comments."
  :type 'integer
  :group 'kickasm)

(defcustom kickasm-command-start-indent 12
  "Left most column for Kick Assembler commands."
  :type 'integer
  :group 'kickasm)

(defcustom kickasm-tab-width 4
  "Tab width to be used in kickasm-mode."
  :type 'integer
  :group 'kickasm)

(defcustom kickasm-show-c64-tooltips t
  "Specify if C64 tooltips should be enabled in kickasm mode."
  :type 'boolean
  :group 'kickasm)

(defcustom kickasm-use-automatic-indentation t
  "Specify if automatic indentation should be used."
  :type 'boolean
  :group 'kickasm)

(defcustom kickasm-mnemonic-indentation-mode 'min
  "Specify how mnemonics should be indented.
If nil then always indent mnemonics to `kickasm-mnemonic-indent'.
If 'min then indent to which is greatest of `kickasm-mnemonic-indent'
and the depth of the preceding code.
If 'depth then always indent to the depth of the preceding code."
  :type 'symbol
  :group 'kickasm)

(defcustom kickasm-split-large-label-lines nil
  "Split lines after label if the label is too large to keep indentation."
  :type 'boolean
  :group 'kickasm)

(defcustom kickasm-indent-labels-to-scoping-level nil
  "Indent labels so they align with the scoping hierarchy."
  :type 'boolean
  :group 'kickasm)

(defcustom kickasm-scoping-indent 4
  "Number of character to indent inside a new scope."
  :type 'integer
  :group 'kickasm)

(defcustom kickasm-scoping-label-indent 2
  "Number of character to indent a label inside a new scope."
  :type 'integer
  :group 'kickasm)

(defcustom kickasm-preprocessor-indent 2
  "Number of spaces used for preprocessor indentation."
  :type 'integer
  :group 'kickasm)

(defcustom kickasm-preprocessor-indentation-mode 'normal
  "Specify how preprocessor lines should be indented.
If nil then ignore all preprocessor lines when indenting.
If 'left then always indent to beginning of line
If 'normal then indent `kickasm-preprocessor-indent' spaces for
each nesting level."
  :type 'symbol
  :group 'kickasm)

(defcustom kickasm-position-stack-depth 5
  "The maximum number of positions saved when searching for definition."
  :type 'integer
  :group 'kickasm)

(defvar kickasm--return-position-stack
  "Internal circular list used for saving markers when searching for definitions.")

(defcustom kickasm-error-regexp-alist
  '((kickasm "^Error: \\([^\n]*\\)\nat line \\([0-9]+\\), column \\([0-9]+\\) in \\(.*\\)"
     4 2 3 2 1))
  "Value for `compilation-error-regexp-alist' in kickasm."
  :type '(repeat (choice (symbol :tag "Predefined symbol")
                         (sexp :tag "Error specification")))
  :group 'kickasm)

(defcustom kickasm-command "java -cp \"/usr/lib/kickassembler/*\" kickass.KickAssembler"
  "Command to run Kick Assembler."
  :type 'string
  :group 'kickasm)

(defcustom kickasm-assemble-command (concat kickasm-command " -vicesymbols -debugdump")
  "Command to assemble Kick Assembler programs."
  :type 'string
  :group 'kickasm)

(defcustom kickasm-vice-command "x64sc"
  "Command to run VICE.
Please note that -moncommands will be added automatically if a vice symbol
file was created by the assembler."
  :type 'string
  :group 'kickasm)

(defcustom kickasm-c64debugger-command "c64debugger -autojmp -wait 4000"
  "Command to run C64 Debugger.
Please note that -symbols will be added automatically if a vice symbol
file was created by the assembler."
  :type 'string
  :group 'kickasm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro kickasm--opt (keywords &optional paren)
  "Make optimized regexp of KEYWORDS.
PAREN determine how the expression is enclosed by parenthesis, see
`regexp-opt' for more details."
  `(eval-when-compile
     (regexp-opt ,keywords ,paren)))

(defun kickasm-compilation-buffer-name (mode)
  "Function to compute the name of the compilation buffer.
MODE is the name of the major mode."
  (concat "*" "kickasm" "*"))

(defun kickasm-compilation-setup-function ()
  "Compilation setup function."
  (if (not (assoc 'kickasm compilation-error-regexp-alist-alist))
             (mapc
              (lambda (item)
                (push (car item) compilation-error-regexp-alist)
                (push item compilation-error-regexp-alist-alist)
                )
              kickasm-error-regexp-alist))

  (setq-local compilation-scroll-output 'first-error)
  (setq-local compilation-auto-jump-to-first-error t))

(defun kickasm-assemble ()
  "Assemble the file in the buffer."
  (interactive)
  (compile (concat kickasm-assemble-command " " (file-name-nondirectory buffer-file-name))))

(defun kickasm--display-byte-buffer (compbuf)
  "Display the bytedump file in a buffer and show it in a window.
COMPBUF is the compilation buffer."
  (let ((dumpfilename (concat (file-name-directory buffer-file-name) "ByteDump.txt"))
	(bytebuffer (get-buffer-create (concat "*" (buffer-name) "<Bytedump>*"))))
	
    (with-current-buffer bytebuffer
      (setq buffer-read-only nil)	    
      (insert-file-contents dumpfilename nil nil nil t)
      (kickasm-mode)
      (buffer-disable-undo)
      (setq buffer-read-only t))

    (set-window-buffer (get-buffer-window compbuf t) bytebuffer)))

;; Called when compilation process changes state.
(defun kickasm--bytedump-sentinel (proc msg)
  "Sentinel for compilation buffers.
PROC is the process and MSG the event message. Checks if
assembling finished without errors and then opens the bytedump file."
  (when (and (eq (process-status proc) 'exit)
	     (= (process-exit-status proc) 0))
    (kickasm--display-byte-buffer (process-buffer proc))
    (advice-remove (process-sentinel proc) #'kickasm--bytedump-sentinel)))

(defun kickasm-assemble-bytedump ()
  "Assemble the file in the buffer and display bytedump file."
  (interactive)
  (let* ((compbuffer (compile (concat kickasm-assemble-command " -bytedump " (file-name-nondirectory buffer-file-name))))
	 (compproc (get-buffer-process compbuffer)))

    (if (and compproc (process-sentinel compproc))
	(with-current-buffer compbuffer
	  (advice-add (process-sentinel compproc)
		      :before #'kickasm--bytedump-sentinel))
      (when (= (process-exit-status compproc) 0)
	(kickasm--display-byte-buffer compbuffer)))))

(defun kickasm-syntactic-face-function (state)
  "Function to determine which face to use when fontifying syntactically.
The function is called with a single parameter (the state as returned by
`parse-partial-sexp' at the beginning of the region to highlight) and
should return a face.  This is normally set via `font-lock-defaults'."
  (if (and (nth 8 state)
	   (get-text-property (nth 8 state) 'font-lock-face))
      'kickasm-disabled-face
    (if (nth 3 state) font-lock-string-face font-lock-comment-face)))

(defun kickasm--get-disabled-code ()
  "Parse current buffer (asminfo) and return a list of disabled code parts."
  (goto-char (point-min))
  (let ((dislist))
    (while (re-search-forward "^ppDisabledCode;\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),0" nil t)
      (setq dislist (append dislist
			    `(,(list (string-to-number (buffer-substring (match-beginning 1) (match-end 1)))
				     (string-to-number (buffer-substring (match-beginning 2) (match-end 2)))
				     (string-to-number (buffer-substring (match-beginning 3) (match-end 3)))
				     (string-to-number (buffer-substring (match-beginning 4) (match-end 4))))))))			   
    dislist))

(defun kickasm--set-disabled-face (range)
  "Applies a disabled face to a text-range.
RANGE is the range to apply the face to and is a list of start and end position in lines and characters."
  (let ((start (save-excursion
		 (goto-char (point-min))
		 (forward-line (1- (nth 0 range)))
		 (forward-char (nth 1 range))
		 (point)))
	(end (save-excursion
		 (goto-char (point-min))
		 (forward-line (1- (nth 2 range)))
		 (forward-char (nth 3 range))
		 (point))))
    (add-text-properties start end '(font-lock-face kickasm-disabled-face))))

(defun kickasm--parse-asminfo ()
  "Load asminfo into temp buffer and interpret it."
  (let* ((asminfofilename (concat (file-name-directory buffer-file-name) "asminfo.txt"))
	 (bytebuffer (get-buffer-create (concat "*" (buffer-name) "<asminfo>*")))
	 (disabledcode (with-current-buffer bytebuffer
			 (insert-file-contents-literally asminfofilename nil nil nil t)
			 (kickasm--get-disabled-code))))
    (kill-buffer bytebuffer)
    (let ((modp (buffer-modified-p)))
      (remove-text-properties (point-min) (point-max) '(font-lock-face nil))
      (mapc 'kickasm--set-disabled-face disabledcode)
      (restore-buffer-modified-p modp))))
  
;; Called when asminfo compilation process changes state.
(defun kickasm--asminfo-sentinel (proc msg)
  "Sentinel for asminfo.
PROC is the process and MSG the event message. Checks if
assembling finished without errors and then processes the result."
  (when (and (eq (process-status proc) 'exit)
	     (= (process-exit-status proc) 0))
    (kickasm--parse-asminfo)
    (advice-remove (process-sentinel proc) #'kickasm--asminfo-sentinel)))

(defun kickasm-assemble-asminfo ()
  "Assemble the file in the buffer to get asminfo and then process result.
The use of this is to gray out parts of the code that is disabled."
  (interactive)
  (when buffer-file-name
    ;; Autosave buffer first  
    (do-auto-save t t)
    (let* ((filename (if (and buffer-auto-save-file-name
			      (file-exists-p buffer-auto-save-file-name))
			 buffer-auto-save-file-name
		       buffer-file-name))
	   (compproc (apply 'start-process
			    "KICKASMINFO"
			    "*Messages*"
			    (append (split-string-and-unquote kickasm-command)
				    `("-asminfo")
				    `("syntax")
				    `(,(file-name-nondirectory filename))
				    `("-noeval")))))
      
      (if (and compproc (process-sentinel compproc))
	  (advice-add (process-sentinel compproc)
		      :before #'kickasm--asminfo-sentinel)
	(when (= (process-exit-status compproc) 0)
	  (kickasm--parse-asminfo)
	  t)))
    ))
  

(defconst kickasm--vice-process-buffer-name "*vice*")
(defconst kickasm--c64debugger-process-buffer-name "*c64debugger*")

(defun kickasm-get-compilation-buffer-data ()
  "Return a list of information from the compilation buffer.
Currently the list consists of three elements
Element 0 is the compilation window
Element 1 is the name of the prg/d64 file created by Kick Assembler
Element 2 is the name of the vice symbol file created by Kick Assembler
Element 3 is the name of the breakpoint file
The list might be extended in the future in case more strings are needed."
  (let* ((compbuf (get-buffer-create (kickasm-compilation-buffer-name nil)))
	 (compwin (get-buffer-window compbuf t))
	 breakname
	 runnablename
	 vicesymname)

    (with-current-buffer compbuf
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "Wrote file.*: \\([^\n]*\\)" nil t)
	    (setq breakname (buffer-substring (match-beginning 1) (match-end 1))))
	(if (re-search-forward "Writing prg file.*: \\([^\n]*\\)" nil t)
	    (setq runnablename (buffer-substring (match-beginning 1) (match-end 1))))
	(if (re-search-forward "Writing d64 file.*: \\([^\n]*\\)" nil t)
	    (setq runnablename (buffer-substring (match-beginning 1) (match-end 1))))
	(if (re-search-forward "Writing Vice symbol file: \\([^\n]*\\)" nil t)
	    (setq vicesymname (buffer-substring (match-beginning 1) (match-end 1))))))

    (list compwin runnablename vicesymname breakname)))

(defun kickasm-run-vice ()
  "Run VICE with an assembled file."
  (interactive)
  (let* ((vicebuf (get-buffer-create kickasm--vice-process-buffer-name))
	 (viceproc (get-buffer-process vicebuf))
	 (compdata (kickasm-get-compilation-buffer-data)))

    (if viceproc (delete-process viceproc))

    (with-current-buffer vicebuf
      (erase-buffer)
      (buffer-disable-undo)
      (unless (derived-mode-p 'comint-mode)
	(comint-mode)))

    (if (nth 0 compdata) (set-window-buffer (nth 0 compdata) vicebuf))

    (if (or (nth 1 compdata)
	    (y-or-n-p "Unable to locate name of .prg/.d64 file, do you want to run VICE anyway? "))
	(apply 'start-process
	       "VICE"
	       kickasm--vice-process-buffer-name
	       (append (split-string-and-unquote kickasm-vice-command)
		       (if (nth 2 compdata) `("-moncommands" ,(nth 2 compdata)))
		       (if (nth 1 compdata) `(,(nth 1 compdata))))))))

(defun kickasm-run-c64debugger ()
  "Run C64 Debugger with an assembled file."
  (interactive)
  (let* ((c64dbgbuf (get-buffer-create kickasm--c64debugger-process-buffer-name))
	 (c64dbgproc (get-buffer-process c64dbgbuf))
	 (compdata (kickasm-get-compilation-buffer-data)))

    (if c64dbgproc (delete-process c64dbgproc))

    (with-current-buffer c64dbgbuf
      (setq buffer-read-only nil)
      (erase-buffer)
      (buffer-disable-undo)
      (setq buffer-read-only t))

    (if (nth 0 compdata) (set-window-buffer (nth 0 compdata) c64dbgbuf))

    (if (or (nth 1 compdata)
	    (y-or-n-p "Unable to locate name of .prg file, do you want to run C64 Debugger anyway? "))
	(apply 'start-process
	       "C64DBG"
	       kickasm--c64debugger-process-buffer-name
	       (append (split-string-and-unquote kickasm-c64debugger-command)
		       (if (nth 2 compdata) `("-symbols" ,(nth 2 compdata)))
		       (if (nth 3 compdata) `("-breakpoints" ,(nth 3 compdata)))
		       (if (nth 1 compdata) `("-prg" ,(nth 1 compdata))))))))

(defun kickasm--make-position-stack ()
  "Make a circular list of markers."
  (let ((stack-size kickasm-position-stack-depth)
	stack)
    (while (>= (setq stack-size (1- stack-size)) 0)
      (setq stack (cons (make-marker) stack)))

    ;; Make list circular
    (setcdr (last stack) stack)))

(defun kickasm-electric-brace (arg)
  "Insert a brace and perform indentation.
ARG is the number of braces to insert."
  (interactive "*P")
  (let ((old-blink-paren blink-paren-function)
	blink-paren-function)
    ;; Insert the brace.
    (self-insert-command (prefix-numeric-value arg))
    (save-excursion (kickasm-indent-line))
    (if old-blink-paren
	(funcall old-blink-paren))))

(defun kickasm-electric-colon (arg)
  "Insert a colon and perform indentation.
ARG is the number of colon to insert."
  (interactive "*P")
  ;; Insert the colon
  (let ((type (kickasm--get-line-type)))
    (self-insert-command (prefix-numeric-value arg))
    (when (and (eq (kickasm--get-line-type) 'label)
	       (not (eq type 'label)))
      (kickasm-indent-line))))
      	
(defun kickasm-insert-tab ()
  "Insert a normal tab character.  Useful for manual indentation."
  (interactive)
  (insert ?\t))

(defun kickasm-find-definition ()
  "Move to definition of the function/macro/etc having the name at point.
It only works if definition are located in the same buffer.  The point
are saved and you should press \\[kickasm-return-from-definition] to go back.
Up to 'kickasm-position-stack-depth' positions will be remembered on the stack."
  (interactive)
  (let ((pos (point))
	(wstop (if (forward-word) (point)))
	(wstart (if (backward-word) (point))))
    (goto-char pos)
    (if (and wstart
	     wstop
	     (re-search-backward
	      (concat (kickasm--opt '(".function" ".macro" ".pseudocommand"
				      ".const" ".label" ".struct" ".var" "var") t)
		      "[[:space:]]+\\("
		      (buffer-substring-no-properties wstart wstop)
		      "\\)\\b\\|@?!?\\("
		      (buffer-substring-no-properties wstart wstop)
		      "\\):")
	      nil t nil))
	(progn (goto-char (if (match-beginning 2) (match-beginning 2) (match-beginning 3)))
	       (set-marker (car kickasm--return-position-stack) pos)
	       (setq kickasm--return-position-stack (cdr kickasm--return-position-stack)))
      (goto-char pos))))
  

(defun kickasm-return-from-definition ()
  "Return to the last position where you pressed \\[kickasm-find-definition].
Up to `kickasm-position-stack-depth' positions will be remembered."
  (interactive)
  (setq kickasm--return-position-stack
	(nthcdr (1- kickasm-position-stack-depth) kickasm--return-position-stack))
  (let ((pos (marker-position (car kickasm--return-position-stack))))
    (if pos (goto-char pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The standard font-lock faces is not enough since Kick Assembler is two
;; languages mixed together. So we define two new faces to use for
;; the 6502 mnemonics (standard and unintended).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface kickasm-disabled-face
  '((((class grayscale) (background light)) :foreground "gainsboro")
    (((class grayscale) (background dark))  :foreground "gray18")
    (((class color) (min-colors 88) (background light)) :foreground "gainsboro")
    (((class color) (min-colors 88) (background dark))  :foreground "DimGray")
    (((class color) (min-colors 16) (background light)) :foreground "gainsboro")
    (((class color) (min-colors 16) (background dark)) :foreground "DimGray")
    (((class color) (min-colors 8)) :foreground "gray")
    (t :weight bold))
  "Kickasm mode face used for disabled code."
  :group 'kickasm-faces)

(defface kickasm-mnemonic-face
  '((((class grayscale) (background light)) :foreground "Gray" :weight bold)
    (((class grayscale) (background dark))  :foreground "DarkGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "DarkOrange3" :slant italic)
    (((class color) (min-colors 88) (background dark))  :foreground "orange1" :slant italic)
    (((class color) (min-colors 16) (background light)) :foreground "DarkOrange3" :slant italic)
    (((class color) (min-colors 16) (background dark)) :foreground "orange1" :slant italic)
    (((class color) (min-colors 8)) :foreground "yellow" :slant italic)
    (t :weight bold))
  "Kickasm mode face used to highlight 6502 mnemonics."
  :group 'kickasm-faces)

(defface kickasm-unintended-mnemonic-face
  '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "red3" :slant italic)
    (((class color) (min-colors 88) (background dark))  :foreground "OrangeRed1" :slant italic)
    (((class color) (min-colors 16) (background light)) :foreground "red3" :slant italic)
    (((class color) (min-colors 16) (background dark)) :foreground "OrangeRed1" :slant italic)
    (((class color) (min-colors 8)) :foreground "red" :slant italic)
    (t :weight bold))
  "Kickasm mode face used to highlight unintended 6502 mnemonics."
  :group 'kickasm-faces)

(defface kickasm-default-face
  '((((class grayscale) (background light)) :foreground "black")
    (((class grayscale) (background dark))  :foreground "white")
    (((class color) (min-colors 88) (background light)) :foreground "black")
    (((class color) (min-colors 88) (background dark))  :foreground "white")
    (((class color) (min-colors 16) (background light)) :foreground "black")
    (((class color) (min-colors 16) (background dark)) :foreground "white")
    (((class color) (min-colors 8)) :foreground "black")
    (t :weight bold))
  "Kickasm mode default face."
  :group 'kickasm-faces)

(defface kickasm-label-face
  '((t :inherit (font-lock-type-face)))
  "Face for labels."
  :group 'kickasm-faces)

(defface kickasm-data-directive-face
  '((t :inherit (font-lock-variable-name-face)))
  "Face for data directives."
  :group 'kickasm-faces)

(defface kickasm-keyword-face
  '((t :inherit (font-lock-keyword-face)))
  "Face for keywords."
  :group 'kickasm-faces)

(defface kickasm-special-keyword-face
  '((t :inherit (font-lock-warning-face)))
  "Face to highlight special keywords."
  :group 'kickasm-faces)

(defface kickasm-name-face
  '((t :inherit (font-lock-function-name-face)))
  "Face for the name of functions, macros, pseudocommands and namespaces."
  :group 'kickasm-faces)

(defface kickasm-preprocessor-face
  '((t :inherit (font-lock-preprocessor-face)))
  "Face for preprocessor commands."
  :group 'kickasm-faces)

(defface kickasm-builtin-keyword-face
  '((t :inherit (font-lock-builtin-face)))
  "Face for builtin keywords."
  :group 'kickasm-faces)

(defface kickasm-constant-face
  '((t :inherit (font-lock-constant-face)))
  "Face for constants."
  :group 'kickasm-faces)

(defface kickasm-mnemonic-slant-face
  '((t :inherit (italic)))
  "Face appended to mnemonic lines to specify slant."
  :group 'kickasm-faces)

(eval-and-compile
  (defconst kickasm-c64-tooltip-strings
    #s(hash-table test equal data
		  ("$d000" "Sprite 0:\nHorizontal Position"
		   "$d001" "Sprite 0:\nVertical Position"
		   "$d002" "Sprite 1:\nHorizontal Position"
		   "$d003" "Sprite 1:\nVertical Position"
		   "$d004" "Sprite 2:\nHorizontal Position"
		   "$d005" "Sprite 2:\nVertical Position"
		   "$d006" "Sprite 3:\nHorizontal Position"
		   "$d007" "Sprite 3:\nVertical Position"
		   "$d008" "Sprite 4:\nHorizontal Position"
		   "$d009" "Sprite 4:\nVertical Position"
		   "$d00a" "Sprite 5:\nHorizontal Position"
		   "$d00b" "Sprite 5:\nVertical Position"
		   "$d00c" "Sprite 6:\nHorizontal Position"
		   "$d00d" "Sprite 6:\nVertical Position"
		   "$d00e" "Sprite 7:\nHorizontal Position"
		   "$d00f" "Sprite 7:\nVertical Position"
		   "$d010" "Sprite 0-7:\nMSB Horizontal Position"
		   "$d011" "Vertical Fine Scrolling and Control Register:\n0-2: Vertical fine scroll\n3: 1=25 Rows, 0=24 Rows\n4: 0=Blank entire screen\n5: 1=Enable bitmap graphics mode\n6: 1=Enable extended color text mode\n7: Bit 8 of raster compare register $d012"
		   "$d012" "Raster Compare Register:\nRead Current Scan Line/Write Raster IRQ Line"
		   "$d013" "Light Pen:\nHorizontal Position"
		   "$d014" "Light Pen:\nVertical Position"
		   "$d015" "Sprite 0-7:\n1=Enable sprite"
		   "$d016" "Horizontal Fine Scrolling and Control Register:\n0-2: Horizontal fine scroll\n3: 1=40 Columns, 0=38 Columns\n4: 1=Enable multicolor text or bitmap mode\n5: 1=Video completely off, 0=normal operation\n6-7: Unused"
		   "$d017" "Sprite 0-7:\n1=Double height, 0=Normal height"
		   "$d018" "VIC-II Chip Memory Control Register:\n0: Unused\n1-3: Text character dot-data base address\n3: Bitmap screen base address\n4-7: Video matrix base address"
		   "$d019" "VIC-II Interrupt Flag Register:\n0: 1=Raster Line IRQ\n1: 1=Sprite to graphics collision\n2: 1=Sprite to sprite collision\n3: 1=Light pen trigger\n4-6: Unused\n7: 1=VIC-II caused IRQ"
		   "$d01a" "IRQ Mask Register:\n0: 1=Enable raster line IRQ\n1: 1=Enable sprite to graphics collision IRQ\n2: 1=Enable sprite to sprite collision IRQ\n3: 1=Enable light pen trigger IRQ\n4-7: Unused"
		   "$d01b" "Sprite 0-7:\n0=Sprite on top of foreground\n1=Foreground on top of sprite"
		   "$d01c" "Sprite 0-7:\n0=Hi-res, 1=Multicolor"
		   "$d01d" "Sprite 0-7:\n1=Double width, 0=Normal width"
		   "$d01e" "Sprite 0-7:\n1=Sprite collided with another sprite"
		   "$d01f" "Sprite 0-7:\n1=Sprite collided with foreground"
		   "$d020" "Border Color Register"
		   "$d021" "Background Color 0"
		   "$d022" "Background Color 1"
		   "$d023" "Background Color 2"
		   "$d024" "Background Color 3"
		   "$d025" "Sprite Multicolor 0"
		   "$d026" "Sprite Multicolor 1"
		   "$d027" "Sprite 0 Color"
		   "$d028" "Sprite 1 Color"
		   "$d029" "Sprite 2 Color"
		   "$d02a" "Sprite 3 Color"
		   "$d02b" "Sprite 4 Color"
		   "$d02c" "Sprite 5 Color"
		   "$d02d" "Sprite 6 Color"
		   "$d02e" "Sprite 7 Color"
		   "$d400" "Voice 1 Frequency Control (low byte)"
		   "$d401" "Voice 1 Frequency Control (high byte)"
		   "$d402" "Voice 1 Pulse Waveform Width (low byte)"
		   "$d403" "Voice 1 Pulse Waveform Width (high byte)"
		   "$d404" "Voice 1 Control Register:\n0: Gate Bit\n1: Sync Bit\n2: Ring Modulation\n3: Test Bit\n4: Select triangle\n5: Select sawtooth\n6: Select puls\n7: Select random noice"
		   "$d405" "Voice 1 Attack/Decay:\n0-3: Decay cycle duration\n4-7: Attack cycle duration"
		   "$d406" "Voice 1 Sustain/Release:\n0-3: Release cycle duration\n4-7: Sustain volume level"
		   "$d407" "Voice 2 Frequency Control (low byte)"
		   "$d408" "Voice 2 Frequency Control (high byte)"
		   "$d409" "Voice 2 Pulse Waveform Width (low byte)"
		   "$d40a" "Voice 2 Pulse Waveform Width (high byte)"
		   "$d40b" "Voice 2 Control Register:\n0: Gate Bit\n1: Sync Bit\n2: Ring Modulation\n3: Test Bit\n4: Select triangle\n5: Select sawtooth\n6: Select puls\n7: Select random noice"
		   "$d40c" "Voice 2 Attack/Decay:\n0-3: Decay cycle duration\n4-7: Attack cycle duration"
		   "$d40d" "Voice 2 Sustain/Release:\n0-3: Release cycle duration\n4-7: Sustain volume level"
		   "$d40e" "Voice 3 Frequency Control (low byte)"
		   "$d40f" "Voice 3 Frequency Control (high byte)"
		   "$d410" "Voice 3 Pulse Waveform Width (low byte)"
		   "$d411" "Voice 3 Pulse Waveform Width (high byte)"
		   "$d412" "Voice 3 Control Register:\n0: Gate Bit\n1: Sync Bit\n2: Ring Modulation\n3: Test Bit\n4: Select triangle\n5: Select sawtooth\n6: Select puls\n7: Select random noice"
		   "$d413" "Voice 3 Attack/Decay:\n0-3: Decay cycle duration\n4-7: Attack cycle duration"
		   "$d414" "Voice 3 Sustain/Release:\n0-3: Release cycle duration\n4-7: Sustain volume level"
		   "$d415" "Filter Cutoff Frequency (low bits):\n0-2 Low bits\n3-7: Unused"
		   "$d416" "Filter Cutoff Frequency (high byte)"
		   "$d417" "Filter Resonance Control Register:\n0: 1=Filter voice 1\n1: 1=Filter voice 2\n2: 1=Filter voice 3\n 3: 1=Filter external audio input\n4-7: Filter resonance"
		   "$d418" "Volume and Filter Select Register:\n0-3: Output volume\n4: 1=Select low-pass filter\n5: 1=Select band-pass filter\n6: 1=Select high-pass filter\n7: 1=Disconnect output of voice 3"
		   "$d419" "Game Paddle 1 (or 3) Position"
		   "$d41a" "Game Paddle 2 (or 4) Position"
		   "$d41b" "Voice 3 Oscillator Output Value"
		   "$d41c" "Voice 3 Envelope Generator Output Value"
		   "$d800" "Color RAM"
		   "$dc00" "CIA #1 Data Port A:\n0: Select keyboard column 0 / Read joystick 2 up\n1: Select keyboard column 1 / Read joystick 2 down\n2: Select keyboard column 2 / Read joystick 2 left / Read paddle 1 fire button\n3: Select keyboard column 3 / Read joystick 2 right / Read paddle 2 fire button\n4: Select keyboard column 4 / Read joystick 2 fire button\n5: Select keyboard column 5\n6: Select keyboard column 6 / Select paddles on Port A or B\n7: Select keyboard column 7 / Select paddles on Port A or B"
		   "$dc01" "CIA #1 Data Port B:\n0: Read keyboard row 0 / Read joystick 1 up\n1: Read keyboard row 1 / Read joystick 1 down\n2: Read keyboard row 2 / Read joystick 1 left / Read paddle 1 fire button\n3: Select keyboard row 3 / Read joystick 1 right / Read paddle 2 fire button\n4: Select keyboard row 4 / Read joystick 1 fire button\n5: Select keyboard row 5\n6: Select keyboard row 6 / Toggle or pulse Timer A output\n7: Select keyboard row 7 / Toggle or pulse Timer B output"
		   "$dc02" "CIA #1 Data Direction Register A\n0=input, 1=output"
		   "$dc03" "CIA #1 Data Direction Register B\n0=input, 1=output"
		   "$dc04" "CIA #1 Timer A (low byte)"
		   "$dc05" "CIA #1 Timer A (high byte)"
		   "$dc06" "CIA #1 Timer B (low byte)"
		   "$dc07" "CIA #1 Timer B (high byte)"
		   "$dc08" "CIA #1 Time of Day Clock (BCD):\n0-3: Tenth of second digit\n4-7: Unused"
		   "$dc09" "CIA #1 Time of Day Clock (BCD):\n0-3: Second digit of seconds\n4-7: First digit of seconds"
		   "$dc0a" "CIA #1 Time of Day Clock (BCD):\n0-3: Second digit of minutes\n4-7: First digit of minutes"
		   "$dc0b" "CIA #1 Time of Day Clock (BCD):\n0-3: Second digit of hours\n4: First digit of hours\n5-6: Unused\n7: 1=PM, 0=AM"
		   "$dc0c" "CIA #1 Serial Data Port"
		   "$dc0d" "CIA #1 Interrupt Control Register:\n0: Timer A interrupt\n1: Timer B interrupt\n2: Time of Day Clock alarm interrupt\n3: Serial shift register interrupt\n4: FLAG line interrupt\n5-6: Unused\n7: 1=CIA #1 caused interrupt"
		   "$dc0e" "CIA #1 Control Register A:\n0: Timer A 1=start, 0=stop\n1: 1=Timer A output appears on Bit 6 of Port B\n2: Port B output mode, 1=toggle Bit 6, 0=pulse Bit 6 for one cycle\n3: Timer A run mode, 1=one-shot, 0=continuous\n4: 1=Force latched value to be loaded to Timer A\n5: Timer A input mode, 1=count CPU cycles, 0=count signals on CNT line at pin 4 of User Port\n6: Serial Port mode, 1=output, 0=input\n7: Time of Day Clock frequency, 1=50 Hz, 0=60 Hz"
		   "$dc0f" "CIA #1 Control Register B:\n0: Timer B 1=start, 0=stop\n1: 1=Timer B output appears on Bit 7 of Port B\n2: Port B output mode, 1=toggle Bit 7, 0=pulse Bit 7 for one cycle\n3: Timer B run mode, 1=one-shot, 0=continuous\n4: 1=Force latched value to be loaded to Timer B\n5-6: Timer B input mode\n  00=count CPU cycles\n  01=count signals on CNT line at pin 4 of User Port\n  10=count each time Timer A counts down to 0\n  11=count Timer A 0's when CNT pulses are also present\n7: Time of Day write, 0=write alarm, 1=write clock"
		   "$dd00" "CIA #2 Data Port A:\n0-1: select VIC-II bank, 00=Bank 3, 01=Bank 2, 10=Bank 1, 11=Bank0\n2: RS-232 data output\n3: Serial bus ATN signal output\n4: Serial bus clock puls output\n5: Serial bus data output\n6: Serial bus clock pulse input\n7: Serial bus data input"
		   "$dd01" "CIA #2 Data Port B:\n0: RS-232 data input (SIN)\n1: RS-232 request to send (RTS)\n2: RS-232 data terminal ready (DTR)\n3: RS-232 ring indicator (RI)\n4: RS-232 carrier detect (DCD)\n5: Pin J of User Port\n6: RS-232 clear to send (CTS) / Toggle or pulse data output for Timer A\n7: RS-232 data set ready (DSR) / Toggle or pulse data output for Timer B"
		   "$dd02" "CIA #2 Data Direction Register A\n0=input, 1=output"
		   "$dd03" "CIA #2 Data Direction Register B\n0=input, 1=output"
		   "$dd04" "CIA #2 Timer A (low byte)"
		   "$dd05" "CIA #2 Timer A (high byte)"
		   "$dd06" "CIA #2 Timer B (low byte)"
		   "$dd07" "CIA #2 Timer B (high byte)"
		   "$dd08" "CIA #2 Time of Day Clock (BCD):\n0-3: Tenth of second digit\n4-7: Unused"
		   "$dd09" "CIA #2 Time of Day Clock (BCD):\n0-3: Second digit of seconds\n4-7: First digit of seconds"
		   "$dd0a" "CIA #2 Time of Day Clock (BCD):\n0-3: Second digit of minutes\n4-7: First digit of minutes"
		   "$dd0b" "CIA #2 Time of Day Clock (BCD):\n0-3: Second digit of hours\n4: First digit of hours\n5-6: Unused\n7: 1=PM, 0=AM"
		   "$dd0c" "CIA #2 Serial Data Port"
		   "$dd0d" "CIA #2 Interrupt Control Register:\n0: Timer A interrupt\n1: Timer B interrupt\n2: Time of Day Clock alarm interrupt\n3: Serial shift register interrupt\n4: FLAG line interrupt\n5-6: Unused\n7: 1=CIA #2 caused interrupt"
		   "$dd0e" "CIA #1 Control Register A:\n0: Timer A 1=start, 0=stop\n1: 1=Timer A output appears on Bit 6 of Port B\n2: Port B output mode, 1=toggle Bit 6, 0=pulse Bit 6 for one cycle\n3: Timer A run mode, 1=one-shot, 0=continuous\n4: 1=Force latched value to be loaded to Timer A\n5: Timer A input mode, 1=count CPU cycles, 0=count signals on CNT line at pin 4 of User Port\n6: Serial Port mode, 1=output, 0=input\n7: Time of Day Clock frequency, 1=50 Hz, 0=60 Hz"
		   "$dd0f" "CIA #1 Control Register B:\n0: Timer B 1=start, 0=stop\n1: 1=Timer B output appears on Bit 7 of Port B\n2: Port B output mode, 1=toggle Bit 7, 0=pulse Bit 7 for one cycle\n3: Timer B run mode, 1=one-shot, 0=continuous\n4: 1=Force latched value to be loaded to Timer B\n5-6: Timer B input mode\n  00=count CPU cycles\n  01=count signals on CNT line at pin 4 of User Port\n  10=count each time Timer A counts down to 0\n  11=count Timer A 0's when CNT pulses are also present\n7: Time of Day write, 0=write alarm, 1=write clock"
		   ))
    "Short description text for the register in C64"))

(defun kickasm-get-tooltip-string (window object pos)
  "Return a string to show as a tooltip.
WINDOW is the window where the mouse click happend.
OBJECT is the buffer in that window.
POS is the position in the buffer."
  (cond ((not kickasm-show-c64-tooltips) nil)
	((bufferp object)
	 (with-current-buffer object
	   (save-excursion
	     (goto-char pos)
	     (let* ((help-string (downcase (current-word t nil)))
		    (tooltip-string (gethash (if (string-match-p "[89ab][[:xdigit:]]\\{2,2\\}"
								 help-string 2)
						 "$d800"
					       help-string)
					     kickasm-c64-tooltip-strings)))
	       (if (not tooltip-string)
		   "Unknown C64 address"
		 tooltip-string)))))
	(t nil)))

(eval-and-compile
  (defconst kickasm-mnemonics
    '("adc" "and" "asl" "bcc" "bcs" "beq" "bit" "bmi" "bne" "bpl" "brk" "bvc" "bvs"
      "clc" "cld" "cli" "clv" "cmp" "cpx" "cpy" "dec" "dex" "dey" "eor" "inc" "inx"
      "iny" "jmp" "jsr" "lda" "ldx" "ldy" "lsr" "nop" "ora" "pha" "php" "pla" "plp"
      "rol" "ror" "rti" "rts" "sbc" "sec" "sed" "sei" "sta" "stx" "sty" "tax" "tay"
      "tsx" "txa" "txs" "tya")
    "6502 mnemonics"))

(eval-and-compile
  (defconst kickasm-mnemonic-extensions
    '(".im" ".imm" ".z" ".zp" ".zx" ".zpx" ".zy" ".zpy" ".izx" ".izpx" ".izy"
      ".izpy" ".a" ".abs" ".ax" ".absx" ".ay" ".absy" ".i" ".ind" ".r" ".rel")
    "6502 mnemonic extensions"))

(eval-and-compile
  (defconst kickasm-unintended-mnemonics
    '("ahx" "alr" "anc" "anc2" "arr" "axs" "dcp" "isc" "las" "lax" "rla" "rra" "sax"
      "sbc2" "shx" "shy" "slo" "sre" "tas" "xaa" "lxa" "dcm" "ins" "isb" "asr" "ane"
      "sbx" "sha" "shs" "lae" "lds")
    "6502 unintended mnemonics"))

(eval-and-compile
  (defconst kickasm-data-directives
    '(".by" ".byte" ".dw" ".dword" ".fill" ".te" ".text" ".wo" ".word")
    "Kick Assembler Data directives"))

(eval-and-compile
  (defconst kickasm-keywords
    '(".const" ".define" ".enum" ".eval" ".for" ".if" "else" ".label"
      ".return" ".struct" ".var" "var" ".while" "virtual")
    "Kick Assembler keywords"))

(eval-and-compile
  (defconst kickasm-special-keywords
    '(".modify" ".filemodify" ".modify" ".align" ".assert" ".asserterror" ".segment"
      ".pc" ".pseudopc" ".encoding" ".error" ".errorif" ".plugin" ".memblock" ".zp"
      ".segmentdef" ".segmentout" ".file" ".disk" ".break" ".watch" ".cpu")
    "Kick Assembler keywords to be extra highlighted"))

(eval-and-compile
  (defconst kickasm-preprocessor-keywords
    '("if" "endif" "define" "undef" "import" "importif" "importonce" "else" "elif")
    "Kick Assembler preprocessor keywords"))
    
(eval-and-compile
  (defconst kickasm-builtin-keywords
    '("abs" "acos" "asin" "atan" "atan2" "cbrt" "ceil" "cos" "cosh" "exp" "expm1"
      "floor" "hypot" "IEEEremainder" "log" "log10" "log1p" "max" "min" "mod" "pow"
      "random" "round" "signum" "sin" "sinh" "sqrt" "tan" "tanh" "toDegrees" "toRadians"
      "BasicUpstart" "BasicUpstart2" "LoadBinary" "Hashtable" "List" "Matrix" "createFile"
      "LoadSid" "Vector" "RotationMatrix" "ScaleMatrix" "MoveMatrix" "PerspectiveMatrix"
      "CmdArgument" "toIntString" "toBinaryString" "toOctalString" "toHexString"
      "LoadPicture" "toUpperCase" "toLowerCase")
    "Kick Assembler builtin keywords"))

(eval-and-compile
  (defconst kickasm-builtin-methods
    '("getStructName" "getNoOfFields" "getFieldNames" "get" "set" "add" "addAll" "size"
      "remove" "shuffle" "reverse" "sort" "put" "keys" "containsKey" "writeln" "width"
      "height" "getPixel" "getSinglecolorByte" "getMulticolorByte" "header" "version"
      "location" "init" "play" "songs" "startSong" "name" "author" "copyright" "speed"
      "flags" "startpage" "pagelength" "getData" "print" "printnow" "getType" "getValue"
      "charAt" "string" "substring" "asNumber" "asBoolean" "getSize" "uget" "lock")
    "Kick Assembler builtin keywords"))

(defconst kickasm--label-regexp
  "@?\\([a-zA-Z0-9_]+\\|\\(![a-zA-Z0-9_]*\\)\\):"
  "Regexp that matches labels.")

(defconst kickasm--command-regexp
  (concat (kickasm--opt '(".if" ".for" ".while") t) "[[:space:]]*([^)]*)\\|\\belse")
  "Regexp for looking back to determine indentation level.")

(defconst kickasm--preprocessor-regexp
  (concat "^[[:space:]]*\\(#\\)" (kickasm--opt kickasm-preprocessor-keywords t) "\\>")
  "Regexp for looking at preprocessor lines.")

(defvar kickasm-font-lock-keywords
  `(
    ;; Unintended mnemonics
    (,(concat "\\<\\(" (kickasm--opt kickasm-unintended-mnemonics t) (kickasm--opt kickasm-mnemonic-extensions t)
    	      "?\\)\\>[[:space:]]*\\([^;\n]*\\)")
     (1 'kickasm-unintended-mnemonic-face) (4 'kickasm-mnemonic-slant-face keep))

    ("\\<\\(nop\\)\\>[[:space:]]+\\([^;\n[:space:]/][^;\n]*\\)"
     (1 'kickasm-unintended-mnemonic-face) (2 'kickasm-mnemonic-slant-face keep))
    
    ;; Mnemonics
    (,(concat "\\<\\(" (kickasm--opt kickasm-mnemonics t) (kickasm--opt kickasm-mnemonic-extensions t)
    	      "?\\)\\>[[:space:]]*\\([^;\n]*\\)")
     (1 'kickasm-mnemonic-face keep) (4 'kickasm-mnemonic-slant-face keep))

    ;; Labels
    (,(concat kickasm--label-regexp) (0 'kickasm-label-face append))

    ;; The purpose of this is to override label face for question mark conditional <exp> ? a : b
    (,(concat "?[[:space:]]*" kickasm--label-regexp) (0 'kickasm-default-face prepend))
    
    ;; Tooltip for IO addresses
    ("$[dD][0489abABcCdD][[:xdigit:]]\\{2,2\\}\\b"
     (0 '(face nil help-echo kickasm-get-tooltip-string mouse-face highlight)))
    
    ;; Data directives
    (,(kickasm--opt kickasm-data-directives 'words) . 'kickasm-data-directive-face)

    ;; Preprocessor keywords
    ;; Change the syntax interpretation of preprocessor lines to comments in order
    ;; to make indentation work better. Can't use syntax tables for this since # is also
    ;; used in mnemonics.
    (,kickasm--preprocessor-regexp
     (1 '(face kickasm-preprocessor-face syntax-table (11 . nil)))
     (2 'kickasm-preprocessor-face keep))
    
    ;; Keywords
    (,(concat "\\<" (kickasm--opt '(".function" ".macro") t)
    	      "[[:space:]]+\\(@?[a-zA-Z0-9_]+\\)\\b")
     (1 'kickasm-keyword-face) (2 'kickasm-name-face))
    ("\\<\\(\\.pseudocommand\\)[[:space:]]+\\(@?[a-zA-Z0-9_]+\\)\\(.*{\\)"
     (1 'kickasm-keyword-face) (2 'kickasm-name-face) (3 'normal t))
    (,(kickasm--opt kickasm-keywords 'words) . 'kickasm-keyword-face)

    ;; Special keywords
    (,(concat "\\<" (kickasm--opt '(".namespace" ".filenamespace") t)
	      "[[:space:]]+\\([a-zA-Z0-9_]+\\)\\b")
     (1 'kickasm-special-keyword-face) (2 'kickasm-name-face))
    (,(kickasm--opt kickasm-special-keywords 'words) . 'kickasm-special-keyword-face)
    ("[^;\n]*\\(\\*[[:space:]]*=\\)" (1 'kickasm-special-keyword-face))
    (,(concat "\\<" (kickasm--opt '(".import" ".importonce") t)
	      "[[:space:]]+\\(binary\\|c64\\|source\\|text\\)\\>") . 'kickasm-special-keyword-face)

    ;; Builtin functions and methods
    (,(kickasm--opt kickasm-builtin-keywords 'words) . 'kickasm-builtin-keyword-face)
    (,(concat "\\." (kickasm--opt kickasm-builtin-methods t) "\\>") (1 'kickasm-builtin-keyword-face))

    ;; Constants
    ;; Consider words written in capital letters as constants
    ("\\<!?\\([[:upper:]][A-Z0-9_]+\\)\\>" (1 'kickasm-constant-face append))
    
    )
  "Expressions to highlight in kickasm-mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kickasm--get-mnemonic-indentation (ind)
  "Return the column to use for indenting a mnemonic.
IND is the indentation column to use for aligning it with code."
  (if (or (not kickasm-mnemonic-indentation-mode)
	  (and (equal kickasm-mnemonic-indentation-mode 'min)
	       (< ind kickasm-mnemonic-indent)))
      kickasm-mnemonic-indent
    ind))

(defun kickasm--get-else-indentation ()
  "Get indentation column for an else clause.
If no matching .if is found or if any other failure happens then
assume that the else should stay unchanged."
  (condition-case nil
      (save-excursion
	(while (progn
		 (goto-char (scan-sexps (point) -1))
		 (not (looking-at "\\.if"))))
	(current-indentation))
    (error (current-indentation))))

(defun kickasm--get-label-indentation ()
  "Get indentation column for a label."
  (save-excursion
    (ignore-errors
      (if kickasm-indent-labels-to-scoping-level
	  (let ((syntax (syntax-ppss)))
	    (if (nth 1 syntax)
		(progn (goto-char (nth 1 syntax))
		       (back-to-indentation)
		       (+ (current-column) kickasm-scoping-label-indent))
	      0))
	0))))

(defun kickasm--get-command-indentation ()
  "Get indentation column."
  (save-excursion
    (ignore-errors
      (back-to-indentation)
      
      (let ((cparen (and (not (eobp)) (equal (char-syntax (char-after)) ?\))))
	    (oparen (and (not (eobp)) (equal (char-syntax (char-after)) ?\()))
	    (syntax (syntax-ppss)))
	
	(forward-comment (- (point-max)))

	(cond ((looking-back kickasm--command-regexp nil)
	       (goto-char (match-beginning 0))
	       (+ (if kickasm-indent-labels-to-scoping-level
		      (current-indentation)
		    (current-column))
		  (if oparen 0 kickasm-scoping-indent)))
	      ((and kickasm-indent-labels-to-scoping-level
		    (looking-back kickasm--label-regexp nil))
	       (goto-char (match-beginning 0))
	       (+ (current-indentation)
		  (if oparen
		      0
		    (- kickasm-scoping-indent
		       kickasm-scoping-label-indent))))
	      (t
	       (if (not (nth 1 syntax))
		   ;; We are at toplevel.
		   kickasm-command-start-indent
		 (goto-char (1+ (nth 1 syntax)))
		 (forward-comment (point-max)) 
		 (if (= (line-number-at-pos) (line-number-at-pos (nth 1 syntax)))
		     ;; There is something that is not a comment or whitespace after the
		     ;; starting parenthesis, like: (a,b ...
		     (if cparen
			 ;; Align with starting parenthesis
			 (progn (goto-char (nth 1 syntax))
				(current-column))
		       ;; Align with first element inside parenthesis
		       (current-column))
		   (goto-char (nth 1 syntax))
		   (forward-comment (- (point-max)))
		   (if (looking-back kickasm--command-regexp nil)
		       (progn
			 (goto-char (match-beginning 0))
			 (if (and (not kickasm-indent-labels-to-scoping-level)
				  (equal (kickasm--get-line-type) 'label))
			     (progn (goto-char (kickasm--get-label-end-pos))
				    (skip-chars-forward " \t"))
			   (back-to-indentation)))
		     (goto-char (nth 1 syntax))
		     (back-to-indentation))
		   (+ (current-column) (if cparen 0 kickasm-scoping-indent))))))))))

(defun kickasm--get-type-at-position ()
  "Examines the text found at current position and returns the type found"
  (save-excursion
    (skip-chars-forward " \t")
    (cond ((looking-at kickasm--label-regexp)
	   'label)
	  ((looking-at (concat "\\<\\("
			       (kickasm--opt (append kickasm-mnemonics kickasm-unintended-mnemonics) t)
			       (kickasm--opt kickasm-mnemonic-extensions t) 
			       "?\\)\\>\\|"
			       (kickasm--opt kickasm-data-directives 'words)))
	   'mnemonic)
	  ((= (point) (line-beginning-position) (line-end-position))
	   'newline)
	  ((= (point) (line-end-position))
	   'emptyline)
	  ((looking-at "else")
	   'else)
	  ((equal (char-after) ?#)
	   'preprocessor)
	  ((looking-at "*[* \t\n/]")
	   'maybe-block-comment)
	  ((equal (char-syntax (char-after)) ?\))
	   'close-paren)
	  ((equal (char-syntax (char-after)) ?\()
	   'open-paren)
	  ((looking-at "\\(//+\\|/\\*+\\)")
	   'comment)
	  ((looking-at kickasm--command-regexp)
	   'command)
	  (t 'something))))
  

(defun kickasm--get-line-type ()
  "Examines the beginning of a line to verify the line type."
  (save-excursion
    (back-to-indentation)
    (kickasm--get-type-at-position)))

(defun kickasm-column-at-pos (&optional pos)
  "Return buffer column at position POS.
If POS is nil, use current buffer location."
  (let ((opoint (or pos (point))))
    (save-excursion
      (goto-char opoint)
      (current-column))))

(defun kickasm--get-label-end-pos ()
  "Return the position of the label end.
Make sure that point is located on a label line before calling this function."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "^:")
    (forward-char)
    (point)))


(defun kickasm--indent-comment (col ind)
  "Indent a comment.
COL is the column where position in buffer is.
IND is the indentation column to use for aligning it with code."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "\\(//+\\|/\\*+\\)" (line-end-position) t)
      (let* ((curcol (kickasm-column-at-pos(match-beginning 0)))
	     (mincol (progn (goto-char (match-beginning 0))
			    (skip-chars-backward " \t")
			    (current-column)))
	     (blockstart (looking-at "\\s-*/\\*+"))
	     poslist)

	(if (and blockstart (= mincol 0))
	    (setq poslist '(0))
	  (setq poslist `(,(max (1+ mincol) kickasm-mnemonic-comment-indent)))
	
	  (when (and (>= ind mincol)
		     (/= ind (car poslist)))
	    (setq poslist (cons ind poslist)))

	  (when (and (>= kickasm-mnemonic-indent mincol)
		     (/= ind kickasm-mnemonic-indent)
		     (or (not kickasm-mnemonic-indentation-mode)
			 (and (equal kickasm-mnemonic-indentation-mode 'min)
			      (< ind kickasm-mnemonic-indent))))
	    (setq poslist (cons kickasm-mnemonic-indent poslist))))

	(setq poslist (sort poslist '<))
	
	;; If the comment are located on any of the anchor positions then
	;; it should only reindent if point is also located there.
	(let ((hitlist (memq curcol poslist))
	      (comcol (car (last poslist))))
	  (unless (and hitlist
		       (or (/= curcol col)
			   (= (length poslist) 1)))
	    (if hitlist
		(progn
		  ;; Make poslist circular to make it easier to cycle through
		  (setcdr (last poslist) poslist)
		  (setq comcol (cadr hitlist)))
	      (while (and poslist (>= col (car poslist)))
		(setq poslist (cdr poslist)))

	      (when poslist (setq comcol (car poslist))))
			     
	    (move-to-column curcol)
	    (delete-horizontal-space)
	    (move-to-column (indent-to comcol))))))))

(defun kickasm--get-comment-indentation ()
  "Return the column of the comment, or where it should be."
  (save-excursion
    (beginning-of-line)
    
    (if (re-search-forward "\\(//+\\|/\\*+\\)" (line-end-position) t)
	;; If a comment exists then use that value
	(kickasm-column-at-pos (match-beginning 0))
      (end-of-line)
      (skip-chars-backward " \t")
      (if (> (current-column) kickasm-mnemonic-comment-indent)
	  (current-column)
	kickasm-mnemonic-comment-indent))))

(defun kickasm--get-newline-indentation (ind)
  "Return the column where new code probably should be indented to.
IND is the indentation column to use for aligning it with code."
  (save-excursion
    (forward-comment (- (point-max)))
    (let ((pretype (kickasm--get-line-type)))
      (cond ((eq pretype 'mnemonic)
	     (current-indentation))
	    ((eq pretype 'label)
	     (beginning-of-line)
	     (skip-chars-forward "^:")
	     (forward-char)
	     (skip-chars-forward " \t")
	     (if (= (point) (line-end-position))
		 ;; We are on a line after a label only line. Lets recurse
		 ;; backwards until we find a line which is not a label only
		 ;; line
		 (progn (beginning-of-line)
			(if (bobp) ind (kickasm--get-newline-indentation ind)))
	       (current-column)))
	    (t ind)))))

(defun kickasm--get-preprocessor-indentation ()
  "Return the indentation column fo a preprocessor lines."
  (save-excursion
    (cond ((not kickasm-preprocessor-indentation-mode)
	   (current-indentation))
	  ((equal kickasm-preprocessor-indentation-mode 'left)
	   0)
	  ((equal kickasm-preprocessor-indentation-mode 'normal)
	   (back-to-indentation)
	   (let ((decrind (if (looking-at (kickasm--opt '("#else" "#elif" "#endif") 'word))
			      (- kickasm-preprocessor-indent)
			    0)))
	     (if (re-search-backward kickasm--preprocessor-regexp nil t)
		 (progn (goto-char (match-beginning 1))
			(if (looking-at (kickasm--opt '("#if" "#else" "#elif") t))
			    (+ (current-indentation) kickasm-preprocessor-indent decrind)
			  (max 0 (+ (current-indentation) decrind))))
	       0)))
	  (t (current-indentation)))))

(defun kickasm--move-to-next-column (col ind &optional force)
  "Move point to the next tab stop.
COL is the column that point was located in.
IND is the indentation column to use for aligning it with code.
If FORCE is true then tabs and spaces will be inserted if needed."
  (let ((tablist (sort `(0 ,ind
			   ,(kickasm--get-comment-indentation)
			   ,(kickasm-column-at-pos (line-end-position))) '<)))
    (unless (or (not kickasm-mnemonic-indentation-mode)
		(and (equal kickasm-mnemonic-indentation-mode 'min)
		     (> ind kickasm-mnemonic-indent)))
      (setq tablist (sort (cons kickasm-mnemonic-indent tablist) '<)))

    (while (and tablist (>= col (car tablist)))
      (setq tablist (cdr tablist)))

    (if (or (eq last-command-event ?\t)
	    (/= (current-indentation) col))
	(move-to-column (if tablist (car tablist) 0) force)
      (move-to-column col force))))

(defun kickasm-indent-line ()
  "Indent current line for `kickasm-mode'."
  (interactive)
  (if (not kickasm-use-automatic-indentation)
      (insert ?\t)
    (let ((savedpos (point))
	  (savedcol (current-column))
	  (indcol (current-indentation))
	  (linetype (kickasm--get-line-type))
	  (syntax (syntax-ppss (line-beginning-position)))
	  (newindcol (kickasm--get-command-indentation)))
      (cond ((not newindcol))
	    ((and (nth 4 syntax)
		  (/= (line-number-at-pos (nth 8 syntax)) (line-number-at-pos)))
	     ;; Inside multiline comment
	     (let ((comcol (kickasm-column-at-pos (nth 8 syntax))))
	       (indent-line-to
		(+ comcol
		   ;; If line start with a star then align with star at comment start,
		   ;; otherwise assume text start after '/* '
		   (if (eq linetype 'maybe-block-comment) 1 3)))))
	    ((eq linetype 'else)
	     ;; else at a start of a line should align with the matching .if
	     (indent-line-to (kickasm--get-else-indentation))
	     (back-to-indentation))
	    ((eq linetype 'label)
	     (save-excursion (indent-line-to (kickasm--get-label-indentation)))
	     (let* ((mnemind (kickasm--get-mnemonic-indentation newindcol))
		    (labelend (kickasm--get-label-end-pos))
		    (labelcol (kickasm-column-at-pos labelend)))
	       (goto-char labelend)
	       (let ((type (kickasm--get-type-at-position))
		     (comcol (save-excursion (skip-chars-forward " \t") (current-column))))
		 ;;(comcol (kickasm-column-at-pos (match-beginning 1))))
		 (cond ((eq type 'comment)
			;; There is a comment following the label
			(kickasm--indent-comment savedcol newindcol)
			(if (= savedcol comcol)
			    (move-to-column (kickasm--get-comment-indentation))
			  (kickasm--move-to-next-column savedcol newindcol)))
		       ((and (>= labelcol mnemind)
			     kickasm-split-large-label-lines
			     (not (looking-at "\\s-*$")))
			;; Label is too large, if there is any code after the label then we
			;; should split the line
			(newline)
			(kickasm-indent-line))
		       ((or (eq type 'command)
			    (eq type 'open-paren))
			(let ((labelcolp1 (1+ labelcol)))
			  ;; There is a command following the label
			  (if (and (= savedcol comcol)
				   (or (> newindcol labelcolp1)
				       (/= comcol labelcolp1)))
			      ;; Cursor is located at start of a command. We should now toggle
			      ;; command indentation between newindcol and label-end
			      (progn 
				(delete-horizontal-space)
				(move-to-column (indent-to
						 (if (= savedcol labelcolp1)
						     newindcol
						   labelcolp1))))
			    (move-to-column (if (and (= savedcol labelcolp1)
						     (< labelcolp1 newindcol))
						newindcol
					      (if (= comcol labelcol) labelcol labelcolp1))
					    t))))
		       ((eq type 'mnemonic)
			(skip-chars-forward " \t")
			(unless (or (= (point) (line-end-position))
				    (= (kickasm-column-at-pos) mnemind))
			  ;; Mnemonic is not indented correctly
			  (save-excursion
			    (delete-horizontal-space)
			    (indent-to mnemind)))
			  
			(kickasm--indent-comment savedcol newindcol)
			(kickasm--move-to-next-column
			 (if (eq last-command-event ?:)
			     labelcol
			   savedcol) newindcol t))))))

	    ((eq linetype 'comment)
	     (kickasm--indent-comment savedcol newindcol)
	     (if (= indcol savedcol)
		 (back-to-indentation)
	       (kickasm--move-to-next-column savedcol (current-indentation))))
	    ((eq linetype 'mnemonic)
	     (let ((mnemind (kickasm--get-mnemonic-indentation newindcol)))
	       (indent-line-to mnemind)
	       (kickasm--indent-comment savedcol newindcol)
	       (if (= indcol (current-indentation))
		   (kickasm--move-to-next-column savedcol newindcol t)
		 ;; Calculate next position from new indentation
		 (kickasm--move-to-next-column (current-column) newindcol t))))
	    ((eq linetype 'emptyline)
	     (kickasm--move-to-next-column savedcol newindcol t))
	    ((eq linetype 'newline)
	     (move-to-column (kickasm--get-newline-indentation newindcol) t))
	    ((eq linetype 'preprocessor)
	     (indent-line-to (kickasm--get-preprocessor-indentation))
	     (back-to-indentation))
	    ((or (eq linetype 'close-paren)
		 (eq linetype 'open-paren))
	     (indent-line-to newindcol)
	     (kickasm--indent-comment savedcol newindcol)

	     (if (= (current-indentation) indcol)
		 (kickasm--move-to-next-column savedcol (current-indentation) t)
	       (back-to-indentation)))
	    (t
	     ;; If code is located at kickasm-mnemonic-indent (and style is not 'depth)
	     ;; Then indentation should change if point is located at the beginning of
	     ;; the code, i.e make it toggle between newindcol and mnem.
	     (let ((mnemind (kickasm--get-mnemonic-indentation newindcol)))
	       (cond ((= mnemind newindcol)
		      (indent-line-to newindcol))
		     ((= savedcol indcol mnemind)
		      (indent-line-to newindcol))
		     ((= savedcol indcol newindcol)
		      (indent-line-to mnemind))
		     ((not (or (= indcol mnemind) (= indcol newindcol)))
		      (indent-line-to newindcol)))
	       (kickasm--indent-comment savedcol newindcol)

	       (if (and (= indcol savedcol)
			(/= (current-indentation) indcol))
		   (back-to-indentation)
		 (kickasm--move-to-next-column savedcol (current-indentation) t))))))))

;;;###autoload
(define-derived-mode
  kickasm-mode
  prog-mode
  "Kick Assembler"
  "Mode for editing Kick Assembler 4.x 6502 cross assembler source."
  (setq-local tab-width kickasm-tab-width)

  (define-key kickasm-mode-map "}" 'kickasm-electric-brace)
  (define-key kickasm-mode-map ":" 'kickasm-electric-colon)
  (define-key kickasm-mode-map [backtab] 'kickasm-insert-tab)
  (define-key kickasm-mode-map "\C-c\C-f" 'kickasm-find-definition)
  (define-key kickasm-mode-map "\C-c\C-b" 'kickasm-return-from-definition)
  (define-key kickasm-mode-map "\C-c\C-c" 'kickasm-assemble)
  (define-key kickasm-mode-map "\C-c\C-s" 'kickasm-assemble-bytedump)
  (define-key kickasm-mode-map "\C-c\C-a" 'kickasm-assemble-asminfo)
  (define-key kickasm-mode-map "\C-c\C-v" 'kickasm-run-vice)
  (define-key kickasm-mode-map "\C-c\C-d" 'kickasm-run-c64debugger)
  (define-key kickasm-mode-map [menu-bar kickasm] (cons "Kick Assembler" (make-sparse-keymap)))
  (define-key kickasm-mode-map [menu-bar kickasm bytedump]
    '(menu-item "Assemble Buffer with bytedump" kickasm-assemble-bytedump
		:help "Assemble the buffer and show resulting code with source"))
  (define-key kickasm-mode-map [menu-bar kickasm assemble]
    '(menu-item "Assemble Buffer" kickasm-assemble
		:help "Assemble the buffer using Kick Assembler"))
  (define-key kickasm-mode-map [menu-bar kickasm vice]
    '(menu-item "Run Vice" kickasm-run-vice
		:help "Run VICE to test the assembled code"))
  (define-key kickasm-mode-map [menu-bar kickasm c64dbg]
    '(menu-item "Run C64 Debugger" kickasm-run-c64debugger
		:help "Run C64 Debugger to test the assembled code"))
  (define-key kickasm-mode-map [menu-bar kickasm finddef]
    '(menu-item "Find name definition" kickasm-find-definition
		:help "Find the definition of the name at point"))
  (define-key kickasm-mode-map [menu-bar kickasm retdef]
    '(menu-item "Return from definition" kickasm-return-from-definition
		:help "Return from the name definition"))

  (modify-syntax-entry ?/ ". 124" kickasm-mode-syntax-table)
  (modify-syntax-entry ?* ". 23b" kickasm-mode-syntax-table)
  (modify-syntax-entry ?\n ">" kickasm-mode-syntax-table)
  (modify-syntax-entry ?\^m ">" kickasm-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" kickasm-mode-syntax-table)
  (modify-syntax-entry ?. "w" kickasm-mode-syntax-table)
  (modify-syntax-entry ?@ "w" kickasm-mode-syntax-table)
  (modify-syntax-entry ?! "w" kickasm-mode-syntax-table)
  (modify-syntax-entry ?$ "_" kickasm-mode-syntax-table)
  (modify-syntax-entry ?% "_" kickasm-mode-syntax-table)
  (modify-syntax-entry ?_ "w" kickasm-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" kickasm-mode-syntax-table)

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-column kickasm-mnemonic-comment-indent)
  (setq-local comment-fill-column 100)
  (setq-local parse-sexp-lookup-properties t)

  (setq-local indent-line-function 'kickasm-indent-line)
  
  (setq-local kickasm--return-position-stack (kickasm--make-position-stack))

  (setq-local font-lock-defaults
	      '(kickasm-font-lock-keywords
		nil
		nil
		nil
		nil
		(font-lock-extra-managed-props . (syntax-table help-echo mouse-face))
		;; This function makes sure that comments and strings uses disabled face
		;; when located in a disabled code region.
		(font-lock-syntactic-face-function . kickasm-syntactic-face-function)))

  ;; Kickassembler reports number of characters for errors and not screen columns
  (setq-local compilation-error-screen-columns nil)
  
  (setq-local compilation-buffer-name-function 'kickasm-compilation-buffer-name)
  (add-hook 'compilation-mode-hook 'kickasm-compilation-setup-function)
  
  (setq imenu-generic-expression
	'(("macro" "\\b\\.macro[[:space:]]+\\(@?[a-zA-Z0-9_]+\\)\\b" 1)
	  
	  ;; labels
	  ("label" "\\b@?\\([a-zA-Z0-9_]+\\):" 1)
	  ("label" "\\b\\.label[[:space:]]+\\([a-zA-Z0-9_]+\\)\\b" 1)
	  
	  ("function" "\\b\\.function[[:space:]]+\\(@?[a-zA-Z0-9_]+\\)\\b" 1)
	  ("pseudocommand" "\\b\\.pseudocommand[[:space:]]+\\(@?[a-zA-Z0-9_]+\\)\\b" 1)
	  ("constant" "\\b\\.const[[:space:]]+\\(@?[a-zA-Z0-9_]+\\)\\b" 1)
	  ("struct" "\\b\\.struct[[:space:]]+\\(@?[a-zA-Z0-9_]+\\)\\b" 1)))
  (imenu-add-menubar-index)
  (setq-local imenu-sort-function 'imenu--sort-by-name)
  (kickasm-assemble-asminfo)
  )


(provide 'kickasm-mode)

;;; kickasm-mode.el ends here
