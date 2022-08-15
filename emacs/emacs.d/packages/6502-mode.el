;;; 6502-mode
;;
;; (Originally dasm-mode,
;; http://www.cling.gu.se/~cl3polof/dasm-mode.el -- now has font lock,
;; TAB a la python-mode, imenu, error regexp, syntax table.)
;;
;; Copyright 2002 Per Olofsson
;;
;; Copyright 2008-10 Tom Seddon
;;                   6502 minus mode snail tomseddon dot plus dot com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; . TAB cycles valid indentations, like python-mode. It knows about
;; comments, subroutine, and local names.
;;
;; . font lock
;;
;; . useful imenu bits
;;
;; . compilation error regexp for M-x compile
;;
;; . syntax table
;;
;; There are some configuration settings, but I haven't really tested
;; them very hard...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; . use `6502-line-type' more
;;
;; . `comment-column' should have a defcustom for the initial value
;;
;; . every now and again the indentation is wrong, but only when I'm
;; in the middle of something else. This hasn't been looked at very
;; hard.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; original dasm-mode comment header:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Works fine with 6502-style 6502 source code
; Requires Emacs 20.x or higher
;
; Questions or suggestions to MagerValp(at)cling.gu.se
;
; Copyright 2002 Per Olofsson
; Released under the Gnu General Public License, GPL
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup _6502 nil
  "Major mode for editing 6502 code"
  :prefix "6502-"
  :group 'languages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom 6502-instruction-indent 16
  "column for instruction"
  :group '_6502)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar 6502-font-lock-keywords
  '(
    ("[^']\\(;.*\\)$" . font-lock-comment-face)
    ("^\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\b" . font-lock-constant-face)
    ("^\\(\\.[a-zA-Z0-9_]+\\)\\b" . font-lock-function-name-face)
    ("[[:space:]]+\\(d[csv]\\.[bwl]?\\|hex\\|eqm\\|equ\\|set\\)\\b" . font-lock-type-face)
    ("[[:space:]]+\\(include\\|incbin\\|incdir\\|ifconst\\|ifnconst\\|if\\|else\\|endif\\|eif\\|subroutine\\|repeat\\|repend\\|mac\\|endm\\)\\b" . font-lock-builtin-face)

    ;; warning-face
    ("[[:space:]]+\\(seg\\.u\\|seg\\|org\\|rorg\\|rend\\|align\\)\\b" . font-lock-warning-face)
    ("[[:space:]]+\\(processor\\|err\\|echo\\|list\\)\\b" . font-lock-warning-face)

    ;; keyword-face
    ("[ \n]+\\(adc\\|and\\|asl\\|bcc\\|bcs\\|beq\\|bit\\|bmi\\|bne\\|bpl\\|brk\\|bvc\\|bvs\\|clc\\|cld\\|cli\\|clv\\|cmp\\|cpx\\|cpy\\|dec\\|dex\\|dey\\|eor\\|inc\\|inx\\|iny\\|jmp\\|jsr\\|lda\\|ldx\\|ldy\\|lsr\\|nop\\|ora\\|pha\\|php\\|pla\\|plp\\|rol\\|ror\\|rti\\|rts\\|sbc\\|sec\\|sed\\|sei\\|sta\\|stx\\|sty\\|tax\\|tay\\|tsx\\|txa\\|txs\\|tya\\)\\b" . font-lock-keyword-face)
    )
  "Expressions to highlight in 6502-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 6502-looking-at-label ()
  (or (looking-at "[[:word:]_]+[[:space:]]+subroutine")
      (looking-at "\\.[[:word:]_]+")))

(defun 6502-looking-at-comment ()
  (looking-at (regexp-quote comment-start)))

(defun 6502-line-type ()
  (save-excursion
    (back-to-indentation)
    (cond
     ((6502-looking-at-label)
      'label)
     ((6502-looking-at-comment)
      'comment)
     ((looking-at "$")
      'empty)
     (t
      nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 6502-indent-line ()
  (interactive)

  (let ((old-column (current-column))
	last-line-indent
	last-line-type)
    
    ;; get type and indentation for line 1 above, or (if that line is
    ;; empty), the line 2 above (allowing easy insertion of a 1 line
    ;; gap).
    (save-excursion
      (forward-line -1)
      (back-to-indentation)
      (if (eolp)
	  (progn
	    (forward-line -1)
	    (back-to-indentation)))

      (setq last-line-indent (current-column))
      (setq last-line-type (6502-line-type)))

    ;; determine new indentation for this line.
    (let (old-indent
	  new-indent)
      (save-excursion
	(back-to-indentation)
	(setq old-indent (current-column))
	;; TODO should really use `case' and `6502-line-type' but I
	;; have actual coding to do :)
	(setq new-indent (cond
			  ;; handle label - always at column 0.
			  ((6502-looking-at-label)
			   0)
			  
			  ;; handle comment - any valid column.
			  ((6502-looking-at-comment)
			   (cond
			    ((>= old-indent comment-column)
			     0)
			    ((>= old-indent 6502-instruction-indent)
			     comment-column)
			    (t
			     6502-instruction-indent)))
			  
			  ;; handle empty line - takes same indent as
			  ;; line "above", unless that was a label, in
			  ;; which case assume instruction indent.
			  ;;
			  ;; TODO (looking-at "[[:space:]]*$"),
			  ;; perhaps?
			  ((looking-at "$")
			   (if (equal last-line-type
				      'label)
			       6502-instruction-indent
			     last-line-indent))
			  
			  ;; handle anything else - column 0, or
			  ;; instruction column.
			  (t
			   (cond
			    ((>= old-indent 6502-instruction-indent)
			     0)
			    (t
			     6502-instruction-indent))))))

      ;; adjust indentation, trying to keep point in the same place in
      ;; the text.
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to new-indent)
      (let ((new-column (+ old-column
			   (- new-indent old-indent))))
	(if (>= new-column 0)
	    (move-to-column new-column))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 6502-subroutine ()
  (interactive)
  (end-of-line)
  (just-one-space)
  (insert "subroutine")
  (6502-indent-line)
  (newline-and-indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode
  6502-mode
  fundamental-mode
  "6502"
  "Mode for editing 6502 cross assembler source."
  (interactive)

  ;;
  ;; default keys
  ;;
  (define-key 6502-mode-map
    (kbd "C-c C-s")
    '6502-subroutine)

  ;;
  ;; Indentation
  ;;

  (setq indent-tabs-mode
	nil)
  
  (set (make-local-variable 'tab-stop-list)
       (list 0
	     6502-instruction-indent))

;;   (define-key 6502-mode-map
;;     (kbd "TAB")
;;     'tab-to-tab-stop)

  (set (make-local-variable 'indent-line-function)
       '6502-indent-line)

  ;;
  ;; Comments
  ;;
  (setq comment-column
	32)

  (make-local-variable 'comment-start)
  (setq comment-start
	";")

  ;; from `asm-mode'
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(?:\\s<+\\|/[/*]+\\)[ \t]*")

  ;; from `asm-mode'
  (make-local-variable 'comment-end-skip)
  (setq comment-end-skip "[ \t]*\\(\\s>\\|\\*+/\\)")


  ;;
  ;; Font lock
  ;;
  (make-local-variable 'font-lock-defaults)
  
  (setq font-lock-defaults '(6502-font-lock-keywords
			     nil
			     t))

  ;;
  ;; syntax table
  ;;

  ;; ; (not ?;, obviously!) starts a comment
  (modify-syntax-entry 59 "<" 6502-mode-syntax-table)

  ;; newline ends comments
  (modify-syntax-entry ?\n ">" 6502-mode-syntax-table)

  ;; various characters are punctuation
  (loop for c in `(?~ ?* ?/ ?% ?+ ?- ?< ?> ?= ?! ?& ?^ ?| ?? ?,) do
	(modify-syntax-entry c "." 6502-mode-syntax-table))

  ;; . and $ are symbol constituent chars
  (modify-syntax-entry ?. "_" 6502-mode-syntax-table)
  (modify-syntax-entry ?$ "_" 6502-mode-syntax-table)

  ;; ' is character quote
  (modify-syntax-entry ?' "/" 6502-mode-syntax-table)

  ;;
  ;; compilation
  ;;
  
  ;; suitable for DASM
  (setq compilation-error-regexp-alist
	'(("\\(.*\\) (\\([0-9]+\\))\\: .*$"
	   1
	   2)))

  ;;
  ;; imenu
  ;; 
  (setq imenu-generic-expression
	'(("Macro"
	   "mac \\([A-Za-z0-9_]+\\)"
	   1)

	  ;; non-local labels
	  ("Label" "^\\([A-Za-z0-9_]+\\)"
	   1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide '6502-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
