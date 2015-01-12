;;; emacs-js.el --- Emacs Javascript Interpreter
;; Copyright (C) 2015 -- Use at 'yer own risk  -- NO WARRANTY! 
;; Author: sam fiechter sam.fiechter(at)gmail
;; Version: 0.01
;; Created: 2015-01-07
;; Keywords: javascript

;; js language charts
;;// http://cdn.oreilly.com/excerpts/9780596517748/web/jsgp_ad21.png
;;http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf
;;;Code
;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar emacs-js-expr (list '( emacs-js-array
			       emacs-js-obj
			       emacs-js-name
			       emacs-js-numeric
			       emacs-js-string
			       emacs-js-prototype
			       emacs-js-math
			       emacs-js-function)))
(defvar emacs-js-array '( "[" ( emacs-js-expr ( emacs-js-zero-or-more "," emacs-js-expr)) "]"))
(defvar emacs-js-obj-expr '( emacs-js-name ":" emacs-js-expr))
(defvar emacs-js-obj '( "{" ( emacs-js-obj-expr  ( emacs-js-zero-or-more "," emacs-js-obj-expr)) "}"))
(defvar emacs-js-numeric "^\\([-+]?[:space:]*[0-9]+\\(\.[0-9]+\\)?\\([eE][+-]?[0-9]+\\)\\|0x[:xdigit:]+\\)?")
(defvar emacs-js-name "^[A-Za-z\$\_0-9][^[:space:]{}().]*")		


(defun ejs-eval (jstext) "evaluate javascript text"
    (let* (
	   
	   )
      )
  )

(defmacro inc (var)
  (list 'setq var (list '1+ var)))

(defmacro skip-chars-listed (idx txt len skipchars)
  (list 'while (list 'and (list '< idx len) (list 'char-in-sstr (list 'elt txt idx) skipchars)) (list 'inc idx)) )
(defmacro skip-chars-not-listed (idx txt len skipchars)
  (list 'while (list 'and (list '< idx len) (list 'not (list 'char-in-sstr (list 'elt txt idx) skipchars))) (list 'inc idx)) )

(defun emacs-js-tokenize (jstext) "Convert arg to a list of tokens"
       (let* (
	      (toksep (strsort (concat emacs-js-whitespace emacs-js-operators "()[]{}")))
	      (ccstart 0)
	      (cend 0)
	      (tokens (list))
	      (len (length jstext))
	      )
	 (while (and (< ccstart len) (<=cend len))
	   (skip-chars-listed cstart jstext len emacs-js-whitespace)  ;; skip whitespace
	   (setq cend cstart)
	   (skip-chars-not-listed cend jstext len 
	   
	   )
	 ))

(defun emacs-js-evalt
    )

(defun char-in-sstr (c s)
  (let ((i 0)
	(l (length s)))
    (while (and (< i l) (< (elt s i) c))
      (setq i (+ 1 i)) )
    (char-equal (elt s i) c)))

(defun strsort (arr)
  (let ((out "")
	(i 0)
	(l (length arr)))    
    (dotimes (i l)
      (let ((x (elt arr i))
	    (j 0)
	    (ol (length out)))
	(while (and (< j ol) (< (elt out j) x ))
	  (setq j (+ j 1)) )
	
	(setq out  (concat
		    (substring out 0 j)
		    (char-to-string x)
		    (substring out j ol)
		    ))
	))
    out
    ))

    
