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

(defvar emacs-js-expr (cons  '( (list
				 emacs-js-array
				 emacs-js-obj
				 emacs-js-name
				 emacs-js-numeric
				 ;;			       emacs-js-string
				 ;;			       emacs-js-prototype
				 ;;			       emacs-js-math
				 ;;			       emacs-js-function
				 ))
			     (lambda (l) l)))

(defvar emacs-js-array-more (cons '( "," emacs-js-expr) (lambda (l) (elt l 1)))) 
(defvar emacs-js-array (cons '( "\[" ( emacs-js-expr (list "\]" emacs-js-array-more )))
			     (lambda (l) (let ((i 0) (n (- (length l) 1)) (a (array))) (while (i < n ) (apush a (elt l i)) (setq i (+ 2 i))) a))))

(defvar emacs-js-obj-expr (cons '( emacs-js-name ":" emacs-js-expr)
				(lambda (l) (list (elt l 0) (elt l 2)))))
(defvar emacs-js-obj-more  (cons '( "," emacs-js-obj-expr) (lambda (l) (list (elt l 1)))))

(defvar emacs-js-obj (cons '( "\{" ( emacs-js-obj-expr  (list "\}" emacs-js-obj-more)))
							"," emacs-js-obj-expr)) "}")
			   (lambda (l) (let ((i 0) (n (- (length l) 1)) (h (makehash))) (while (i < n) (puthash (elt l i) (elt l (+ 1 i)) h) (setq i (+ 2 i))) h))))
(defvar emacs-js-numeric (cons "\\([-+]?[:space:]*[0-9]+\\(\.[0-9]+\\)?\\([eE][+-]?[0-9]+\\)\\|0x[:xdigit:]+\\)?"
			       (lambda (l) (string-to-number (l)))))

(defvar emacs-js-name (cons "[a-zA-Z\$_][^\s]*") (lambda (l) l))
(defvar emacs-js-defvar (cons '( "var" emacs-js-name "=" emacs-js-expr ";") (lambda (l) (defvar (elt l 1) (elt l 3))))) 

(defun emacs-js-testp (jstext var)

  )

(defun emacs-js-eval (jstext) "evaluate javascript text"
    (let* (
	   
	   )
      )
  )
(defmacro apush (arr var)
  (aset arr (length arr) var))
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
)))



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

    
