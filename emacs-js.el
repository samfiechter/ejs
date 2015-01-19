;;; emacs-js.el --- Emacs Javascript Interpreter (or prehap translator)
;; Copyright (C) 2015 -- Use at 'yer own risk  -- NO WARRANTY!
;; Author: sam fiechter sam.fiechter(at)gmail
;; Version: 0.01
;; Created: 2015-01-07
;; Keywords: javascript

;; So this is an attempt to translate JS to emacs lisp...
;; possible uses are in eww type browsers, or for doing config for people that don't want to learn emacs's lisp
;;


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
                                 ;;                            emacs-js-string
                                 ;;                            emacs-js-prototype
                                 ;;                            emacs-js-math
                                 ;;                            emacs-js-function
                                 ))
                             (lambda (l) l))) ;; pass through

(defvar emacs-js-operator-more (cons '( (list emacs-js-operator-calc emacs-js-expr)) (lambda (l) l)))

(defvar emacs-js-operator-calc (cons '( emacs-js-expr (list "+" "-" "/" "*" "||" "&&" "|" "&" ) emacs-js-operator-more) (lambda (l) l)))
(defvar emacs-js-operator (cons '( emacs-js-operator-calc)
				(lambda (l)
				  ;; l should be of format expr op expr op expr...
				  (let ((op-order (cons "*" (quote *))
						  (cons "/" (quote /))
						  (cons "+" (quote +))
						  (cons "-" (quote -))
						  (cons "|" '( logior))
						  
						  ))
				    (dolist (i op-order)
				      
				    ))) ))
								       
(defvar emacs-js-array-more (cons '( "," emacs-js-expr) (lambda (l) (elt l 1))))
(defvar emacs-js-array (cons '( "\[" ( emacs-js-expr (list "\]" emacs-js-array-more )))
                             (lambda (l) (let ((i 0) (n (- (length l) 1)) (a (array))) (while (i < n ) (apush a (elt l i)) (setq i (+ 2 i))) a))))
(defvar emacs-js-obj-expr (cons '( emacs-js-name ":" emacs-js-expr)
                                (lambda (l) (list (elt l 0) (elt l 2)))))
(defvar emacs-js-obj-more  (cons '( "," emacs-js-obj-expr) (lambda (l) (list (elt l 1)))))
(defvar emacs-js-obj (cons '( "\{" ( emacs-js-obj-expr  (list "\}" emacs-js-obj-more)))
                           (lambda (l) (let ((i 0) (n (- (length l) 1)) (h (makehash))) (while (i < n) (puthash (elt l i) (elt l (+ 1 i)) h) (setq i (+ 2 i))) h))))
(defvar emacs-js-numeric (cons "\\([-+]?[:space:]*[0-9]+\\(\.[0-9]+\\)?\\([eE][+-]?[0-9]+\\)\\|0x[:xdigit:]+\\)?"
                               (lambda (l) (string-to-number (l)))))
(defvar emacs-js-name-more (cons '( "," emacs-js-name) (lambda (l) (elt l 1)))
(defvar emacs-js-name (cons "[a-zA-Z\$_][^\s]*" (lambda (l) l)))

(defvar emacs-js-defvar (cons '( "var" emacs-js-name "=" emacs-js-expr ";")
			      (lambda (l) (let ((h (elt emacs-js-symbols emacs-js-stack-level)))
											  (if (hashp h) (puthash (elt l 1) (elt l 3) h)
											    (progn
											      (setq h (makehash))
											      (puthash (elt l 1) (elt l 3) h)
k											      (aset  emacs-js-symbols emacs-js-stack-level h)
											      ))))))
(defvar emacs-js-function (cons '( "function" emacs-js-name "(" emacs-js-name (list ")" emacs-js-name-more) "{" emacs-js-statements "}")
				(lambda (l) )))
				

(defvar emacs-js-statement-expr (cons '( emacs-js-expr ";") (lambda (l) )))

(defvar emacs-js-statements (cons '( (list
                                      emacs-js-defvar
                                      ))
                                  (lambda (l) l )))

(defvar emacs-js-stack-level 0)
(defvar emacs-js-symbols (list ))
(defvar emacs-js-exec (list )) 

(defun emacs-js-test (jstext var)
  (let* ((s "")
         (regexes (car var))
         (testFunc (cdr var))
         (m (list))
         (lm 0)
         (n 0)
         (a (array))
         (j 0)
         (i 0)
         )
    (while (and (n < (length regexes)) lm)
      ;;(elt regexes i) can be:
      ;;        a string -- simple test
      ;;        a symbol -- recusive other test
      ;;        a list -- or any option
      (setq s (replace-regexp-in-string "^[:space:]+" "" jstext )) ;; kill starting whitspace
      (setq s (replace-regexp-in-string "\/\/.*?\n" "" jstext )) ;; kill comments
      (setq s (replace-regexp-in-string "\/\*.*?\*\/" "" jstext )) ;; kill comments
      (if (stringp (elt regexes i))
          (progn
            (setq lm  (string-match (elt regexes i) s))
            (if (= 0 lm)
                (progn
                  (setq j (match-string 1 s))
                  (aset a i j)
                  (setq s (substring s (length j)))
                  )
              (setq lm nil)))
        (if (listp (elt regexes i))
            (let ((k 0) (orlist (elt regexes i)))
              (while (and (k < (length orlist) (not (= 0 lm))))
                (setq lm  (string-match (elt regexes i) s))
                (if (= 0 lm)  ;; match
                    (progn
                      (setq j (match-string 1 s))
                      (aset a i j)
                      (setq s (substring s (length j)))
                      ) nil)
                )
              (setq lm (= 0 lm))
              ))
        (if (symbolp (elt regexes i))
            (let ((symbol-test (emacs-js-test-eval s (symbol-value symbolp))))
              (if (symbol-test)
                  (progn
                    (aset a i (car symbol-test))
                    (setq s (cdr symbol-test))
                    ) (setq lm nil)) )
          (message "ERROR : DEF NOT SYMBOL, LIST, OR STRING"))) )
    (inc i)
    (cons (funcall testfunc a) s) ;; return the element and string
    ))


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
