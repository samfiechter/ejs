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

(defvar emacs-js-expr (cons  '( (
				 emacs-js-numeric
				 emacs-js-string
                                 emacs-js-array
                                 emacs-js-obj
                                 emacs-js-name

                                 ;;                            emacs-js-string
                                 ;;                            emacs-js-prototype
                                 ;;                            emacs-js-math
                                 ;;                            emacs-js-function
                                 ))
                             (lambda (l) (elt l 0)))) ;; pass through

(defvar emacs-js-string (cons '( "\\(\"[^\"]*\"\\|\'[^\']*'\\)") (lambda (l)(let* ((s (elt l 0))(ll (length s)) )(substring s 1 (- ll 2))))))

(defvar emacs-js-operator-more (cons '( ( emacs-js-operator-calc emacs-js-expr)) (lambda (l) l)))

(defvar emacs-js-operator-calc (cons '( emacs-js-expr ( "+" "-" "/" "*" "|q|" "&&" "|" "&" ) emacs-js-operator-more) (lambda (l) l)))
;; collect the whole operation
(defvar emacs-js-operator (cons '( emacs-js-operator-calc)
                                (lambda (l)
                                  ;; l should be of format expr op expr op expr...
                                  (let ((op-order (cons "*" '*)
                                                  (cons "/" '/)
                                                  (cons "+" '+)
                                                  (cons "-" '-)
                                                  (cons "|" 'logior)
                                                  (cons "&" 'logand)
                                                  (cons "||" 'or)
                                                  (cons "&&" 'and)
                                                  ))
                                    (dolist (i op-order)

                                      ))) ))

(defvar emacs-js-array-more (cons '( "," emacs-js-expr) (lambda (l) (elt l 1))))

(defvar emacs-js-array (cons '( "\[" ( emacs-js-expr ( "\]" ema-js-array-more )))
                             (lambda (l) (let ((i 0) (n (- (length l) 1)) (a [])) (while (i < n ) (vconcat a (elt l i)) (setq i (+ 2 i))) a))))

(defvar emacs-js-obj-expr (cons '( emacs-js-name ":" emacs-js-expr)
                                (lambda (l) (list (elt l 0) (elt l 2)))))

(defvar emacs-js-obj-more  (cons '( "," emacs-js-obj-expr) (lambda (l) (list (elt l 1)))))

(defvar emacs-js-obj (cons '( "\{" ( emacs-js-obj-expr  ( "\}" emacs-js-obj-more)))
                           (lambda (l) (let ((i 0) (n (- (length l) 1)) (h (make-hash-table))) (while (i < n) (puthash (elt l i) (elt l (+ 1 i)) h) (setq i (+ 2 i))) h))))

(defvar emacs-js-numeric (cons '( "[+-]?[0-9]+\\(\\.[0-9]+\\)?" )
                               (lambda (l) (string-to-number (elt l 0)))))

(defvar emacs-js-name-more (cons '( "," emacs-js-name) (lambda (l) (elt l 1))))

(defvar emacs-js-name (cons (list "[a-zA-Z\$_][^\s]*" ) (lambda (l) (elt l 0))))

(defvar emacs-js-defvar (cons '( "var" emacs-js-name "=" emacs-js-expr ";")
                              (lambda (l)
                                (let ((h (elt emacs-js-symbols 0)))
                                  (if (and (hash-table-p h) (> 0 (length emacs-js-symbols)))
                                      (puthash (elt l 1) (elt l 3) h)
                                    (progn
                                      (setq h (make-hash-table))
                                      (puthash (elt l 1) (elt l 3) h)
                                      (push h  emacs-js-symbols)
                                      ))))))

(defvar emacs-js-function (cons '( "function" emacs-js-name "(" emacs-js-name ( ")" emacs-js-name-more) "{" emacs-js-statements "}")
                                (lambda (l) )))

(defvar emacs-js-statement-expr (cons '( emacs-js-expr ";") (lambda (l) )))

(defvar emacs-js-statements (cons '( (
                                      emacs-js-defvar
                                      ))
                                  (lambda (l) l )))

(defvar emacs-js-prototypes (list ))
(defvar emacs-js-symbols nil)
(defvar emacs-js-bytecode (list ))


(defun emacs-js-test (jstext var)
  (let* ((s jstext)                                     ;
         (test-patterns (car var))                      ;test pattern
         (testFunc (cdr var))                          ;function to translate tokens into bc
         (strlens (list))                               ;lengths for each token (for error report)
         (tokens (list))                                ;lists of tokens
         (lm 0)                                         ;lost match?
         (i 0)
         )
    (while (and (< i (length test-patterns)) lm)

      ;;(elt test-patterns i) can be:
      ;;        a string -- simple test
      ;;        a symbol -- recusive other test
      ;;        a list -- or any option

      (setq s (replace-regexp-in-string "^[\s\t]*" "" s ))                                 ;; kill starting whitspace
      (setq s (replace-regexp-in-string "\/\/.*?\n" "" s ))  ; kill comments
      (setq s (replace-regexp-in-string "\/\*.*?\*\/" "" s )) ; kill comments
      (if (stringp (elt test-patterns i))
          (progn
            (setq lm  (string-match (elt test-patterns i) s))
            (if (= 0 lm)
                (let ((token (match-string 0 s)))
                  (setq tokens (append tokens (list token)))
                  (setq s (substring s (length token)))
		  (setq strlens (append strlens (list (length s))))
		  )
	      (setq lm nil)
	      )) ;; if the first character is not a match, barf
        (if (listp (elt test-patterns i))  ;; list is an OR-list
            (let ((k 0)
                  (orlist (elt test-patterns i)))
	      (setq lm 1)
	      (while (and (< k (length orlist)) (if lm (not (= 0 lm)) 1))
                (if (symbolp (elt orlist k))  ;; list can be symbol or string (two ors are one or)
                    (let ((symbol-test (emacs-js-test s (symbol-value (elt orlist k)))))
                      (if symbol-test
                          (progn
			    (setq tokens (append tokens (list (car symbol-test))))
                            (setq s (cdr symbol-test))
			    (setq strlens (append strlens (list (length s))))
                            (setq lm 0) )                        
			(setq lm nil) ))
                  (progn ;; stpring if not symbol
                    (setq lm  (string-match (elt orlist k) s))
                    (if (= 0 lm)  ;; match
                        (progn
                          (let ((token (match-string 0 s)))
			    (setq tokens (append tokens (list token)))
                            (setq s (substring s (length token)))
			    (setq strlens (append strlens (list (length s))))
                            )) )
		    )) (inc k) )
              (setq lm (= 0 lm))
              ) 
          ;; If its not a list or a string its a symbol
          (if (symbolp (elt test-patterns i))
              (let ((symbol-test (emacs-js-test s (symbol-value (elt test-patterns i)))))
                (if symbol-test
                    (progn
		      (setq tokens (append tokens (list (car symbol-test))))
                      (setq s (cdr symbol-test))
		      (setq strlens (append strlens (list (length s))))
                      ) (setq lm nil)) )
            (message "ERROR : DEF NOT SYMBOL, LIST, OR STRING"))))
      (inc i) )
    (if lm
	(cons (funcall testFunc tokens) s)
      nil) ;; return the element and string
    ))


(defun emacs-js-eval (jstext) "evaluate javascript text"
       (setq emacs-js-prototypes (list ))
       (setq emacs-js-symbols (list ))
       (setq emacs-js-bytecode (list ))
       (let* ((txt-len (length jstext))
              (s jstext)
              )
         )
       )

(defun elts (mylst start end)
  (mapcar (lambda (x) (elt mylst x)) (number-sequence start end)) )

(defmacro inc (var)
  (list 'setq var (list '1+ var)) )

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
