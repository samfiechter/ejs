;;; emacs-js.el --- Emacs Javascript Interpreter (or prehap translator)
;; Copyright (C) 2015 -- Use at 'yer own risk  -- NO WARRANTY!
;; Author: sam fiechter sam.fiechter(at)gmail
;; Version: 0.01
;; Created: 2015-01-07
;; Keywords: javascript

;; So this is an attempt to translate JS to emacs lisp...
;; possible uses are in eww type browsers, or for doing config for people that don't want to learn emacs's lisp

;; js language charts
;;// http://cdn.oreilly.com/excerpts/9780596517748/web/jsgp_ad21.png
;;http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf

;;;Code
;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  ____                  _           _   _____     _     _
;; / ___| _   _ _ __ ___ | |__   ___ | | |_   _|_ _| |__ | | ___
;; \___ \| | | | '_ ` _ \| '_ \ / _ \| |   | |/ _` | '_ \| |/ _ \
;;  ___) | |_| | | | | | | |_) | (_) | |   | | (_| | |_) | |  __/
;; |____/ \__, |_| |_| |_|_.__/ \___/|_|   |_|\__,_|_.__/|_|\___|
;;        |___/


;; the symbol table (tree) is a numbre expressoins that are in the same format:
;;  (cons '( def) (lambda (tokens) (what-to-do-with-the-tokens)))
;; where the '(def) can be one of:
;;                                another symbol def (as defined herein);
;;                                a string with regex to find
;;                                a list of available defs or strings (any of which will work)
;;

(defvar emacs-js-single (cons  '( (
                                   emacs-js-null
                                   emacs-js-boolean
                                   emacs-js-parens
                                   emacs-js-numeric
                                   emacs-js-string
                                   emacs-js-array
                                   emacs-js-obj
                                   emacs-js-symbol
                                   ;;  emacs-js-prototype
                                   ;;  emacs-js-function
                                   ))
                               (lambda (l) (car l))))
(defvar emacs-js-expr (cons (list (list 'emacs-js-operator 'emacs-js-single )) (lambda (l) (car l)  )))
(defvar emacs-js-operator-more (cons '( ( emacs-js-operator-collect emacs-js-single)) (lambda (l) (car l) )))
(defvar emacs-js-operator-collect (cons '( emacs-js-single ( "+" "-" "/" "*" "||" "&&" "|" "&" ) emacs-js-operator-more) (lambda (l) (car l))))
(defvar emacs-js-operator (cons '( emacs-js-operator-collect) (lambda (l) (emacs-js-operator-parse (car l)))))

(defvar emacs-js-fn-arg-more (cons (list "," 'emacs-js-name (list ")"  'emacs-js-fn-arg-more)) (lambda(lama) (cdr (car l))))) ;; cdr to skip comma
(defvar emacs-js-fn-arg-first (cons (list 'emacs-js-name (list ")"  'emacs-js-fn-arg-more)) (lambda (l) (car l))))
(defvar emacs-js-fn-stmt-more (cons '( emacs-js-statement ( emacs-js-fn-stmt-more "}")) (lambda (l) (car l))))
(defvar emacs-js-defun (cons (list "function" "(" (list 'emacs-js-fn-arg-first ")" ) "{" (list "}" 'emacs-js-fn-stmt-more )) ( lambda (l) (emacs-js-defunf (car l)))))

(defvar emacs-js-null (cons '( "null" ) (lambda(l) '(nil))))
(defvar emacs-js-boolean (cons '( "\\(true\\|false\\)") (lambda (l) (if (and (stringp (elt (car l) 0)) (string-equal (elt (car l) 0) "true")) '( t ) '( nil )))))
(defvar emacs-js-string (cons '( "\\(\"[^\"]*\"\\|\'[^\']*'\\)") (lambda (l)(list (substring (elt (car l) 0) 1 -1)))))
(defvar emacs-js-parens (cons '( "(" emacs-js-expr ")" ) (lambda (l) (list (elt (car l) 1)))))
;; array
(defvar emacs-js-array-more (cons (list "," 'emacs-js-expr (list "\\]"  'emacs-js-array-more)) (lambda(l) (cdr (car l)))))
(defvar emacs-js-array-first (cons (list 'emacs-js-expr (list "\\]"  'emacs-js-array-more)) (lambda (l) (car l))))
(defvar emacs-js-array (cons (list "\\[" (list 'emacs-js-array-first  "\\]")) (lambda (l) (emacs-js-make-array (car l)))))

;; Object
(defvar emacs-js-obj-more  (cons (list "," 'emacs-js-obj-expr (list "}" 'emacs-js-obj-more))  (lambda (l) (cdr (car l)))))
(defvar emacs-js-obj-first  (cons (list 'emacs-js-obj-expr (list "}" 'emacs-js-obj-more ) ) (lambda (l) (car l))))
(defvar emacs-js-obj-expr (cons '( emacs-js-name ":" emacs-js-expr) (lambda (l) (list (elt (car l) 0) (elt (car l) 2)))))

(defvar emacs-js-obj (cons (list "{" (list 'emacs-js-obj-first   "}" ) ) (lambda (l) (emacs-js-make-obj (car l)))))

(defvar emacs-js-numeric (cons '( "[+-]?[0-9]+\\(\\.[0-9]+\\)?" )
                               (lambda (l) (string-to-number (elt (car l) 0)))))

(defvar emacs-js-name-regex "[a-zA-Z\$_][^\s:\\*\\/\\+\\-\\|\\&]*")

(defvar emacs-js-name (cons (list emacs-js-name-regex ) (lambda (l) (car l))))
(defvar emacs-js-symbol (cons (list emacs-js-name-regex ) (lambda (l) (list (list 'emacs-js-getvarf (list 'sxhash (elt (car l) 0)))))))  ;; double list commands

(defvar emacs-js-defvar (cons '( "var" emacs-js-name "=" emacs-js-expr )
                              (lambda (l) (emacs-js-defvarf (sxhash (elt (car l) 1)) (elt (car l) 1) (elt (car l) 3)))))

(defvar emacs-js-statement-expr (cons '( emacs-js-expr ";") (lambda (l) (elt (car l) 0) )))

(defvar emacs-js-statement (cons (list (list
                                        'emacs-js-defvar
                                        'emacs-js-expr
                                        'emacs-js-function
                                        ) ";")
                                 (lambda (l) (elt (car l) 0) )))

;;   ____                _              _
;;  / ___|___  _ __  ___| |_ __ _ _ __ | |_ ___
;; | |   / _ \| '_ \/ __| __/ _` | '_ \| __/ __|
;; | |__| (_) | | | \__ \ || (_| | | | | |_\__ \
;;  \____\___/|_| |_|___/\__\__,_|_| |_|\__|___/

(defvar emacs-js-STRING (sxhash "string"))
(defvar emacs-js-NUMERIC (sxhash "numeric"))
(defvar emacs-js-OBJECT (sxhash "object"))
(defvar emacs-js-UNDEFINED (sxhash "undefined"))
(defvar emacs-js-NULL (sxhash "null"))

(defvar emacs-js-THIS (sxhash "this"))
(defvar emacs-js-TOSTRING (sxhash "toString"))
;;  ____  _        _        __     __         _       _     _
;; / ___|| |_ __ _| |_ ___  \ \   / /_ _ _ __(_) __ _| |__ | | ___  ___
;; \___ \| __/ _` | __/ _ \  \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;  ___) | || (_| | ||  __/   \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;; |____/ \__\__,_|\__\___|    \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/


(defvar emacs-js-prototypes (make-hash-table))  ;; a hash of the prototypes
(defvar emacs-js-symbols nil)  ;;a list of hashes (one for each stack level) with vars (sxhash varname) = (varname . value)
(defvar emacs-js-bytecode (list )) ;;a list of lists of code (again one for each stack level)

;;   __                  _   _
;;  / _|_   _ _ __   ___| |_(_) ___  _ __  ___
;; | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; |  _| |_| | | | | (__| |_| | (_) | | | \__ \
;; |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/


(defun emacs-js-test (jstext var start)
  (let* ((s jstext)                                     ;
         (test-patterns (car var))                      ;test pattern
         (testFunc (cdr var))                          ;function to translate tokens into bc
         (strlens (list))                               ;lengths for each token (for error report)
         (tokens (list))                                ;lists of tokens
         (lm 0)                                         ;lost match?
         (i 0)
         (txtl (+ start (length jstext)))
         )
    (while (and (< i (length test-patterns)) lm)

      ;;(elt test-patterns i) can be:
      ;;        a string -- simple test
      ;;        a symbol -- recusive other test
      ;;        a list -- or any option

      (setq s (replace-regexp-in-string "^[\s\t]*" "" s ))                                 ;; kill starting whitspace
      (setq s (replace-regexp-in-string "^\/\/.*?\n" "" s ))  ; kill comments
      (setq s (replace-regexp-in-string "^/\\*[^\\*/]*\\*/" "" s )) ; kill comments
      (if (stringp (elt test-patterns i))
          (progn
            (setq lm  (string-match (elt test-patterns i) s))
            (if (and lm (= 0 lm))
                (let ((token (match-string 0 s)))
                  (setq tokens (append tokens (if (listp token) token (list token))))
                  (setq s (substring s (length token) ))
                  (setq strlens (cons (length s) strlens))
                  )
              (setq lm nil)
              )) ;; if the first character is not a match, barf
        (if (listp (elt test-patterns i))  ;; list is an OR-list
            (let ((k 0)
                  (orlist (elt test-patterns i)))
              (setq lm 1)
              (while (and (< k (length orlist)) (if lm (not (= 0 lm)) 1))
                (if (symbolp (elt orlist k))  ;; list can be symbol or string (two ors are one or)
                    (let* ((symbol-test (emacs-js-test s (symbol-value (elt orlist k)) (- txtl (length s))))
                           (token  (car symbol-test))) 
                      (if symbol-test
                          (progn
                        ;;                          (print (symbol-name (elt orlist k)))
                            (setq tokens (append tokens (if (listp token) token (list token))))
                            (setq s (cdr symbol-test))
                            (setq strlens (cons (length s) strlens))
                            (setq lm 0) )
                        (setq lm nil) ))
                  (progn ;; stpring if not symbol
                    (setq lm  (string-match (elt orlist k) s))
                    (if (and lm (= 0 lm))  ;; match
                        (progn
                          (let ((token (match-string 0 s)))
                            (setq tokens (append tokens (if (listp token) token (list token))))
                            (setq s (substring s (length token) ))
                            (setq strlens (cons (length s) strlens))
                            )) )
                    )) (inc k) )
              (setq lm (and lm (= 0 lm)))
              )
          ;; If its not a list or a string its a symbol
          (if (symbolp (elt test-patterns i))
              (let* ((symbol-test (emacs-js-test s (symbol-value (elt test-patterns i))  (- txtl (length s))))
                     (token (car symbol-test))) 
                (if symbol-test
                    (progn
                      ;;                      (print (symbol-name (elt test-patterns i)))
                      (setq tokens (append tokens (if (listp token) token (list token))))
                      (setq s (cdr symbol-test))
                      (setq strlens (cons (length s) strlens))
                      ) (setq lm nil)) )
            (message "ERROR : DEF NOT SYMBOL, LIST, OR STRING"))))
      (inc i))
    (if lm
        (cons (funcall testFunc (cons tokens (cons start (- txtl (length s))))) s)
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


(defun emacs-js-defunf (toks) ;; function is defined as
  (let ((i 1)
        (j (- (length toks) 2)))
    (while (not (string= ")" (elt toks i))) (inc i))
    (cons (elts 1 (- i 1)) (list (if (> j i) (elts (+ 1 i) j) (list))))
    ))

(defun emacs-js-defvarf (a b c) " emacs-js-defvar HASH SYMBOL VALUE declare a varialbe in the top stack level.

HASH is (sxhash SYMBOL) -- done at parse time to speed execution
SYMBOL is the symbolname
value is the vale
"
  (if (not (elt emacs-js-symbols 0)) (setq emacs-js-symbols (list (make-hash-table)))
    (if (not (hash-table-p (elt emacs-js-symbols 0))) (setq emacs-js-symbols (cons (make-hash-table) (cdr emacs-js-symbols)))))
  (puthash (sxhash a) (cons b c) (elt emacs-js-symbols 0))
  c )

(defun emacs-js-getvarf (a) "emacs-js-getvarf HASH get the value of a symbol.
HASH is (sxhash  SYMBOL)

"
  (let ((ret nil)
        (i  0))
    (while (and (< i (length emacs-js-symbols) (not ret)))
      (setq ret (gethash a (elt emacs-js-symbols i) nil))
      (inc i))
    (if ret (cdr ret) nil) ;; sumbols are stored  (symbolname . value)
    ));; there should be a cant find it error in here somewhere...

(defun emacs-js-make-obj (l)
  (let ((i 1)
        (n (- (length l) 2))
        (h (make-hash-table)))
    (while (< i  n)
      (puthash (sxhash (elt l i)) (cons (elt l i) (elt l (+ 1 i))) h)
      (setq i (+ 2 i)))
    h))


(defun emacs-js-make-array (l)
  (let ((i 1)
        (n (- (length l) 2))
        (a []))
    (if (> n i)
        (setq a (vconcat (elts l 1 n))))
    (list a)))





(defun emacs-js-operator-parse (tokens) "parses a list of tokens and spits out the value or code to calc..."
  (let ((tok tokens)
        (op-order (list (cons "*" '*) (cons "/" '/) (cons "+" '+) (cons "-" '-) (cons "|" 'logior) (cons "&" 'logand)
                        (cons "||" 'or) (cons "&&" 'and) )) )
    ;; 3 "+" 2 "*" 3 "-" 2 "+" 2
    (dolist (i op-order)
      (let ((j 1))
        (while (< j (length tok))
          (if (and (stringp (elt tok j)) (string-equal (car i) (elt tok j)))
              (progn
                (setq tok (append (if (> j 1) (elts tok 0 (- j 2)) nil)
                                  (if (and (numberp (elt tok (- j 1))) (numberp (elt tok (+ j 1))))
                                      (list (eval (list (cdr i) (elt tok (- j 1)) (elt tok (+ j 1)))))
                                    (list (list (cdr i) (elt tok (- j 1)) (elt tok (+ j 1)))))
                                  (if (< (+ j 2) (length tok)) (elts tok (+ 2 j) (- (length tok) 1)) nil)))
                (setq j (- j 2))
                ))
          (inc j)
          )))
    ;;    (dolist (tk tok) (print (if (numberp tk) (number-to-string tk) tk)))
    (if (numberp (elt tok 0)) (elt tok 0) tok)
    ))

(defun emacs-js-primatives () "Create prototypes for js primatives undefined, null, boolean, string, number, object"
  (let* ((tx "numeric")
         (n (make-hash-table))
         (props (list (cons "*" '*) (cons "^" 'expt) (cons "/" '/) (cons "+" '+) (cons "-" '-) (cons "|" 'logior) (cons "&" 'logand)
                      (cons "||" 'or) (cons "&&" 'and) (cons "type" tx)
                      (cons "toString" (list 'number-to-string (list 'emacs-js-getvarf emacs-js-THIS)))
                      )
                )
         )
    (dolist (o props)
      (puthash (sxhash (car o)) o n) )
    (puthash (sxhash tx) (cons tx n) emacs-js-prototypes)
    )
  (let* ((tx "string")
         (n (make-hash-table))
         (props (list (cons "+" 'concat)
                      (cons "substring" 'emacs-js-string-substring)
                      )))
    (dolist (o props)
      (puthash (sxhash (car o)) o n) )
    (puthash (sxhash tx) (cons tx n) emacs-js-prototypes)
    )
  )



(defun emacs-js-string-substring  (l)
  (substring (emacs-js-getvarf (sxhash this) (elt l 0) elt (l 1)))
  )

;;  _   _      _
;; | | | | ___| |_ __   ___ _ __
;; | |_| |/ _ \ | '_ \ / _ \ '__|
;; |  _  |  __/ | |_) |  __/ |
;; |_| |_|\___|_| .__/ \___|_|
;;              |_|
;;  _____                 _   _
;; |  ___|   _ _ __   ___| |_(_) ___  _ __  ___
;; | |_ | | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; |  _|| |_| | | | | (__| |_| | (_) | | | \__ \
;; |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|___/


(defun elts (mylst start end)
  (mapcar (lambda (x) (elt mylst x)) (number-sequence start end)) )

(defmacro inc (var)
  (list 'setq var (list '+ 1 var)) )


(defun fpdiv (a b) (/ (* 1.0 a) b))
