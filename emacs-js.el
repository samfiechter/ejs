;;; emacs-js.el --- Emacs Javascript Interpreter
;; Copyright (C) 2015 -- Use at 'yer own risk  -- NO WARRANTY! 
;; Author: sam fiechter sam.fiechter(at)gmail
;; Version: 0.01
;; Created: 2015-01-07
;; Keywords: javascript

;;;Code
;;// http://cdn.oreilly.com/excerpts/9780596517748/web/jsgp_ad21.png
;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar emacs-js-whitespace " \t\n")
(defvar emacs-js-operators "+-/*^%&|[]():\\")
				    
(defun ejs-eval (jstext) "evaluate javascript text"
    (let* (
	   
	   )
      )
  )

(defun emacs-js-tokenize (jstext) "Convert arg to a list of tokens"
       (let* (
	      (toksep (concat emacs-js-whitespace emacs-js-operators "()[]{}"))
	      (ccstart 0)
	      (cend 0)
	      (tokens (list))
	      )

	 ))

(defun emacs-js-evalt
    )


