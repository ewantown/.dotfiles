;; -*- lexical-binding: t -*-
;; Utility functions and macros
;==============================================================================
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(require 'pcase)
;; Examples patterns are syntactic, but also predicates, funcs,...

(defmacro match (EXP &rest CASES)
  "Racket-style alias for pcase"
  `(pcase ,EXP ,@CASES))
;; E.g.:
;; (match '(2 3)
;;        (`(,a ,b ,c) (+ a b c))
;;        (`(,a ,b) (- a b))     ;=> -1
;;        (_ "no match"))

(defmacro match-let (BINDINGS &rest BODY)
  "Racket-style alias for pcase-let"
  `(pcase-let ,BINDINGS ,@BODY))
;; E.g.: 
;; (match-let ((`(,a ,b ,c) '(1 2 3))
;; 	    (`(,d ,e) '(4 5)))
;; 	   '(e d c b a))         ;=> '(5 4 3 2 1)

(defmacro match-let* (BINDINGS &rest BODY)
  "Racket-style alias for pcase-let*"
  `(pcase-let* ,BINDINGS ,@BODY))
;; E.g.:
;; (match-let* ((`(,a b) '((1 2) 3))
;; 	     (`(,c d) a))
;; 	    c)                   ;=> 1

;; Schemey... and with lexical binding this is better
;; but for the sake of others, I've made it visual only
;; (defmacro : (OPERATOR &rest OPERANDS)
;;   "Alias for funcall"
;;   `(funcall ,OPERATOR ,@OPERANDS))

;==============================================================================
(provide 'utils)
