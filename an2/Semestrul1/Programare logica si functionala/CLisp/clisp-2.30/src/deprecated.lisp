;; deprecated CLISP functionality
;; present for now, will be removed later
;; Sam Steingold 2001

;; the standard way to deprecate a function is to define a
;; compiler-marco for it which will issue a warning

(in-package "SYSTEM")

(defun deprecate (symbol superseded &optional (def (fdefinition superseded)))
  (export symbol "EXT")
  (sys::%putd symbol def)
  (setf (get symbol 'deprecated) superseded)
  #+compiler
  (pushnew symbol *deprecated-functions-list*))

;; ---------------------------------------------------------
;; `type-expand-1' -- superseded by (type-expand typespec t)

(deprecate 'ext::type-expand-1 'ext::type-expand
           (lambda (typespec) (ext::type-expand typespec t)))
#+compiler
(define-compiler-macro type-expand-1 (typespec)
  (let ((ret `(type-expand ,typespec t)))
    (c-warn "~s is deprecated and will be removed in a future release.
Use ~s instead"
            'type-expand-1 ret)
    ret))

;; ------------------------------------------------
;; http://www.lisp.org/HyperSpec/Issues/iss321.html
;; `special-form-p' -- renamed to `special-operator-p'

(deprecate 'ext::special-form-p 'special-operator-p)

;; ------------------------------------------------
;; http://www.lisp.org/HyperSpec/Issues/iss308.html
;; `get-setf-method-multiple-value' -- renamed to `get-setf-expansion'
;; `define-setf-method' -- renamed to `define-setf-expander'

(deprecate 'ext::get-setf-method-multiple-value 'get-setf-expansion)
(deprecate 'ext::define-setf-method 'define-setf-expander)
