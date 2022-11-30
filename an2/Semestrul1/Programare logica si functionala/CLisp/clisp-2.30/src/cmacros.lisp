;;; CLISP Compiler Macros
;;; Sam Steingold 2001-05-09
;;; CLHS 3.2.2.1 http://www.lisp.org/HyperSpec/Body/sec_3-2-2-1.html

(in-package "SYSTEM")

;; a legitimate option is to keep the `compiler-macro' definition of the
;; symbol in a global hash-table instead of the `symbol-plist'.
;; the reason we use plists is that
;; * this performance issue is related only to the compilation speed,
;;   not the execution speed
;; * the plists are actually quite short:
;;   [non-standard functions & macros used in this snippet are in CLOCC:
;;    compose:            <http://clocc.sf.net/clocc/src/port/ext.lisp>
;;    standard-deviation: <http://clocc.sf.net/clocc/src/cllib/math.lisp>
;;    top-bottom-ui:      <http://clocc.sf.net/clocc/src/cllib/sorted.lisp>
;;    CLOCC is available at <http://clocc.sf.net>]
;; (let ((al nil)
;;       (acc (compose length symbol-plist)))
;;   (do-all-symbols (sy) (push sy al))
;;   (delete-duplicates al :test #'eq)
;;   (format t "~&none:~10t ~5:d~%" (count-if #'zerop al :key acc))
;;   (multiple-value-bind (de me le) (standard-deviation al :key acc)
;;     (format t "std dev:~10t ~5f~%mean:~10t ~5f~%length:~10t ~5:d~%"
;;             de me le))
;;   (top-bottom-ui al 5 nil nil :key acc))
;; none:      4,206
;; std dev:   1.874
;; mean:      .6492
;; length:    5,089
;; Top/Bottom: list: 5,089 records.
;; Top (5):
;;   1: hostent-addrtype    ==> 10
;;   2: hostent-aliases     ==> 10
;;   3: hostent-addr-list   ==> 10
;;   4: hostent-name        ==> 10
;;   5: dir-key-info-type   ==> 10
;; also, compiler macros are probably not used often anyway.
;; At any rate, if someone will want to switch to a global hash-table,
;; one needs to change only the following two functions:
;;    compiler-macro-function and
;;    (setf compiler-macro-function)

(defun compiler-macro-function (name &optional environment)
  (declare (ignore environment))
  (cond ((symbolp name) (get name 'compiler-macro))
        ((function-name-p name) ; (setf name)
         (get (second name) 'compiler-macro-setf))
        (t (error-function-name 'compiler-macro-function name))))

(defun (setf compiler-macro-function) (newf name &optional environment)
  (declare (ignore environment))
  (cond ((symbolp name) (setf (get name 'compiler-macro) newf))
        ((function-name-p name) ; (setf name)
         (setf (get (second name) 'compiler-macro-setf) newf))
        (t (error-function-name '(setf compiler-macro-function) name))))

;; (proclaim '(inline function-form-funform simple-function-form-p))

;; check whether the form is (FUNCTION fun-form) and return the fun-form
(defun function-form-funform (form)
  (and (consp form) (eq (car form) 'FUNCTION)
       (consp (cdr form)) (null (cddr form))
       (second form)))

;; check whether the form is #'symbol
(defun simple-function-form-p (form)
  (let ((ff (function-form-funform form)))
    (and ff (function-name-p ff))))

;; (funcall (function foo) ...) ==> (foo ...)
(defun strip-funcall-form (form)
  (if (and (eq (car form) 'funcall) (simple-function-form-p (second form)))
      (cons (second (second form)) (cddr form))
      form))

(defmacro define-compiler-macro (&whole form name args &body body)
  (declare (ignore args body))
  (sys::check-redefinition name 'define-compiler-macro "compiler macro")
  (multiple-value-bind (expansion name lambdalist docstring)
      (sys::make-macro-expansion (cdr form) 'strip-funcall-form)
    (declare (ignore lambdalist))
    `(EVAL-WHEN (COMPILE LOAD EVAL)
      ,@(when docstring
         `((SYSTEM::%SET-DOCUMENTATION ',name 'COMPILER-MACRO ,docstring)))
      (setf (compiler-macro-function ',name) ,expansion)
      ',name)))
