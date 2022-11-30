;; Tracer
;; Bruno Haible 13.2.1990, 15.3.1991, 4.4.1991
;; German comments translated into English: Stefan Kain 2001-12-26
;; Sam Steingold 2001-2002

;; (TRACE) returns a list of traced functions
;; (TRACE fun ...) additionally traces the functions fun, ... .
;; Format for fun:
;;  Either a Symbol
;;   symbol
;;  or a List made of a Symbol and a few Keyword-Arguments (pair-wise!)
;;   (symbol
;;    [:suppress-if form]   ; no Trace-Output, as long as form is true
;;    [:step-if form]       ; Trace moves into the Stepper, if form is true
;;    [:pre form]           ; executes form before function call
;;    [:post form]          ; executes form after  function call
;;    [:pre-break-if form]  ; Trace moves into break-loop before function call,
;;                          ; if form is true
;;    [:post-break-if form] ; Trace moves into break-loop after  function call,
;;                          ; if form is true
;;    [:pre-print form]     ; prints the values of form before function call
;;    [:post-print form]    ; prints the values of form after  function call
;;    [:print form]         ; prints the values of form before
;;                          ; and after the function call
;;   )
;;   In all these forms *TRACE-FUNCTION* (the function itself),
;;   *TRACE-ARGS* (the function arguments),
;;   *TRACE-FORM* (the function-/macro-call as form),
;;   and after function call also *TRACE-VALUES* (the list of values
;;   of the function call) can be accessed,
;;   and the function can be left with RETURN with given values.
;; (UNTRACE) returns list of traced functions, discards the all.
;; (UNTRACE symbol ...) discards symbol, ... from the list of traced
;;   functions.
;; TRACE and UNTRACE are also applicable to functions (SETF symbol) and macros,
;;   not however applicable to locally defined functions and macros.

(in-package "COMMON-LISP")
(export '(trace untrace))
(export '(custom::*trace-indent*) "CUSTOM")
(in-package "EXT")
(export '(*trace-function* *trace-args* *trace-form* *trace-values*
          custom::*trace-indent*))
(in-package "SYSTEM")

(defvar *trace-indent* nil
  "Use indentation in addition to numbering to indicate the trace level.")

(proclaim '(special *trace-function* *trace-args* *trace-form* *trace-values*))
(defvar *traced-functions* nil) ; list of currently traced function-names
;; So long as a function-name funname [resp. more exactly: the Symbol
;; symbol = (get-funname-symbol funname)] are traced, the Property
;; sys::traced-definition contains the old content of the function-cell, the
;; Property sys::tracing-definition contains the new content of the
;; function-cell, and the function-name is element of the list
;; *traced-functions*.
;; Meanwhile the content of the function-cell can change, however!
;; At all events the following is true:
;;  (and (fboundp symbol)
;;       (eq (symbol-function symbol) (get symbol 'sys::tracing-definition)))
;; ===>   (member funname *traced-functions* :test #'equal)
;; <==>   (get symbol 'sys::traced-definition)
(defvar *trace-level* 0) ; nesting depth for Trace-Output

(labels ((subclosure-pos (closure name)
           (do ((length (sys::%record-length closure))
                ;; compiler::symbol-suffix is defined in compiler.lisp
                (nm (compiler::symbol-suffix (closure-name closure) name))
                (pos 2 (1+ pos)) obj)
               ((= pos length)
                (error (TEXT "~s: no local name ~s in ~s")
                       'local name closure))
             (setq obj (sys::%record-ref closure pos))
             (when (and (closurep obj)
                        (or (eq (closure-name obj) nm)
                            (eq (closure-name obj)
                                (concat-pnames "TRACED-" nm))))
               (return pos))))
         (force-cclosure (name)
           (let ((closure (fdefinition name)))
             (unless (closurep closure)
               (error-of-type 'type-error
                 :datum closure :expected-type 'closure
                 (TEXT "~S: ~S must name a closure") 'local name))
             (if (compiled-function-p closure) closure
                 (fdefinition (compile name closure)))))
         (local-helper (spec)
           (do* ((spe (cdr spec) (cdr spe))
                 (clo (force-cclosure (car spec))
                      (sys::%record-ref clo pos))
                 (pos (subclosure-pos clo (car spe))
                      (subclosure-pos clo (car spe))))
                ((endp (cdr spe)) (values clo pos)))))

  (defun %local-get (spec)
    (multiple-value-bind (clo pos) (local-helper spec)
      (sys::%record-ref clo pos)))
  (defun %local-set (new-def spec)
    (unless (closurep new-def)
      (error-of-type 'type-error
        :datum new-def :expected-type 'closure
        (TEXT "~S: ~S must be a closure") `(setf (local ,@spec) ,new-def)))
    (multiple-value-bind (clo pos) (local-helper spec)
      (sys::%record-store clo pos
         (if (compiled-function-p new-def) new-def
             (fdefinition (compile (closure-name (sys::%record-ref clo pos))
                                   new-def)))))))

(defmacro local (&rest spec)
  "Return the closure defined locally with LABELS or FLET.
SPEC is a list of (CLOSURE SUB-CLOSURE SUB-SUB-CLOSURE ...)
CLOSURE must be compiled."
  (%local-get spec))

(define-setf-expander local (&rest spec)
  "Modify the local definition (LABELS or FLET).
This will not work with closures that use lexical variables!"
  (let ((store (gensym "LOCAL-")))
    (values nil nil `(,store) `(%local-set ,store ',spec)
            `(%local-get ,spec))))

;; check whether the object might name a local (LABELS or FLET) function
(defun local-function-name-p (obj)
  (and (consp obj) (eq 'local (car obj))))

(defstruct (tracer (:type vector))
  name symb cur-def local-p
  suppress-if step-if pre post pre-break-if post-break-if
  pre-print post-print print)

;; install the new function definition
(defun tracer-set-fdef (trr new-fdef)
  (if (tracer-local-p trr)
      (%local-set new-fdef (rest (tracer-name trr)))
      (setf (symbol-function (tracer-symb trr)) new-fdef)))

;; Functions, that the Tracer calls at runtime and that the user could
;; trace, must be called in their untraced form.
;; Instead of (fun arg ...) use instead (SYS::%FUNCALL '#,#'fun arg ...)
;; or (SYS::%FUNCALL (LOAD-TIME-VALUE #'fun) arg ...).
;; This applies for all used functions here from #<PACKAGE LISP> except for
;; CAR, CDR, CONS, APPLY, VALUES-LIST (which are all compiled inline).

(defmacro trace (&rest funs)
  (if (null funs)
    '*traced-functions*
    (cons 'append
      (mapcar (lambda (fun)
                (cond ((or (function-name-p fun) (local-function-name-p fun))
                       (let ((trr (make-tracer :name fun)))
                         (check-traceable fun trr 'trace)
                         `(trace1 ,trr)))
                      ((let ((trr (apply #'make-tracer :name fun)))
                         (check-traceable (first fun) trr 'trace)
                         `(trace1 ,trr)))))
              funs))))

(defun error-function-name (caller funname)
  (error-of-type 'source-program-error
    (TEXT "~s: ~s is not a function name")
    caller funname))

;; check whether the FUNNAME can be traced,
;; fill SYMB, CUR-DEF and LOCAL-P slots of TRR and return TRR
(defun check-traceable (funname trr caller)
  (cond ((function-name-p funname)
         (let ((sym (if (atom funname) funname
                        (get-setf-symbol (second funname)))))
           (unless (fboundp sym)
             (error (TEXT "~S: undefined function ~S") caller funname))
           (when (special-operator-p sym)
             (error (TEXT "~S: cannot trace special operator ~S")
                    caller funname))
           (setf (tracer-symb trr) sym
                 (tracer-cur-def trr) (symbol-function sym)
                 (tracer-local-p trr) nil)))
        ((local-function-name-p funname)
         (setf (tracer-cur-def trr) (%local-get (rest funname))
               (tracer-symb trr) (closure-name (tracer-cur-def trr))
               (tracer-local-p trr) t)
         (when (get (tracer-symb trr) 'sys::untraced-name)
           (setf (tracer-symb trr)
                 (get (tracer-symb trr) 'sys::untraced-name))))
        (t (error-function-name caller funname)))
  (check-redefinition funname caller "function")
  trr)

(defun trace1 (trr)
  (let ((macro-flag (macrop (tracer-cur-def trr)))
        (sig (when (tracer-local-p trr)
               (sig-to-list (get-signature (tracer-cur-def trr))))))
    (unless (eq (tracer-cur-def trr) ; already traced?
                (get (tracer-symb trr) 'sys::tracing-definition))
      (setf (get (tracer-symb trr) 'sys::traced-definition)
            (tracer-cur-def trr))
      (pushnew (tracer-name trr) *traced-functions* :test #'equal))
    (format t (TEXT "~&;; Tracing ~:[function~;macro~] ~S.")
            macro-flag (tracer-name trr))
    (setf (get (tracer-symb trr) 'sys::tracing-definition)
          ;; new function, that replaces the original one:
          (let ((newname (concat-pnames "TRACED-" (tracer-symb trr)))
                (body
                 `((declare (inline car cdr cons apply values-list))
                   (let ((*trace-level* (trace-level-inc)))
                     (block nil
                       (unless ,(tracer-suppress-if trr) (trace-pre-output))
                       ,@(when (tracer-pre-print trr)
                           `((trace-print (multiple-value-list
                                           ,(tracer-pre-print trr)))))
                       ,@(when (tracer-print trr)
                           `((trace-print (multiple-value-list
                                           ,(tracer-print trr)))))
                       ,(tracer-pre trr)
                       ,@(when (tracer-pre-break-if trr)
                           `((when ,(tracer-pre-break-if trr)
                               (sys::break-loop t))))
                       (let ((*trace-values*
                              (multiple-value-list
                               ,(if (tracer-local-p trr)
                                    `(funcall ,(tracer-cur-def trr) ,@sig)
                                  (if (tracer-step-if trr)
                                    '(trace-step-apply)
                                    '(apply *trace-function* *trace-args*))))))
                         ,@(when (tracer-post-break-if trr)
                             `((when ,(tracer-post-break-if trr)
                                 (sys::break-loop t))))
                         ,(tracer-post trr)
                         ,@(when (tracer-print trr)
                             `((trace-print (multiple-value-list
                                             ,(tracer-print trr)))))
                         ,@(when (tracer-post-print trr)
                                `((trace-print (multiple-value-list
                                             ,(tracer-post-print trr)))))
                         (unless ,(tracer-suppress-if trr)
                               (trace-post-output))
                             (values-list *trace-values*)))))))
            (setf (get newname 'sys::untraced-name) (tracer-symb trr))
            (cond (macro-flag
                   (make-macro
                    (fdefinition
                     (compile newname
                      `(lambda (&rest *trace-args*
                                &aux (*trace-form* (car *trace-args*))
                                     (*trace-function*
                                      (macro-expander
                                       (get-traced-definition
                                   ',(tracer-symb trr)))))
                        ,@body)))))
                  ((tracer-local-p trr)
                   (fdefinition
                    (compile newname
                     `(lambda ,sig
                        (let* ((*trace-args* (list ,@sig))
                               (*trace-form*
                                (make-apply-form ',(tracer-name trr)
                                                 *trace-args*))
                               (*trace-function*
                                (get-traced-definition
                                 ',(tracer-symb trr))))
                          ,@body)))))
                  (t (fdefinition
                      (compile newname
                       `(lambda (&rest *trace-args*
                                 &aux (*trace-form*
                                       (make-apply-form ',(tracer-name trr)
                                                        *trace-args*))
                                 (*trace-function*
                                  (get-traced-definition
                                   ',(tracer-symb trr))))
                         ,@body)))))))
    ;; install the new definition
    (tracer-set-fdef trr (get (tracer-symb trr) 'sys::tracing-definition))
    ;; return the name
    (list (tracer-name trr))))

;; auxiliary functions:
;; return next-higher Trace-Level:
(defun trace-level-inc ()
  (%funcall '#,#'1+ *trace-level*))
;; fetch original function definition:
(defun get-traced-definition (symbol)
  (%funcall '#,#'get symbol 'sys::traced-definition))
;; apply, but step by step:
(defun trace-step-apply ()
  ;; (eval `(step (apply ',*trace-function* ',*trace-args*)))
  (%funcall '#,#'eval
    (cons 'step
     (cons
       (cons 'apply
        (cons (cons 'quote (cons *trace-function* nil))
         (cons (cons 'quote (cons *trace-args* nil))
          nil)))
      nil))))
;; build Eval-Form, that corresponds to an Apply (approximately) :
(defun make-apply-form (funname args)
  (declare (inline cons mapcar))
  (cons funname
    (mapcar #'(lambda (arg)
                ;; (list 'quote arg)
                (cons 'quote (cons arg nil)))
            args)))
;; Output before call, uses *trace-level* and *trace-form*
(defun trace-pre-output ()
  (%funcall '#,#'terpri *trace-output*)
  (when *trace-indent*
    (%funcall '#,#'write-spaces *trace-level* *trace-output*))
  (%funcall '#,#'write *trace-level* :stream *trace-output* :base 10 :radix t)
  (%funcall '#,#'write-string " Trace: " *trace-output*)
  (%funcall '#,#'prin1 *trace-form* *trace-output*))
;; Output after call, uses *trace-level*, *trace-form* and *trace-values*
(defun trace-post-output ()
  (declare (inline car cdr consp atom))
  (%funcall '#,#'terpri *trace-output*)
  (when *trace-indent*
    (%funcall '#,#'write-spaces *trace-level* *trace-output*))
  (%funcall '#,#'write *trace-level* :stream *trace-output* :base 10 :radix t)
  (%funcall '#,#'write-string " Trace: " *trace-output*)
  (%funcall '#,#'write (car *trace-form*) :stream *trace-output*)
  (%funcall '#,#'write-string " ==> " *trace-output*)
  (trace-print *trace-values* nil))
;; Output of a list of values:
(defun trace-print (vals &optional (nl-flag t))
  (when nl-flag (%funcall '#,#'terpri *trace-output*))
  (when (consp vals)
    (loop
      (let ((val (car vals)))
        (%funcall '#,#'prin1 val *trace-output*))
      (setq vals (cdr vals))
      (when (atom vals) (return))
      (%funcall '#,#'write-string ", " *trace-output*))))

(defmacro untrace (&rest funs)
  `(mapcan #'untrace1
    ,(if (null funs) `(copy-list *traced-functions*) `',funs)))

(defun untrace1 (funname)
  (let* ((trr (check-traceable funname (make-tracer :name funname) 'untrace))
         (symbol (tracer-symb trr))
         (old-definition (get symbol 'sys::traced-definition)))
    (prog1
      (if old-definition
        ;; symbol was traced
        (progn
          (if (eq (tracer-cur-def trr)
                  (get symbol 'sys::tracing-definition))
            (tracer-set-fdef trr old-definition)
            (warn (TEXT "~S: ~S was traced and has been redefined!")
                  'untrace funname))
          `(,funname))
        ;; funname was not traced
        '())
      (untrace2 trr))))

(defun untrace2 (funname)
  ;; funname can be either a tracer (from untrace1)
  ;; or a function name (from remove-old-definitions)
  (let ((symbol (if (vectorp funname) (tracer-symb funname)
                    (get-funname-symbol funname))))
    (remprop symbol 'sys::traced-definition)
    (remprop symbol 'sys::tracing-definition))
  (setq *traced-functions*
        (delete (if (vectorp funname) (tracer-name funname) funname)
                *traced-functions* :test #'equal)))
