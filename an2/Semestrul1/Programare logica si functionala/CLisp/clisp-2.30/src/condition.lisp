;;; Condition System for CLISP
;;; David Gadbois <gadbois@cs.utexas.edu> 30.11.1993
;;; Bruno Haible 24.11.1993, 2.12.1993

(in-package "COMMON-LISP")
;;; exports:
(export '(
;; types:
restart condition serious-condition error program-error control-error
arithmetic-error division-by-zero floating-point-overflow
floating-point-underflow floating-point-inexact
floating-point-invalid-operation
cell-error unbound-variable undefined-function unbound-slot
type-error package-error print-not-readable parse-error stream-error
end-of-file reader-error file-error storage-condition warning
style-warning simple-condition simple-error simple-type-error simple-warning
;; macros:
define-condition handler-bind ignore-errors handler-case
with-condition-restarts restart-bind restart-case
with-simple-restart check-type assert etypecase ctypecase ecase ccase
;; functions:
make-condition arithmetic-error-operation arithmetic-error-operands
cell-error-name unbound-slot-instance type-error-datum
type-error-expected-type package-error-package print-not-readable-object
stream-error-stream file-error-pathname
simple-condition-format-control simple-condition-format-arguments
signal restart-name compute-restarts find-restart invoke-restart
invoke-restart-interactively invoke-debugger break error cerror warn
;; functions and restart names:
abort continue muffle-warning store-value use-value
;; variables:
*break-on-signals* *debugger-hook*))
;; extensions:
(in-package "EXT")
(export
 '(muffle-cerrors appease-cerrors exit-on-error with-restarts os-error
   simple-condition-format-string simple-charset-type-error)
 "EXT")
(in-package "CUSTOM")
(common-lisp:export '(*break-on-warnings*) "CUSTOM")
(ext:re-export "CUSTOM" "EXT")
(common-lisp:in-package "SYSTEM")

;;; Overview of Concepts

; A condition is some information about an exceptional situation the program
; cannot or does not want handle locally.
; A handler is some code that tries to do recovery from exceptional situations
; that happen elsewhere, or that decides to transfer control.
; A restart is a point where control may be transferred to, together with a
; description what is about to happen in this case.


;;; The CONDITION type

; The condition type system is integrated with CLOS.
(clos:defclass condition () ())

; 29.3.18. Printing Conditions when *print-escape* and *print-readably* are NIL.
(definternational print-condition-format
  (t ENGLISH)
)
(deflocalized print-condition-format ENGLISH
  (formatter "Condition of type ~S.")
)
(clos:defgeneric print-condition (condition stream)
  (:method ((condition condition) stream)
    (format stream (localized 'print-condition-format) (type-of condition))
  )
)
(clos:defmethod clos:print-object ((object condition) stream)
  (if (or *print-escape* *print-readably*)
    (clos:call-next-method)
    (print-condition object stream)
) )
; Avoid warnings caused by DEFCONDITION adding methods to PRINT-CONDITION.
(pushnew 'print-condition clos::*dynamically-modifiable-generic-function-names*)

;;; 29.4.5. Defining Conditions

; DEFINE-CONDITION, CLtL2 p. 898
(defmacro define-condition (name parent-types slot-specs &rest options)
  (unless (symbolp name)
    (error-of-type 'source-program-error
      (TEXT "~S: the name of a condition must be a symbol, not ~S")
      'define-condition name))
  (unless (and (listp parent-types) (every #'symbolp parent-types))
    (error-of-type 'source-program-error
      (TEXT "~S: the parent-type list must be a list of symbols, not ~S")
      'define-condition parent-types))
  (unless (listp slot-specs)
    (error-of-type 'source-program-error
      (TEXT "~S: the slot description list must be a list, not ~S")
      'define-condition slot-specs))
  (let ((default-initargs-option nil)
        (docstring-option nil)
        (report-function nil))
    (dolist (option options)
      (if (listp option)
        (cond ((and (eq (car option) ':DEFAULT-INITARGS) (oddp (length option)))
               (setq default-initargs-option option)
              )
              ((and (keywordp (car option)) (eql (length option) 2))
               (case (first option)
                 (:DOCUMENTATION (setq docstring-option option))
                 (:REPORT (setq report-function (rest option)))
                 (T (error-of-type 'source-program-error
                      (TEXT "~S ~S: unknown option ~S")
                      'define-condition name (first option)))))
              (t
               (error-of-type 'source-program-error
                 (TEXT "~S ~S: invalid syntax in ~S option: ~S")
                 'define-condition name 'define-condition option)))
        (error-of-type 'source-program-error
          (TEXT "~S ~S: not a ~S option: ~S")
          'define-condition name 'define-condition option)))
    (let ((defclass-form
            `(CLOS:DEFCLASS ,name
               ,(clos::add-default-superclass parent-types 'CONDITION)
               ,slot-specs
               ,@(if docstring-option `(,docstring-option))
               ,@(if default-initargs-option `(,default-initargs-option))
             )
         ))
      (if report-function
        `(PROGN
           ,defclass-form
           (CLOS:DEFMETHOD PRINT-CONDITION ((CONDITION ,name) STREAM)
             ,(if (stringp (first report-function))
                `(WRITE-STRING ,(first report-function) STREAM)
                `(FUNCALL (FUNCTION ,@report-function) CONDITION STREAM)
              )
         ) )
        defclass-form
) ) ) )

;;; 29.4.6. Creating Conditions

; MAKE-CONDITION, CLtL2 p. 901
(defun make-condition (type &rest slot-initializations)
  (unless (subtypep type 'condition)
    (error-of-type 'error
      (TEXT "~S: type ~S is not a subtype of ~S")
      'make-condition type 'condition))
  (apply #'clos:make-instance type slot-initializations))

; canonicalize a condition argument, CLtL2 p. 888
(defun coerce-to-condition (datum arguments
                            caller-name
                            default-type &rest more-initargs)
  (typecase datum
    (condition
      (when arguments
        (unless (eq caller-name 'cerror)
          (error-of-type 'type-error
            :datum arguments :expected-type 'null
            (TEXT "~S ~S: superfluous arguments ~S")
            caller-name datum arguments)))
      datum
    )
    (symbol
      (apply #'make-condition datum arguments)
    )
    ((or string function) ; only this case uses default-type and more-initargs
      (apply #'make-condition default-type
             :format-control datum
             :format-arguments arguments
             more-initargs
    ) )
    (t
      (error-of-type 'type-error
        :datum datum :expected-type '(or condition symbol string function)
        (TEXT "~S: the condition argument must be a string, a symbol or a condition, not ~S")
        caller-name datum))))

;;; 29.5. Predefined Condition Types

; Hierarchy:
;
;   condition
;   |
;   |-- simple-condition
;   |
;   |-- serious-condition
;   |   |
;   |   |-- error
;   |   |   |
;   |   |   |-- simple-error
;   |   |   |
;   |   |   |-- arithmetic-error
;   |   |   |   |
;   |   |   |   |-- division-by-zero
;   |   |   |   |
;   |   |   |   |-- floating-point-overflow
;   |   |   |   |
;   |   |   |   |-- floating-point-underflow
;   |   |   |   |
;   |   |   |   |-- floating-point-inexact
;   |   |   |   |
;   |   |   |   |-- floating-point-invalid-operation
;   |   |   |
;   |   |   |-- cell-error
;   |   |   |   |
;   |   |   |   |-- unbound-variable
;   |   |   |   |
;   |   |   |   |-- undefined-function
;   |   |   |   |
;   |   |   |   |-- unbound-slot
;   |   |   |
;   |   |   |-- control-error
;   |   |   |
;   |   |   |-- file-error
;   |   |   |
;   |   |   |-- os-error
;   |   |   |
;   |   |   |-- package-error
;   |   |   |
;   |   |   |-- print-not-readable
;   |   |   |
;   |   |   |-- program-error
;   |   |   |
;   |   |   |-- parse-error
;   |   |   |   |
;   |   |   |   +---------------------+
;   |   |   |                         |
;   |   |   |-- stream-error          |
;   |   |   |   |                     |
;   |   |   |   |-- end-of-file       |
;   |   |   |   |                     |
;   |   |   |   +---------------------+-- reader-error
;   |   |   |
;   |   |   |-- type-error
;   |   |       |
;   |   |       |-- simple-type-error
;   |   |
;   |   |-- storage-condition
;   |   |
;   |   |-- interrupt-condition
;   |
;   |-- warning
;       |
;       |-- simple-warning
;       |
;       |-- style-warning
;

; X3J13 writeup <CONDITION-SLOTS:HIDDEN> wants the slot names to be hidden,
; (e.g. no slot named `package', `stream', `pathname'), hence we prepend $.

; conditions that require interactive intervention
(define-condition serious-condition () ())

  ; serious conditions that occur deterministically
  (define-condition error (serious-condition) ())

    ; mostly statically detectable errors of a program
    (define-condition program-error (error) ())
    ; all the other errors must be detected by the runtime system

      ; statically detectable errors of a program, source available
      (define-condition source-program-error (program-error) ())
      ; CLISP specific

    ; not statically detectable errors in program control
    (define-condition control-error (error) ())

    ; errors that occur while doing arithmetic operations
    (define-condition arithmetic-error (error)
      (($operation :initarg :operation :reader arithmetic-error-operation)
       ($operands  :initarg :operands  :reader arithmetic-error-operands)
    ) )

      ; trying to evaluate a mathematical function at a singularity
      (define-condition division-by-zero (arithmetic-error) ())

      ; trying to get too close to infinity in the floating point domain
      (define-condition floating-point-overflow (arithmetic-error) ())

      ; trying to get too close to zero in the floating point domain
      (define-condition floating-point-underflow (arithmetic-error) ())

      (define-condition floating-point-inexact (arithmetic-error) ())

      (define-condition floating-point-invalid-operation (arithmetic-error) ())

    ; trying to access a location which contains #<UNBOUND>
    (define-condition cell-error (error)
      (($name :initarg :name :reader cell-error-name))
    )

      ; trying to get the value of an unbound variable
      (define-condition unbound-variable (cell-error) ())

      ; trying to get the global function definition of an undefined function
      (define-condition undefined-function (cell-error) ())

      ; trying to get the value of an unbound slot
      (define-condition unbound-slot (cell-error)
        (($instance :initarg :instance :reader unbound-slot-instance))
      )

    ; when some datum does not belong to the expected type
    (define-condition type-error (error)
      (($datum         :initarg :datum         :reader type-error-datum)
       ($expected-type :initarg :expected-type :reader type-error-expected-type)
    ) )

      ; when some keyword does not belong to one of the allowed keywords
      (define-condition keyword-error (program-error type-error) ())
      ; CLISP specific

      ; when some character does not belong to a given character set
      (define-condition charset-type-error (type-error) ())
      ; CLISP specific

    ; errors during operation on packages
    (define-condition package-error (error)
      (($package :initarg :package :reader package-error-package))
    )

    ; attempted violation of *PRINT-READABLY*
    (define-condition print-not-readable (error)
      (($object :initarg :object :reader print-not-readable-object))
    )

    ; errors related to parsing
    (define-condition parse-error (error) ())

    ; errors while doing stream I/O
    (define-condition stream-error (error)
      (($stream :initarg :stream :reader stream-error-stream))
    )

      ; unexpected end of stream
      (define-condition end-of-file (stream-error) ())

      ; parsing/tokenization error during READ
      (define-condition reader-error (parse-error stream-error) ())

    ; errors with pathnames, OS level errors with streams
    (define-condition file-error (error)
      (($pathname :initarg :pathname :reader file-error-pathname))
    )

    ; general OS errors
    (define-condition os-error (error) ())
    ; CLISP specific

  ; "Virtual memory exhausted"
  (define-condition storage-condition (serious-condition) ())

  ; "User break"
  (define-condition interrupt-condition (serious-condition) ())
  ; CLISP specific

; conditions for which user notification is appropriate
(define-condition warning () ())

  ; conditions which are a matter of programming style (not serious)
  (define-condition style-warning (warning) ())

;; These shouldn't be separate types but we cannot adjoin slots without
;; defining subtypes.

; conditions usually created by SIGNAL
(define-condition simple-condition ()
  (($format-control :initarg :format-control :initform nil
                    :reader simple-condition-format-string ; for CLtL2 backward compatibility
                    :reader simple-condition-format-control
   )
   ($format-arguments :initarg :format-arguments :initform nil
                      :reader simple-condition-format-arguments
  ))
  #|
  (:report
    (lambda (condition stream)
      (let ((fstring (simple-condition-format-control condition)))
        (when fstring
          (apply #'format stream fstring (simple-condition-format-arguments condition))
  ) ) ) )
  |#
)
; We don't use the :report option here. Instead we define a print-condition
; method which will be executed regardless of the condition type's CPL.
(clos:defmethod print-condition :around ((condition simple-condition) stream)
  (let ((fstring (simple-condition-format-control condition)))
    (if fstring
      (apply #'format stream fstring (simple-condition-format-arguments condition))
      (clos:call-next-method)
) ) )

; conditions usually created by ERROR or CERROR
(define-condition simple-error (simple-condition error) ())

; conditions usually created by CHECK-TYPE
(define-condition simple-type-error (simple-condition type-error) ())

; conditions usually created by WARN
(define-condition simple-warning (simple-condition warning) ())

; All conditions created by the C runtime code are of type simple-condition.
; Need the following types. Don't use them for discrimination.
(define-condition simple-serious-condition (simple-condition serious-condition) ())
(define-condition simple-program-error (simple-error program-error) ())
(define-condition simple-source-program-error (simple-error source-program-error) ())
(define-condition simple-control-error (simple-error control-error) ())
(define-condition simple-arithmetic-error (simple-error arithmetic-error) ())
(define-condition simple-division-by-zero (simple-error division-by-zero) ())
(define-condition simple-floating-point-overflow (simple-error floating-point-overflow) ())
(define-condition simple-floating-point-underflow (simple-error floating-point-underflow) ())
(define-condition simple-cell-error (simple-error cell-error) ())
(define-condition simple-unbound-variable (simple-error unbound-variable) ())
(define-condition simple-undefined-function (simple-error undefined-function) ())
(define-condition simple-unbound-slot (simple-error unbound-slot) ())
(define-condition simple-keyword-error (simple-error keyword-error) ())
(define-condition simple-charset-type-error (simple-error charset-type-error) ())
(define-condition simple-package-error (simple-error package-error) ())
(define-condition simple-print-not-readable (simple-error print-not-readable) ())
(define-condition simple-parse-error (simple-error parse-error) ())
(define-condition simple-stream-error (simple-error stream-error) ())
(define-condition simple-end-of-file (simple-error end-of-file) ())
(define-condition simple-reader-error (simple-error reader-error) ())
(define-condition simple-file-error (simple-error file-error) ())
(define-condition simple-os-error (simple-error os-error) ())
(define-condition simple-storage-condition (simple-condition storage-condition) ())
(define-condition simple-interrupt-condition (simple-condition interrupt-condition) ())

; Bootstrapping
(%defclcs
  ; The order of the types in this vector must be the same as in lispbibl.d.
  '#((condition                . simple-condition)
     (serious-condition        . simple-serious-condition)
     (error                    . simple-error)
     (program-error            . simple-program-error)
     (source-program-error     . simple-source-program-error)
     (control-error            . simple-control-error)
     (arithmetic-error         . simple-arithmetic-error)
     (division-by-zero         . simple-division-by-zero)
     (floating-point-overflow  . simple-floating-point-overflow)
     (floating-point-underflow . simple-floating-point-underflow)
     (cell-error               . simple-cell-error)
     (unbound-variable         . simple-unbound-variable)
     (undefined-function       . simple-undefined-function)
     (unbound-slot             . simple-unbound-slot)
     (type-error               . simple-type-error)
     (keyword-error            . simple-keyword-error)
     (charset-type-error       . simple-charset-type-error)
     (package-error            . simple-package-error)
     (print-not-readable       . simple-print-not-readable)
     (parse-error              . simple-parse-error)
     (stream-error             . simple-stream-error)
     (end-of-file              . simple-end-of-file)
     (reader-error             . simple-reader-error)
     (file-error               . simple-file-error)
     (os-error                 . simple-os-error)
     (storage-condition        . simple-storage-condition)
     (interrupt-condition      . simple-interrupt-condition)
     (warning                  . simple-warning)
    )
)


;;; Handling and Signalling - Primitives

(defvar *break-on-signals* nil)

#|
;; This would be a possible implementation. However, it forces too many
;; variables into closures although in the most frequent case - no condition
;; at all - they won't be needed. Furthermore, it conses too much.

;; List of active invocations of HANDLER-BIND.
 (defvar *handler-clusters* '())

;; HANDLER-BIND, CLtL2 p. 898
 (defmacro handler-bind (clauses &body body)
  `(LET ((*HANDLER-CLUSTERS*
           (CONS
             (LIST ,@(mapcar #'(lambda (clause)
                                 (let ((type (first clause))
                                       (function-form (second clause)))
                                   `(CONS ',type ,function-form)))
                             clauses))
             *HANDLER-CLUSTERS*)))
     (PROGN ,@body)))

;; SIGNAL, CLtL2 p. 888
 (defun signal (datum &rest arguments)
  (let ((condition
         ;; CLtL2 p. 918 specifies this
         (coerce-to-condition datum arguments 'signal 'simple-condition)))
    (when (typep condition *break-on-signals*)
      ; Enter the debugger prior to signalling the condition
      (restart-case (invoke-debugger condition)
        (continue ())))
    ; CLtL2 p. 884: "A handler is executed in the dynamic context of the
    ; signaler, except that the set of available condition handlers will
    ; have been rebound to the value that was active at the time the condition
    ; handler was made active."
    (let ((*handler-clusters* *handler-clusters*))
      (loop
        (when (null *handler-clusters*) (return))
        (dolist (handler (pop *handler-clusters*))
          (when (typep condition (car handler))
            (funcall (cdr handler) condition)
            (return)))))
    nil))

|#

;; HANDLER-BIND, CLtL2 p. 898
;; Since we can build handler frames only in compiled code
;; there is SYS::%HANDLER-BIND which is synonymous to HANDLER-BIND except
;; that SYS::%HANDLER-BIND only occurs in compiled code.
(defmacro handler-bind (clauses &body body)
  (let ((typespecs (mapcar #'first clauses))
        (handlers (append (mapcar #'rest clauses) (list body))))
    (let ((handler-vars (gensym-list (length handlers))))
      `(LET ,(mapcar #'list
               handler-vars
               (mapcar #'(lambda (handler)
                           `(FUNCTION (LAMBDA () (PROGN ,@handler))))
                       handlers))
         (LOCALLY (DECLARE (COMPILE))
           (SYS::%HANDLER-BIND
             ,(mapcar #'(lambda (typespec handler-var)
                          `(,typespec #'(LAMBDA (CONDITION)
                                          (FUNCALL (FUNCALL ,handler-var)
                                                   CONDITION))))
                      typespecs handler-vars)
             (FUNCALL ,(car (last handler-vars)))))))))

;; SIGNAL, CLtL2 p. 888
; is in error.d


;;; Handling and Signalling - Part 2

;; IGNORE-ERRORS, CLtL2 p. 897
(defmacro ignore-errors (&body body)
  (let ((blockname (gensym)))
    `(BLOCK ,blockname
       (HANDLER-BIND
         ((ERROR #'(LAMBDA (CONDITION) (RETURN-FROM ,blockname (VALUES NIL CONDITION)))))
         ,@body
     ) )
) )

;; HANDLER-CASE, CLtL2 p. 895
(defmacro handler-case (form &rest clauses)
  ;; split off the :NO-ERROR clause and
  ;; add a GO tag to the other clauses (type varlist . body)
  (let ((no-error-clause nil) ; the :no-error clause, if found
        (extended-clauses '())) ; ((tag type varlist . body) ...)
    (do ()
        ((endp clauses))
      (let ((clause (pop clauses)))
        (block check-clause
          (unless (and (consp clause) (consp (cdr clause))
                       (listp (second clause)))
            (error-of-type 'source-program-error
                           (TEXT "~S: illegal syntax of clause ~S")
                           'handler-case clause))
          (when (eq (first clause) ':no-error)
            (if (null no-error-clause)
              (setq no-error-clause clause)
              (warn (TEXT "~S: multiple ~S clauses: ~S and ~S")
                    'handler-case ':no-error clause no-error-clause))
            (return-from check-clause))
          (let ((varlist (second clause))) ; known to be a list
            (unless (null (cdr varlist))
              (error-of-type 'source-program-error
                (TEXT "~S: too many variables ~S in clause ~S")
                'handler-case varlist clause)))
          (push (cons (gensym) clause) extended-clauses))))
    (setq extended-clauses (nreverse extended-clauses))
    (let ((blockname (gensym))
          (tempvar (gensym)))
      `(BLOCK ,blockname
         (LET (,tempvar) ; tempvar is IGNORABLE since it is a gensym
           (TAGBODY
             (RETURN-FROM ,blockname
               ,(let ((main-form
                        `(HANDLER-BIND
                           ,(mapcar #'(lambda (xclause)
                                        (let ((tag (first xclause))
                                              (type (first (rest xclause)))
                                              (varlist (second (rest xclause))))
                                          `(,type
                                            #'(LAMBDA (CONDITION)
                                                ,(if (null varlist)
                                                   `(DECLARE (IGNORE CONDITION))
                                                   `(SETQ ,tempvar CONDITION)
                                                 )
                                                (GO ,tag)
                                           )  )
                                      ) )
                                    extended-clauses
                            )
                           ,form
                         )
                     ))
                  (if no-error-clause
                    `(MULTIPLE-VALUE-CALL #'(LAMBDA ,@(rest no-error-clause))
                       ,main-form
                     )
                    main-form
                ) )
             )
             ,@(mapcap #'(lambda (xclause)
                           (let ((tag (first xclause))
                                 (varlist (second (rest xclause)))
                                 (body (cddr (rest xclause)))) ; may contain declarations
                             `(,tag
                               (RETURN-FROM ,blockname
                                 (LET ,(if (null varlist) '() `((,@varlist ,tempvar)))
                                   ,@body
                              )) )
                         ) )
                       extended-clauses
               )
       ) ) )
) ) )


;;; Restarts

;; This stuff is needed only once an exception has already occurred. No need
;; to optimize the hell out of it.

; The default test function for restarts always returns T. See CLtL2 p. 905,909.
(defun default-restart-test (condition)
  (declare (ignore condition))
  t
)

; The default interactive function for restarts returns the empty argument list.
(defun default-restart-interactive ()
  '()
)

;; The RESTART type, CLtL2 p. 916
;; Also defines RESTART-NAME, CLtL2 p. 911
(defstruct (restart (:print-object print-restart))
  name             ; its name, or NIL if it is not named
  (test #'default-restart-test) ; function that tests whether this restart
                                ; applies to a given condition
  (invoke-tag nil) ; tag used to invoke the restart, or nil
  invoke-function  ; function used to invoke the restart, if invoke-tag is nil
  (report nil)     ; function used to print a description of the restart
  (interactive #'default-restart-interactive)
                   ; function used to gather additional data from the user
                   ; before invoking the restart
)
#| ; We could also define it as a CLOS class:
 (clos:defclass restart ()
  (name            :initarg :name            :reader restart-name)
  (test            :initarg :test            :reader restart-test
                   :initform #'default-restart-test)
  (invoke-tag      :initarg :invoke-tag      :reader restart-invoke-tag
                   :initform nil)
  (invoke-function :initarg :invoke-function :reader restart-invoke-function)
  (report          :initarg :report          :reader restart-report
                   :initform nil)
  (interactive     :initarg :interactive     :reader restart-interactive
                   :initform #'default-restart-interactive))
|#

;; Printing restarts
(defun print-restart (restart stream)
  (if (or *print-escape* *print-readably*)
    (print-unreadable-object (restart stream :type t :identity t)
      (write (restart-name restart) :stream stream)
    )
    (let ((report-function (restart-report restart)))
      (if report-function
        (funcall report-function stream)
        (prin1 (restart-name restart) stream)
) ) ) )
#| ; If RESTART were a CLOS class:
 (clos:defmethod clos:print-object ((restart restart) stream)
  (if (or *print-escape* *print-readably*)
    (clos:call-next-method)
    (let ((report-function (restart-report restart)))
      (if report-function
        (funcall report-function stream)
        (prin1 (restart-name restart) stream)))))
|#

;; Expands to the equivalent of `(MAKE-RESTART :NAME name ...)
;; but makes intelligent use of the defaults to reduce code size.
(defun make-restart-form (name test invoke-tag invoke-function report interactive)
  `(MAKE-RESTART
     :NAME ,name
     ,@(if (not (equal test '(FUNCTION DEFAULT-RESTART-TEST)))
         `(:TEST ,test)
       )
     ,@(if (not (equal invoke-tag 'NIL))
         `(:INVOKE-TAG ,invoke-tag)
       )
     :INVOKE-FUNCTION ,invoke-function
     ,@(if (not (equal report 'NIL))
         `(:REPORT ,report)
       )
     ,@(if (not (equal interactive '(FUNCTION DEFAULT-RESTART-INTERACTIVE)))
         `(:INTERACTIVE ,interactive)
       )
   )
)

;; The list of active restarts.
(defvar *active-restarts* nil)

;; A list of pairs of conditions and restarts associated with them. We have to
;; keep the associations separate because there can be a many-to-many mapping
;; between restarts and conditions, and this mapping has dynamic extent.
(defvar *condition-restarts* nil)

; Add an association between a condition and a couple of restarts.
(defun add-condition-restarts (condition restarts)
  (dolist (restart restarts)
    (push (cons condition restart) *condition-restarts*)
) )

;; WITH-CONDITION-RESTARTS, CLtL2 p. 910
(defmacro with-condition-restarts (condition-form restarts-form &body body)
  `(LET ((*CONDITION-RESTARTS* *CONDITION-RESTARTS*))
     (ADD-CONDITION-RESTARTS ,condition-form ,restarts-form)
     (LET () ,@body)
   )
)

;;; 29.4.8. Finding and Manipulating Restarts

; Tests whether a given restart is applicable to a given condition
(defun applicable-restart-p (restart condition)
  (and
    #| ; We choose the ANSI-CL behaviour because it makes the need for the
       ; syntax-dependent implicit restart association in RESTART-CASE
       ; nearly obsolete.
    #-ANSI-CL
    ; A restart is applicable iff it is associated to that condition.
    (dolist (asso *condition-restarts* nil)
      (when (and (eq (car asso) condition) (eq (cdr asso) restart))
        (return t)
    ) )
    #+ANSI-CL
    |#
    ; A restart is applicable if it is associated to that condition
    ; or if it is not associated to any condition.
    (let ((not-at-all t))
      (dolist (asso *condition-restarts* not-at-all)
        (when (eq (cdr asso) restart)
          (if (eq (car asso) condition)
            (return t)
            (setq not-at-all nil)
    ) ) ) )
    ; Call the restart's test function:
    (funcall (restart-test restart) condition)
) )

;; COMPUTE-RESTARTS, CLtL2 p. 910
(defun compute-restarts (&optional condition)
  (if condition
    ; return only restarts that are applicable to that condition
    (remove-if-not #'(lambda (restart) (applicable-restart-p restart condition))
                   *active-restarts*
    )
    ; return all restarts
    *active-restarts*
) )

;; FIND-RESTART, CLtL2 p. 911
; returns a restart or nil
(defun find-restart (restart-identifier &optional condition)
  (cond ((null restart-identifier)
         (error-of-type 'error
           (TEXT "~S: ~S is not a valid restart name here. Use ~S instead.")
           'find-restart restart-identifier 'compute-restarts))
        ((symbolp restart-identifier)
         (dolist (restart *active-restarts*)
           (when (and (eq (restart-name restart) restart-identifier)
                      (or (null condition)
                          (applicable-restart-p restart condition)
                 )    )
             (return restart)
        )) )
        ((typep restart-identifier 'restart)
         (dolist (restart *active-restarts*)
           (when (and (eq restart restart-identifier)
                      (or (null condition)
                          (applicable-restart-p restart condition)
                 )    )
             (return restart)
        )) )
        (t (error-of-type 'type-error
             :datum restart-identifier :expected-type '(or symbol restart)
             (TEXT "~S: invalid restart name ~S")
             'find-restart restart-identifier))))

(defun restart-not-found (restart-identifier)
  (error-of-type 'control-error
    (TEXT "~S: No restart named ~S is visible.")
    'invoke-restart restart-identifier))

(defun %invoke-restart (restart arguments)
  (if (restart-invoke-tag restart)
    (throw (restart-invoke-tag restart) arguments)
    (apply (restart-invoke-function restart) arguments)
    ; This may return normally, the restart need not transfer control.
    ; (See CLtL2 p. 880.)
) )

;; INVOKE-RESTART, CLtL2 p. 911
(defun invoke-restart (restart-identifier &rest arguments)
  (let ((restart (find-restart restart-identifier)))
    (unless restart (restart-not-found restart-identifier))
    (%invoke-restart restart arguments)
) )

(defun invoke-restart-condition (restart-identifier condition &rest arguments)
  (let ((restart (find-restart restart-identifier condition)))
    (unless restart (restart-not-found restart-identifier))
    (%invoke-restart restart arguments)
) )

(defun invoke-restart-condition-if-exists (restart-identifier condition &rest arguments)
  (let ((restart (find-restart restart-identifier condition)))
    (when restart
      (%invoke-restart restart arguments)
) ) )

;; INVOKE-RESTART-INTERACTIVELY, CLtL2 p. 911
(defun invoke-restart-interactively (restart-identifier)
  (let ((restart (find-restart restart-identifier)))
    (unless restart (restart-not-found restart-identifier))
    (let ((arguments (funcall (restart-interactive restart))))
      (%invoke-restart restart arguments)
) ) )

;;; 29.4.7. Establishing Restarts

;; This conses out the wazoo, but there seems to be no good way around it short
;; of special casing things a zillion ways.  The main problem is that someone
;; could write:
;;
;; (restart-bind ((nil *fun-1*
;;                     :interactive-function *fun-2*
;;                     :report-function *fun-3*
;;                     :test-function *fun-4*
;;                 )) ...)
;;
;; and it is supposed to work.

;; RESTART-BIND, CLtL2 p. 909
(defmacro restart-bind (restart-specs &body body)
  (setq body `(PROGN ,@body))
  (unless (listp restart-specs)
    (error-of-type 'source-program-error
      (TEXT "~S: not a list: ~S")
      'restart-bind restart-specs))
  (if restart-specs
    `(LET ((*ACTIVE-RESTARTS*
             (LIST*
               ,@(mapcar #'(lambda (spec)
                             (unless (and (listp spec) (consp (cdr spec)) (symbolp (first spec)))
                               (error-of-type 'source-program-error
                                 (TEXT "~S: invalid restart specification ~S")
                                 'restart-bind spec))
                             (apply #'(lambda (name function
                                               &key (test-function '(FUNCTION DEFAULT-RESTART-TEST))
                                                    (interactive-function '(FUNCTION DEFAULT-RESTART-INTERACTIVE))
                                                    (report-function 'NIL))
                                        (when (and (null name) (eq report-function 'NIL))
                                          ; CLtL2 p. 906: "It is an error if an unnamed restart is used
                                          ; and no report information is provided."
                                          (error-of-type 'source-program-error
                                            (TEXT "~S: unnamed restarts require ~S to be specified: ~S")
                                            'restart-bind ':REPORT-FUNCTION spec))
                                        (make-restart-form `',name
                                                           test-function
                                                           'NIL
                                                           function
                                                           report-function
                                                           interactive-function
                                      ) )
                                    spec
                           ) )
                         restart-specs
                 )
               *ACTIVE-RESTARTS*
          )) )
       ,body
     )
    body
) )

;; RESTART-CASE, CLtL2 p. 903
;; WITH-RESTARTS
;; Syntax: (RESTART-CASE form {restart-clause}*)
;;         (WITH-RESTARTS ({restart-clause}*) {form}*)
;; restart-clause ::=   (restart-name arglist {keyword value}* {form}*)
;;                    | (restart-name {keyword value}* arglist {form}*)

;; There are a number of special cases we could optimize for. If we
;; can determine that we will not have to cons any closures at
;; runtime, then we could statically allocate the list of restarts.
;;
;; Since we have to deal with the wacky way RESTART-CASE interacts with
;; WITH-CONDITION-RESTARTS, we do not go through RESTART-BIND.

(eval-when (compile load eval)
  (defun expand-restart-case (caller restart-clauses form)
    (unless (listp restart-clauses)
      (error-of-type 'source-program-error
        (TEXT "~S: not a list: ~S")
        caller restart-clauses))
    (let ((xclauses ; list of expanded clauses
                    ; ((tag name test interactive report lambdalist . body) ...)
            (mapcar
              #'(lambda (restart-clause &aux (clause restart-clause))
                  (unless (and (consp clause) (consp (cdr clause)) (symbolp (first clause)))
                    (error-of-type 'source-program-error
                      (TEXT "~S: invalid restart specification ~S")
                      caller clause))
                  (let ((name (pop clause))
                        (passed-arglist nil)
                        (passed-keywords nil)
                        arglist
                        (keywords '()))
                    (loop
                      (when (endp clause) (return))
                      (cond ((and (not passed-arglist) (listp (first clause)))
                             (setq arglist (pop clause))
                             (setq passed-arglist t)
                             (when keywords (setq passed-keywords t))
                            )
                            ((and (not passed-keywords) (consp (cdr clause)) (symbolp (first clause)))
                             (push (pop clause) keywords)
                             (push (pop clause) keywords)
                            )
                            (t (return))
                    ) )
                    (unless passed-arglist
                      (error-of-type 'source-program-error
                        (TEXT "~S: missing lambda list in restart specification ~S")
                        caller clause))
                    (multiple-value-bind (test interactive report)
                        (apply #'(lambda (&key (test 'DEFAULT-RESTART-TEST)
                                               (interactive 'DEFAULT-RESTART-INTERACTIVE)
                                               (report 'DEFAULT-RESTART-REPORT))
                                   (values test interactive report)
                                 )
                               (nreverse keywords)
                        )
                      (when (and (null name) (eq report 'DEFAULT-RESTART-REPORT))
                        ; CLtL2 p. 906: "It is an error if an unnamed restart is used
                        ; and no report information is provided."
                        (error-of-type 'source-program-error
                          (TEXT "~S: unnamed restarts require ~S to be specified: ~S")
                          caller ':REPORT restart-clause))
                      (when (and (consp arglist) (not (member (first arglist) lambda-list-keywords))
                                 (eq interactive 'DEFAULT-RESTART-INTERACTIVE)
                            )
                        ; restart takes required arguments but does not have an
                        ; interactive function that will prompt for them.
                        (warn (TEXT "~S: restart cannot be invoked interactively because it is missing a ~S option: ~S")
                              caller ':INTERACTIVE restart-clause))
                      `(,(gensym)
                        ,name
                        ,test ,interactive ,report
                        ,arglist
                        ,@clause
                       )
                ) ) )
              restart-clauses
          ) )
          (blockname (gensym))
          (arglistvar (gensym))
          (associate
            ;; Yick.  As a pretty lame way of allowing for an
            ;; association between conditions and restarts,
            ;; RESTART-CASE has to notice if its body is signalling a
            ;; condition, and, if so, associate the restarts with the
            ;; condition.
            (and (consp form)
                 (case (first form) ((SIGNAL ERROR CERROR WARN) t) (t nil))
                 (gensym)
         )) )
      `(BLOCK ,blockname
         (LET (,arglistvar) ; arglistvar is IGNORABLE since it is a gensym
           (TAGBODY
             ,(let ((restart-forms
                      (mapcar #'(lambda (xclause)
                                  (let ((tag (first xclause))
                                        (name (second xclause))
                                        (test (third xclause))
                                        (interactive (fourth xclause))
                                        (report (fifth xclause)))
                                    (make-restart-form `',name
                                                       `(FUNCTION ,test)
                                                       'NIL
                                                       `(FUNCTION
                                                          (LAMBDA (&REST ARGUMENTS)
                                                            (SETQ ,arglistvar ARGUMENTS)
                                                            (GO ,tag)
                                                        ) )
                                                       (if (eq report 'DEFAULT-RESTART-REPORT)
                                                         `NIL
                                                         `(FUNCTION
                                                            ,(if (stringp report)
                                                               `(LAMBDA (STREAM) (WRITE-STRING ,report STREAM))
                                                                report
                                                             )
                                                          )
                                                       )
                                                       `(FUNCTION ,interactive)
                                     )
                                ) )
                              xclauses
                    ) )
                    (form `(RETURN-FROM ,blockname ,form)))
                `(LET* ,(if associate
                          `((,associate (LIST ,@restart-forms))
                            (*ACTIVE-RESTARTS* (APPEND ,associate *ACTIVE-RESTARTS*))
                            (*CONDITION-RESTARTS* *CONDITION-RESTARTS*)
                           )
                          `((*ACTIVE-RESTARTS* (LIST* ,@restart-forms *ACTIVE-RESTARTS*)))
                        )
                   ,(if associate
                      #| ; This code resignals the condition in a different dynamic context!
                      `(LET ((CONDITION
                               (HANDLER-CASE ,form ; evaluate the form
                                 (CONDITION (C) C) ; catch the condition
                            )) )
                         (WITH-CONDITION-RESTARTS CONDITION ,associate ; associate the condition with the restarts
                           (SIGNAL CONDITION) ; resignal the condition
                       ) )
                      |#
                      #| ; This code invokes the debugger even if it shouldn't!
                      `(HANDLER-BIND
                         ((CONDITION ; catch the condition
                            #'(LAMBDA (CONDITION)
                                (WITH-CONDITION-RESTARTS CONDITION ,associate  ; associate the condition with the restarts
                                  (SIGNAL CONDITION) ; resignal the condition
                                  (INVOKE-DEBUGGER CONDITION) ; this is weird!
                         ))   ) )
                         ,form
                       )
                      |#
                      `(HANDLER-BIND
                         ((CONDITION ; catch the condition
                            #'(LAMBDA (CONDITION)
                                (ADD-CONDITION-RESTARTS CONDITION ,associate) ; associate the condition with the restarts
                                (SIGNAL CONDITION) ; resignal the condition
                         ))   )
                         ,form
                       )
                      form
                    )
                 )
              )
             ,@(mapcap #'(lambda (xclause)
                           (let ((tag (first xclause))
                                 (lambdabody (cdddr (cddr xclause))))
                             `(,tag
                               (RETURN-FROM ,blockname
                                 (APPLY (FUNCTION (LAMBDA ,@lambdabody)) ,arglistvar)
                              ))
                         ) )
                       xclauses
               )
       ) ) )
  ) )
)

(defmacro restart-case (form &rest restart-clauses)
  (expand-restart-case 'restart-case restart-clauses form)
)

(defmacro with-restarts (restart-clauses &body body)
  (expand-restart-case 'with-restarts restart-clauses `(PROGN ,@body))
)

;; WITH-SIMPLE-RESTART, CLtL2 p. 902
(defmacro with-simple-restart ((name format-string &rest format-arguments) &body body)
  (if (or format-arguments (not (constantp format-string)))
    `(WITH-RESTARTS
         ((,name
           :REPORT (LAMBDA (STREAM) (FORMAT STREAM ,format-string ,@format-arguments))
           () (VALUES NIL T)
         ))
       ,@body
     )
    ;; Here's an example of how we can easily optimize things. There is no
    ;; need to refer to anything in the lexical environment, so we can avoid
    ;; consing a restart at run time.
    (let ((blockname (gensym))
          (tag (gensym)))
      `(BLOCK ,blockname
         (CATCH ',tag
           (LET ((*ACTIVE-RESTARTS*
                   (CONS
                     (LOAD-TIME-VALUE
                       (MAKE-RESTART :NAME ',name
                                     :INVOKE-TAG ',tag
                                     :REPORT #'(LAMBDA (STREAM) (FORMAT STREAM ,format-string))
                     ) )
                     *ACTIVE-RESTARTS*
                )) )
             (RETURN-FROM ,blockname (PROGN ,@body))
           )
           (VALUES NIL T)
       ) )
) ) )


;;; 29.4.10. Restart Functions

;; These functions are customary way to pass control from a handler to a
;; restart. They just invoke the restart of the same name.

;; ABORT, CLtL2 p. 913
(defun abort (&optional condition)
  (invoke-restart-condition 'abort condition)
)

;; CONTINUE, CLtL2 p. 913
(defun continue (&optional condition)
  (invoke-restart-condition-if-exists 'continue condition)
)

;; MUFFLE-WARNING, CLtL2 p. 913
(defun muffle-warning (&optional condition)
  (invoke-restart-condition 'muffle-warning condition)
)

;; STORE-VALUE, CLtL2 p. 913
(defun store-value (value &optional condition)
  (invoke-restart-condition-if-exists 'store-value condition value)
)

;; USE-VALUE, CLtL2 p. 914
(defun use-value (value &optional condition)
  (invoke-restart-condition-if-exists 'use-value condition value)
)


;;; 29.4.2. Assertions

;; These macros supersede the corresponding ones from macros2.lisp.

(defun report-new-value (stream)
  (write-string (report-one-new-value-string) stream)
)

(defun prompt-for-new-value (place)
  (let ((nn (length (nth-value 2 (get-setf-expansion place)))))
    (cond ((= nn 1)
           (format *debug-io* (prompt-for-new-value-string) place)
           (list (read *debug-io*)))
          ((do ((ii 1 (1+ ii)) res)
               ((> ii nn) (nreverse res))
             (format *debug-io* (TEXT "~%New ~S [value ~D of ~D]: ")
                     place ii nn)
             (push (read *debug-io*) res))))))

;; CHECK-TYPE, CLtL2 p. 889
(defmacro check-type (place typespec &optional (string nil))
  (let ((tag1 (gensym))
        (tag2 (gensym))
        (var (gensym)))
    `(TAGBODY
       ,tag1
       (LET ((,var ,place))
         (WHEN (TYPEP ,var ',typespec) (GO ,tag2))
         (RESTART-CASE
           (ERROR-OF-TYPE 'TYPE-ERROR
             :DATUM ,var :EXPECTED-TYPE ',typespec
             (TYPE-ERROR-STRING)
             (CHECK-TYPE-ERROR-STRING ',place ,string ',typespec)
             ,var
           )
           ; only one restart, will "continue" invoke it?
           (STORE-VALUE
             :REPORT REPORT-NEW-VALUE
             :INTERACTIVE (LAMBDA () (PROMPT-FOR-NEW-VALUE ',place))
             (NEW-VALUE) (SETF ,place NEW-VALUE)
           )
       ) )
       (GO ,tag1)
       ,tag2
     )
) )

(defun report-no-new-value (stream)
  (write-string (report-no-new-value-string) stream)
)

(defun report-new-values (stream)
  (write-string (report-new-values-string) stream)
)

;; this is the same as `default-restart-interactive' but it must
;; be kept a separate object for the benefit of `appease-cerrors'
(defun assert-restart-no-prompts () nil)

;; ASSERT, CLtL2 p. 891
(defmacro assert (test-form &optional (place-list nil) (datum nil) &rest args)
  (let ((tag1 (gensym))
        (tag2 (gensym)))
    `(TAGBODY
       ,tag1
       (WHEN ,test-form (GO ,tag2))
       (RESTART-CASE
           ;; no need for explicit association, see APPLICABLE-RESTART-P
           (ERROR ; of-type ??
            ,@(if datum
                `(,datum ,@args) ; use coerce-to-condition??
                `("~A" (ASSERT-ERROR-STRING ',test-form))))
         ;; only one restart: CONTINUE
         (CONTINUE
             :REPORT ,(case (length place-list)
                            (0 'REPORT-NO-NEW-VALUE)
                            (1 'REPORT-NEW-VALUE)
                            (t 'REPORT-NEW-VALUES))
             :INTERACTIVE
               ,(let ((prompts (mapcar #'(lambda (place)
                                           `(PROMPT-FOR-NEW-VALUE ',place))
                                       place-list)))
                     (if prompts
                         (compile 'assert-restart-prompt
                                  `(LAMBDA () (APPEND ,@prompts)))
                         'assert-restart-no-prompts))
               ,@(do ((pl place-list (cdr pl))
                      (all-setter-vars '())
                      (all-setter-forms '()))
                     ((endp pl)
                      (cons (nreverse all-setter-vars)
                            (nreverse all-setter-forms)))
                     (multiple-value-bind (vr vl sv se ge)
                         (get-setf-expansion (car pl))
                       (declare (ignore ge))
                       (setq all-setter-vars
                             (revappend sv all-setter-vars))
                       (push `(LET* ,(mapcar #'list vr vl) ,se)
                             all-setter-forms)))))
       (GO ,tag1)
       ,tag2)))

;;; 29.4.3. Exhaustive Case Analysis

;; These macros supersede the corresponding ones from macros2.lisp.
(flet ((parenthesize-keys (clauses)
         ;; PARENTHESIZE-KEYS is necessary to avoid confusing
         ;; the symbols OTHERWISE and T used as keys, with the same
         ;; symbols used in the syntax of the non exhaustive CASE.
         (mapcar #'(lambda (c)
                     (cond ((or (eq (car c) 't)
                                (eq (car c) 'otherwise))
                            (warn (TEXT "~S used as a key in ~S, it would be better to use parentheses.")
                                  (car c) c)
                            (cons (list (car c)) (cdr c)))
                           (t c)))
                 clauses)))
  (flet ((typecase-errorstring (keyform keyclauselist)
           (let ((typelist (mapcar #'first keyclauselist)))
             `(TYPECASE-ERROR-STRING ',keyform ',typelist)
         ) )
         (typecase-expected-type (keyclauselist)
           `(OR ,@(mapcar #'first keyclauselist))
         )
         (case-errorstring (keyform keyclauselist)
           (let ((caselist
                   (mapcap #'(lambda (keyclause)
                               (setq keyclause (car keyclause))
                               (if (listp keyclause) keyclause (list keyclause))
                             )
                           keyclauselist
                )) )
             `(CASE-ERROR-STRING ',keyform ',caselist)
         ) )
         (case-expected-type (keyclauselist)
           `(MEMBER ,@(mapcap #'(lambda (keyclause)
                                  (setq keyclause (car keyclause))
                                  (if (listp keyclause) keyclause (list keyclause))
                                )
                              keyclauselist
            )         )
         )
         (simply-error (casename form clauselist errorstring expected-type)
           (let ((var (gensym)))
             `(LET ((,var ,form))
                (,casename ,var
                  ,@(parenthesize-keys clauselist) ; if a clause contains an OTHERWISE or T key,
                                                   ; it is treated as a normal key, as per CLHS.
                  (OTHERWISE
                    (ERROR-OF-TYPE 'TYPE-ERROR
                                   :DATUM ,var :EXPECTED-TYPE ',expected-type
                                   (TYPE-ERROR-STRING)
                                   ,errorstring ,var
              ) ) ) )
         ) )
         (retry-loop (casename place clauselist errorstring expected-type)
           (let ((g (gensym))
                 (h (gensym)))
             `(BLOCK ,g
                (TAGBODY
                  ,h
                  (RETURN-FROM ,g
                    (,casename ,place
                      ,@(parenthesize-keys clauselist) ; if a clause contains an OTHERWISE or T key,
                                                       ; it is treated as a normal key, as per CLHS.
                      (OTHERWISE
                        (RESTART-CASE
                          (PROGN       ; no need for explicit association, see applicable-restart-p
                            (ERROR-OF-TYPE 'TYPE-ERROR
                              :DATUM ,place :EXPECTED-TYPE ',expected-type
                              (TYPE-ERROR-STRING)
                              ,errorstring
                              ,place
                          ) )
                                        ; only one restart, will "continue" invoke it?
                          (STORE-VALUE
                            :REPORT REPORT-NEW-VALUE
                            :INTERACTIVE (LAMBDA () (PROMPT-FOR-NEW-VALUE ',place))
                            (NEW-VALUE) (SETF ,place NEW-VALUE)
                        ) )
                        (GO ,h)
              ) ) ) ) )
        )) )
    (defmacro etypecase (keyform &rest keyclauselist)
      (simply-error 'TYPECASE keyform keyclauselist
                    (typecase-errorstring keyform keyclauselist)
                    (typecase-expected-type keyclauselist)
    ) )
    (defmacro ctypecase (keyplace &rest keyclauselist)
      (retry-loop 'TYPECASE keyplace keyclauselist
                  (typecase-errorstring keyplace keyclauselist)
                  (typecase-expected-type keyclauselist)
    ) )
    (defmacro ecase (keyform &rest keyclauselist)
      (simply-error 'CASE keyform keyclauselist
                    (case-errorstring keyform keyclauselist)
                    (case-expected-type keyclauselist)
    ) )
    (defmacro ccase (keyform &rest keyclauselist)
      (retry-loop 'CASE keyform keyclauselist
                  (case-errorstring keyform keyclauselist)
                  (case-expected-type keyclauselist)
    ) )
) )

;;; 29.4.11. Debugging Utilities

(defvar *debugger-hook* nil)

;; INVOKE-DEBUGGER, CLtL2 p. 915
; is in error.d

;; BREAK, CLtL2 p. 914
; (BREAK [format-string {arg}*])
; It would be unfair to bypass the *debugger-hook* test in INVOKE-DEBUGGER.
; So we call INVOKE-DEBUGGER and therefore need a condition.
(defun break (&optional (format-string "Break") &rest args)
  (if (not *use-clcs*)
    (progn
      (terpri *error-output*)
      (apply #'format *error-output*
                      (concatenate 'string "*** - " format-string)
                      args
      )
      (funcall *break-driver* t)
    )
    (let ((condition
            (make-condition 'simple-condition
                            :format-control format-string
                            :format-arguments args
         )) )
      (with-restarts
          ((CONTINUE
            :report (lambda (stream)
                      (format stream (TEXT "Return from ~S loop")
                                     'break))
            ()
          ))
        (with-condition-restarts condition (list (find-restart 'CONTINUE))
          (invoke-debugger condition)
    ) ) )
  )
  nil
)

;;; 29.4.1. Signaling Conditions

;; ERROR, CLtL2 p. 886
#| ; is in error.d
 (defun error (errorstring &rest args)
  (if (or *error-handler* (not *use-clcs*))
    (progn
      (if *error-handler*
        (apply *error-handler* nil errorstring args)
        (progn
          (terpri *error-output*)
          (write-string "*** - " *error-output*)
          (apply #'format *error-output* errorstring args)))
      (funcall *break-driver* nil))
    (let ((condition (coerce-to-condition errorstring args 'error 'simple-error)))
      (signal condition)
      (invoke-debugger condition))))
|#

;; CERROR, CLtL2 p. 887
(defun cerror (continue-format-string error-format-string &rest args)
  (if *error-handler*
    (apply *error-handler*
           (or continue-format-string t) error-format-string args
    )
    (if (not *use-clcs*)
      (progn
        (terpri *error-output*)
        (write-string "** - Continuable Error" *error-output*)
        (terpri *error-output*)
        (apply #'format *error-output* error-format-string args)
        (terpri *debug-io*)
        (if (interactive-stream-p *debug-io*)
          (progn
            (write-string (TEXT "If you continue (by typing 'continue'): ")
                          *debug-io*)
            (apply #'format *debug-io* continue-format-string args)
            (funcall *break-driver* t)
          )
          (apply #'format *debug-io* continue-format-string args)
      ) )
      (let ((condition (coerce-to-condition error-format-string args 'cerror 'simple-error)))
        (with-restarts
            ((CONTINUE
              :report (lambda (stream)
                        (apply #'format stream continue-format-string args)
                      )
              ()
            ))
          (with-condition-restarts condition (list (find-restart 'CONTINUE))
            (signal condition)
            (invoke-debugger condition)
      ) ) )
  ) )
  nil
)

;;; 29.4.9. Warnings

(defvar *break-on-warnings* nil)

;; WARN, CLtL2 p. 912
; (WARN format-string {arg}*)
(defun warn (format-string &rest args)
  (if (not *use-clcs*)
    (progn
      (terpri *error-output*)
      (write-string (TEXT "WARNING:") *error-output*)
      (terpri *error-output*)
      (apply #'format *error-output* format-string args)
      (when *break-on-warnings* (funcall *break-driver* t))
    )
    (block warn
      (let ((condition (coerce-to-condition format-string args 'warn 'simple-warning)))
        (unless (typep condition 'warning)
          (error-of-type 'type-error
            :datum condition :expected-type 'warning
            (TEXT "~S: This is more serious than a warning: ~A")
            'warn condition))
        (with-restarts
            ((MUFFLE-WARNING
               () (return-from warn)
            ))
          (with-condition-restarts condition (list (find-restart 'MUFFLE-WARNING))
            (signal condition)
        ) )
        (terpri *error-output*)
        (write-string (TEXT "WARNING:") *error-output* )
        (terpri *error-output*)
        (print-condition condition *error-output*)
        (when *break-on-warnings*
          (with-restarts
              ((CONTINUE
                :report (lambda (stream)
                          (format stream (TEXT "Return from ~S loop") 'break))
                () (return-from warn)
              ))
            (with-condition-restarts condition (list (find-restart 'CONTINUE))
              ; We don't call  (invoke-debugger condition)  because that
              ; would tell the user about a "Continuable error". Actually,
              ; it is only a warning!
              (funcall *break-driver* nil condition nil)
        ) ) )
    ) )
  )
  nil
)


#|
Todo:
29.3.6 29.3.7 29.3.8 29.3.9 29.3.10
      29.3.11 29.3.12 29.3.13 29.3.14 29.3.15 29.3.16 29.3.17 29.3.18
29.4. 29.4.9 29.4.11
29.5.
|#


;; Extensions. They assume *USE-CLCS* is T.

(defun maybe-continue (condition report-p)
  (let ((restart (find-restart 'CONTINUE condition)))
    (when restart
      (let ((res-int (restart-interactive restart)))
        (case (and res-int (closure-name res-int))
          ((assert-restart-no-prompts)
           (if (interactive-stream-p *debug-io*)
             (invoke-debugger condition)
             (exitunconditionally condition)))
          ((assert-restart-prompt) ; prompt for new values
           (if (interactive-stream-p *debug-io*)
             (progn
               (write-string "** - Continuable Error" *error-output*)
               (terpri *error-output*)
               (print-condition condition *error-output*)
               (invoke-restart-interactively restart))
             (exitunconditionally condition)))
          (otherwise            ; general automatic error handling
           (when report-p
             (warn "~A" (with-output-to-string (stream)
                          (print-condition condition stream)
                          (let ((report-function (restart-report restart)))
                            (when report-function
                           (terpri stream)
                           (funcall report-function stream))))))
           (if (restart-interactive restart)
             (invoke-restart-interactively restart)
             (invoke-restart restart))))))))

(defun muffle-cerror (condition) (maybe-continue condition nil))
(defmacro muffle-cerrors (&body body)
  "(MUFFLE-CERRORS {form}*) executes the forms, but when a continuable
error occurs, the CONTINUE restart is silently invoked."
  `(HANDLER-BIND ((ERROR #'MUFFLE-CERROR))
     ,@body))
#|| ; This works as well, but looks more like a hack.
 (defmacro muffle-cerrors (&body body)
  (let ((old-debugger-hook (gensym)))
    `(LET* ((,old-debugger-hook *DEBUGGER-HOOK*)
            (*DEBUGGER-HOOK*
             (LAMBDA (CONDITION DEBUGGER-HOOK)
               (CONTINUE CONDITION)
               (WHEN ,old-debugger-hook
                 (FUNCALL ,old-debugger-hook CONDITION ,old-debugger-hook)))))
      (PROGN ,@body))))
||#

(defun appease-cerror (condition) (maybe-continue condition t))
(defmacro appease-cerrors (&body body)
  "(APPEASE-CERRORS {form}*) executes the forms, but turns continuable errors
into warnings. A continuable error is signalled again as a warning, then
its CONTINUE restart is invoked."
  `(HANDLER-BIND ((ERROR #'APPEASE-CERROR))
     ,@body))

(defun exitunconditionally (condition)
  (terpri *error-output*)
  (write-string "*** - " *error-output*)
  (print-condition condition *error-output*)
  (exit t))                     ; exit Lisp with error
(defun exitonerror (condition)
  (unless (find-restart 'CONTINUE condition)
    (exitunconditionally condition)))
(defmacro exit-on-error (&body body)
  "(EXIT-ON-ERROR {form}*) executes the forms, but exits Lisp if a
non-continuable error or a Ctrl-C interrupt occurs."
  `(HANDLER-BIND ((INTERRUPT-CONDITION #'EXITUNCONDITIONALLY)
                  (SERIOUS-CONDITION #'EXITONERROR))
    ,@body))

(defmacro batchmode-errors (&body body)
  "(SYSTEM::BATCHMODE-ERRORS {form}*) executes the forms, but handles errors
just as a batch program should do: continuable errors are signalled as
warnings, non-continuable errors and Ctrl-C interrupts cause Lisp to exit."
  `(EXIT-ON-ERROR (APPEASE-CERRORS ,@body)))

