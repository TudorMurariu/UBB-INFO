;;;; Common Lisp Object System for CLISP
;;;; Bruno Haible 21.8.1993 -- 2002
;;;; Sam Steingold 1998 - 2002
;;;; German comments translated into English: Stefan Kain 2002-04-08

;; to use it: (USE-PACKAGE "CLOS").

(in-package "COMMON-LISP")
(pushnew ':clos *features*)

(in-package "SYSTEM") ; necessary despite DEFPACKAGE!

(defpackage "CLOS"
  (:import-from "EXT" ext:mapcap)
  (:import-from "SYSTEM"
    ;; Import:
    sys::text                   ; for error messages (i18n.d)
    sys::error-of-type          ; defined in error.d
    sys::function-name-p        ; defined in control.d
    sys::function-block-name    ; defined in init.lisp
    sys::gensym-list            ; defined in macros2.lisp
    ;; clos::generic-function-p ; defined in predtype.d
    ;; clos::class-p clos:class-of clos:find-class ; defined in predtype.d
    ;; clos::structure-object-p ; defined in record.d
    ;; clos::std-instance-p clos::allocate-std-instance ; defined in record.d
    ;; clos::%allocate-instance ; defined in record.d
    ;; clos:slot-value clos::set-slot-value ; defined in record.d
    ;; clos:slot-boundp clos:slot-makunbound ; defined in record.d
    ;; clos:slot-exists-p ; defined in record.d
    ;; clos::class-gethash clos::class-tuple-gethash ; defined in hashtabl.d
    compiler::memq compiler::*keyword-package* ; defined in compiler.lisp
    compiler::%generic-function-lambda ; defined in compiler.lisp
    compiler::%optimize-function-lambda ; defined in compiler.lisp
    compiler::make-signature compiler::sig-req-num compiler::sig-opt-num
    compiler::sig-rest-p compiler::sig-keys-p compiler::sig-keywords
    compiler::sig-allow-p compiler::signature ; defined in compiled.lisp
    ;; clos:generic-flet clos:generic-labels ; treated in compiler.lisp
    ;; Export:
    ;; clos::closclass ; property in predtype.d, type.lisp, compiler.lisp
    ;; clos:class      ; used in record.d
    ;; clos:generic-function ; used in type.lisp, compiler.lisp
    ;; clos:standard-generic-function ; used in predtype.d, type.lisp, compiler.lisp
    ;; clos:slot-missing clos:slot-unbound  ; called by record.d
    ;; clos::*make-instance-table*          ; used in record.d
    ;; clos::*reinitialize-instance-table*  ; used in record.d
    ;; clos::initial-reinitialize-instance  ; called by record.d
    ;; clos::initial-initialize-instance    ; called by record.d
    ;; clos::initial-make-instance          ; called by record.d
    ;; clos:print-object                    ; called by io.d
    ;; clos:describe-object                 ; called by user2.lisp
    ;; clos::define-structure-class         ; called by defstruct.lisp
    ;; clos::defstruct-remove-print-object-method ; called by defstruct.lisp
    ;; clos::built-in-class-p               ; called by type.lisp
    ;; clos::subclassp  ; called by type.lisp, used in compiler.lisp
    ;; clos:class-name                      ; used in type.lisp, compiler.lisp
    ;; clos:find-class                      ; used in compiler.lisp
    ;; clos::defgeneric-lambdalist-callinfo ; called by compiler.lisp
    ;; clos::make-generic-function-form     ; called by compiler.lisp
    )) ; defpackage

(in-package "CLOS")

;;; exports: ** also in init.lisp ** !
(export
 '(;; names of functions and macros:
   slot-value slot-boundp slot-makunbound slot-exists-p with-slots
   with-accessors
   find-class class-of defclass defmethod call-next-method next-method-p
   defgeneric generic-function generic-flet generic-labels
   class-name no-applicable-method no-next-method no-primary-method
   find-method add-method remove-method
   compute-applicable-methods method-qualifiers function-keywords
   slot-missing slot-unbound
   print-object describe-object
   make-instance allocate-instance initialize-instance reinitialize-instance
   shared-initialize
   make-load-form make-load-form-saving-slots
   ;; names of classes:
   standard-class structure-class built-in-class
   standard-object structure-object
   generic-function standard-generic-function method standard-method
   ;; other symbols:
   standard)) ; method combination

;;; preliminary remarks:

;; abbreviations:
;; std = standard
;; gf = generic function
;; <...> = (class ...), mostly = (find-class '...)
;; em = effective method


;;; predefined classes:
;; metaclasses:
(defvar <class>)                       ; here <structure-class>
(defvar <standard-class>)              ; here <structure-class>
(defvar <structure-class>)             ; here <structure-class>
(defvar <built-in-class>)              ; here <structure-class>
;; classes:
(defvar <standard-object>)             ; <standard-class>
(defvar <structure-object>)            ; <structure-class>
(defvar <generic-function>)            ; <built-in-class>
(defvar <standard-generic-function>)   ; <built-in-class>
;;(defvar <method>)                     ; here <structure-class>
;;(defvar <standard-method>)            ; here <structure-class>
(defvar <array>)                       ; <built-in-class>
(defvar <bit-vector>)                  ; <built-in-class>
(defvar <character>)                   ; <built-in-class>
(defvar <complex>)                     ; <built-in-class>
(defvar <cons>)                        ; <built-in-class>
(defvar <float>)                       ; <built-in-class>
(defvar <function>)                    ; <built-in-class>
(defvar <hash-table>)                  ; <built-in-class>
(defvar <integer>)                     ; <built-in-class>
(defvar <list>)                        ; <built-in-class>
(defvar <null>)                        ; <built-in-class>
(defvar <number>)                      ; <built-in-class>
(defvar <package>)                     ; <built-in-class>
(defvar <pathname>)                    ; <built-in-class>
#+LOGICAL-PATHNAMES
(defvar <logical-pathname>)            ; <built-in-class>
(defvar <random-state>)                ; <built-in-class>
(defvar <ratio>)                       ; <built-in-class>
(defvar <rational>)                    ; <built-in-class>
(defvar <readtable>)                   ; <built-in-class>
(defvar <real>)                        ; <built-in-class>
(defvar <sequence>)                    ; <built-in-class>
(defvar <stream>)                      ; <built-in-class>
(defvar <file-stream>)                 ; <built-in-class>
(defvar <synonym-stream>)              ; <built-in-class>
(defvar <broadcast-stream>)            ; <built-in-class>
(defvar <concatenated-stream>)         ; <built-in-class>
(defvar <two-way-stream>)              ; <built-in-class>
(defvar <echo-stream>)                 ; <built-in-class>
(defvar <string-stream>)               ; <built-in-class>
(defvar <string>)                      ; <built-in-class>
(defvar <symbol>)                      ; <built-in-class>
(defvar <t>)                           ; <built-in-class>
(defvar <vector>)                      ; <built-in-class>


;;; low-level-representation:

;; in the runtime-system, the type "CLOS-instance" exists.
;; first component is the class.

;; classes are structures of type CLASS,
;;   first component is the metaclass, second component is the name.

;; the "value" of a slot that is unbound, is #<UNBOUND> - what else?

;;; see RECORD.D :
;; (STD-INSTANCE-P obj) tests, if an object is a CLOS-instance.
;; (ALLOCATE-STD-INSTANCE class n) returns a CLOS-instance with Class class
;; and n-1 additional slots.
;;; see IO.D :
;; CLOS-instances are printed via (PRINT-OBJECT object stream) .

;;; global management of classes and their names:

#|| ; see PREDTYPE.D
(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment)) ; what should be the meaning of the environment?
  (unless (symbolp symbol)
    (error-of-type 'type-error
      :datum symbol :expected-type 'symbol
      (TEXT "~S: argument ~S is not a symbol")
      'find-class symbol))
  (let ((class (get symbol 'CLOSCLASS)))
    (if (not (class-p class))
      (if errorp
        (error-of-type 'error
          (TEXT "~S: ~S does not name a class")
          'find-class symbol)
        nil)
      class)))
||#

(defun (setf find-class) (new-value symbol &optional errorp environment)
  (declare (ignore errorp environment)) ; what should be the meaning of environment?
  (unless (symbolp symbol)
    (error-of-type 'type-error
      :datum symbol :expected-type 'symbol
      (TEXT "~S: argument ~S is not a symbol")
      '(setf find-class) symbol))
  (unless (class-p new-value)
    (error-of-type 'type-error
      :datum new-value :expected-type 'class
      (TEXT "~S: ~S is not a class")
      '(setf find-class) new-value))
  (let ((h (get symbol 'CLOSCLASS)))
    (when (class-p h)
      (when (and (built-in-class-p h) (eq (class-name h) symbol)) ; protect structure classes, too??
        (error-of-type 'error
          (TEXT "~S: cannot redefine built-in class ~S")
          '(setf find-class) h))
      (sys::check-redefinition symbol '(setf find-class) "class")
      ;; should we do (setf (class-name h) nil) ??
      ))
  (setf (get symbol 'CLOSCLASS) new-value))

;; (CLASS-OF object) see PREDTYPE.D, uses property CLASS.


;;; slots:

#||
;; The access functions could look like this, if we use
;; SLOT-VALUE-USING-CLASS.

;; access to the slots of objects of the metaclass <standard-class>:
(defun std-slot-value (instance slot-name)
  (declare (compile))
  (let* ((class (class-of instance))
         (slot-location (gethash slot-name (class-slot-location-table class))))
    ((lambda (value)
       (if (eq value unbound)
         (slot-unbound class instance slot-name)
         value))
     (cond ((null slot-location)
            (slot-missing class instance slot-name 'slot-value))
           ((atom slot-location) ; access local slot
            (sys::%record-ref instance slot-location))
           (t ; access shared slot
            (svref (class-shared-slots (car slot-location))
                   (cdr slot-location)))))))
(defun std-setf-slot-value (instance slot-name new-value)
  (let* ((class (class-of instance))
         (slot-location (gethash slot-name (class-slot-location-table class))))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'setf new-value))
          ((atom slot-location) ; access local slot
           (sys::%record-store instance slot-location new-value))
          (t ; access shared slot
           (setf (svref (class-shared-slots (car slot-location))
                        (cdr slot-location))
                 new-value)))))
(defun std-slot-boundp (instance slot-name)
  (declare (compile))
  (let* ((class (class-of instance))
         (slot-location (gethash slot-name (class-slot-location-table class))))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'slot-boundp))
          ((atom slot-location) ; access local slot
           (not (eq (sys::%record-ref instance slot-location) unbound)))
          (t ; access shared slot
           (not (eq (svref (class-shared-slots (car slot-location))
                           (cdr slot-location)) unbound))))))
(defun std-slot-makunbound (instance slot-name)
  (declare (compile))
  (let* ((class (class-of instance))
         (slot-location (gethash slot-name (class-slot-location-table class))))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'slot-makunbound))
          ((atom slot-location) ; access local slot
           (sys::%record-store instance slot-location unbound))
          (t ; access shared slot
           (setf (svref (class-shared-slots (car slot-location))
                        (cdr slot-location))
                 unbound)))))
(defun std-slot-exists-p (instance slot-name)
  (and (gethash slot-name (class-slot-location-table (class-of instance))) t))

;; general access to slots:
(defun slot-value (object slot-name)
  (let ((class (class-of object)))
    ;; treat metaclass <standard-class> separately
    ;; for efficiency reasons and because of bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-slot-value object slot-name)
      (slot-value-using-class class object slot-name))))
(defun (setf slot-value) (new-value object slot-name)
  (let ((class (class-of object)))
    ;; treat metaclass <standard-class> separately
    ;; for efficiency reasons and because of bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-setf-slot-value object slot-name new-value)
      (setf-slot-value-using-class new-value class object slot-name))))
(defun slot-boundp (object slot-name)
  (let ((class (class-of object)))
    ;; treat metaclass <standard-class> separately
    ;; for efficiency reasons and because of bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-slot-boundp object slot-name)
      (slot-boundp-using-class class object slot-name))))
(defun slot-makunbound (object slot-name)
  (let ((class (class-of object)))
    ;; treat metaclass <standard-class> separately
    ;; for efficiency reasons and because of bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-slot-makunbound object slot-name)
      (slot-makunbound-using-class class object slot-name))))
(defun slot-exists-p (object slot-name)
  (let ((class (class-of object)))
    ;; treat metaclass <standard-class> separately
    ;; for efficiency reasons and because of bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-slot-exists-p object slot-name)
      (slot-exists-p-using-class class object slot-name))))

(defun slot-value-using-class (class object slot-name)
  (no-slot-error class object slot-name))
(defun setf-slot-value-using-class (new-value class object slot-name)
  (declare (ignore new-value))
  (no-slot-error class object slot-name))
(defun slot-boundp-using-class (class object slot-name)
  (no-slot-error class object slot-name))
(defun slot-makunbound-using-class (class object slot-name)
  (no-slot-error class object slot-name))
(defun slot-exists-p-using-class (class object slot-name)
  (no-slot-error class object slot-name))

(defun no-slot-error (class object slot-name)
  (declare (ignore slot-name))
  (error-of-type 'error
    (TEXT "instance ~S of class ~S has no slots (wrong metaclass)")
    object class))
||#

;; For efficiency - we want to circumvent the test for <standard-class> -,
;; all classes (no matter if standard- or built-in-) get a
;; slot-location-table.
;; Furthermore, we can deal here with unbound only very badly.
;; Hence,
;;   slot-value, set-slot-value, slot-boundp, slot-makunbound, slot-exists-p
;; are now contained already in RECORD.D.

(defsetf slot-value set-slot-value)

;; WITH-SLOTS
(defmacro with-slots (slot-entries instance-form &body body &environment env)
  (let ((vars '())
        (slots '()))
    (unless (listp slot-entries)
      (error-of-type 'sys::source-program-error
        (TEXT "~S: not a list of slots: ~S")
        'with-slots slot-entries))
    (dolist (slot slot-entries)
      (let ((var slot))
        (when (consp slot)
          (unless (eql (length slot) 2)
            (error-of-type 'sys::source-program-error
              (TEXT "~S: invalid slot and variable specification ~S")
              'with-slots slot))
          (setq var (first slot) slot (second slot))
          (unless (symbolp var)
            (error-of-type 'sys::source-program-error
              (TEXT "~S: variable ~S should be a symbol")
              'with-slots var)))
        (unless (symbolp slot)
          (error-of-type 'sys::source-program-error
            (TEXT "~S: slot name ~S should be a symbol")
            'with-slots slot))
        (push var vars)
        (push slot slots)))
    (multiple-value-bind (body-rest declarations)
        (sys::parse-body body nil env)
      (let ((instance-var (gensym)))
        `(LET ((,instance-var ,instance-form))
           (SYMBOL-MACROLET
             ,(mapcar #'(lambda (var slot)
                          `(,var (SLOT-VALUE ,instance-var ',slot)))
                      (nreverse vars) (nreverse slots))
             ,@(if declarations `((DECLARE ,@declarations)))
             ,@body-rest))))))

;; WITH-ACCESSORS
(defmacro with-accessors (slot-entries instance-form &body body
                          &environment env)
  (unless (listp slot-entries)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: not a list of slots: ~S")
      'with-accessors slot-entries))
  (dolist (slot-entry slot-entries)
    (unless (and (consp slot-entry) (eql (length slot-entry) 2))
      (error-of-type 'sys::source-program-error
        (TEXT "~S: invalid slot and accessor specification ~S")
        'with-accessors slot-entry))
    (unless (symbolp (first slot-entry))
      (error-of-type 'sys::source-program-error
        (TEXT "~S: variable ~S should be a symbol")
        'with-accessors (first slot-entry)))
    (unless (symbolp (second slot-entry))
      (error-of-type 'sys::source-program-error
        (TEXT "~S: accessor name ~S should be a symbol")
        'with-accessors (second slot-entry))))
  (multiple-value-bind (body-rest declarations) (sys::parse-body body nil env)
    (let ((instance-var (gensym)))
      `(LET ((,instance-var ,instance-form))
         (SYMBOL-MACROLET
           ,(mapcar #'(lambda (slot-entry)
                        `(,(first slot-entry)
                           (,(second slot-entry) ,instance-var)))
                    slot-entries)
           ,@(if declarations `((DECLARE ,@declarations)))
           ,@body-rest)))))


;;; Classes

; for bootstrapping
(eval-when (compile load eval)
  (defun define-structure-class (name) (declare (ignore name)) ) ; preliminary
  (defun defstruct-remove-print-object-method (name) (declare (ignore name)) ) ; preliminary
)
;; wipe out all traces of an earlier loaded CLOS
(eval-when (load eval)
  (do-all-symbols (s) (remprop s 'CLOSCLASS)))

(defconstant empty-ht (make-hash-table :test #'eq :size 0))

(defstruct (class (:predicate nil))
  metaclass ; (class-of class) = (class-metaclass class), a class
  classname ; (class-name class) = (class-classname class), a symbol
  direct-superclasses ; list of all direct superclasses
  all-superclasses ; hash table of all superclasses (incl. the class itself)
  precedence-list ; ordered list of all superclasses (with the class itself first)
  (slot-location-table empty-ht)) ; hash table slotname -> location of the slot

(defstruct (built-in-class (:inherit class) (:conc-name "CLASS-")))
(proclaim '(notinline built-in-class-p))

(defstruct (slotted-class (:inherit class) (:predicate nil) (:copier nil)
                          (:conc-name "CLASS-"))
  slots                    ; list of all slots (as slot-definitions)
  default-initargs         ; default-initargs (as alist initarg -> initer)
  valid-initargs           ; list of valid initargs
  instance-size)           ; number of slots of the direct instances + 1

(defstruct (structure-class (:inherit slotted-class) (:conc-name "CLASS-"))
  names)                       ; encoding of the include-nesting, a list

(defstruct (standard-class (:inherit slotted-class) (:conc-name "CLASS-"))
  shared-slots             ; simple-vector with the values of all shared slots, or nil
  direct-slots             ; list of all freshly added slots (as plists)
  direct-default-initargs) ; freshly added default-initargs (as plist)

;; access to slots of instances of the class <class> by means of the
;; defstruct-accessors, hence no bootstrapping-problems here.

;; continue bootstrapping
(%defclos
  ;; distinctive mark for CLASS-P
  (svref (get 'class 'sys::defstruct-description) 0)
  ;; built-in-classes for CLASS-OF
  (vector 'array 'bit-vector 'character 'complex 'cons 'float 'function
          'hash-table 'integer 'null 'package 'pathname
          #+LOGICAL-PATHNAMES 'logical-pathname
          'random-state 'ratio 'readtable 'standard-generic-function
          'stream 'file-stream 'synonym-stream 'broadcast-stream
          'concatenated-stream 'two-way-stream 'echo-stream 'string-stream
          'string 'symbol 't 'vector))

;;; DEFCLASS

(defmacro defclass (name superclass-specs slot-specs &rest options)
  (unless (symbolp name)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: class name ~S should be a symbol")
      'defclass name))
  (let* ((superclass-forms
           (progn
             (unless (listp superclass-specs)
               (error-of-type 'sys::source-program-error
                 (TEXT "~S ~S: expecting list of superclasses instead of ~S")
                 'defclass name superclass-specs))
             (mapcar #'(lambda (superclass)
                         (unless (symbolp superclass)
                           (error-of-type 'sys::source-program-error
                             (TEXT "~S ~S: superclass name ~S should be a symbol")
                             'defclass name superclass))
                         `(FIND-CLASS ',superclass))
                     superclass-specs)))
         (accessor-def-forms '())
         (slot-forms
           (let ((slot-names '()))
             (unless (listp slot-specs)
               (error-of-type 'sys::source-program-error
                 (TEXT "~S ~S: expecting list of slot specifications instead of ~S")
                 'defclass name slot-specs))
             (mapcar #'(lambda (slot-spec)
                         (let ((slot-name slot-spec) (slot-options '()))
                           (when (consp slot-spec)
                             (setq slot-name (car slot-spec)
                                   slot-options (cdr slot-spec)))
                           (unless (symbolp slot-name)
                             (error-of-type 'sys::source-program-error
                               (TEXT "~S ~S: slot name ~S should be a symbol")
                               'defclass name slot-name))
                           (if (memq slot-name slot-names)
                             (error-of-type 'sys::source-program-error
                               (TEXT "~S ~S: There may be only one direct slot with the name ~S.")
                               'defclass name slot-name)
                             (push slot-name slot-names))
                           (let ((accessors '())
                                 (readers '())
                                 (writers '())
                                 (allocation '())
                                 (initargs '())
                                 (initform nil) (initer nil)
                                 (types '())
                                 (documentation nil))
                             (when (oddp (length slot-options))
                               (error-of-type 'sys::source-program-error
                                 (TEXT "~S ~S: slot options for slot ~S must come in pairs")
                                 'defclass name slot-name))
                             (do ((optionsr slot-options (cddr optionsr)))
                                 ((atom optionsr))
                               (let ((optionkey (first optionsr))
                                     (argument (second optionsr)))
                                 (case optionkey
                                   ((:READER :WRITER)
                                    (unless (function-name-p argument)
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a function name")
                                        'defclass name slot-name argument))
                                    (case optionkey
                                      (:READER (push argument readers))
                                      (:WRITER (push argument writers))))
                                   (:ACCESSOR
                                    (unless (symbolp argument)
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a symbol")
                                        'defclass name slot-name argument))
                                    (push argument accessors)
                                    (push argument readers)
                                    (push `(SETF ,argument) writers))
                                   (:ALLOCATION
                                    (when allocation
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':allocation slot-name))
                                    (case argument
                                      ((:INSTANCE :CLASS) (setq allocation argument))
                                      (t (error-of-type 'sys::source-program-error
                                           (TEXT "~S ~S, slot option for slot ~S must have the value ~S or ~S, not ~S")
                                           'defclass name slot-name ':instance ':class argument))))
                                   (:INITARG
                                    (unless (symbolp argument)
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a symbol")
                                        'defclass name slot-name argument))
                                    (push argument initargs))
                                   (:INITFORM
                                    (when initform
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':initform slot-name))
                                    (setq initform `(QUOTE ,argument)
                                          initer (make-initer argument)))
                                   (:TYPE
                                    (when types
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':type slot-name))
                                    (setq types (list argument)))
                                   (:DOCUMENTATION
                                    (when documentation
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':documentation slot-name))
                                    (unless (stringp argument)
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a string")
                                        'defclass name slot-name argument))
                                    (setq documentation argument))
                                   (t
                                     (error-of-type 'sys::source-program-error
                                       (TEXT "~S ~S, slot option for slot ~S: ~S is not a valid slot option")
                                       'defclass name slot-name optionkey)))))
                             (setq readers (nreverse readers))
                             (setq writers (nreverse writers))
                             (dolist (funname readers)
                               (push `(DEFMETHOD ,funname ((OBJECT ,name))
                                        (SLOT-VALUE OBJECT ',slot-name))
                                     accessor-def-forms))
                             (dolist (funname writers)
                               (push `(DEFMETHOD ,funname (NEW-VALUE (OBJECT ,name))
                                        (SETF (SLOT-VALUE OBJECT ',slot-name) NEW-VALUE))
                                     accessor-def-forms))
                             `(LIST
                                :NAME ',slot-name
                                ,@(when accessors `(:ACCESSORS ',(nreverse accessors)))
                                ,@(when readers `(:READERS ',readers))
                                ,@(when writers `(:WRITERS ',writers))
                                ,@(when (eq allocation ':class) `(:ALLOCATION :CLASS))
                                ,@(when initargs `(:INITARGS ',(nreverse initargs)))
                                ,@(when initform `(#| :INITFORM ,initform |# :INITER ,initer))
                                ,@(when types `(:TYPE ',(first types)))
                                ,@(when documentation `(:DOCUMENTATION ',documentation))))))
                     slot-specs))))
    `(LET ()
       (EVAL-WHEN (COMPILE LOAD EVAL)
         (ENSURE-CLASS
           ',name
           :DIRECT-SUPERCLASSES (LIST ,@superclass-forms)
           :DIRECT-SLOTS (LIST ,@slot-forms)
           ,@(let ((metaclass nil)
                   (direct-default-initargs nil)
                   (documentation nil))
               (dolist (option options)
                 (block nil
                   (when (listp option)
                     (let ((optionkey (first option)))
                       (when (case optionkey
                               (:METACLASS metaclass)
                               (:DEFAULT-INITARGS direct-default-initargs)
                               (:DOCUMENTATION documentation))
                         (error-of-type 'sys::source-program-error
                           (TEXT "~S ~S, option ~S may only be given once")
                           'defclass name optionkey))
                       (case optionkey
                         (:METACLASS
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (unless (symbolp argument)
                                (error-of-type 'sys::source-program-error
                                  (TEXT "~S ~S, option ~S: ~S is not a symbol")
                                  'defclass name option argument))
                              (setq metaclass `(:METACLASS (FIND-CLASS ',argument))))
                            (return)))
                         (:DEFAULT-INITARGS
                          (let ((list (rest option)))
                            (when (and (consp list) (null (cdr list)) (listp (car list)))
                              (setq list (car list))
                              (warn (TEXT "~S ~S: option ~S should be written ~S")
                                    'defclass name option (cons ':DEFAULT-INITARGS list)))
                            (when (oddp (length list))
                              (error-of-type 'sys::source-program-error
                                (TEXT "~S ~S, option ~S: arguments must come in pairs")
                                'defclass name option))
                            (setq direct-default-initargs
                                  `(:DIRECT-DEFAULT-INITARGS
                                    (LIST
                                     ,@(let ((arglist nil) (formlist nil))
                                         (do ((list list (cddr list)))
                                             ((atom list))
                                           (unless (symbolp (first list))
                                             (error-of-type 'sys::source-program-error
                                               (TEXT "~S ~S, option ~S: ~S is not a symbol")
                                               'defclass name option (first list)))
                                           (when (member (first list) arglist)
                                             (error-of-type 'sys::source-program-error
                                               (TEXT "~S ~S, option ~S: ~S may only be given once")
                                               'defclass name option (first list)))
                                           (push (first list) arglist)
                                           (push (second list) formlist))
                                         (mapcan #'(lambda (arg form)
                                                     `(',arg ,(make-initer form)))
                                                 (nreverse arglist) (nreverse formlist)))))))
                          (return))
                         (:DOCUMENTATION
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (unless (stringp argument)
                                (error-of-type 'sys::source-program-error
                                  (TEXT "~S ~S, option ~S: ~S is not a string")
                                  'defclass name option argument))
                              (setq documentation
                                    `(:DOCUMENTATION ',argument)))
                            (return))))))
                   (error-of-type 'sys::source-program-error
                     (TEXT "~S ~S: invalid option ~S")
                     'defclass name option)))
               `(,@metaclass ,@direct-default-initargs ,@documentation))))
       ,@(nreverse accessor-def-forms) ; the DEFMETHODs
       (FIND-CLASS ',name))))
;; At runtime, an initer is - in order to save function calls -
;; in general a cons (init-function . nil),
;; for constants it is (nil . init-value).
(defun make-initer (form)
  (if (constantp form)
    `(CONS 'NIL ,form)
    `(CONS (FUNCTION (LAMBDA () ,form)) 'NIL)))

;; DEFCLASS-execution:

;; information of a slot that is still significant at runtime:
(defstruct (slot-definition
             (:conc-name "SLOTDEF-")
             (:type vector) (:predicate nil) (:copier nil) (:constructor nil))
  (name nil :type symbol)
  (initargs '() :type list)
  (location nil :type (or null integer cons))
  (initer nil :type (or null cons)))
(defstruct (standard-slot-definition
             (:inherit slot-definition) (:conc-name "SLOTDEF-")
             (:type vector) (:predicate nil)
             (:constructor make-standard-slot-definition
                           (name allocation initargs location initer)))
  (allocation :instance :type (or (member :class :instance) class)))

(defun make-slotdef (&key name (allocation ':instance) (initargs '()) location
                     (initer nil) (initform nil) (accessors '()) (readers '())
                     (writers '()) type documentation)
  (declare (ignore initform accessors readers writers type documentation))
  (make-standard-slot-definition name allocation initargs location initer))

#||
;; defstruct.lisp essentially contains the following.
;; In record.d and here we exploit that the first four attributes match!
(defstruct (structure-slot-definition
             (:include slot-definition) (:conc-name "DS-SLOT-")
             (:type vector) (:predicate nil)
             (:constructor make-ds-slot (name offset location initer
                                         default type readonly)))
  ;;(name nil :type symbol)         ; ds-slot-name = slotdef-name !!
  ;;(initargs '() :type list)       ; ds-slot-initargs = slotdef-initargs !!
  ;;(offset nil :type (or null integer)) ; ds-slot-offset = slotdef-location !!
  ;;(initer nil :type (or null cons))    ; ds-slot-initer = slotdef-initer !!
  (default nil)                         ; ds-slot-default
  (type nil)                            ; ds-slot-type
  (readonly nil))                       ; ds-slot-readonly
||#

(defun ensure-class (name &rest all-keys
                          &key (metaclass <standard-class>)
                               (direct-superclasses '())
                               (direct-slots '())
                               (direct-default-initargs '())
                               (documentation nil)
                          &allow-other-keys)
  (let ((class (find-class name nil)))
    (when class
      ;; The only modifications that we permit for classes are those,
      ;; that can occur when doubly loading the same code:
      ;; changed slot-options :initform, :documentation,
      ;; changed class-options :default-initargs, :documentation.
      (if (and (eq metaclass <standard-class>)
               (eq metaclass (class-of class))
               (equal direct-superclasses (class-direct-superclasses class))
               (equal-slots direct-slots (class-direct-slots class))
               (equal-default-initargs direct-default-initargs
                                       (class-direct-default-initargs class)))
        (progn
          ;; store new slot-inits:
          (do ((l-old (class-direct-slots class) (cdr l-old))
               (l-new direct-slots (cdr l-new)))
              ((null l-new))
            (let ((old (getf (car l-old) ':initer))
                  (new (getf (car l-new) ':initer)))
              (when old
                ;; move slot-initer new destructively into the slot-initer old:
                (setf (car old) (car new))
                (setf (cdr old) (cdr new)))))
          ;; store new default-initargs:
          (do ((l-old (class-direct-default-initargs class) (cddr l-old))
               (l-new direct-default-initargs (cddr l-new)))
              ((null l-new))
            (let ((old (second l-old))
                  (new (second l-new)))
              ;; move initer new destructively into the initer old:
              (setf (car old) (car new))
              (setf (cdr old) (cdr new))))
          ;; NB: These modifications are automatically inherited by the
          ;; subclasses of class!
          ;; store new documentation:
          (when documentation (setf (documentation name 'TYPE) documentation))
          ;; modified class as value:
          (return-from ensure-class class))
        (progn
          (warn (TEXT "~S: Class ~S is being redefined, instances are obsolete")
                'defclass name)
          ;; traverse all symbols in order to peruse the subclasses.
          ;; should be more elegant??
          (let ((subclass-names (list name)))
            (do-all-symbols (sym)
              (let ((c (get sym 'CLOSCLASS)))
                (when (and c (subclassp c class))
                  (pushnew sym subclass-names))))
            (dolist (sym subclass-names)
              (let ((c (get sym 'CLOSCLASS)))
                (setf (class-name c) (gensym "OBSOLETE-CLASS-"))
                (remprop sym 'CLOSCLASS)
                (setf (documentation sym 'TYPE) '())))))))
    (when documentation (setf (documentation name 'TYPE) documentation))
    (setf (find-class name)
          (apply (cond ((eq metaclass <standard-class>)
                        #'make-instance-standard-class)
                       ((eq metaclass <built-in-class>)
                        #'make-instance-built-in-class)
                       ((eq metaclass <structure-class>)
                        #'make-instance-structure-class)
                       (t #'make-instance))
                 metaclass
                 :name name
                 all-keys))))
(defun equal-slots (slots1 slots2)
  (or (and (null slots1) (null slots2))
      (and (consp slots1) (consp slots2)
           (equal-slot (first slots1) (first slots2))
           (equal-slots (rest slots1) (rest slots2)))))
(defun equal-slot (slot1 slot2) ; slot1, slot2 Plists
  (or (and (null slot1) (null slot2))
      (and #| (consp slot1) (consp slot2) |#
           (eq (first slot1) (first slot2))
           (or (memq (first slot1)
                     '(#| :initform |# :initer #| :documentation |# ))
               (equal (second slot1) (second slot2)))
           (equal-slot (cddr slot1) (cddr slot2)))))
(defun equal-default-initargs (initargs1 initargs2)
  (or (and (null initargs1) (null initargs2))
      (and (consp initargs1) (consp initargs2)
           (eq (first initargs1) (first initargs2))
           (equal-default-initargs (cddr initargs1) (cddr initargs2)))))

(defun add-default-superclass (direct-superclasses default-superclass)
  ;; Sometimes one wants to coerce a certain superclass.
  ;; But it may not be specified twice.
  (if (memq default-superclass direct-superclasses)
    direct-superclasses
    (append direct-superclasses (list default-superclass))))

;; When this is true, all safety checks about the metaclasses
;; of superclasses are omitted.
(defparameter *allow-mixing-metaclasses* nil)

(defun check-metaclass-mix (name direct-superclasses metaclass-test metaclass)
  (unless *allow-mixing-metaclasses*
    (unless (every metaclass-test direct-superclasses)
      (error-of-type 'error
        (TEXT "(~S ~S): superclass ~S should be of class ~S")
        'DEFCLASS name (find-if-not metaclass-test direct-superclasses)
        metaclass))))

;; creation of an instance of <standard-class>:

(let (unbound) (declare (compile)) ; unbound = #<unbound>
(defun def-unbound (x) (declare (compile)) (setq unbound x))
(defun make-instance-standard-class
    (metaclass &rest args
     &key name (direct-superclasses '()) (direct-slots '())
     (direct-default-initargs '()) &allow-other-keys)
  ;; metaclass = <standard-class>
  (declare (ignore direct-superclasses direct-slots direct-default-initargs))
  (let ((class (make-standard-class :classname name :metaclass metaclass)))
    (apply #'initialize-instance-standard-class class args)))
(defun initialize-instance-standard-class
    (class &key name (direct-superclasses '()) (direct-slots '())
     (direct-default-initargs '()) &allow-other-keys)
  ;; metaclass <= <standard-class>
  (check-metaclass-mix name direct-superclasses
                       #'standard-class-p 'STANDARD-CLASS)
  (setf (class-direct-superclasses class) (copy-list direct-superclasses))
  (setf (class-precedence-list class)
        (std-compute-cpl class
          (add-default-superclass direct-superclasses <standard-object>)))
  (setf (class-all-superclasses class)
        (std-compute-superclasses (class-precedence-list class)))
  (setf (class-direct-slots class) direct-slots)
  (setf (class-slots class) (std-compute-slots class))
  (setf (class-slot-location-table class) (make-hash-table :test #'eq))
  (setf (class-instance-size class) 1) ; Index 0 is occupied by the class
  (let ((shared-index (std-layout-slots class (class-slots class))))
    (when (plusp shared-index)
      (setf (class-shared-slots class)
            (let ((v (make-array shared-index))
                  (i 0))
              (mapc #'(lambda (slot)
                        (when (eq (slotdef-allocation slot) class)
                          (setf (svref v i)
                            (let ((init (slotdef-initer slot)))
                              (if init
                                (if (car init) (funcall (car init)) (cdr init))
                                unbound)))
                          (incf i)))
                    (class-slots class))
              v))))
  (setf (class-direct-default-initargs class) direct-default-initargs)
  (setf (class-default-initargs class) ; 28.1.3.3.
        (remove-duplicates
          (mapcan
            #'(lambda (c)
                (when (standard-class-p c)
                  (plist-to-alist (class-direct-default-initargs c))))
            (class-precedence-list class))
          :key #'car
          :from-end t))
  (setf (class-valid-initargs class)
        (remove-duplicates (mapcap #'slotdef-initargs (class-slots class))))
  (system::note-new-standard-class)
  class)
) ; let

;;; 28.1.5. Determining the Class Precedence List

;; The set of all classes forms a directed graph: Class C is located
;; below the direct superclasses of C. This graph is acyclic, because
;; at the moment of definition of the class C all direct superclasses must
;; already be present.

;; Hence, one can use Noether Induction (Induction from above to below in
;; the class graph) .

;; For a class C let DS(n) be the list of all direct superclasses of C.
;; The set of all superclasses (incl. C itself) is inductively defined as
;; S(C) := {C} union union_{D in DS(C)} S(D).

;; In other words:
;; S(C) = { C_n : C_n in DS(C_{n-1}), ..., C_1 in DS(C_0), C_0 = C }

;; Lemma 1: (a) C in S(C).
;;          (b) DS(C) subset S(C).
;;          (c) D in DS(C) ==> S(D) subset S(C).
;;          (d) D in S(C) ==> S(D) subset S(C).
;; proof:  (a) follows from the definition.
;;         (b) from (a) and from the definition.
;;         (c) from the definition.
;;         (d) from (c) with fixed D via induction over C.

;; The CPL of a class C is one order of set S(C).
;; If CPL(C) = (... D1 ... D2 ...), one writes D1 < D2.
;; The relation introduced by this is a total order upon S(C).
;; The following set of restrictions has to be taken into account:
;; R(C) := union_{D in S(C)} DR(D)  with
;; DR(C) := { C < C1, C1 < C2, ..., C{n-1} < C_n } if DS(C) = (C1, ..., Cn).
;; If R(C) contains a cycle, R(C) cannot be completed into a total order,
;; of course. Then, R(C) is called inconsistent.
;; CPL(C) is constructed as follows:
;;   L := (), R := R(C).
;;   L := (L | C), remove all (C < ..) from R.
;;   while R /= {}, deal with the set M of all minimal elements of R
;;     (those classes, that can be added to L without violating R(C) ).
;;     If M is empty, then there is a cycle in R(C) and
;;     the algorithm is finished. Else, choose that element among the
;;     elements E of M, which has a D being rightmost in L with
;;     E in DS(D) .
;;     L := (L | E), remove all (E < ..) from R.
;;   CPL(C) := L.
;; L is lengthened stepwise by one element, R is shortened stepwise,
;; and R always consists solely of relations between elements
;; of S(C)\L.

;; Lemma 2: (a) CPL(C) = (C ...).
;;          (b) If DS(C) = (C1, ..., Cn), then
;;              CPL(C) = (C ... C1 ... C2 ... ... Cn ...).
;; proof:  (a) obvious by construction.
;;         (b) If Ci is added to the CPL, then the restriction
;;             C{i-1} < Ci can no longer be in R, so C{i-1} must already be
;;             in the CPL.

;; The following statement is wrong:
;; (*) If D is in DS(C) and CPL(D) = (D1, ..., Dn), then
;;     CPL(C) = (C ... D1 ... D2 ... ... Dn ...).
;; Example:
;;     z
;;    /|\             CPL(z) = (z)
;;   / | \            CPL(x) = (x z)
;;  x  |  x           CPL(y) = (y z)
;;  |  |  |           CPL(d) = (d x z)
;;  d  y  e           CPL(e) = (e x z)
;;   \/ \/            CPL(b) = (b d x y z)
;;   b   c            CPL(c) = (c y e x z)
;;    \ /             CPL(a) = (a b d c y e x z)
;;     a
;;                    CPL(a) does not contain CPL(b) !

#||
(defclass z () ())
(defclass x (z) ())
(defclass y (z) ())
(defclass d (x z) ())
(defclass e (x z) ())
(defclass b (d y) ())
(defclass c (y e) ())
(defclass a (b c) ())
(mapcar #'find-class '(z x y d e b c a))
||#

(defun std-compute-cpl (class direct-superclasses)
  (let* ((superclasses ; list of all superclasses in any order
          (remove-duplicates
           (mapcap #'class-precedence-list direct-superclasses)))
         (L '())
         (R1 (list (cons class direct-superclasses)))
         (R2 (mapcar #'(lambda (D) (cons D (class-direct-superclasses D)))
                     superclasses)))
    (loop
      ;; L is the reversed, so far constructed CPL.
      ;; R1 is the list of the so far relevant restrictions, in the form
      ;; R1 = (... (Dj ... Dn) ...) if from DR(D) = (D1 ... Dn) only
      ;; Dj,...,Dn is left over. The order in R1 corresponds to that in L.
      ;; R2 is the list of all so far irrelevant restrictions.
      (when (null R1)
        (return)) ; R1 = R2 = () -> finished
      (let ((M (remove-duplicates (mapcar #'first R1) :from-end t)))
        (setq M (remove-if #'(lambda (E)
                               (or (dolist (r R1 nil)
                                     (when (member E (cdr r)) (return t)))
                                   (dolist (r R2 nil)
                                     (when (member E (cdr r)) (return t)))))
                           M))
        (when (null M)
          (error-of-type 'error
            (TEXT "~S ~S: inconsistent precedence graph, cycle ~S")
            'defclass (class-classname class)
            ;; find cycle: advance to ever smaller elements
            ;; with aid of the restrictions.
            (let* ((R0 (append R1 R2))
                   (cycle (list (car (first R0)))))
              (loop
                (let* ((last (car cycle))
                       (next (dolist (r R0 nil)
                               (when (member last (cdr r))
                                 (return (nth (position last (cdr r)) r))))))
                  (when (null next)
                    ;; last is now apparently a minimal element, after all!
                    (return '??))
                  (when (member next cycle)
                    (setf (cdr (member next cycle)) nil)
                    (return cycle))
                  (push next cycle))))))
        (let ((E (first M)))
          (push E L)
          (push (assoc E R2) R1)
          (setq R2 (delete E R2 :key #'first))
          (mapl #'(lambda (r) (when (eq (first (car r)) E) (pop (car r)))) R1)
          (setq R1 (delete-if #'null R1)))))
    (setq L (nreverse L))
    ;; Test, if L is compatible with the CPL(D), D in direct-superclasses:
    (mapc #'(lambda (D)
              (unless ; Is (class-precedence-list D) sublist of L ?
                  (do ((CL L)
                       (DL (class-precedence-list D) (cdr DL)))
                      ((null DL) t)
                    (when (null (setq CL (member (car DL) CL))) (return nil)))
                (warn (TEXT "(class-precedence-list ~S) and (class-precedence-list ~S) are inconsistent")
                      class D)))
          direct-superclasses)
    L))

;; stuff all superclasses (from the precedence-list) into a hash-table.
(defun std-compute-superclasses (precedence-list)
  (let ((ht (make-hash-table :test #'eq)))
    (mapc #'(lambda (superclass) (setf (gethash superclass ht) t))
          precedence-list)
    ht))

;; auxiliary function (p1 v1 ... pn vn) -> ((p1 . v1) ... (pn . vn))
(defun plist-to-alist (pl &aux (al '()))
  (loop
    (when (null pl) (return))
    (setq al (acons (first pl) (second pl) al))
    (setq pl (cddr pl)))
  (nreverse al))

;; auxiliary function ((p1 . v1) ... (pn . vn)) -> (p1 v1 ... pn vn)
(defun alist-to-plist (al)
  (mapcan #'(lambda (pv) (list (car pv) (cdr pv))) al))

;; 28.1.3.2. Inheritance of Slots and Slot Options

(defun std-compute-slots (class &optional (more-direct-slots '()))
  ;; Gather all slot-specifiers, ordered by precedence:
  (let ((all-slots
         (mapcan
          #'(lambda (c)
              (mapcar #'(lambda (slot)
                          (setq slot (plist-to-alist slot))
                          (when (eq (cdr (assoc ':allocation slot)) ':class)
                            (setf (cdr (assoc ':allocation slot)) c))
                          slot)
                      (append
                       (if (standard-class-p c) (class-direct-slots c))
                       (if (eq c class) more-direct-slots))))
          (class-precedence-list class))))
    ;; partition by slot-names:
    (setq all-slots
          (let ((ht (make-hash-table :test #'eq)))
            (dolist (slot all-slots)
              (assert (eq (caar slot) ':name))
              (push (cdr slot) (gethash (cdar slot) ht nil)))
            (let ((L nil))
              (maphash #'(lambda (name slots)
                           (push (cons name (nreverse slots)) L))
                       ht)
              L))) ; not (nreverse L), because maphash reverses the order
    ;; all-slots is now a list of lists of the form
    ;; (name most-specific-slotspec ... least-specific-slotspec).
    (mapcar
     #'(lambda (slot)
         (let ((name (car slot))
               (slotspecs (cdr slot)))
           (apply #'make-slotdef
                  :name name
                  (alist-to-plist
                   `(,(or (assoc ':allocation (first slotspecs))
                          `(:allocation . :instance))
                      #||
                      ,@(let ((accessors
                               (mapcap #'(lambda (slotspec) (cdr (assoc ':accessors slotspec)))
                                       slotspecs)))
                             (if accessors `((:accessors . ,accessors))))
                      ||#
                      ,@(let ((initargs
                               (remove-duplicates
                                (mapcap #'(lambda (slotspec)
                                            (cdr (assoc ':initargs slotspec)))
                                        slotspecs)
                                :from-end t)))
                             (if initargs `((:initargs . ,initargs))))
                      ,@(dolist (slotspec slotspecs '())
                          (when (assoc ':initer slotspec)
                            (return `(#| ,(assoc ':initform slotspec) |#
                                      ,(assoc ':initer slotspec)))))
                      #||
                      ,(let ((types '()))
                            (dolist (slotspec slotspecs)
                              (when (assoc ':type slotspec)
                                (push (cdr (assoc ':type slotspec)) types)))
                            `(:type . ,(if types `(AND ,@(nreverse types)) 'T)))
                      ||#
                      #||
                      ,@(dolist (slotspec slotspecs '())
                          (when (assoc ':documentation slotspec)
                            (return `(,(assoc ':documentation slotspec)))))
                      ||#
                      )))))
            all-slots)))

;; Allocation of local and shared slots

;; Add the local and shared slots to the slot-location-table ht,
;; incrementing the instance-size, and return the new shared-size.
(defun std-layout-slots (class slots)
  (let ((ht (class-slot-location-table class))
        (local-index (class-instance-size class))
        (shared-index 0))
    (mapc #'(lambda (slot)
              (let* ((name (slotdef-name slot))
                     (allocation (slotdef-allocation slot))
                     (location
                       (cond ((eq allocation ':instance) ; local slot
                              (prog1 local-index (incf local-index)))
                             ((eq allocation class) ; new shared slot
                              (prog1 (cons class shared-index)
                                (incf shared-index)))
                             (t ; inherited shared slot
                              (gethash name (class-slot-location-table
                                             allocation))))))
                (setf (slotdef-location slot) location)
                (setf (gethash name ht) location)))
          slots)
    (setf (class-instance-size class) local-index)
    shared-index))

;; creation of an instance of <built-in-class>:

(defun make-instance-built-in-class
    (metaclass &key name (direct-superclasses '()) &allow-other-keys)
  ;; metaclass = <built-in-class>
  (check-metaclass-mix name direct-superclasses
                       #'built-in-class-p 'BUILT-IN-CLASS)
  (let ((class (make-built-in-class :classname name :metaclass metaclass)))
    (setf (class-direct-superclasses class) (copy-list direct-superclasses))
    (setf (class-precedence-list class)
          (std-compute-cpl class direct-superclasses))
    (setf (class-all-superclasses class)
          (std-compute-superclasses (class-precedence-list class)))
    class))


;; creation of an instance of <structure-class>:

(defun make-instance-structure-class
    (metaclass &rest args
     &key name (direct-superclasses '())
     ;; The following keys come from ENSURE-CLASS.
     (direct-slots '()) (direct-default-initargs '())
     ;; The following keys come from DEFINE-STRUCTURE-CLASS.
     names (slots '()) (size 1) &allow-other-keys)
  ;; metaclass = <structure-class>
  (declare (ignore direct-superclasses direct-slots direct-default-initargs
                   names slots size))
  (let ((class (make-structure-class :classname name :metaclass metaclass)))
    (apply #'initialize-instance-structure-class class args)))
(defun initialize-instance-structure-class
    (class &key name (direct-superclasses '())
     ;; The following keys come from ENSURE-CLASS.
     (direct-slots '()) (direct-default-initargs '())
     ;; The following keys come from DEFINE-STRUCTURE-CLASS.
     names (slots '()) (size 1) &allow-other-keys)
  ;; metaclass <= <structure-class>
  (unless (null (cdr direct-superclasses))
    (error-of-type 'error
      (TEXT "(~S ~S): metaclass ~S forbids more than one direct superclass")
      'DEFCLASS name 'STRUCTURE-CLASS))
  (check-metaclass-mix name direct-superclasses
                       #'structure-class-p 'STRUCTURE-CLASS)
  (setf (class-direct-superclasses class) (copy-list direct-superclasses))
  (setf (class-precedence-list class)
        (std-compute-cpl class
          (add-default-superclass
            (add-default-superclass direct-superclasses
                                    <structure-object>)
                                    <t>)))
  (setf (class-all-superclasses class)
        (std-compute-superclasses (class-precedence-list class)))
  ;; When called via ENSURE-CLASS, we have to do inheritance of slots.
  (unless names
    (setq names
          (cons name
                (if direct-superclasses
                    (class-names (first direct-superclasses)) '())))
    (when direct-superclasses
      (setq slots (class-slots (first direct-superclasses)))
      (setq size (class-instance-size (first direct-superclasses)))))
  (setf (class-slot-location-table class)
        (make-hash-table :test #'eq
          :initial-contents
            (mapcar #'(lambda (slot)
                        (cons (slotdef-name slot) (slotdef-location slot)))
                    slots)))
  (setf (class-instance-size class) size)
  (setf (class-slots class) slots)
  ;; When called via ENSURE-CLASS,
  ;; we may have to treat additional direct slots.
  (when direct-slots
    (let* ((more-slots (std-compute-slots class direct-slots))
           (shared-index (std-layout-slots class more-slots)))
      (when (plusp shared-index)
        (error-of-type 'error
          (TEXT "(~S ~S): metaclass ~S does not support shared slots")
          'DEFCLASS name 'STRUCTURE-CLASS))
      (setf (class-slots class) (append (class-slots class) more-slots))))
  (setf (class-default-initargs class)
        (remove-duplicates
          (append (plist-to-alist direct-default-initargs)
                  (mapcap
                    #'(lambda (c)
                        (when (structure-class-p c)
                          (class-default-initargs c)))
                    (cdr (class-precedence-list class))))
          :key #'car
          :from-end t))
  (setf (class-valid-initargs class)
        (remove-duplicates (mapcap #'slotdef-initargs (class-slots class))))
  (setf (class-names class) names)
  (system::note-new-structure-class)
  class)

;; DEFSTRUCT-Hook
(defun define-structure-class (name)
  (let ((descr (get name 'sys::defstruct-description)))
    (when descr
      (let* ((names (svref descr 0))
             (all-slots (svref descr 3))
             (slots (remove-if-not #'sys::ds-slot-name all-slots)))
        (setf (find-class name)
              (make-instance-structure-class
               <structure-class> :name name
               :direct-superclasses
               (if (cdr names) (list (find-class (second names))) '())
               :names names :slots slots
               :size (if all-slots
                         (1+ (sys::ds-slot-offset (car (last all-slots))))
                         1)))))))

;; Bootstrapping
(progn
  ;; 1. class <t>
  (setq <t>
        (make-instance-built-in-class nil :name 't :direct-superclasses '()))
  ;; 2. classes <structure-class> and <structure-object>
  (setq <structure-class> (make-structure-class)) ; Dummy, so that (setf find-class) works
  (setq <structure-object> <t>)
  (setq <structure-object>
        (make-instance-structure-class
         <structure-class> :name 'structure-object
         :direct-superclasses '()
         :names (svref (get 'structure-object 'sys::defstruct-description) 0)))
  (setf (find-class 'structure-object) <structure-object>)
  (setq <class> (define-structure-class 'class))
  (let ((<slotted-class> (define-structure-class 'slotted-class)))
    (setq <structure-class> (define-structure-class 'structure-class))
    (setf (class-metaclass <structure-object>) <structure-class>)
    (setf (class-metaclass <class>) <structure-class>)
    (setf (class-metaclass <slotted-class>) <structure-class>)
    (setf (class-metaclass <structure-class>) <structure-class>))
  ;; 3. all structure-classes
  (labels ((define-structure-class-with-includes (name)
             (when (get name 'sys::defstruct-description)
               (unless (find-class name nil)
                 (let ((names (svref (get name 'sys::defstruct-description)
                                     0)))
                   (when (cdr names)
                     (define-structure-class-with-includes (second names))))
                 (define-structure-class name)))))
    (do-all-symbols (s) (define-structure-class-with-includes s)))
  ;; 4. classes <standard-class>, <built-in-class>
  (setq <standard-class> (find-class 'standard-class))
  (setq <built-in-class> (find-class 'built-in-class))
  ;; 5. finish class <t>
  (setf (class-metaclass <t>) <built-in-class>)
  (setf (find-class 't) <t>)
  ;; 6. class <standard-object>
  (setq <standard-object>
        (make-standard-class
          :classname 'standard-object
          :metaclass <standard-class>
          :direct-superclasses `(,<t>)
          :direct-slots '()
          :slots '()
          :slot-location-table empty-ht
          :instance-size 1
          :direct-default-initargs nil
          :default-initargs nil))
  (setf (class-all-superclasses <standard-object>)
        (std-compute-superclasses
          (setf (class-precedence-list <standard-object>)
                `(,<standard-object> ,<t>))))
  (setf (find-class 'standard-object) <standard-object>)
  (system::note-new-standard-class)
  ;; 7. value #<unbound>
  (def-unbound
    (sys::%record-ref (allocate-std-instance <standard-object> 2) 1)))


;; 28.1.4. Integrating Types and Classes
(defun subclassp (class1 class2)
  (values
   (gethash class2 (class-all-superclasses class1)))) ; T or (default) NIL

;;; install built-in-classes
;; table 28-1, CLtL2 p. 783
(macrolet ((def (&rest classes &aux (new (car (last classes))))
             (let ((name (intern (string-trim "<>" (symbol-name new)))))
               `(setf (find-class ',name)
                  (setq ,new
                    (make-instance-built-in-class
                     <built-in-class> :name ',name
                     :direct-superclasses
                     (list ,@(cdr (reverse classes)))))))))
 ;(def <t>)
  (def <t> <character>)
  (def <t> <function>)
  (def     <function> <generic-function>)
  (def                <generic-function> <standard-generic-function>)
  (def <t> <hash-table>)
  (def <t> <package>)
  (def <t> <pathname>)
  #+LOGICAL-PATHNAMES
  (def     <pathname> <logical-pathname>)
  (def <t> <random-state>)
  (def <t> <readtable>)
  (def <t> <stream>)
  (def     <stream> <file-stream>)
  (def     <stream> <synonym-stream>)
  (def     <stream> <broadcast-stream>)
  (def     <stream> <concatenated-stream>)
  (def     <stream> <two-way-stream>)
  (def     <stream> <echo-stream>)
  (def     <stream> <string-stream>)
  (def <t> <symbol>)
  (def <t> <sequence>)
  (def     <sequence> <list>)
  (def                <list> <cons>)
  (def                <list> <symbol> <null>)
  (def <t>            <array>)
  (def     <sequence> <array> <vector>)
  (def                        <vector> <bit-vector>)
  (def                        <vector> <string>)
  (def <t> <number>)
  (def     <number> <complex>)
  (def     <number> <real>)
  (def              <real> <float>)
  (def              <real> <rational>)
  (def                     <rational> <ratio>)
  (def                     <rational> <integer>)
)

;; continue bootstrapping
(%defclos
  ;; distinctive mark for CLASS-P
  (svref (get 'class 'sys::defstruct-description) 0)
  ;; built-in-classes for CLASS-OF
  (vector <array> <bit-vector> <character> <complex> <cons> <float> <function>
          <hash-table> <integer> <null> <package> <pathname>
          #+LOGICAL-PATHNAMES <logical-pathname>
          <random-state> <ratio> <readtable> <standard-generic-function>
          <stream> <file-stream> <synonym-stream> <broadcast-stream>
          <concatenated-stream> <two-way-stream> <echo-stream> <string-stream>
          <string> <symbol> <t> <vector>))

;;; intersection of two built-in-classes:
;; deviations from the single-inheritance are only
;; (AND <sequence> <array>) = <vector> and (AND <list> <symbol>) = <null>.
(defun bc-p (class)
  (or (built-in-class-p class)
      (eq class <standard-object>)
      (eq class <structure-object>)))
(defun bc-and (class1 class2) ; returns (AND class1 class2)
  (cond ((subclassp class1 class2) class1)
        ((subclassp class2 class1) class2)
        ((or (and (subclassp <sequence> class1) (subclassp <array> class2))
             (and (subclassp <sequence> class2) (subclassp <array> class1)))
         <vector>)
        ((or (and (subclassp <list> class1) (subclassp <symbol> class2))
             (and (subclassp <list> class2) (subclassp <symbol> class1)))
         <null>)
        (t nil)))
(defun bc-and-not (class1 class2) ; returns a class c with
                                  ; (AND class1 (NOT class2)) <= c <= class1
  (cond ((subclassp class1 class2) nil)
        ((and (eq class1 <sequence>) (subclassp <vector> class2)) <list>)
        ((and (eq class1 <sequence>) (subclassp <list> class2)) <vector>)
        ((and (eq class1 <list>) (subclassp <null> class2)) <cons>)
        (t class1)))


;;; Methods

(defstruct (method (:predicate nil) (:copier nil) (:constructor nil)))

(defstruct (standard-method (:include method) (:conc-name "STD-METHOD-")) ; (:print-object print-std-method)
  function               ; the function
  wants-next-method-p    ; flag, if the NEXT-METHOD (as function with all
                         ; arguments) resp. NIL is to be passed as first argument
                         ; (= NIL for :BEFORE- and :AFTER-methods)
  parameter-specializers ; list ({class | (EQL object)}*)
  qualifiers             ; list of symbols, e.g. (:before)
  signature              ; signature struct (see sompiler.lisp)
  gf                     ; the generic function, which this method belongs to
                         ; (only for the demand of CALL-NEXT-METHOD and
                         ; NO-NEXT-METHOD)
  initfunction           ; returns - if called - the function
                         ; (only for the demand of ADD-METHOD)
)

;; For CALL-NEXT-METHOD and NO-NEXT-METHOD the generic function must be known.
;; As methods do not belong to certain generic functions in principle
;; (because of ADD-METHOD), we must copy the method at ADD-METHOD.
;; We determine the identity of two copies of the same method
;; by looking at std-method-initfunction. (One could also pass
;; the generic function at each call as first argument to the effective
;; method instead, but this is certainly more inefficient.)

(defun print-std-method (method stream)
  (print-unreadable-object (method stream :type t)
    (dolist (q (std-method-qualifiers method))
      (write q :stream stream)
      (write-char #\Space stream))
    (write (std-method-parameter-specializers method) :stream stream)))

;; auxiliary function: test for lambda-list-marker.
(defun lambda-list-keyword-p (x)
  (memq x lambda-list-keywords))

;;; For DEFMETHOD, DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET,
;;;     GENERIC-LABELS, WITH-ADDED-METHODS
;; caller: symbol
;; funname: function name, symbol or (SETF symbol)
;; description: (qualifier* spec-lambda-list {declaration|docstring}* form*)
;; ==> method-building-form
(defun analyze-method-description (caller funname description env)
  (let ((qualifiers nil))
    (loop
      (when (atom description)
        (error-of-type 'sys::source-program-error
          (TEXT "~S ~S: missing lambda list")
          caller funname ))
      (when (listp (car description)) (return))
      (push (pop description) qualifiers))
    ;; only STANDARD method combination is implemented.
    (cond ((equal qualifiers '()))
          ((equal qualifiers '(:before)))
          ((equal qualifiers '(:after)))
          ((equal qualifiers '(:around)))
          (t (error-of-type 'sys::source-program-error
               (TEXT "STANDARD method combination does not allow the method qualifiers to be ~S")
               (nreverse qualifiers))))
    ;; build lambdalist, extract parameter-specializer and signature:
    (let ((specialized-lambda-list (car description))
          (body (cdr description)))
      (let ((req-vars '())
            (ignorable-req-vars '())
            (spec-list '())
            (req-specializer-forms '()))
        (do ()
            ((or (atom specialized-lambda-list)
                 (lambda-list-keyword-p (car specialized-lambda-list))))
          (let* ((item (pop specialized-lambda-list))
                 (specializer-name
                   (if (atom item)
                     (progn (push item req-vars) 't)
                     (progn
                       (push (first item) req-vars)
                       (push (first item) ignorable-req-vars) ; CLtL2 p. 840 top
                       (second item)))))
            (push specializer-name spec-list)
            (push (if (class-p specializer-name)
                    `',specializer-name
                    (if (and (consp specializer-name)
                             (eq (car specializer-name) 'EQL)
                        )
                      `(LIST 'EQL ,(second specializer-name))
                      `(FIND-CLASS ',specializer-name)))
                  req-specializer-forms )))
        (sys::check-redefinition (cons funname (nreverse spec-list))
                                 caller "method")
        (let* ((reqanz (length req-vars))
               (lambda-list (nreconc req-vars specialized-lambda-list))
               (optanz
                 (let ((h (cdr (memq '&OPTIONAL lambda-list))))
                   (or (position-if #'lambda-list-keyword-p h) (length h))))
               (keyp (not (null (memq '&KEY lambda-list))))
               (restp (or keyp (not (null (memq '&REST lambda-list)))))
               (keywords
                 (mapcar
                   #'(lambda (item)
                       (when (consp item) (setq item (first item)))
                       (if (consp item)
                         (first item)
                         (intern (symbol-name item) *keyword-package*)))
                   (let ((h (cdr (memq '&KEY lambda-list))))
                     (subseq h 0 (position-if #'lambda-list-keyword-p h)))))
               (allowp (and keyp (not (null (memq '&ALLOW-OTHER-KEYS
                                                  lambda-list))))))
          ;; methods have an implicit &allow-other-keys (28.1.6.4.):
          (when (and keyp (not allowp))
            (let ((index (+ (position '&KEY lambda-list :test #'eq) 1 (length keywords))))
              (setq lambda-list
                `(,@(subseq lambda-list 0 index) &ALLOW-OTHER-KEYS
                  ,@(subseq lambda-list index)))))
          (let* ((self (gensym))
                 (wants-next-method-p
                   (or (equal qualifiers '()) (equal qualifiers '(:around))))
                 (compile nil)
                 (lambdabody
                   (multiple-value-bind (body-rest declarations docstring)
                       (sys::parse-body body t env)
                     (declare (ignore docstring))
                     (setq compile (member '(COMPILE) declarations :test #'equal))
                     (when ignorable-req-vars
                       (push `(IGNORABLE ,@(nreverse ignorable-req-vars))
                             declarations))
                     (let ((lambdabody-part1
                            `(,lambda-list
                              ,@(if declarations `((DECLARE ,@declarations)))))
                           (lambdabody-part2
                             (if (eq caller 'generic-function)
                               body-rest
                               ;; implicit block
                               `((BLOCK ,(function-block-name funname)
                                   ,@body-rest)))))
                       (if wants-next-method-p
                         (let ((cont (gensym)) ; variable for the continuation
                               (req-dummies ; list of reqanz dummies (engl. reqanz =required number)
                                (gensym-list reqanz))
                               (rest-dummy (if (or restp (> optanz 0)) (gensym)))
                               (lambda-expr `(LAMBDA ,@lambdabody-part1 ,@lambdabody-part2)))
                           `(; new lambda-list:
                             (,cont
                              ,@req-dummies
                              ,@(if rest-dummy `(&REST ,rest-dummy) '()))
                             (SYSTEM::FUNCTION-MACRO-LET
                               ((CALL-NEXT-METHOD
                                  ((&REST NEW-ARGS)
                                   (IF NEW-ARGS
                                     (IF ,cont
                                       ;; Let's do argument checking in the interpreter only
                                       (IF (EVAL-WHEN (EVAL) T)
                                         (%CALL-NEXT-METHOD
                                           ,self
                                           ,cont
                                           ,(if rest-dummy
                                              `(LIST* ,@req-dummies ,rest-dummy)
                                              `(LIST ,@req-dummies))
                                           NEW-ARGS)
                                         (APPLY ,cont NEW-ARGS))
                                       (APPLY (FUNCTION %NO-NEXT-METHOD) ,self NEW-ARGS))
                                     ,(if rest-dummy
                                        `(IF ,cont
                                           (APPLY ,cont ,@req-dummies ,rest-dummy)
                                           (APPLY (FUNCTION %NO-NEXT-METHOD) ,self ,@req-dummies ,rest-dummy))
                                        `(IF ,cont
                                           (FUNCALL ,cont ,@req-dummies)
                                           (%NO-NEXT-METHOD ,self ,@req-dummies)))))
                                  ((&REST NEW-ARG-EXPRS)
                                   (IF NEW-ARG-EXPRS
                                     (LIST 'IF ',cont
                                       ;; Let's do argument checking in the interpreter only
                                       (LIST 'IF '(EVAL-WHEN (EVAL) T)
                                         (LIST '%CALL-NEXT-METHOD
                                           ',self
                                           ',cont
                                           (LIST ',(if rest-dummy 'LIST* 'LIST)
                                             ,@(mapcar #'(lambda (x) `',x) req-dummies)
                                             ,@(if rest-dummy `(',rest-dummy) '()))
                                           (CONS 'LIST NEW-ARG-EXPRS))
                                         (LIST* 'FUNCALL ',cont NEW-ARG-EXPRS))
                                       (LIST* '%NO-NEXT-METHOD ',self NEW-ARG-EXPRS))
                                     ,(if rest-dummy
                                        `(LIST 'IF ',cont
                                           (LIST 'APPLY ',cont
                                             ,@(mapcar #'(lambda (x) `',x) req-dummies)
                                             ',rest-dummy)
                                           (LIST 'APPLY '(FUNCTION %NO-NEXT-METHOD)
                                             ',self
                                             ,@(mapcar #'(lambda (x) `',x) req-dummies)
                                             ',rest-dummy))
                                        `(LIST 'IF ',cont
                                           (LIST 'FUNCALL ',cont
                                             ,@(mapcar #'(lambda (x) `',x) req-dummies))
                                           (LIST '%NO-NEXT-METHOD
                                             ',self
                                             ,@(mapcar #'(lambda (x) `',x) req-dummies)))))))
                                (NEXT-METHOD-P
                                  (() ,cont)
                                  (() ',cont)))
                               ;; new body:
                               ,(if rest-dummy
                                  `(APPLY (FUNCTION ,lambda-expr)
                                          ,@req-dummies ,rest-dummy)
                                  `(,lambda-expr ,@req-dummies)))))
                         (let ((errorform1
                                `(ERROR-OF-TYPE 'PROGRAM-ERROR
                                   (TEXT "~S ~S: ~S is invalid within ~S methods")
                                   ',caller ',funname 'CALL-NEXT-METHOD ',(first qualifiers)))
                               (errorform2
                                `(ERROR-OF-TYPE 'PROGRAM-ERROR
                                   (TEXT "~S ~S: ~S is invalid within ~S methods")
                                   ',caller ',funname 'NEXT-METHOD-P ',(first qualifiers))))
                           `(,@lambdabody-part1
                             (SYSTEM::FUNCTION-MACRO-LET
                              ((CALL-NEXT-METHOD
                                ((&REST NEW-ARGS)
                                 (DECLARE (IGNORE NEW-ARGS))
                                 ,errorform1) ; error at run time
                                ((&REST NEW-ARG-EXPRS)
                                 (DECLARE (IGNORE NEW-ARG-EXPRS))
                                 ,errorform1)) ; error at macroexpansion time
                               (NEXT-METHOD-P
                                (() ,errorform2) ; error at run time
                                (() ,errorform2))) ; error at macroexpansion time
                              ,@lambdabody-part2)))))))
                 (sig (make-signature :req-num reqanz :opt-num optanz
                                      :rest-p restp :keys-p keyp
                                      :keywords keywords :allow-p allowp)))
            (values
            `(MAKE-STANDARD-METHOD
               :INITFUNCTION
                 #'(LAMBDA (,self)
                     ,@(if compile '((DECLARE (COMPILE))))
                     (%OPTIMIZE-FUNCTION-LAMBDA
                       ,(if wants-next-method-p `(T) `())
                       ,@lambdabody))
               :WANTS-NEXT-METHOD-P ',wants-next-method-p
               :PARAMETER-SPECIALIZERS
               (LIST ,@(nreverse req-specializer-forms))
               :QUALIFIERS ',qualifiers
               :SIGNATURE ,sig)
            sig)))))))

;; 28.1.6.3. agreement on parameter specializers and qualifiers
(defun methods-agree-p (method1 method2)
  (and (equal (std-method-qualifiers method1) (std-method-qualifiers method2))
       (specializers-agree-p (std-method-parameter-specializers method1)
                             (std-method-parameter-specializers method2))))
(defun specializers-agree-p (specializers1 specializers2)
  (and (eql (length specializers1) (length specializers2))
       (every #'same-specializers-p specializers1 specializers2)))
(defun same-specializers-p (parspec1 parspec2)
  (or ;; two equal classes?
      (eq parspec1 parspec2)
      ;; two equal EQL-specializers?
      (and (consp parspec1) (consp parspec2)
           (eql (second parspec1) (second parspec2)))))

;; 28.1.6.2. applicable methods
(defun method-applicable-p (method required-arguments)
  (every #'typep required-arguments
         (std-method-parameter-specializers method)))

;; 28.1.7.1. sorting the applicable methods by precedence order
(defun sort-applicable-methods (methods required-arguments argument-order)
  (sort (copy-list methods)
        #'(lambda (method1 method2) ; method1 < method2 ?
            (let ((specializers1 (std-method-parameter-specializers method1))
                  (specializers2 (std-method-parameter-specializers method2)))
              (dolist (arg-index argument-order nil)
                (let ((arg (nth arg-index required-arguments))
                      (psp1 (nth arg-index specializers1))
                      (psp2 (nth arg-index specializers2)))
                  (if (consp psp1)
                    (if (consp psp2)
                      nil         ; (EQL x) = (EQL x)
                      (return t)) ; (EQL x) < <class>  ==>  method1 < method2
                    (if (consp psp2)
                      (return nil) ; <class> > (EQL x)   ==>  method1 > method2
                      ;; two classes: compare the position in the CPL of arg:
                      (let* ((cpl (class-precedence-list (class-of arg)))
                             (pos1 (position psp1 cpl))
                             (pos2 (position psp2 cpl)))
                        (cond ((< pos1 pos2) (return t)) ; method1 < method2
                              ((> pos1 pos2) (return nil)) ; method1 > method2
                              ))))))))))

;; For STANDARD method combination:
;; partition the methods according to qualifiers
(defun partition-method-list (methods)
  (let ((primary-methods '())
        (before-methods '())
        (after-methods '())
        (around-methods '()))
    (dolist (method methods)
      (let ((quals (std-method-qualifiers method)))
        (cond ((equal quals '())        (push method primary-methods))
              ((equal quals '(:before)) (push method before-methods))
              ((equal quals '(:after))  (push method after-methods))
              ((equal quals '(:around)) (push method around-methods)))))
    (values
      (nreverse primary-methods)
      (nreverse before-methods)
      (nreverse after-methods)
      (nreverse around-methods))))


;;; Generic Functions

;; low-level-representation:
;; Compiled functions (Cclosures), for which a certain bit is set in
;; the flag-byte of the code-vector. Additionally behind it:
;; - the signature, a signature struct (see compiler.lisp)
;; - the argument-precedence-order, as list of numbers from 0 to reqanz-1,
;; - the list of all methods.

;; The compiler uses (at GENERIC-FLET, GENERIC-LABELS) and the evaluator
;; presupposes likewise, that a generic function does not change its
;; calling convention.
;; A generic function with signature (reqanz optanz restp keywords allowp)
;; is from the very beginning (!) a compiled function with
;;         reqanz  required parameters
;;         0       optional parameters
;;         &rest if and only if (or (> optanz 0) restp),
;;         without &key.
(defun callinfo (reqanz optanz restp keywords allowp)
  (declare (ignore keywords allowp))
  (list reqanz 0 (or (> optanz 0) restp) nil nil nil))

(defun gf-signature (gf)
  (sys::%record-ref gf 3))
(defun (setf gf-signature) (new gf)
  (setf (sys::%record-ref gf 3) new))

(defun gf-argorder (gf)
  (sys::%record-ref gf 4))
(defun (setf gf-argorder) (new gf)
  (setf (sys::%record-ref gf 4) new))

(defun gf-methods (gf)
  (sys::%record-ref gf 5))
(defun (setf gf-methods) (new gf)
  (setf (sys::%record-ref gf 5) new))

;; The dispatch-code for generic functions is formed with
;; `(%GENERIC-FUNCTION-LAMBDA ,@lambdabody)
;; - similar to `(FUNCTION (LAMBDA ,@lambdabody)) -.
;; The following must not occur therein:
;; - access to dynamic variables, binding of dynamic variables,
;; - nontrivial BLOCK, RETURN-FROM, TAGBODY, GO constructions,
;; - invocation of global functions, that are not inline,
;; - formation of non-autonomous functions (closures).
;; So the following is necessary:
;;   (declare (inline case eql eq typep
;;                    arrayp bit-vector-p characterp complexp consp floatp
;;                    functionp clos::generic-function-p hash-table-p integerp
;;                    listp null numberp packagep pathnamep sys::logical-pathname-p
;;                    random-state-p rationalp readtablep realp sys::sequencep
;;                    clos::std-instance-p streamp sys::file-stream-p
;;                    sys::synonym-stream-p sys::broadcast-stream-p
;;                    sys::concatenated-stream-p sys::two-way-stream-p
;;                    sys::echo-stream-p sys::string-stream-p stringp
;;                    clos::structure-object-p symbolp vectorp
;;                    class-of cons gethash funcall apply ...
;;   )        )

;; returns a generic function without dispatch-code. Not callable!!
(let* ((prototype ; a senseless function
        #'(lambda (&rest args)
            (declare (compile) (ignore args))
            (tagbody 1 (go 1))))
       (prototype-code (sys::%record-ref prototype 1)))
  (defun %make-gf (name signature argorder methods)
    (sys::%make-closure name prototype-code
                        (list nil signature argorder methods))))

#||
(defun make-gf (name lambdabody signature argorder methods)
  (let ((preliminary
         (eval `(LET ()
                  (DECLARE (COMPILE))
                  (%GENERIC-FUNCTION-LAMBDA ,@lambdabody)))))
    (sys::%make-closure
     name
     (sys::closure-codevec preliminary)
     (list
      (sys::%record-ref preliminary 2)
      signature
      argorder
      methods))))
||#


#|| ;; Generic functions with primitive dispatch:

(defun make-slow-gf (name signature argorder methods)
  (let* ((final (%make-gf name signature argorder methods))
         (preliminary
           (eval `(LET ((GF ',final))
                    (DECLARE (COMPILE))
                    (%GENERIC-FUNCTION-LAMBDA (&REST ARGS)
                      (DECLARE (INLINE APPLY))
                      (APPLY 'SLOW-FUNCALL-GF GF ARGS))))))
    (setf (sys::%record-ref final 1) (sys::closure-codevec preliminary))
    (setf (sys::%record-ref final 2) (sys::%record-ref preliminary 2))
    final))

(let* ((prototype
         (let ((gf 'magic))
           (declare (compile))
           (%generic-function-lambda (&rest args)
             (declare (inline apply))
             (apply 'slow-funcall-gf gf args))))
       (prototype-code (sys::%record-ref prototype 1))
       (prototype-consts (sys::%record-ref prototype 2)))
  (defun finalize-slow-gf (gf)
    (setf (sys::%record-ref gf 1) prototype-code)
    (setf (sys::%record-ref gf 2)
          (let ((v (copy-seq prototype-consts)))
            (setf (svref v 0) (substitute gf 'magic (svref v 0)))
            v)))
  (defun gf-never-called-p (gf) (eq (sys::%record-ref gf 1) prototype-code))
  (defun warn-if-gf-already-called (gf) ))

;; call of a generic function
(defun slow-funcall-gf (gf &rest args)
  (let ((reqanz (sig-req-num (gf-signature gf)))
        (arg-order (gf-argorder gf))
        (methods (gf-methods gf)))
    (unless (>= (length args) reqanz)
      (error-of-type 'program-error
        (TEXT "Too few arguments to ~S: ~S")
        gf args))
    (let ((req-args (subseq args 0 reqanz)))
      ;; Determine the effective method:
      ;; 1. Select the applicable methods:
      (setq methods
        (remove-if-not
         #'(lambda (method) (method-applicable-p method req-args))
         methods))
      (when (null methods)
        (return-from slow-funcall-gf (apply #'no-applicable-method gf args))
      )
      ;; 2. Sort the applicable methods by precedence order:
      (setq methods (sort-applicable-methods methods req-args arg-order))
      ;; 3. Apply method combination:
      ;; Only STANDARD method-combination is implemented.
      ;; partition into the distinct method-types:
      (multiple-value-bind
            (primary-methods before-methods after-methods around-methods)
          (partition-method-list methods)
        (when (null primary-methods)
          (return-from slow-funcall-gf (apply #'no-primary-method gf args)))
        ;; combine methods into an "effective method" :
        (labels ((ef-1 (primary-methods before-methods after-methods around-methods)
                   (if (null around-methods)
                     (ef-2 primary-methods before-methods after-methods)
                     (let* ((1method (first around-methods))
                            (1function (std-method-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef
                                 (ef-1 primary-methods before-methods after-methods (rest around-methods))))
                           #'(lambda (&rest args) (apply 1function next-ef args)))
                         #'(lambda (&rest args) (apply 1function args))))))
                 (ef-2 (primary-methods before-methods after-methods)
                   (if (null after-methods)
                     (ef-3 primary-methods before-methods)
                     (let* ((1method (first after-methods))
                            (1function (std-method-function 1method)))
                       (let ((next-ef (ef-2 primary-methods before-methods (rest after-methods))))
                         #'(lambda (&rest args) (multiple-value-prog1 (apply next-ef args) (apply 1function args)))))))
                 (ef-3 (primary-methods before-methods)
                   (if (null before-methods)
                     (ef-4 primary-methods)
                     (let* ((1method (first before-methods))
                            (1function (std-method-function 1method)))
                       (let ((next-ef (ef-3 primary-methods (rest before-methods))))
                         #'(lambda (&rest args) (progn (apply 1function args) (apply next-ef args)))
                 ) ) ) )
                 (ef-4 (primary-methods)
                   (if (null primary-methods)
                     nil ; no function, NEXT-METHOD-P reacts on it
                     (let* ((1method (first primary-methods))
                            (1function (std-method-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef (ef-4 (rest primary-methods))))
                           #'(lambda (&rest args) (apply 1function next-ef args))
                         )
                         #'(lambda (&rest args) (apply 1function args))
                )) ) ) )
          (let ((ef (ef-1 primary-methods before-methods after-methods
                          around-methods)))
            ;; keyword-check (28.1.6.4., 28.1.6.5.) ??
            ;; return effective methode.
            ;; It will then be applied to the arguments:
            ef))))))

||#

(defun gf-sig-restp (sig)
  (or (sig-rest-p sig) (> (sig-opt-num sig) 0)))

;; Generic functions with optimized dispatch:

(defun make-fast-gf (name signature argorder)
  (let ((gf (%make-gf name signature argorder '())))
    (finalize-fast-gf gf)
    gf))

(let ((prototype-table (make-hash-table :test #'equal)))
  (defun finalize-fast-gf (gf)
    (let* ((signature (gf-signature gf))
           (reqanz (sig-req-num signature))
           (restp (gf-sig-restp signature))
           (hash-key (cons reqanz restp))
           (prototype
            (or (gethash hash-key prototype-table)
                (setf (gethash hash-key prototype-table)
                      (let* ((reqvars (gensym-list reqanz))
                             (proto-gf
                              (eval `(LET ((GF 'MAGIC))
                                       (DECLARE (COMPILE))
                                       (%GENERIC-FUNCTION-LAMBDA
                                        (,@reqvars ,@(if restp '(&REST ARGS) '()))
                                        (DECLARE (INLINE FUNCALL) (IGNORABLE ,@reqvars ,@(if restp '(ARGS) '())))
                                        (FUNCALL 'INITIAL-FUNCALL-GF GF))))))
                        ;; we must keep (sys::%record-ref proto-gf 1) .
                        ;; (sys::%record-ref proto-gf 2) = #(NIL INITIAL-FUNCALL-GF MAGIC)
                        (sys::%record-ref proto-gf 1))))))
      (setf (sys::%record-ref gf 1) prototype)
      (setf (sys::%record-ref gf 2) (vector 'NIL 'INITIAL-FUNCALL-GF gf))))
  (defun gf-never-called-p (gf)
    (let* ((signature (gf-signature gf))
           (reqanz (sig-req-num signature))
           (restp (gf-sig-restp signature))
           (hash-key (cons reqanz restp))
           (prototype (gethash hash-key prototype-table)))
      (eq (sys::%record-ref gf 1) prototype)))
  (defvar *dynamically-modifiable-generic-function-names*
    ;; A list of names of functions, which ANSI CL explicitly denotes as
    ;; "Standard Generic Function"s, meaning that the user may add methods.
    '(add-method allocate-instance class-name describe-object find-method
      function-keywords initialize-instance make-instance method-qualifiers
      no-applicable-method no-next-method no-primary-method print-object
      reinitialize-instance remove-method shared-initialize slot-missing
      slot-unbound make-load-form))
  (defvar *warn-if-gf-already-called* t)
  (defun warn-if-gf-already-called (gf)
    (when (and *warn-if-gf-already-called* (not (gf-never-called-p gf))
               (not (memq (sys::%record-ref gf 0)
                          *dynamically-modifiable-generic-function-names*)))
      (warn (TEXT "The generic function ~S is being modified, but has already been called.")
            gf)))
) ; let

;; The actual dispatch-code is calculated at the first call of the
;; function, in order to make successive method-definitions not too
;; expensive.

;; first call of a generic function:
(defun initial-funcall-gf (gf)
  (install-dispatch gf)
  gf)

;; Installs the final dispatch-code into a generic function.
(defun install-dispatch (gf)
  (multiple-value-bind (bindings lambdabody) (compute-dispatch gf)
    (let ((preliminary
           (eval `(LET ,bindings
                    (DECLARE (COMPILE))
                     (%GENERIC-FUNCTION-LAMBDA ,@lambdabody)))))
      (setf (sys::%record-ref gf 1) (sys::%record-ref preliminary 1))
      (setf (sys::%record-ref gf 2) (sys::%record-ref preliminary 2)))))

;; Calculates the dispatch-code of a generic function.
;; It looks as follows:
;; (LAMBDA (variables) ; the required vars separately, everything else with &rest
;;   (DECLARE (INLINE ...)) ; everything inline because of %GENERIC-FUNCTION-LAMBDA
;;   If-cascades, where EQL-parameter-specializers and most of the
;;   builtin-classes are queried online via TYPEP.
;;   CLASS-OF is called for the other required-parameters, the results
;;   are gathered and inserted into a hash table as index. There, the effective
;;   method is located:
;;   (LET ((EM (GETHASH (CONS (CLASS-OF ...) ...) ht1)))
;;     (WHEN EM (RETURN-FROM block (APPLY EM Arguments))))

;;   If that failed:
;;   (APPLY 'COMPUTE-AND-ADD-EFFECTIVE-METHOD gf Arguments)
;; )
;; One does not need to write (APPLY ... Arguments)
;; it is done by %GENERIC-FUNCTION-LAMBDA automatically.
(defun compute-dispatch (gf)
  (let* ((signature (gf-signature gf))
         (req-anz (sig-req-num signature))
         (req-vars (gensym-list req-anz))
         (restp (gf-sig-restp signature))
         (rest-var (if restp (gensym)))
         (apply-fun (if restp 'APPLY 'FUNCALL))
         (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
         (arg-order (gf-argorder gf))
         (methods (gf-methods gf))
         (block-name (gensym))
         (maybe-no-applicable nil)
         (ht-vars '())) ; list of hashtable variables and their inits
    ;; we do a recursion over the arguments.
    (labels
       ((recursion (remaining-args ; an nthcdr of arg-order
                    remaining-methods ; sublist of methods
                    class-of-exprs) ; list of CLASS-OF expressions
          (if (null remaining-methods)
            (progn
              (setq maybe-no-applicable t)
              'NIL) ; nothing to do, call NO-APPLICABLE-METHOD later
            (if (null remaining-args)
              ;; all arguments processed
              #|| ; use GETHASH :
              (let ((ht-var (gensym))
                    (n (length class-of-exprs)) ; index with n-tuples
                    ht-init ; expression for initialization of ht-var
                    ht-key-binding ; binding of a variable to an n-tuple
                    em-expr ; expression for looking up the EM
                    setf-em-expr) ; expression-part for setting the EM
                (if (eql n 0)
                  (setq ht-init 'NIL
                        ht-key-binding '()
                        em-expr ht-var
                        setf-em-expr `(SETQ ,ht-var))
                  (let ((tuple-var (gensym)))
                    (setq ht-init
                          `(MAKE-HASH-TABLE
                             :TEST (FUNCTION ,(if (eql n 1) 'EQ 'EQUAL)))
                          ht-key-binding
                          `((,tuple-var
                             ,(let ((tuple-fun (hash-tuple-function n)))
                                (if (member '&rest (second tuple-fun))
                                  `(,tuple-fun ,@(reverse class-of-exprs))
                                  ;; no &rest -> can optimize
                                  ;; (the compiler is not yet too good at that)
                                  (sublis (mapcar #'cons (second tuple-fun) (reverse class-of-exprs))
                                          (third tuple-fun))))))
                          em-expr
                          `(GETHASH ,tuple-var ,ht-var)
                          setf-em-expr
                          ;; `(SETF (GETHASH ,tuple-var ,ht-var)) would also work;
                          ;; but the following spares two temporary variables:
                          `(SYSTEM::PUTHASH ,tuple-var ,ht-var))))
                (push (list ht-var ht-init) ht-vars)
                `(LET ,ht-key-binding
                   (RETURN-FROM ,block-name
                     (OR ,em-expr
                         (,@setf-em-expr
                               (,apply-fun 'COMPUTE-EFFECTIVE-METHOD ',gf
                                           ,@apply-args))))))
              |# ; use CLASS-GETHASH and CLASS-TUPLE-GETHASH :
              (let ((ht-var (gensym))
                    (n (length class-of-exprs)) ; index with n-tuples
                    ht-init ; expression for initialization of ht-var
                    em-expr ; expression for looking up the EM
                    setf-em-expr) ; expression-part for setting the EM
                (if (eql n 0)
                  (setq ht-init 'NIL
                        em-expr ht-var
                        setf-em-expr `(SETQ ,ht-var))
                  (setq class-of-exprs
                        (reverse class-of-exprs)
                        ht-init
                        `(MAKE-HASH-TABLE
                           :TEST (FUNCTION ,(if (eql n 1) 'EQ 'EQUAL)))
                        em-expr
                        (if (eql n 1) ; whatever is faster
                          ;; `(GETHASH ,@class-of-exprs ,ht-var) ==
                          `(CLASS-GETHASH ,ht-var ,(second (first class-of-exprs)))
                          `(CLASS-TUPLE-GETHASH ,ht-var ,@(mapcar #'second class-of-exprs)))
                        setf-em-expr
                        `(SYSTEM::PUTHASH
                          ,(let ((tuple-fun (hash-tuple-function n)))
                             (if (memq '&rest (second tuple-fun))
                               `(,tuple-fun ,@class-of-exprs)
                               ;; no &rest -> can optimize
                               ;; (the compiler is not yet too good at that)
                               (sublis (mapcar #'cons (second tuple-fun) class-of-exprs)
                                       (third tuple-fun))))
                          ,ht-var)))
                (push (list ht-var ht-init) ht-vars)
                `(RETURN-FROM ,block-name
                   (OR ,em-expr
                       (,@setf-em-expr
                             (,apply-fun 'COMPUTE-EFFECTIVE-METHOD ',gf
                                         ,@apply-args)))))
              ;; process next argument:
              (let* ((arg-index (first remaining-args))
                     (arg-var (nth arg-index req-vars))
                     (eql-cases ; all EQL-specializers for this argument
                       (remove-duplicates
                         (mapcar #'second
                           (remove-if-not #'consp
                             (mapcar #'(lambda (m)
                                         (nth arg-index
                                           (std-method-parameter-specializers m)))
                               remaining-methods)))
                         :test #'eql))
                     (eql-caselist ; case-list for CASE
                       (mapcar
                         #'(lambda (object)
                             `((,object)
                               ,(recursion
                                  (cdr remaining-args)
                                  (remove-if-not
                                    #'(lambda (m)
                                        (typep object
                                          (nth arg-index
                                            (std-method-parameter-specializers m))))
                                    remaining-methods)
                                  class-of-exprs)))
                         eql-cases)))
                ;; until further notice we do not need to consider the
                ;; EQL-methods anymore.
                (setq remaining-methods
                      (remove-if
                        #'(lambda (m)
                            (consp
                              (nth arg-index
                                (std-method-parameter-specializers m))))
                        remaining-methods))
                ((lambda (other-cases)
                   (if eql-caselist
                     `(CASE ,arg-var ,@eql-caselist (T ,other-cases))
                     other-cases))
                 (let ((classes
                         (delete <t>
                           (delete-duplicates
                             (mapcar #'(lambda (m)
                                         (nth arg-index
                                           (std-method-parameter-specializers m)))
                                     remaining-methods)))))
                   ;; If all classes that are to be tested for are
                   ;; built-in-classes, then we will inline the type-dispatch,
                   ;; because in the hierarchy of the built-in-classes
                   ;; (that does not know multiple inheritance except for NULL
                   ;; and VECTOR) all CPLs are consistent.
                   ;; Hence, we can work with
                   ;; (subclassp (class-of obj) class) == (typep obj class)
                   ;; In the other case a hash-table-access is necessary
                   ;; anyway. Then we spare the test for the built-in-
                   ;; classes and include it into the hash-table.
                   (if (and (every #'bc-p classes)
                            (<= (length classes) 5)) ; too many cases -> hash
                     (labels
                        ((built-in-subtree (class remaining-classes remaining-methods)
                           ;; treats the cases, when the argument belongs to
                           ;; the Class class and affiliation to one of the
                           ;; remaining-classes has to be tested.
                           ;; (One can presume that (bc-and class x) /= nil
                           ;; for all x from remaining-classes.)
                           (if (null remaining-classes)
                             ;; case differentiation is no longer necessary
                             (recursion
                               (cdr remaining-args)
                               (remove-if-not
                                 #'(lambda (m)
                                     (bc-and class
                                       (nth arg-index
                                         (std-method-parameter-specializers m))))
                                 remaining-methods)
                               class-of-exprs)
                             ;; case differentiation via TYPEP
                             (let ((test-class (first remaining-classes)))
                               ;; better choose test-class maximal:
                               (loop
                                 (let ((other-class
                                         (find-if
                                           #'(lambda (x)
                                               (and (subclassp test-class x)
                                                    (not (eq test-class x))))
                                           remaining-classes)))
                                   (unless other-class (return))
                                   (setq test-class other-class)))
                               `(IF (TYPEP ,arg-var ',(class-classname test-class))
                                  ,(built-in-subtree
                                     (bc-and class test-class) ; /= nil !
                                     (remove 'nil
                                       (mapcar
                                         #'(lambda (x) (bc-and x test-class))
                                         (remove test-class remaining-classes)))
                                     (remove-if-not
                                       #'(lambda (m)
                                           (bc-and
                                             (nth arg-index
                                               (std-method-parameter-specializers m))
                                             test-class))
                                       remaining-methods))
                                  ,(built-in-subtree
                                     (bc-and-not class test-class) ; /= nil !
                                     (remove 'nil
                                       (mapcar
                                         #'(lambda (x) (bc-and-not x test-class))
                                         remaining-classes))
                                     (remove-if-not
                                       #'(lambda (m)
                                           (bc-and-not
                                             (nth arg-index
                                               (std-method-parameter-specializers m))
                                             test-class))
                                       remaining-methods)))))))
                       (built-in-subtree <t> classes remaining-methods))
                     (recursion
                       (cdr remaining-args)
                       remaining-methods
                       (cons `(CLASS-OF ,arg-var) class-of-exprs))))))))))
      (let ((form (recursion arg-order methods '())))
        (values
          ;; bindings
          (nreverse ht-vars)
          ;; lambdabody
          `((,@req-vars ,@(if restp `(&REST ,rest-var) '()))
            (DECLARE
              (INLINE
               ;; for the case differentiations:
               CASE EQL EQ TYPEP
               ;; at the inline-expansion of TYPEP on built-in-classes:
               ARRAYP BIT-VECTOR-P CHARACTERP COMPLEXP CONSP FLOATP
               FUNCTIONP CLOS::GENERIC-FUNCTION-P HASH-TABLE-P INTEGERP
               LISTP NULL NUMBERP PACKAGEP PATHNAMEP SYS::LOGICAL-PATHNAME-P
               RANDOM-STATE-P RATIONALP READTABLEP REALP SYS::SEQUENCEP
               CLOS::STD-INSTANCE-P STREAMP SYS::FILE-STREAM-P
               SYS::SYNONYM-STREAM-P SYS::BROADCAST-STREAM-P
               SYS::CONCATENATED-STREAM-P SYS::TWO-WAY-STREAM-P
               SYS::ECHO-STREAM-P SYS::STRING-STREAM-P STRINGP
               CLOS::STRUCTURE-OBJECT-P SYMBOLP VECTORP
               ;; looking up and calling of the effective method:
               CLASS-OF CONS GETHASH CLASS-GETHASH CLASS-TUPLE-GETHASH
               SYS::PUTHASH FUNCALL APPLY))
            (BLOCK ,block-name
              ,form
              ,@(if maybe-no-applicable
                  `((,apply-fun 'NO-APPLICABLE-METHOD ',gf
                                ,@apply-args))))))))))

;; Our EQUAL hash-function looks into cons-trees only upto depth 4.
;; A tuple of at most 16 elements can be turned into such a tree.
(defun hash-tuple-function (n) ; n>0
  (case n
    (1 '(lambda (t1) t1))
    (2 '(lambda (t1 t2) (cons t1 t2)))
    (3 '(lambda (t1 t2 t3) (cons t1 (cons t2 t3))))
    (4 '(lambda (t1 t2 t3 t4) (cons (cons t1 t2) (cons t3 t4))))
    (5 '(lambda (t1 t2 t3 t4 t5) (cons (cons t1 t2) (cons t3 (cons t4 t5)))))
    (6 '(lambda (t1 t2 t3 t4 t5 t6)
         (cons (cons t1 t2) (cons (cons t3 t4) (cons t5 t6)))))
    (7 '(lambda (t1 t2 t3 t4 t5 t6 t7)
         (cons (cons t1 (cons t2 t3)) (cons (cons t4 t5) (cons t6 t7)))))
    (8 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8)
         (cons (cons (cons t1 t2) (cons t3 t4))
          (cons (cons t5 t6) (cons t7 t8))) ))
    (9 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9)
         (cons (cons (cons t1 t2) (cons t3 t4))
          (cons (cons t5 t6) (cons t7 (cons t8 t9))))))
    (10 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)
          (cons (cons (cons t1 t2) (cons t3 t4))
           (cons (cons t5 t6) (cons (cons t7 t8) (cons t9 t10))))))
    (11 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11)
           (cons (cons (cons t1 t2) (cons t3 t4))
            (cons (cons t5 (cons t6 t7))
             (cons (cons t8 t9) (cons t10 t11))))))
    (12 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)
           (cons (cons (cons t1 t2) (cons t3 t4))
            (cons (cons (cons t5 t6) (cons t7 t8))
             (cons (cons t9 t10) (cons t11 t12))))))
    (13 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13)
           (cons (cons (cons t1 t2) (cons t3 (cons t4 t5)))
            (cons (cons (cons t6 t7) (cons t8 t9))
             (cons (cons t10 t11) (cons t12 t13))))))
    (14 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14)
           (cons (cons (cons t1 t2) (cons (cons t3 t4) (cons t5 t6)))
            (cons (cons (cons t7 t8) (cons t9 t10))
             (cons (cons t11 t12) (cons t13 t14))))))
    (15 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15)
           (cons (cons (cons t1 (cons t2 t3)) (cons (cons t4 t5) (cons t6 t7)))
            (cons (cons (cons t8 t9) (cons t10 t11))
             (cons (cons t12 t13) (cons t14 t15))))))
    (16 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16)
           (cons (cons (cons (cons t1 t2) (cons t3 t4))
                  (cons (cons t5 t6) (cons t7 t8)))
            (cons (cons (cons t9 t10) (cons t11 t12))
             (cons (cons t13 t14) (cons t15 t16))))))
    (t '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 &rest more-t)
          (cons (cons (cons (cons t1 t2) (cons t3 t4))
                 (cons (cons t5 t6) (cons t7 t8)))
           (cons (cons (cons t9 t10) (cons t11 t12))
            (cons (cons t13 t14) more-t)))))))

;;; Calculate the effective method for the given arguments.
;;; It is actually the effective method for all arguments,
;;; for the same EQL and class restrictions as the given arguments,
;;; therefore compute dispatch is already taken care of.
(defun compute-effective-method (gf &rest args)
  (tagbody restart-compute
    (return-from compute-effective-method
      (let* ((signature (gf-signature gf))
             (req-anz (sig-req-num signature))
             (req-vars (gensym-list req-anz))
             (req-args (subseq args 0 req-anz))
             (restp (gf-sig-restp signature))
             (rest-var (if restp (gensym)))
             (apply-fun (if restp 'APPLY 'FUNCALL))
             (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
             (lambdalist `(,@req-vars ,@(if restp `(&REST ,rest-var) '())))
             (opt-vars '())
             (key-vars '())
             (lambdalist-keypart '())
             (arg-order (gf-argorder gf))
             (methods (gf-methods gf)))
        ;; Determine the effective method:
        ;; 1. Select the applicable methods:
        (setq methods
              (remove-if-not #'(lambda (method)
                                 (method-applicable-p method req-args))
                             methods))
        (when (null methods)
          (apply #'no-applicable-method gf args)
          (go restart-compute))
        ;; 28.1.6.4., 28.1.6.5.: Keyword arguments in generic functions
        (when restp
          ;; The generic function has &REST or &KEY, thus try all methods.
          ;; "If the lambda-list of ... the generic function definition
          ;;  contains &allow-other-keys, all keyword arguments are accepted."
          (unless (sig-allow-p signature)
            ;; "The specific set of keyword arguments accepted ...
            ;;  varies according to the applicable methods."
            (let ((signatures (mapcar #'std-method-signature methods)))
              ;; "A method that has &rest but not &key does not affect the
              ;;   set of acceptable keyword arguments."
              (setq signatures (delete-if-not #'sig-keys-p signatures))
              ;; No method with &key ==> no restriction on the arguments
              (unless (null signatures)
                ;; "If the lambda-list of any applicable method ... contains
                ;;  &allow-other-keys, all keyword arguments are accepted."
                (unless (some #'sig-allow-p signatures)
                  ;; "The set of keyword arguments accepted for a
                  ;;  particular call is the union of the keyword
                  ;;  arguments accepted by all applicable methods and
                  ;;  the keyword arguments mentioned after &key in the
                  ;;  generic function definition."
                  (let ((keywords
                         (remove-duplicates
                          (append (sig-keywords signature)
                                  (mapcap #'sig-keywords signatures))
                            :from-end t)))
                    (setq opt-vars (gensym-list (sig-opt-num signature)))
                    (setq key-vars (gensym-list keywords))
                    (setq lambdalist-keypart
                          `(&KEY
                            ,@(mapcar #'(lambda (kw var) `((,kw ,var)))
                                      keywords key-vars)))))))))
        ;; 2. Sort the applicable methods by precedence order:
        (setq methods (sort-applicable-methods methods req-args arg-order))
        ;; 3. Apply method combination:
        ;; only the STANDARD method combination is implemented.
        ;; split up into individual method types.
        (multiple-value-bind
              (primary-methods before-methods after-methods around-methods)
            (partition-method-list methods)
          (when (null primary-methods)
            (apply #'no-primary-method gf args)
            (go restart-compute))
          ;; combine methods into an "effective method":
          (labels ((ef-1 (primary-methods before-methods after-methods around-methods)
                     (if (null around-methods)
                       (ef-2 primary-methods before-methods after-methods)
                       (let* ((1method (first around-methods))
                              (1function (std-method-function 1method)))
                         (if (std-method-wants-next-method-p 1method)
                           (let ((next-ef
                                     (ef-1 primary-methods before-methods after-methods (rest around-methods))))
                             `(,apply-fun ',1function
                                          #'(LAMBDA ,lambdalist ,next-ef)
                                          ,@apply-args))
                           `(,apply-fun ',1function ,@apply-args)))))
                   (ef-2 (primary-methods before-methods after-methods)
                     (let ((next-ef (ef-3 primary-methods after-methods)))
                       (if (null before-methods)
                         next-ef
                         `(PROGN
                            ,@(mapcar
                                #'(lambda (method)
                                    `(,apply-fun ',(std-method-function method)
                                                 ,@apply-args))
                                before-methods) ; most-specific-first
                            ,next-ef))))
                   (ef-3 (primary-methods after-methods)
                     (let ((next-ef (ef-4 primary-methods)))
                       (if (null after-methods)
                         next-ef
                         `(MULTIPLE-VALUE-PROG1
                            ,next-ef
                            ,@(mapcar
                                #'(lambda (method)
                                    `(,apply-fun ',(std-method-function method)
                                                 ,@apply-args))
                                (reverse after-methods)))))) ; most-specific-last
                   (ef-4 (primary-methods)
                     (let* ((1method (first primary-methods))
                            (1function (std-method-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef-fun (ef-5 (rest primary-methods))))
                           `(,apply-fun ',1function ,next-ef-fun ,@apply-args))
                         `(,apply-fun ',1function ,@apply-args))))
                   (ef-5 (primary-methods)
                     (if (null primary-methods)
                       'NIL ; no function, NEXT-METHOD-P reacts on it
                       `#'(LAMBDA ,lambdalist ,(ef-4 primary-methods)))))
            (let* ((ef-form (ef-1 primary-methods before-methods after-methods around-methods))
                   (ef-fun (if (and (eq (car ef-form) apply-fun)
                                    (equal (cddr ef-form) apply-args)
                                    (null lambdalist-keypart))
                             (cadr ef-form)
                             `#'(LAMBDA
                                  ,@(if (null opt-vars)
                                      `(,(append lambdalist lambdalist-keypart)
                                        ,@(if key-vars `((DECLARE (IGNORE ,@key-vars)))))
                                      `(,lambdalist
                                        (APPLY #'(LAMBDA (&OPTIONAL ,@opt-vars ,@lambdalist-keypart)
                                                   (DECLARE (IGNORE ,@opt-vars ,@key-vars)))
                                               ,rest-var)))
                                  ,ef-form))))
              ; (eval ef-fun)                                 ; interpreted
              ; (eval `(LOCALLY (DECLARE (COMPILE)) ,ef-fun)) ; compiled
              (eval `(LET () (DECLARE (COMPILE) (INLINE FUNCALL APPLY))
                          ,ef-fun)))))))))

; runtime-support for CALL-NEXT-METHOD.
(defun %call-next-method (method next-methods original-args new-args)
  (let* ((gf (std-method-gf method))
         (emf (sys::generic-function-effective-method-function gf))
         (original-em (apply emf original-args))
         (new-em (apply emf new-args)))
    (if (eq original-em new-em)
      (apply next-methods new-args)
      (error-of-type 'error
        (TEXT "~S in ~S: the new arguments ~S have a different effective method than the old arguments ~S")
        'call-next-method gf new-args original-args))))


;; Cruel hack (CLtL2 28.1.9.2., ANSI CL 7.1.2.):
;; - MAKE-INSTANCE must be informed about the methods of ALLOCATE-INSTANCE,
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - INITIALIZE-INSTANCE must be informed about the methods of
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - REINITIALIZE-INSTANCE must be informed about the methods of
;;   REINITIALIZE-INSTANCE and SHARED-INITIALIZE.
(defvar |#'allocate-instance| nil)
(defvar |#'initialize-instance| nil)
(defvar |#'reinitialize-instance| nil)
(defvar |#'shared-initialize| nil)
(defvar *gf-warn-on-replacing-method* t)

;; 28.1.6.4. congruent lambda lists
(defun check-signature-congruence (gf method &optional
                                   (gf-sign (gf-signature gf))
                                   (m-sign (std-method-signature method)))
  (unless (= (sig-req-num m-sign) (sig-req-num gf-sign))
    (error-of-type 'error
      (TEXT "~S has ~D, but ~S has ~D required parameter~:P")
      method (sig-req-num m-sign) gf (sig-req-num gf-sign)))
  (unless (= (sig-opt-num m-sign) (sig-opt-num gf-sign))
    (error-of-type 'error
      (TEXT "~S has ~D, but ~S has ~D optional parameter~:P")
      method (sig-opt-num m-sign) gf (sig-opt-num gf-sign)))
  (when (and (sig-rest-p m-sign) (not (sig-rest-p gf-sign)))
    (error-of-type 'error
      (TEXT "~S accepts &REST or &KEY, but ~S does not.")
      method gf))
  (when (and (sig-rest-p gf-sign) (not (sig-rest-p m-sign)))
    (error-of-type 'error
      (TEXT "~S accepts &REST or &KEY, but ~S does not.")
      gf method))
  (when (sig-keys-p gf-sign)    ; gf has keywords?
    ;; yes ==> method must accept it
    (unless (if (sig-keys-p m-sign)
                (or (sig-allow-p m-sign) ; keywords match
                    (subsetp (sig-keywords gf-sign) (sig-keywords m-sign)))
                (sig-rest-p m-sign)) ; method must have &rest!
      (error-of-type 'error
        (TEXT "~S does not accept the keywords ~S of ~S")
        method (sig-keywords gf-sign) gf))))

;; Add a method to a generic function
(defun std-add-method (gf method)
  (check-signature-congruence gf method)
  ;; copy method, so that one can enter gf:
  (when (std-method-wants-next-method-p method)
    (setq method (copy-standard-method method))
    (setf (std-method-function method) nil)
    (setf (std-method-gf method) gf))
  ;; determine function from initfunction:
  (when (null (std-method-function method))
    (let ((h (funcall (std-method-initfunction method) method)))
      (setf (std-method-function method) (car h))
      (when (car (cdr h)) ; could the variable ",cont" be optimized away?
        (setf (std-method-wants-next-method-p method) nil))))
  ;; method is finished. store:
  (warn-if-gf-already-called gf)
  (let ((old-method (find method (gf-methods gf) :test #'methods-agree-p)))
    (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
          ((eq gf |#'initialize-instance|) (note-ii-change method))
          ((eq gf |#'reinitialize-instance|) (note-ri-change method))
          ((eq gf |#'shared-initialize|) (note-si-change method)))
    (setf (gf-methods gf)
          (cons method
                (if old-method
                  (progn
                    (when *gf-warn-on-replacing-method*
                      (warn (TEXT "Replacing method ~S in ~S")
                            old-method gf))
                    (remove old-method (gf-methods gf)))
                  (gf-methods gf))))
    (finalize-fast-gf gf))
  gf)

;; removal of a method from a generic function:
(defun std-remove-method (gf method)
  (let ((old-method (find (std-method-initfunction method) (gf-methods gf)
                          :key #'std-method-initfunction)))
    (when old-method
      (warn-if-gf-already-called gf)
      (warn (TEXT "Removing method ~S in ~S")
            old-method gf)
      (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
            ((eq gf |#'initialize-instance|) (note-ii-change method))
            ((eq gf |#'reinitialize-instance|) (note-ri-change method))
            ((eq gf |#'shared-initialize|) (note-si-change method)))
      (setf (gf-methods gf) (remove old-method (gf-methods gf)))
      (finalize-fast-gf gf)))
  gf)

;; find a method in a generic function:
(defun std-find-method (gf qualifiers specializers &optional (errorp t))
  ;; so to speak
  ;;   (find hypothetical-method (gf-methods gf) :test #'methods-agree-p)
  ;; cf. methods-agree-p
  (dolist (method (gf-methods gf))
    (when (and (equal (std-method-qualifiers method) qualifiers)
               (specializers-agree-p (std-method-parameter-specializers method)
                                     specializers))
      (return-from std-find-method method)))
  (if errorp
    (error-of-type 'error
      (TEXT "~S has no method with qualifiers ~:S and specializers ~S")
      gf qualifiers specializers)
    nil))


;;; DEFMETHOD

(defmacro defmethod (funname &rest method-description &environment env)
  (unless (function-name-p funname)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      'defmethod funname))
  (multiple-value-bind (method sig)
      (analyze-method-description 'defmethod funname method-description env)
    `(LET ()
      (COMPILER::EVAL-WHEN-COMPILE
       (COMPILER::C-DEFUN ',funname ,sig nil 'defmethod))
      (DO-DEFMETHOD ',funname ,method))))

(defun do-defmethod (funname method)
  (std-add-method
    (if (fboundp funname)
      (let ((gf (fdefinition funname)))
        (if (clos::generic-function-p gf)
          gf
          (error-of-type 'error
            (TEXT "~S does not name a generic function")
            funname)))
      (setf (fdefinition funname)
            (let ((signature (std-method-signature method)))
              (make-fast-gf funname
                            ;; GF signature <== method signature
                            (make-signature
                             :req-num (sig-req-num signature)
                             :opt-num (sig-opt-num signature)
                             :rest-p (sig-rest-p signature))
                            ;; argorder := (0 ... reqanz-1)
                            (countup (sig-req-num signature))))))
    method)
  method)

; n --> list (0 ... n-1)
(defun countup (n)
  (do* ((count n (1- count))
        (l '() (cons count l)))
       ((eql count 0) l)))


;;; For DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS,
;;;     WITH-ADDED-METHODS
;; caller: symbol
;; funname: function name, symbol or (SETF symbol)
;; lambdalist: lambdalist of the generic function
;; options: (option*)
;; --> signature, argorder, method-forms, docstring
(defun analyze-defgeneric (caller funname lambdalist options env)
  (unless (function-name-p funname)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      caller funname lambdalist))
  ;; parse the lambdalist:
  (multiple-value-bind (reqanz req-vars optanz restp keyp keywords allowp)
      (analyze-defgeneric-lambdalist caller funname lambdalist)
    ;; process the options:
    (let ((method-forms '())
          (argorders nil)
          (docstrings nil))
      (dolist (option options)
        (unless (listp option)
          (error-of-type 'sys::source-program-error
            (TEXT "~S ~S: not a ~S option: ~S")
            caller funname 'defgeneric option))
        (case (first option)
          (DECLARE
           ;; the declaration is being ignored.
           ;; the compiler will ignore it in any case.
           (unless (every
                    #'(lambda (x) (and (consp x) (eq (first x) 'OPTIMIZE)))
                    (rest option))
             (warn (TEXT "~S ~S: Only ~S declarations are permitted: ~S")
                   caller funname 'optimize option)))
          (:ARGUMENT-PRECEDENCE-ORDER
           (when argorders
             (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: ~S may only be specified once.")
               caller funname ':argument-precedence-order))
           (setq argorders option))
          (:DOCUMENTATION
           (unless (and (eql (length option) 2) (stringp (second option)))
             (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: A string must be specified after ~S : ~S")
               caller funname ':documentation option))
           (when docstrings
             (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: Only one ~S string is allowed")
               caller funname ':documentation))
           (setq docstrings (rest option)))
          (:METHOD-COMBINATION
           ;; the method combination is being ignored.
           (unless (equal (rest option) '(STANDARD))
             (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: The only valid method combination is ~S : ~S")
               caller funname 'standard option)))
          (:GENERIC-FUNCTION-CLASS
           ;; the class of the generic function is being ignored.
           (unless (equal (rest option) '(STANDARD-GENERIC-FUNCTION))
             (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: The only valid generic function class name is ~S : ~S")
               caller funname 'standard-generic-function option)))
          (:METHOD-CLASS
           ;; the class of the methods is being ignored.
           (unless (equal (rest option) '(STANDARD-METHOD))
             (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: The only valid method class name is ~S : ~S")
               caller funname 'standard-method option)))
          (:METHOD
           (push (analyze-method-description caller funname (rest option) env)
            method-forms))
          (t (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: invalid syntax in ~S option: ~S")
               caller funname 'defstruct option))))
      ;; check :argument-precedence-order :
      (let ((argorder
             (if argorders
               (let ((l (mapcar #'(lambda (x)
                                    (or (position x req-vars)
                                        (error-of-type 'sys::source-program-error
                                          (TEXT "~S ~S: ~S is not one of the required parameters: ~S")
                                          caller funname x argorders)))
                                (rest argorders))))
                 ;; Is (rest argorders) a permutation of req-vars ?
                 ;; In other words: Is the mapping
                 ;;        (rest argorders)  -->  req-vars
                 ;; resp.   l --> {0, ..., reqanz-1}
                 ;; bijective?
                 (unless (apply #'/= l) ; injective?
                   (error-of-type 'sys::source-program-error
                     (TEXT "~S ~S: some variable occurs twice in ~S")
                     caller funname argorders))
                 (unless (eql (length l) reqanz) ; surjective?
                   (error-of-type 'sys::source-program-error
                     (TEXT "~S ~S: ~S is missing some required parameter")
                     caller funname argorders))
                 l)
               (countup reqanz))))
        (values (make-signature :req-num reqanz :opt-num optanz
                                :rest-p restp :keys-p keyp
                                :keywords keywords :allow-p allowp)
                ;; argorder
                argorder
                ;; list of the method-forms
                (nreverse method-forms)
                ;; docstring or nil
                (car docstrings))))))

;; parse the lambdalist:
;; lambdalist --> reqanz, req-vars, optanz, restp, keyp, keywords, allowp
(defun analyze-defgeneric-lambdalist (caller funname lambdalist)
  (let ((req-vars '())
        (optanz 0)
        (restp nil)
        (keyp nil)
        (keywords '())
        (allowp nil))
    (when (some #'(lambda (item) (and (consp item) (cdr item))) lambdalist)
      (error-of-type 'sys::source-program-error
        (TEXT "~S ~S: No initializations are allowed in a generic function lambda-list: ~S")
        caller funname lambdalist))
    (flet ((check-varname (var)
             (unless (symbolp var)
               (error-of-type 'sys::source-program-error
                 (TEXT "~S ~S: variable name ~S should be a symbol")
                 caller funname var))
             (when (memq var req-vars)
               (error-of-type 'sys::source-program-error
                 (TEXT "~S ~S: duplicate variable name ~S")
                 caller funname var))
             var))
      (loop
        (when (or (atom lambdalist) (lambda-list-keyword-p (first lambdalist)))
          (return))
        (push (check-varname (pop lambdalist)) req-vars))
      (when (and (consp lambdalist) (eq (first lambdalist) '&optional))
        (pop lambdalist)
        (loop
          (when (or (atom lambdalist)
                    (lambda-list-keyword-p (first lambdalist)))
            (return))
          (let ((item (pop lambdalist)))
            (check-varname (if (consp item) (first item) item))
            (incf optanz))))
      (when (and (consp lambdalist) (eq (first lambdalist) '&rest)
                 (consp (rest lambdalist)))
        (pop lambdalist)
        (check-varname (pop lambdalist))
        (setq restp t))
      (when (and (consp lambdalist) (eq (first lambdalist) '&key))
        (pop lambdalist)
        (setq restp t keyp t) ; &key implies &rest
        (loop
          (when (or (atom lambdalist)
                    (lambda-list-keyword-p (first lambdalist)))
            (return))
          (let ((item (pop lambdalist)))
            (when (consp item) (setq item (first item)))
            (check-varname (if (consp item) (second item) item))
            (push (if (consp item)
                    (first item)
                    (intern (symbol-name item) *keyword-package*))
                  keywords)))
        (when (and (consp lambdalist)
                   (eq (first lambdalist) '&allow-other-keys))
          (pop lambdalist)
          (setq allowp t))))
    (when lambdalist
      (error-of-type 'sys::source-program-error
        (TEXT "~S ~S: invalid lambda list portion: ~S")
        caller funname lambdalist))
    (values (length req-vars) (nreverse req-vars) optanz
            restp keyp keywords allowp)))

;; transform lambdalist into calling convention:
(defun defgeneric-lambdalist-callinfo (caller funname lambdalist)
  (multiple-value-bind (reqanz req-vars optanz restp keyp keywords allowp)
      (analyze-defgeneric-lambdalist caller funname lambdalist)
    (declare (ignore req-vars keyp))
    (callinfo reqanz optanz restp keywords allowp)))


;;; DEFGENERIC

(defmacro defgeneric (funname lambda-list &rest options &environment env)
  (multiple-value-bind (signature argorder method-forms docstring)
      (analyze-defgeneric 'defgeneric funname lambda-list options env)
    `(LET ()
       (COMPILER::EVAL-WHEN-COMPILE
        (COMPILER::C-DEFUN ',funname ',signature nil 'defgeneric))
       ;; NB: no (SYSTEM::REMOVE-OLD-DEFINITIONS ',funname)
       ,@(if docstring
           (let ((symbolform
                   (if (atom funname)
                     `',funname
                     `(LOAD-TIME-VALUE (SYSTEM::GET-SETF-SYMBOL
                                        ',(second funname))))))
             `((SYSTEM::%SET-DOCUMENTATION ,symbolform 'FUNCTION
                                           ',docstring))))
       (DO-DEFGENERIC ',funname ',signature ',argorder ,@method-forms))))

(defun make-generic-function (funname signature argorder &rest methods)
  (let ((gf (make-fast-gf funname signature argorder)))
    (dolist (method methods) (std-add-method gf method))
    (finalize-fast-gf gf)
    gf))

(defvar *gf-warn-on-removing-all-methods* t)

(defun do-defgeneric (funname signature argorder &rest methods)
  (if (fboundp funname)
    (let ((gf (fdefinition funname)))
      (if (clos::generic-function-p gf)
        ;; redefinition of a generic function
        (progn
          (warn-if-gf-already-called gf)
          (when (and *gf-warn-on-removing-all-methods* (gf-methods gf))
            (warn (TEXT "Removing all methods of ~S") gf)
            (setf (gf-methods gf) nil))
          (unless (and (equalp signature (gf-signature gf))
                       (equal argorder (gf-argorder gf)))
            (warn (TEXT "Modifying the parameter profile of ~S") gf)
            (setf (gf-signature gf) signature)
            (setf (gf-argorder gf) argorder))
          (dolist (method methods) (std-add-method gf method))
          (finalize-fast-gf gf)
          gf)
        (error-of-type 'program-error
          (TEXT "~S does not name a generic function")
          funname)))
    (setf (fdefinition funname)
          (apply #'make-generic-function funname signature argorder methods))))


#||  ;; For GENERIC-FLET, GENERIC-LABELS

;; like make-generic-function, only that the dispatch-code is
;; installed immediately.
(defun make-generic-function-now (funname signature argorder &rest methods)
  (let ((gf (make-fast-gf funname signature argorder)))
    (dolist (method methods) (std-add-method gf method))
    (install-dispatch gf)
    gf))
||#


;; For GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS

(defun make-generic-function-form (caller funname lambda-list options env)
  (multiple-value-bind (signature argorder method-forms docstring)
      (analyze-defgeneric caller funname lambda-list options env)
    (declare (ignore docstring))
    `(MAKE-GENERIC-FUNCTION ',funname ',signature ',argorder ,@method-forms)))


;;; GENERIC-FUNCTION

(defmacro generic-function (lambda-list &rest options &environment env)
  (make-generic-function-form 'generic-function 'LAMBDA
                              lambda-list options env))


;; For GENERIC-FLET, GENERIC-LABELS
(defun analyze-generic-fundefs (caller fundefs env)
  (let ((names '())
        (funforms '()))
    (dolist (fundef fundefs)
      (unless (and (consp fundef) (consp (cdr fundef)))
        (error-of-type 'sys::source-program-error
          (TEXT "~S: ~S is not a generic function specification")
          caller fundef))
      (push (first fundef) names)
      (push (make-generic-function-form caller (first fundef) (second fundef) (cddr fundef) env) funforms))
    (values (nreverse names) (nreverse funforms))))


;;; GENERIC-FLET

(defmacro generic-flet (fundefs &body body &environment env)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-flet fundefs env)
    (let ((varnames (gensym-list funnames)))
      `(LET ,(mapcar #'list varnames funforms)
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args)))
                        varnames funnames)
           ,@body)))))

;;; GENERIC-LABELS

(defmacro generic-labels (fundefs &body body &environment env)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-labels fundefs env)
    (let ((varnames (gensym-list funnames)))
      `(LET ,varnames
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args)))
                        varnames funnames)
           ,@(mapcar #'(lambda (varname funform) `(SETQ ,varname ,funform))
                     varnames funforms)
           ,@body)))))


;;; WITH-ADDED-METHODS
;; is screwed up and therefore will not be implemented.

;;; miscellaneous generic functions that we have retarded so far:

(defgeneric class-name (class)
  (:method ((class class))
    (class-classname class)))

(defgeneric (setf class-name) (new-value class)
  (:method (new-value (class class))
    (unless (symbolp new-value)
      (error-of-type 'type-error
        :datum new-value :expected-type 'symbol
        (TEXT "~S: The name of a class must be a symbol, not ~S")
        '(setf class-name) new-value))
    (when (built-in-class-p class)
      (error-of-type 'error
        (TEXT "~S: The name of the built-in class ~S cannot be modified")
        '(setf class-name) class))
    (setf (class-classname class) new-value)))

;; An argument is called "dispatching" if not all the corresponding parameter
;; specializers are <t>.
(defun dispatching-arg-p (index methods)
  (notevery #'(lambda (method)
                (eq (nth index (std-method-parameter-specializers method))
                    <t>))
            methods))
(defun single-dispatching-arg (reqanz methods)
  (let ((first-dispatching-arg
         (dotimes (i reqanz nil)
           (when (dispatching-arg-p i methods) (return i)))))
    (and first-dispatching-arg
         (do ((i (1+ first-dispatching-arg) (1+ i)))
             ((>= i reqanz) first-dispatching-arg)
           (when (dispatching-arg-p i methods) (return nil))))))
(defun dispatching-arg-type (index methods)
  `(OR ,@(remove-duplicates
           (mapcar #'(lambda (method)
                       (nth index (std-method-parameter-specializers method)))
                   methods)
           :test #'same-specializers-p)))

(defgeneric no-applicable-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((reqanz (sig-req-num (gf-signature gf)))
           (methods (gf-methods gf))
           (dispatching-arg (single-dispatching-arg reqanz methods)))
      (if dispatching-arg
        (error-of-type 'type-error
          :datum (first args)
          :expected-type (dispatching-arg-type dispatching-arg methods)
          (TEXT "~S: When calling ~S with arguments ~S, no method is applicable.")
          'no-applicable-method gf args)
        (error-of-type 'error
          (TEXT "~S: When calling ~S with arguments ~S, no method is applicable.")
          'no-applicable-method gf args)))))

(defgeneric no-primary-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((reqanz (sig-req-num (gf-signature gf)))
           (methods (mapcan #'(lambda (method)
                                (when (equal (std-method-qualifiers method) '())
                                  (list method)))
                            (gf-methods gf)))
           (dispatching-arg (single-dispatching-arg reqanz methods)))
      (if dispatching-arg
        (error-of-type 'type-error
          :datum (first args)
          :expected-type (dispatching-arg-type dispatching-arg methods)
          (TEXT "~S: When calling ~S with arguments ~S, no primary method is applicable.")
          'no-primary-method gf args)
        (error-of-type 'error
          (TEXT "~S: When calling ~S with arguments ~S, no primary method is applicable.")
          'no-primary-method gf args)))))

(defun %no-next-method (method &rest args)
  (apply #'no-next-method (std-method-gf method) method args))
(defgeneric no-next-method (gf method &rest args)
  (:method ((gf standard-generic-function) (method standard-method) &rest args)
    (error-of-type 'error
      (TEXT "~S: When calling ~S with arguments ~S, there is no next method after ~S, and ~S was called.")
      'no-next-method gf args method '(call-next-method))))

(defgeneric find-method (gf qualifiers specializers &optional errorp)
  (:method ((gf standard-generic-function) qualifiers specializers &optional (errorp t))
     (std-find-method gf qualifiers specializers errorp)))

(defgeneric add-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    (std-add-method gf method)))

(defgeneric remove-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    (std-remove-method gf method)))

(defgeneric compute-applicable-methods (gf args)
  (:method ((gf standard-generic-function) args)
    (let ((reqanz (sig-req-num (gf-signature gf)))
          (methods (gf-methods gf)))
      (if (>= (length args) reqanz)
        (let ((req-args (subseq args 0 reqanz)))
          ;; 1. Select the applicable methods:
          (setq methods
            (remove-if-not
              #'(lambda (method) (method-applicable-p method req-args))
              methods))
          ;; 2. Sort the applicable methods by precedence order:
          (sort-applicable-methods methods req-args (gf-argorder gf)))
        nil)))) ; rather no error

(defgeneric method-qualifiers (method)
  (:method ((method standard-method))
    (std-method-qualifiers method)))

(defgeneric function-keywords (method)
  (:method ((method standard-method))
    (let ((sig (std-method-signature method)))
      (values (sig-keywords sig) (sig-allow-p sig)))))

(defgeneric slot-missing (class instance slot-name operation
                          &optional new-value)
  (:method ((class t) instance slot-name operation &optional new-value)
    (declare (ignore instance new-value))
    (error-of-type 'error
      (TEXT "~S: The class ~S has no slot named ~S")
      operation class slot-name)))

(defgeneric slot-unbound (class instance slot-name)
  (:method ((class t) instance slot-name)
    (declare (ignore class))
    (error-of-type 'unbound-slot
      :name slot-name
      :instance instance
      (TEXT "~S: The slot ~S of ~S has no value")
      'slot-value slot-name instance)))

(defgeneric print-object (object stream)
  (:method ((object standard-object) stream)
    (if *print-readably*
        (let ((form (make-init-form object)))
          (if form
              (write (sys::make-load-time-eval form) :stream stream)
              (print-unreadable-object (object stream :type t :identity t))))
        (print-unreadable-object (object stream :type t :identity t))))
  (:method ((object structure-object) stream)
    (system::print-structure object stream))
  (:method ((object class) stream)
    (if *print-readably*
        (write (sys::make-load-time-eval
                `(find-class ',(class-classname object)))
               :stream stream)
        (print-unreadable-object (object stream :type t)
          (write (class-classname object) :stream stream))))
  (:method ((object standard-method) stream)
    (print-std-method object stream)))

;; another DEFSTRUCT-hook
(defun defstruct-remove-print-object-method (name)
  (let ((method (find-method #'print-object nil
                             (list (find-class name) <t>) nil)))
    (when method (remove-method #'print-object method))))

;;; 28.1.9. Object creation and initialization

;; Cruel hack (CLtL2 28.1.9.2., ANSI CL 7.1.2.):
;; - MAKE-INSTANCE must be informed about the methods of ALLOCATE-INSTANCE,
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - INITIALIZE-INSTANCE must be informed about the methods of
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - REINITIALIZE-INSTANCE must be informed about the methods of
;;   REINITIALIZE-INSTANCE and SHARED-INITIALIZE.

(defparameter *make-instance-table* (make-hash-table :test #'eq))
  ;; Hash table, mapping a class to a simple-vector containing
  ;; - a list of valid keyword arguments,
  ;; - the effective method of allocate-instance,
  ;; - the effective method of initialize-instance,
  ;; - the effective method of shared-initialize.

(defparameter *reinitialize-instance-table* (make-hash-table :test #'eq))
  ;; Hash table, mapping a class to a cons containing
  ;; - a list of valid keyword arguments,
  ;; - the effective method of shared-initialize.

(defun note-i-change (specializer table)
  (maphash #'(lambda (class value) (declare (ignore value))
               (when (subclassp class specializer)
                 (remhash class table)))
           table))
(defun note-i-meta-change (meta-specializer table)
  (maphash #'(lambda (class value) (declare (ignore value))
               (when (subclassp (class-of class) meta-specializer) ; <==> (typep class meta-specializer)
                 (remhash class table)))
           table))

(defun note-ai-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    (if (consp specializer)
      ;; EQL-method for ALLOCATE-INSTANCE:
      ;; object must be a class, else worthless
      (let ((specialized-object (second specializer)))
        (when (class-p specialized-object)
          ;; remove the entries from *make-instance-table* for which the
          ;; implied method might be applicable:
          (note-i-change specialized-object *make-instance-table*)))
      ;; remove the entries from *make-instance-table* for which the
      ;; implied method might be applicable:
      (note-i-meta-change specializer *make-instance-table*))))

(defun note-ii-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    ;; EQL-methods for INITIALIZE-INSTANCE are worthless in any case
    (unless (consp specializer)
      ;; remove the entries from *make-instance-table* for which the
      ;; implied method might be applicable:
      (note-i-change specializer *make-instance-table*))))

(defun note-ri-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    ;; EQL-methods for REINITIALIZE-INSTANCE are essentially worthless
    (unless (consp specializer)
      ;; remove the entries from *reinitialize-instance-table* for which the
      ;; implied method might be applicable:
      (note-i-change specializer *reinitialize-instance-table*))))

(defun note-si-change (method)
  (let* ((specializers (std-method-parameter-specializers method))
         (specializer1 (first specializers))
         (specializer2 (second specializers)))
    ;; EQL-methods for SHARED-INITIALIZE are essentially worthless
    (unless (consp specializer1)
      ;; As second argument, INITIALIZE-INSTANCE passes always T .
      (when (typep 'T specializer2)
        ;; remove the entries from *make-instance-table* for which the
        ;; implied method might be applicable:
        (note-i-change specializer1 *make-instance-table*))
      ;; As second argument, REINITIALIZE-INSTANCE passes always NIL .
      (when (typep 'NIL specializer2)
        ;; remove the entries from *reinitialize-instance-table* for which the
        ;; implied method might be applicable:
        (note-i-change specializer1 *reinitialize-instance-table*)))))

;;; collect all keywords from a list of applicable methods
(defun valid-initarg-keywords (class methods)
  (let ((signatures (mapcar #'std-method-signature methods)))
    ;; "A method that has &rest but not &key does not affect the set of
    ;;  acceptable keyword srguments."
    (setq signatures (delete-if-not #'sig-keys-p signatures))
    ;; "The presence of &allow-other-keys in the lambda list of an applicable
    ;;  method disables validity checking of initialization arguments."
    ;; (ANSI CL section 7.1.2)
    (if (some #'sig-allow-p signatures)
      't
      ;; "The keyword name of each keyword parameter specified in the method's
      ;;  lambda-list becomes an initialization argument for all classes for
      ;;  which the method is applicable."
      (remove-duplicates
       (append (class-valid-initargs class) (mapcap #'sig-keywords signatures))
       :from-end t))))

;; NB: On calculation of an effective method, the residual
;; arguments do not count.
;; At the first call of INITIALIZE-INSTANCE or MAKE-INSTANCE of each class
;; we memorize the needed information in *make-instance-table*.

;; For MAKE-INSTANCE the following is necessary as keys:
;; - the initargs that are used for the initialization of slots,
;; - the keywords of methods from SHARED-INITIALIZE,
;; - the keywords of methods from INITIALIZE-INSTANCE,
;; - the keywords of methods from ALLOCATE-INSTANCE.
(defun valid-make-instance-keywords (class)
  (valid-initarg-keywords
    class
    (append ; list of all applicable methods from SHARED-INITIALIZE
     (remove-if-not
      #'(lambda (method)
          (let* ((specializers (std-method-parameter-specializers method))
                 (specializer1 (first specializers))
                 (specializer2 (second specializers)))
            (and (atom specializer1) (subclassp class specializer1)
                 (typep 'T specializer2))))
      (gf-methods |#'shared-initialize|))
     ;; list of all applicable methods from INITIALIZE-INSTANCE
     (remove-if-not
      #'(lambda (method)
          (let ((specializer
                 (first (std-method-parameter-specializers method))))
            (and (atom specializer) (subclassp class specializer))))
      (gf-methods |#'initialize-instance|))
     ;; list of all applicable methods from ALLOCATE-INSTANCE
     (remove-if-not
      #'(lambda (method)
          (let ((specializer (first (std-method-parameter-specializers method))))
            (if (consp specializer)
              (eql class (second specializer))
              (subclassp (class-of class) specializer)))) ; <==> (typep class specializer)
      (gf-methods |#'allocate-instance|)))))
(defun make-instance-table-entry1 (class)
  (values (valid-make-instance-keywords class)
          (compute-effective-method |#'allocate-instance| class)))
(defun make-instance-table-entry2 (instance)
  (values (compute-effective-method |#'initialize-instance| instance)
          (compute-effective-method |#'shared-initialize| instance 'T)))

;; 28.1.9.5., 28.1.9.4.
(defgeneric shared-initialize (instance slot-names &rest initargs))
(setq |#'shared-initialize| #'shared-initialize)
#||
(defmethod shared-initialize ((instance standard-object)
                              slot-names &rest initargs)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slotname (slotdef-name slot)))
      (multiple-value-bind (init-key init-value foundp)
          (get-properties initargs (slotdef-initargs slot))
        (declare (ignore init-key))
        (if foundp
          (setf (slot-value instance slotname) init-value)
          (unless (slot-boundp instance slotname)
            (let ((init (slotdef-initer slot)))
              (when init
                (when (or (eq slot-names 'T) (memq slotname slot-names))
                  (setf (slot-value instance slotname)
                        (if (car init) (funcall (car init))
                            (cdr init)))))))))))
  instance)
||#
;; the main work is done by a SUBR:
(do-defmethod 'shared-initialize
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%shared-initialize '(T)))
   :wants-next-method-p nil
   :parameter-specializers (list (find-class 'standard-object) (find-class 't))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 2 :rest-p t)))
(do-defmethod 'shared-initialize
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%shared-initialize '(T)))
   :wants-next-method-p nil
   :parameter-specializers
   (list (find-class 'structure-object) (find-class 't))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 2 :rest-p t)))

;; 28.1.12.
(defgeneric reinitialize-instance (instance &rest initargs))
(setq |#'reinitialize-instance| #'reinitialize-instance)
#||
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance 'NIL initargs))
||#
#|| ; optimized:
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (let ((h (gethash (class-of instance) *reinitialize-instance-table*)))
    (if h
      (progn
        ;; 28.1.9.2. validity of initialization arguments
        (let ((valid-keywords (car h)))
          (unless (eq valid-keyword 't)
            (sys::keyword-test initargs valid-keywords)))
        (if (not (eq (cdr h) #'clos::%shared-initialize))
          ;; apply effective method from shared-initialize:
          (apply (cdr h) instance 'NIL initargs)
          ;; clos::%shared-initialize with slot-names=NIL can be simplified:
          (progn
            (dolist (slot (class-slots (class-of instance)))
              (let ((slotname (slotdef-name slot)))
                (multiple-value-bind (init-key init-value foundp)
                    (get-properties initargs (slotdef-initargs slot))
                  (declare (ignore init-key))
                  (if foundp
                    (setf (slot-value instance slotname) init-value)))))
            instance)))
      (apply #'initial-reinitialize-instance instance initargs))))
||#
;; the main work is done by a SUBR:
(do-defmethod 'reinitialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                            (cons #'clos::%reinitialize-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-object))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))
(do-defmethod 'reinitialize-instance
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%reinitialize-instance '(T)))
   :wants-next-method-p nil
   :parameter-specializers (list (find-class 'structure-object))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 1 :rest-p t)))
;; At the first call of REINITIALIZE-INSTANCE of each class
;; we memorize the needed information in *reinitialize-instance-table*.
(defun initial-reinitialize-instance (instance &rest initargs)
  (let* ((class (class-of instance))
         (valid-keywords
          (valid-initarg-keywords
           class
           ;; list of all applicable methods from SHARED-INITIALIZE
           (remove-if-not
            #'(lambda (method)
                (let* ((specializers (std-method-parameter-specializers method))
                       (specializer1 (first specializers))
                       (specializer2 (second specializers)))
                  (and (atom specializer1) (subclassp class specializer1)
                       (typep 'NIL specializer2))))
            (gf-methods |#'shared-initialize|)))))
    ;; 28.1.9.2. validity of initialization arguments
    (unless (eq valid-keywords 't)
      (sys::keyword-test initargs valid-keywords))
    (let ((si-ef (compute-effective-method
                  |#'shared-initialize| instance 'NIL)))
      (setf (gethash class *reinitialize-instance-table*)
            (cons valid-keywords si-ef))
      (apply si-ef instance 'NIL initargs))))

;; 28.1.9.6.
(defgeneric initialize-instance (instance &rest initargs
                                 &key &allow-other-keys))
(setq |#'initialize-instance| #'initialize-instance)
#||
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance 'T initargs))
||#
#|| ; optimized:
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (let ((h (gethash class *make-instance-table*)))
    (if h
      (if (not (eq (svref h 3) #'clos::%shared-initialize))
        ;; apply effective method from shared-initialize:
        (apply (svref h 3) instance 'T initargs)
        ;; clos::%shared-initialize with slot-names=T can be simplified:
        (progn
          (dolist (slot (class-slots (class-of instance)))
            (let ((slotname (slotdef-name slot)))
              (multiple-value-bind (init-key init-value foundp)
                  (get-properties initargs (slotdef-initargs slot))
                (declare (ignore init-key))
                (if foundp
                  (setf (slot-value instance slotname) init-value)
                  (unless (slot-boundp instance slotname)
                    (let ((init (slotdef-initer slot)))
                      (when init
                        (setf (slot-value instance slotname)
                              (if (car init) (funcall (car init))
                                  (cdr init))))))))))
          instance))
      (apply #'initial-initialize-instance instance initargs))))
||#
;; the main work is done by a SUBR:
(do-defmethod 'initialize-instance
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%initialize-instance '(T)))
   :wants-next-method-p nil
   :parameter-specializers (list (find-class 'standard-object))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 1 :rest-p t)))
(do-defmethod 'initialize-instance
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%initialize-instance '(T)))
   :wants-next-method-p nil
   :parameter-specializers (list (find-class 'structure-object))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 1 :rest-p t)))
(defun initial-initialize-instance (instance &rest initargs)
  (let ((class (class-of instance)))
    (multiple-value-bind (valid-keywords ai-ef)
        (make-instance-table-entry1 class)
      (multiple-value-bind (ii-ef si-ef) (make-instance-table-entry2 instance)
        (setf (gethash class *make-instance-table*)
              (vector valid-keywords ai-ef ii-ef si-ef))
        ;; apply effective method from SHARED-INITIALIZE:
        (apply si-ef instance 'T initargs)))))

;; User-defined methods on allocate-instance are now supported.
(defgeneric allocate-instance (instance &rest initargs))
(setq |#'allocate-instance| #'allocate-instance)
#||
(defgeneric allocate-instance (class)
  (:method ((class standard-class))
    (allocate-std-instance class (class-instance-size class)))
  (:method ((class structure-class))
    (sys::%make-structure (class-names class) (class-instance-size class)
                          :initial-element unbound)))
||#
#||
(defun %allocate-instance (class &rest initargs)
  (declare (ignore initargs))
  ;; Quick and dirty dispatch among <standard-class> and <structure-class>.
  ;; (class-shared-slots class) is a simple-vector, (class-names class) a cons.
  (if (atom (class-shared-slots class))
    (allocate-std-instance class (class-instance-size class))
    (sys::%make-structure (class-names class) (class-instance-size class)
                          :initial-element unbound)))
||#
; the main work is done by a SUBR:
(do-defmethod 'allocate-instance
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%allocate-instance '(T)))
   :wants-next-method-p nil
   :parameter-specializers (list (find-class 'standard-class))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 1 :rest-p t)))
(do-defmethod 'allocate-instance
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%allocate-instance '(T)))
   :wants-next-method-p nil
   :parameter-specializers (list (find-class 'structure-class))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 1 :rest-p t)))

;; 28.1.9.7.
(defgeneric make-instance (class &rest initargs)
  (:method ((class symbol) &rest initargs)
    (apply #'make-instance (find-class class) initargs)))
#||
(defmethod make-instance ((class standard-class) &rest initargs)
  ;; 28.1.9.3., 28.1.9.4. take note of default-initargs:
  (dolist (default-initarg (class-default-initargs class))
    (let ((nothing default-initarg))
      (when (eq (getf initargs (car default-initarg) nothing) nothing)
        (setq initargs
              (append initargs
                (list (car default-initarg)
                      (let ((init (cdr default-initarg)))
                        (if (car init) (funcall (car init)) (cdr init)))))))))
  #||
  ;; 28.1.9.2. validity of initialization arguments
  (sys::keyword-test initargs
      (union (class-valid-initargs class)
             (applicable-keywords #'initialize-instance class))) ; ??
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs))
  ||#
  (let ((h (gethash class *make-instance-table*)))
    (if h
      (progn
        ;; 28.1.9.2. validity of initialization arguments
        (let ((valid-keywords (svref h 0)))
          (unless (eq valid-keywords 't)
            (sys::keyword-test initargs valid-keywords)))
        (let ((instance (apply #'allocate-instance class initargs)))
          (if (not (eq (svref h 2) #'clos::%initialize-instance))
            ;; apply effective method from initialize-instance:
            (apply (svref h 2) instance initargs)
            ;; clos::%initialize-instance can be simplified (one does not need
            ;; to look into *make-instance-table* once again):
            (if (not (eq (svref h 3) #'clos::%shared-initialize))
              ;; apply effective method from shared-initialize:
              (apply (svref h 3) instance 'T initargs)
              ...
            ))))
      (apply #'initial-make-instance class initargs))))
||#
;; the main work is done by a SUBR:
(do-defmethod 'make-instance
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%make-instance '(T)))
   :wants-next-method-p nil
   :parameter-specializers (list (find-class 'standard-class))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 1 :rest-p t)))
(do-defmethod 'make-instance
  (make-standard-method
   :initfunction #'(lambda (gf) (declare (ignore gf))
                           (cons #'clos::%make-instance '(T)))
   :wants-next-method-p nil
   :parameter-specializers (list (find-class 'structure-class))
   :qualifiers '()
   :signature #s(compiler::signature :req-num 1 :rest-p t)))
(defun initial-make-instance (class &rest initargs)
  (multiple-value-bind (valid-keywords ai-ef)
      (make-instance-table-entry1 class)
    ;; http://www.lisp.org/HyperSpec/Body/sec_7-1-2.html
    ;; 7.1.2 Declaring the Validity of Initialization Arguments
    (unless (eq valid-keywords 't)
      (sys::keyword-test initargs valid-keywords))
    ;; call the effective method of ALLOCATE-INSTANCE:
    (let ((instance (apply ai-ef class initargs)))
      (unless (eq (class-of instance) class)
        (error-of-type 'error
          (TEXT "~S method for ~S returned ~S")
          'allocate-instance class instance))
      (multiple-value-bind (ii-ef si-ef) (make-instance-table-entry2 instance)
        (setf (gethash class *make-instance-table*)
              (vector valid-keywords ai-ef ii-ef si-ef))
        ;; call the effective method of INITIALIZE-INSTANCE:
        (apply ii-ef instance initargs))
      ;; return the instance
      instance)))

;; Users want to be able to create instances of subclasses of <standard-class>
;; and <structure-class>. So, when creating a class, we now go through
;; MAKE-INSTANCE and INITIALIZE-INSTANCE.
(defun make-instance-standard-class (&rest args)
  (apply #'make-instance args))
(defun make-instance-structure-class (&rest args)
  (apply #'make-instance args))
(defmethod initialize-instance ((new-class-object standard-class) &rest args
                                &key name (metaclass <standard-class>)
                                documentation direct-superclasses direct-slots
                                direct-default-initargs)
  (declare (ignore documentation direct-superclasses direct-slots
                   direct-default-initargs))
  (setf (class-classname new-class-object) name)
  (setf (class-metaclass new-class-object) metaclass) ; = (class-of new-class-object)
  (apply #'initialize-instance-standard-class new-class-object args)
  (call-next-method)
  new-class-object)
(defmethod initialize-instance ((new-class-object structure-class) &rest args
                                &key name (metaclass <structure-class>)
                                documentation direct-superclasses direct-slots
                                direct-default-initargs
                                names slots size)
  (declare (ignore documentation direct-superclasses direct-slots
                   direct-default-initargs names slots size))
  (setf (class-classname new-class-object) name)
  (setf (class-metaclass new-class-object) metaclass) ; = (class-of new-class-object)
  (apply #'initialize-instance-structure-class new-class-object args)
  (call-next-method)
  new-class-object)

;;; Utility functions

;; Returns the slot names of an instance of a slotted-class
;; (i.e. of a structure-object or standard-object).
(defun slot-names (object)
  (mapcar #'slotdef-name (class-slots (class-of object))))
