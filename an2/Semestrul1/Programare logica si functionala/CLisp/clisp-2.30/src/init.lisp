;;;;   INITIALIZATION-FILE

;; German comments translated into English: Stefan Kain 2001-08-27

(eval-when (compile load eval)
  (setq *package* (sys::%find-package "COMMON-LISP")))

(shadow 'system::debug (find-package "SYSTEM"))

;;; Exports:
(export
 '(;; types:
   array atom base-char base-string bignum bit bit-vector boolean character
   compiled-function complex cons double-float extended-char fixnum float
   function hash-table integer keyword list list* char-code-limit
   #+LOGICAL-PATHNAMES logical-pathname
   long-float nil null number package pathname random-state ratio rational
   readtable real sequence short-float simple-array simple-base-string
   simple-bit-vector simple-string simple-vector single-float standard-char
   stream file-stream synonym-stream broadcast-stream concatenated-stream
   two-way-stream echo-stream string-stream string symbol t vector
   satisfies values mod signed-byte unsigned-byte
   ;; constants:
   lambda-list-keywords lambda-parameters-limit nil t call-arguments-limit
   multiple-values-limit pi
   boole-clr boole-set boole-1 boole-2 boole-c1 boole-c2 boole-and
   boole-ior boole-xor boole-eqv boole-nand boole-nor boole-andc1
   boole-andc2 boole-orc1 boole-orc2
   most-positive-fixnum most-negative-fixnum
   most-positive-short-float least-positive-short-float
   least-negative-short-float most-negative-short-float
   most-positive-single-float least-positive-single-float
   least-negative-single-float most-negative-single-float
   most-positive-double-float least-positive-double-float
   least-negative-double-float most-negative-double-float
   most-positive-long-float least-positive-long-float
   least-negative-long-float most-negative-long-float
   least-positive-normalized-short-float
   least-negative-normalized-short-float
   least-positive-normalized-single-float
   least-negative-normalized-single-float
   least-positive-normalized-double-float
   least-negative-normalized-double-float
   least-positive-normalized-long-float
   least-negative-normalized-long-float
   short-float-epsilon single-float-epsilon
   double-float-epsilon long-float-epsilon
   short-float-negative-epsilon single-float-negative-epsilon
   double-float-negative-epsilon long-float-negative-epsilon
   array-rank-limit array-dimension-limit
   array-total-size-limit internal-time-units-per-second
   ;; variables:
   *macroexpand-hook* *gensym-counter* *package* *modules* *random-state*
   + ++ +++ - * ** *** / // /// *standard-input*
   *standard-output* *error-output* *query-io* *debug-io* *terminal-io*
   *trace-output* *read-base* *read-suppress* *read-eval* *readtable*
   *print-readably* *print-escape* *print-pretty* *print-circle* *print-base*
   *print-radix* *print-case* *print-gensym* *print-level* *print-length*
   *print-lines* *print-miser-width* *print-pprint-dispatch* *print-array*
   *print-right-margin* *read-default-float-format* *default-pathname-defaults*
   *load-verbose* *load-print* *load-pathname* *load-truename*
   *compile-verbose* *compile-print* *compile-file-pathname*
   *compile-file-truename* *features*
   ;; functions:
   coerce type-of upgraded-array-element-type upgraded-complex-part-type
   typep subtypep null symbolp
   atom consp listp numberp integerp rationalp floatp realp complexp characterp
   stringp bit-vector-p vectorp simple-vector-p simple-string-p
   simple-bit-vector-p arrayp packagep functionp compiled-function-p eq
   eql equal equalp not symbol-value symbol-function fdefinition boundp fboundp
   special-operator-p set makunbound fmakunbound
   get-setf-expansion
   apply funcall mapcar maplist mapc mapl mapcan mapcon values values-list
   macro-function macroexpand macroexpand-1 proclaim get remprop symbol-plist
   getf get-properties symbol-name make-symbol copy-symbol gensym gentemp
   symbol-package keywordp make-package in-package find-package package-name
   package-nicknames rename-package package-use-list package-used-by-list
   package-shadowing-symbols list-all-packages delete-package
   intern find-symbol unintern export unexport import shadowing-import shadow
   use-package unuse-package find-all-symbols provide require zerop plusp
   minusp oddp evenp = /= < > <= >= max min + - * / 1+ 1- conjugate gcd lcm exp
   expt log sqrt isqrt abs phase signum sin cos tan cis asin acos atan sinh
   cosh tanh asinh acosh atanh float rational rationalize numerator denominator
   floor ceiling truncate round mod rem ffloor fceiling ftruncate fround
   decode-float scale-float float-radix float-sign float-digits float-precision
   integer-decode-float complex realpart imagpart logior logxor logand logeqv
   lognand lognor logandc1 logandc2 logorc1 logorc2 boole lognot logtest
   logbitp ash logcount integer-length
   byte byte-size byte-position ldb ldb-test mask-field dpb deposit-field
   random make-random-state random-state-p
   standard-char-p graphic-char-p alpha-char-p upper-case-p lower-case-p
   both-case-p digit-char-p alphanumericp char= char/= char< char> char<=
   char>= char-equal char-not-equal char-lessp char-greaterp char-not-greaterp
   char-not-lessp char-code code-char character char-upcase char-downcase
   digit-char char-int char-name name-char
   complement constantly elt subseq copy-seq length reverse
   nreverse make-sequence concatenate map map-into some every notany notevery
   reduce fill replace remove remove-if remove-if-not delete delete-if
   delete-if-not remove-duplicates delete-duplicates substitute substitute-if
   substitute-if-not nsubstitute nsubstitute-if nsubstitute-if-not find find-if
   find-if-not position position-if position-if-not count count-if count-if-not
   mismatch search sort stable-sort merge car cdr caar cadr cdar cddr caaar
   caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   cons tree-equal endp list-length nth first second third fourth fifth sixth
   seventh eighth ninth tenth rest nthcdr last list make-list append
   copy-list copy-alist copy-tree revappend nconc nreconc butlast nbutlast
   ldiff rplaca rplacd subst subst-if subst-if-not nsubst nsubst-if
   nsubst-if-not sublis nsublis member member-if member-if-not tailp adjoin
   union nunion intersection nintersection set-difference nset-difference
   set-exclusive-or nset-exclusive-or subsetp acons pairlis assoc assoc-if
   assoc-if-not rassoc rassoc-if rassoc-if-not
   make-hash-table hash-table-p gethash remhash maphash clrhash
   hash-table-count hash-table-rehash-size hash-table-rehash-threshold
   hash-table-size hash-table-test sxhash
   make-array vector aref svref array-element-type array-rank array-dimension
   array-dimensions array-total-size array-in-bounds-p array-row-major-index
   row-major-aref adjustable-array-p array-displacement
   bit sbit bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1
   bit-andc2 bit-orc1 bit-orc2 bit-not
   array-has-fill-pointer-p fill-pointer vector-push
   vector-push-extend vector-pop adjust-array char schar string= string-equal
   string< string> string<= string>= string/= string-lessp string-greaterp
   string-not-greaterp string-not-lessp string-not-equal make-string
   string-trim string-left-trim string-right-trim string-upcase string-downcase
   string-capitalize nstring-upcase nstring-downcase nstring-capitalize string
   copy-structure
   eval constantp make-synonym-stream make-broadcast-stream
   make-concatenated-stream make-two-way-stream make-echo-stream
   make-string-input-stream make-string-output-stream get-output-stream-string
   streamp open-stream-p input-stream-p output-stream-p stream-element-type
   stream-external-format close broadcast-stream-streams
   concatenated-stream-streams echo-stream-input-stream
   echo-stream-output-stream synonym-stream-symbol two-way-stream-input-stream
   two-way-stream-output-stream interactive-stream-p
   copy-readtable readtablep set-syntax-from-char set-macro-character
   get-macro-character make-dispatch-macro-character
   set-dispatch-macro-character get-dispatch-macro-character readtable-case
   read read-preserving-whitespace read-delimited-list read-line read-char
   unread-char peek-char listen read-char-no-hang clear-input read-from-string
   parse-integer read-byte write prin1 print pprint princ write-to-string
   pprint-dispatch copy-pprint-dispatch set-pprint-dispatch pprint-fill
   pprint-indent pprint-linear pprint-newline pprint-tab pprint-tabular
   pprint-exit-if-list-exhausted pprint-logical-block pprint-pop
   prin1-to-string princ-to-string write-char write-string write-line terpri
   fresh-line finish-output force-output clear-output write-byte format
   y-or-n-p yes-or-no-p wild-pathname-p pathname-match-p translate-pathname
   #+LOGICAL-PATHNAMES logical-pathname
   #+LOGICAL-PATHNAMES translate-logical-pathname
   #+LOGICAL-PATHNAMES logical-pathname-translations
   #+LOGICAL-PATHNAMES load-logical-pathname-translations
   compile-file-pathname pathname truename parse-namestring merge-pathnames
   make-pathname pathnamep pathname-host pathname-device pathname-directory
   pathname-name pathname-type pathname-version namestring file-namestring
   directory-namestring host-namestring enough-namestring user-homedir-pathname
   open rename-file delete-file probe-file file-write-date file-author
   file-position file-length file-string-length load directory
   ensure-directories-exist error cerror warn break compile
   compile-file disassemble with-compilation-unit
   documentation variable structure type ; three documentation-types
   describe inspect room ed dribble apropos apropos-list get-decoded-time
   get-universal-time decode-universal-time encode-universal-time
   get-internal-run-time get-internal-real-time sleep lisp-implementation-type
   lisp-implementation-version machine-type machine-version machine-instance
   software-type software-version short-site-name long-site-name identity
   ;; Special-forms:
   eval-when quote function setq progn let let* locally progv flet
   labels macrolet symbol-macrolet if block return-from tagbody go
   multiple-value-call multiple-value-prog1 catch unwind-protect throw declare
   the load-time-value
   ;; Macros:
   deftype defun defvar defparameter defconstant and or psetq setf psetf shiftf
   rotatef define-modify-macro defsetf define-setf-expander
   prog1 prog2 when unless cond
   case typecase otherwise ; otherwise is the marker for the catchall-clause
   return loop do do* dolist dotimes prog prog* multiple-value-list
   multiple-value-bind multiple-value-setq defmacro remf do-symbols
   do-external-symbols do-all-symbols with-package-iterator incf decf
   push pushnew pop defstruct
   with-open-stream with-input-from-string with-output-to-string
   with-standard-io-syntax with-open-file define-symbol-macro
   check-type assert etypecase ctypecase ecase ccase trace untrace step time
   formatter
   compiler-macro compiler-macro-function define-compiler-macro
   ;; other markers:
   ;; EVAL-WHEN situations:
   eval load compile
   ;; DECLARE specifiers:
   special type ftype function inline notinline ignore ignorable optimize speed
   space safety compilation-speed debug declaration dynamic-extent compile
   ;; exports from condition.lisp
   handler-bind                  ; pre-drawn for compiler.lisp
   find-restart compute-restarts ; pre-drawn for user1.lisp
   invoke-restart-interactively  ; dito
   restart                       ; avoid conflict with user1.lisp
   continue                      ; avoid conflict with user1.lisp
   end-of-file                   ; avoid conflict with init.lisp, user2.lisp
   ;; types for error-of-type:
   condition warning serious-condition error storage-condition type-error
   program-error control-error package-error print-not-readable parse-error
   stream-error end-of-file reader-error file-error cell-error unbound-variable
   undefined-function unbound-slot arithmetic-error division-by-zero
   floating-point-overflow floating-point-underflow floating-point-inexact
   floating-point-invalid-operation))

(sys::%proclaim-constant 'lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &body &whole &environment)
)
(export lambda-list-keywords)

(proclaim '(special *features*))
(proclaim '(special compiler::*compiling* compiler::*compiling-from-file*))
(setq compiler::*compiling* nil)

(use-package '("COMMON-LISP" "EXT") "SYSTEM")
;; (in-package "SYSTEM")
(common-lisp:eval-when (common-lisp:compile common-lisp:load common-lisp:eval)
  (common-lisp:setq common-lisp:*package* (sys::%find-package "SYSTEM")))

#-COMPILER ; only for bootstrapping
(progn

;; preliminary, no expansion at GET_CLOSURE:
(sys::%putd '%expand-lambdabody-main
  (function %expand-lambdabody-main
    (lambda (lambdabody venv fenv)
      (declare (source nil) (ignore venv fenv))
      lambdabody)))

;; preliminary, defun is to be expanded trivially:
(sys::%putd 'defun
  (sys::make-macro
    (function defun
      (lambda (form env)
        (declare (ignore env))
        #|
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          `(SYS::%PUTD ',name (FUNCTION ,name (LAMBDA ,lambdalist ,@body)))
        )
        |#
        (let ((name (cadr form)))
          (list 'sys::%putd (list 'quote name)
                (list 'function name (cons 'lambda (cddr form)))))))))

)

(sys::%putd 'in-package
  (sys::make-macro
    (function in-package
      (lambda (form env)
        (declare (ignore env))
        (let ((package-name (cadr form)))
          (cond ((stringp package-name))
                ((symbolp package-name)
                 (setq package-name (symbol-name package-name)))
                (t (error-of-type 'source-program-error
                     (TEXT "~S: argument ~S should be a string or a symbol")
                     'common-lisp:in-package package-name)))
          (list 'EVAL-WHEN '(COMPILE LOAD EVAL)
                (list 'SETQ 'COMMON-LISP::*PACKAGE*
                      (list 'SYS::%FIND-PACKAGE package-name))))))))

;; this is yet another temporary definition
(sys::%putd 'cerror
  (function cerror
    (lambda (&rest args)
      (fresh-line) (princ "cerror: ") (prin1 args) (terpri))))

;; the following is a temporary hack
(sys::%putd 'format
  (function format (lambda (&rest rest) (print rest))))
;(setq custom:*error-handler* 'format)

(use-package '("COMMON-LISP" "CUSTOM") "EXT")

(in-package "EXT")

(sys::%putd 'exit #'sys::%exit)
(sys::%putd 'quit #'sys::%exit)
(sys::%putd 'bye #'sys::%exit)

(export
 '(re-export make-encoding encoding encoding-charset
   show-stack gc exit quit bye
   probe-directory cd make-dir delete-dir default-directory dir
   xgcd exquo ! evalhook applyhook substring string-concat
   string-char make-char string-width char-width
   int-char char-bits char-font char-bit set-char-bit
   base-char-code-limit char-font-limit char-bits-limit char-control-bit
   char-meta-bit char-super-bit char-hyper-bit string-char-p
   long-float-digits package-lock hash-table-weak-p weak-key-value-table
   the-environment arglist getenv special-variable-p
   *driver* *break-driver* *args* *keyboard-input* clhs-root shell execute
   ;; declarations
   constant-inline constant-notinline
   ;; pseudo-types:
   simple-2bit-vector 2bit-vector simple-4bit-vector 4bit-vector
   simple-8bit-vector 8bit-vector simple-16bit-vector 16bit-vector
   simple-32bit-vector 32bit-vector special-form system-function macro
   function-macro foreign-pointer symbol-macro symbol-macro-expand designator
   address special-operator finalize finalizer
   weak-pointer make-weak-pointer weak-pointer-p weak-pointer-value
   read-integer read-float write-integer write-float
   read-byte-lookahead read-byte-will-hang-p read-byte-no-hang
   read-char-will-hang-p
   read-char-sequence write-char-sequence
   read-byte-sequence write-byte-sequence
   convert-string-from-bytes convert-string-to-bytes
   #+(or UNIX OS/2 WIN32) make-pipe-output-stream
   #+(or UNIX OS/2 WIN32) make-pipe-input-stream
   #+(or UNIX OS/2 WIN32) make-pipe-io-stream
   make-buffered-input-stream make-buffered-output-stream
   get-setf-method local
   compiler-let load-time-eval)
 "EXT")

(common-lisp:in-package "CUSTOM")

(common-lisp:export
 '(*load-paths* *editor* *clhs-root-default* *browsers* *browser*
   *load-echo* *applyhook* *evalhook* *load-compiling* *compile-warnings*
   ;; places.lisp
   *ansi* *current-language* *lib-directory* *default-file-encoding*
   #+UNICODE *misc-encoding*
   #+UNICODE *terminal-encoding*
   #+UNICODE *pathname-encoding*
   *source-file-types* *compiled-file-types*)
 "CUSTOM")
(common-lisp:in-package "SYSTEM")

(ext:re-export "CUSTOM" "EXT")

(in-package "COMMON-LISP")
(proclaim
  '(ext::constant-notinline
    ;; These constants are platform dependent and therefore shouldn't be
    ;; inlined in compiled bytecode files.
    lambda-parameters-limit call-arguments-limit
    system::*jmpbuf-size* system::*big-endian*
    most-positive-fixnum most-negative-fixnum
    most-positive-short-float most-negative-short-float
    least-positive-short-float least-negative-short-float
    most-positive-single-float most-negative-single-float
    least-positive-single-float least-negative-single-float
    most-positive-double-float most-negative-double-float
    least-positive-double-float least-negative-double-float
    short-float-epsilon short-float-negative-epsilon
    single-float-epsilon single-float-negative-epsilon
    double-float-epsilon double-float-negative-epsilon
    char-code-limit
    array-total-size-limit array-dimension-limit array-rank-limit
    internal-time-units-per-second))

(use-package '("COMMON-LISP") "CLOS")
(in-package "CLOS")

;;; Exports:
(export
 '(;; names of functions and macros:
   slot-value slot-boundp slot-makunbound slot-exists-p with-slots
   with-accessors
   find-class class-of defclass defmethod call-next-method next-method-p
   defgeneric generic-function
   class-name no-applicable-method no-next-method
   find-method add-method remove-method
   compute-applicable-methods method-qualifiers function-keywords
   slot-missing slot-unbound
   print-object describe-object
   make-instance allocate-instance initialize-instance reinitialize-instance
   shared-initialize
   make-load-form make-load-form-saving-slots
   ;; names of classes:
   class standard-class structure-class built-in-class
   standard-object structure-object
   generic-function standard-generic-function method standard-method
   ;; other symbols:
   standard)) ; method combination

(use-package '("CLOS") "COMMON-LISP")
(ext:re-export "CLOS" "COMMON-LISP")
(let ((clos-extra '(generic-flet generic-labels no-primary-method)))
  ;; not in ANSI - export separately, after `re-export' above
  (export clos-extra "CLOS")
  ;; so that they are available in CL-USER even though it does not use CLOS
  (import clos-extra "EXT")
  (export clos-extra "EXT"))

(in-package "SYSTEM")

(use-package '("COMMON-LISP" "EXT") "CL-USER")

(sys::%putd 'sys::fbound-string
  (function sys::fbound-string
    (lambda (sym)
      (cond ((special-operator-p sym) (TEXT "special operator"))
            ((macro-function sym)     (TEXT "macro"))
            ((fboundp sym)            (TEXT "function"))))))

(sys::%putd 'sys::check-redefinition
  (function sys::check-redefinition
    (lambda (symbol caller what)
      (declare (ignore what))   ; for now...
      (sys::check-package-lock
       caller
       (cond ((atom symbol) (symbol-package symbol))
             ((function-name-p symbol) (symbol-package (second symbol)))
             ((mapcar #'(lambda (obj) ; handle (setf NAME) and (eql NAME)
                          (let ((oo (if (atom obj) obj (second obj))))
                            (when (symbolp oo)
                              (symbol-package oo))))
                      symbol)))
       symbol))))

(sys::%putd 'sys::remove-old-definitions
  (function sys::remove-old-definitions
    (lambda (symbol) ; removes the old function-definitions of a symbol
      (if (special-operator-p symbol)
        (error-of-type 'error
          (TEXT "~S is a special operator and may not be redefined.")
          symbol))
      (sys::check-redefinition symbol "DEFUN/DEFMACRO"
                               (sys::fbound-string symbol))
      (fmakunbound symbol) ; discard function & macro definition
      ;; Property sys::definition is not discarded, because it is
      ;; soon reset, anyway.
      (remprop symbol 'sys::macro) ; discard macro definition
      (remprop symbol 'sys::defstruct-reader) ; discard DEFSTRUCT information
      (when (get symbol 'sys::documentation-strings) ; discard documentation
        (sys::%set-documentation symbol 'FUNCTION nil))
      (when (get symbol 'sys::inline-expansion)
        (sys::%put symbol 'sys::inline-expansion t))
      (when (get symbol 'sys::traced-definition) ; discard Trace
        (warn (TEXT "DEFUN/DEFMACRO: redefining ~S; it was traced!")
              symbol)
        (untrace2 symbol)))))

;; THE-ENVIRONMENT as in SCHEME
(sys::%putd '%the-environment
  (function %the-environment
    (lambda (form env)
      (declare (ignore form))
      (sys::svstore env 0 (svref (svref env 0) 2)) ; nuke *evalhook* binding
      env)))
(sys::%putd '%the-environment-error
  (function %the-environment-error
    (lambda ()
      (error-of-type 'source-program-error
        (TEXT "~S is impossible in compiled code")
        'the-environment))))
(sys::%putd 'the-environment
  (sys::make-macro
    (function the-environment
      (lambda (form env)
        (declare (ignore form env))
        '(progn
           (eval-when ((not eval)) (%the-environment-error))
           (let ((*evalhook* #'%the-environment)) 0))))))
;; The toplevel environment
(proclaim '(special *toplevel-environment*))
(setq *toplevel-environment* (eval '(the-environment)))
(proclaim '(special *toplevel-denv*))
(setq *toplevel-denv* (svref *toplevel-environment* 4))

;; returns the name of the implicit block for a function-name
(defun function-block-name (funname)
  (if (atom funname) funname (second funname)))

;; inserts an implicit BLOCK in the BODY.
;; uses *VENV* and *FENV*.
(defun add-implicit-block (name body)
  (multiple-value-bind (body-rest declarations docstring)
      (sys::parse-body body t (vector *venv* *fenv*))
    (append (if declarations (list (cons 'DECLARE declarations)))
            (if docstring (list docstring))
            (list (list* 'BLOCK (function-block-name name) body-rest)))))

;;; functions for expansion of macros within a piece of code
;;;
;;; Altogether, the whole Code (of a function) is walked through and
;;; global and local Macros are expanded.
;;;           #'(lambda lambdalist . body)
;;; becomes   #'(lambda expanded-lambdalist
;;;               (declare (source (lambdalist . body))) . expanded-body
;;;             )
;;; This declaration guarantees, that a formerly processed
;;; function is recognized as such and not unnecessarily processed
;;; a second time.

;; Caution! For bootstrapping purposes (recognizable with #-COMPILER) some
;; of the functions have to be written in a more primitive Lisp
;; (without do, do*, case).

(PROGN

(proclaim '(special *keyword-package*))
(setq *keyword-package* (find-package "KEYWORD"))

(proclaim '(special *fenv*))
;; *FENV* = the current function environment during expansion of a form.
;; structure: NIL or a 2n+1-element vector
;;   (n1 f1 ... nn fn next),
;; where the ni are function-names,
;;       the fi are their functional meanings
;;           (closure or macro or function-macro or still NIL)
;; continued similarly at 'next'.

;; (fenv-assoc s fenv) searches symbol S in function-environment FENV.
;; the search routine uses EQUAL
(defun fenv-assoc (s fenv)
  (if fenv
    (if (simple-vector-p fenv)
      #+COMPILER
      (do ((l (1- (length fenv)))
           (i 0 (+ i 2)))
          ((= i l) (fenv-assoc s (svref fenv i)))
        (if (equal s (svref fenv i))
          (return (svref fenv (1+ i)))))
      #-COMPILER
      (let ((l (1- (length fenv)))
            (i 0))
        (block nil
          (tagbody
            1 (if (= i l) (return-from nil (fenv-assoc s (svref fenv i))))
              (if (equal s (svref fenv i))
                (return-from nil (svref fenv (1+ i))))
              (setq i (+ i 2))
              (go 1))))
      (error-of-type 'type-error
        :datum fenv :expected-type '(or null simple-vector)
        (TEXT "~S is an invalid function environment")
        fenv))
    'T)) ; not found
;; Determines, if a function-name S in function-environment FENV is not
;; defined and thus refers to the global function.
(defun global-in-fenv-p (s fenv) ; preliminary
  (eq (fenv-assoc s fenv) 'T))

(proclaim '(special *venv*))
;; *VENV* = the current variable-environment during the expansion of a form.
;; Structure: NIL or a 2n+1-element Vector
;;   (n1 v1 ... nn vn next),
;; where the ni are Symbols,
;;      the vi are their syntactic meanings (symbol-macro-object or sth. else)
;; continued similarly at 'next'.

;; (venv-assoc s venv) searches symbol S in variable-environment VENV.
;; Returns the value (or NIL if there's no value).
;; Caution: The value can be #<SPECDECL> or #<SYMBOL-MACRO ...> , thus
;; may not be temporarily saved in a variable in interpreted Code.
;; the search routine uses EQ
(defun venv-assoc (s venv)
  (if venv
    (if (simple-vector-p venv)
      #+COMPILER
      (do ((l (1- (length venv)))
           (i 0 (+ i 2)))
          ((= i l) (venv-assoc s (svref venv i)))
        (if (eq s (svref venv i))
          (return (svref venv (1+ i)))))
      #-COMPILER
      (let ((l (1- (length venv)))
            (i 0))
        (block nil
          (tagbody
            1 (if (= i l) (return-from nil (venv-assoc s (svref venv i))))
              (if (eq s (svref venv i))
                (return-from nil (svref venv (1+ i))))
              (setq i (+ i 2))
              (go 1))))
      (error-of-type 'type-error
        :datum venv :expected-type '(or null simple-vector)
        (TEXT "~S is an invalid variable environment")
        venv))
    (and (boundp s) (%symbol-value s)))) ; not found

;; Most of the Expansion-functions return two values:
;;  (1) the expansion result,
;;  (2) (NIL or T) indicates, if something was changed within it.

;; (%expand-cons ...) composes a cons. returns 2 values.
;; form=old Form,
;; expf,flagf = expansion of the first-part,
;; expr,flagr = expansion of the rest-part.
(defun %expand-cons (form expf flagf expr flagr)
  (if (or flagf flagr)
    (values (cons expf expr) t)
    (values form nil)))

;; (%expand-form form) expands a whole Form. returns 2 values.
(defun %expand-form (form)
  (if (atom form)
    #+COMPILER
    (let (h)
      (if (and (symbolp form)
               (symbol-macro-p (setq h (venv-assoc form *venv*))))
        (values (sys::%record-ref h 0) t)
        (values form nil)))
    #-COMPILER
    (if (and (symbolp form) (symbol-macro-p (venv-assoc form *venv*)))
      (values (sys::%record-ref (venv-assoc form *venv*) 0) t)
      (values form nil))
    ;; form is a CONS
    (let ((f (first form)))
      (if (function-name-p f)
        (let ((h (fenv-assoc f *fenv*)))
          ;; f is in *fenv* associated to h
          (if (eq h 'T)
            ;; f has no local definition
            ;; Now the separate expanders for the special-forms:
            (case f
              ((RETURN-FROM THE) ; skip the 1st argument, expand the rest
               (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (multiple-value-call #'%expand-cons (rest form)
                    (second form) nil
                    (%expand-list (cddr form)))))
              ((QUOTE GO DECLARE LOAD-TIME-VALUE) ; expand nothing
               (values form nil))
              ((FUNCTION)
               ;; if 1st or 2nd argument is a list,
               ;; expand as lambda expression.
               (multiple-value-call #'%expand-cons form
                 'FUNCTION nil
                 (if (atom (cddr form))
                   (if (function-name-p (second form))
                     (let ((h (fenv-assoc (second form) *fenv*)))
                       (cond ((or (eq h 'T) (closurep h)
                                  (function-macro-p h) (null h))
                              (values (rest form) nil))
                             ((macrop h)
                              (error-of-type 'source-program-error
                                (TEXT "~S: ~S is illegal since ~S is a local macro")
                                '%expand form (second form)))
                             (t (error-of-type 'error
                                  (TEXT "~S: invalid function environment ~S")
                                  '%expand *fenv*))))
                     (if (atom (second form))
                       (error-of-type 'source-program-error
                         (TEXT "~S: ~S is invalid since ~S is not a symbol")
                         '%expand form (second form))
                       (multiple-value-call #'%expand-cons (rest form)
                         (%expand-lambda (second form))
                         (cddr form) nil)))
                   (multiple-value-call #'%expand-cons (rest form)
                     (second form) nil
                     (multiple-value-call #'%expand-cons (cddr form)
                       (%expand-lambda (third form))
                       (cdddr form) nil)))))
              ((EVAL-WHEN)
               ;; if situation COMPILE is given, execute the body
               ;; as PROGN, return a form, that returns without side-effects
               ;; the same values.
               ;; Else expand all arguments from the second one as forms.
               (if (member 'COMPILE (second form)) ; not :COMPILE-TOPLEVEL !
                 (let ((compiler::*compiling-from-file* nil))
                   (values
                    (list 'values-list
                          (list 'quote
                                (multiple-value-list
                                 (eval (cons 'PROGN (cddr form))))))
                    t))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (second form) nil
                     (%expand-list (cddr form))))))
              ((LET)            ; expand variable-list and body
               (let ((*venv* *venv*))
                 (%expand-special-declarations (cddr form))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (%expand-varspez (second form))
                     (%expand-list (cddr form))))))
              ((LET*)           ; expand variable-list and body
               (let ((*venv* *venv*))
                 (%expand-special-declarations (cddr form))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (%expand-varspez* (second form))
                     (%expand-list (cddr form))))))
              ((LOCALLY)        ; expand body
               (let ((*venv* *venv*))
                 (%expand-special-declarations (cdr form))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (%expand-list (cdr form)))))
              ((MULTIPLE-VALUE-BIND) ; expand form and body, separately
               (let ((*venv* *venv*))
                 (%expand-special-declarations (cdddr form))
                 (multiple-value-call #'%expand-cons form
                   'MULTIPLE-VALUE-BIND nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (second form) nil
                     (multiple-value-call #'%expand-cons (cddr form)
                       (%expand-form (third form))
                       (progn
                         (%expand-lexical-variables (second form))
                         (%expand-list (cdddr form))))))))
              ((COMPILER-LET) ; expand var-list in empty environment and body
               (progv
                   (mapcar #'%expand-varspec-var (second form))
                   (mapcar #'%expand-varspec-val (second form))
                 (values (%expand-form (cons 'PROGN (cddr form))) t)))
              ((COND) ; expand all Sub-Forms of the clauses:
               (multiple-value-call #'%expand-cons form
                 (first form) nil
                 (%expand-cond (rest form))))
              ((CASE) ; expand 1st argument and all sub-forms of the clauses
               (multiple-value-call #'%expand-cons form
                 (first form) nil
                 (multiple-value-call #'%expand-cons (rest form)
                   (%expand-form (second form))
                   (%expand-case (cddr form)))))
              ((BLOCK)
               ;; expand body. If there is a RETURN-FROM in this
               ;; block, keep BLOCK, else turn it into a PROGN.
               (multiple-value-bind (body flagb) (%expand-list (cddr form))
                 (if (%return-p (second form) body)
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (multiple-value-call #'%expand-cons (rest form)
                       (second form) nil
                       body flagb))
                    (values
                      (cond ((atom body) body)
                            ((null (cdr body)) (car body))
                            (t (cons 'progn body)))
                      t))))
              ((SETQ PSETQ) ; expand each second Argument
               (if (%expand-setqlist-macrop (rest form))
                 (let ((new (if (eq (first form) 'SETQ) 'SETF 'PSETF)))
                   (values
                    (%expand-form
                     (funcall (macro-function new) (cons new (rest form))
                              (vector *venv* *fenv*)))
                    t))
                 (multiple-value-call #'%expand-cons form
                   (first form) nil
                   (%expand-setqlist (rest form)))))
              ((MULTIPLE-VALUE-SETQ) ; skip 1st argument, expand the rest
               (if (%expand-varlist-macrop (second form))
                 (values (%expand-form (cons 'MULTIPLE-VALUE-SETF (rest form)))
                         t)
                 (multiple-value-call #'%expand-cons form
                   'MULTIPLE-VALUE-SETQ nil
                   (multiple-value-call #'%expand-cons (rest form)
                     (second form) nil
                     (%expand-list (cddr form))))))
              ((TAGBODY)
               ;; expand all arguments,
               ;; skip atoms that are created during expansion
               (multiple-value-call #'%expand-cons form
                 (first form) nil
                 (%expand-tagbody (rest form))))
              ((PROGN) ; expand all arguments, possibly simplify them.
               (if (null (rest form))
                 (values nil t)
                 (if (null (cddr form))
                   (values (%expand-form (second form)) t)
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (%expand-list (rest form))))))
              ((FLET) ; expand function definitions
               (if (null (second form))
                 (values (%expand-form (cons 'PROGN (cddr form))) t)
                 (let ((newfenv (%expand-fundefs-1 (second form))))
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (multiple-value-call #'%expand-cons (rest form)
                       (%expand-fundefs-2 (second form))
                       (let ((*fenv* (apply #'vector newfenv)))
                         (%expand-list (cddr form))))))))
              ((LABELS)
               ;; expand function definitions and body
               ;; in the extended environment
               (if (null (second form))
                 (values (%expand-form (cons 'PROGN (cddr form))) t)
                 (let ((newfenv (%expand-fundefs-1 (second form))))
                   (let ((*fenv* (apply #'vector newfenv)))
                     (multiple-value-call #'%expand-cons form
                       (first form) nil
                       (multiple-value-call #'%expand-cons (rest form)
                         (%expand-fundefs-2 (second form))
                         (%expand-list (cddr form))))))))
              ((MACROLET) ; expand the body in the extended environment
               (do ((L1 (second form) (cdr L1))
                    (L2 nil))
                   ((atom L1)
                    (if L1
                      (error-of-type 'source-program-error
                        (TEXT "code after MACROLET contains a dotted list, ending with ~S")
                        L1)
                      (let ((*fenv* (apply #'vector
                                           (nreverse (cons *fenv* L2)))))
                        (values (%expand-form (cons 'PROGN (cddr form))) t))))
                 (let ((macrodef (car L1)))
                   (if (and (consp macrodef)
                            (symbolp (car macrodef))
                            (consp (cdr macrodef)))
                     (setq L2 (cons (make-macro-expander macrodef)
                                    (cons (car macrodef) L2)))
                     (error-of-type 'source-program-error
                       (TEXT "illegal syntax in MACROLET: ~S")
                       macrodef)))))
              ((FUNCTION-MACRO-LET)
               ;; expand function-definitions,
               ;; expand body in extended environment
               (if (null (second form))
                 (values (%expand-form (cons 'PROGN (cddr form))) t)
                 (let ((newfenv (%expand-funmacdefs-1 (second form))))
                   (multiple-value-call #'%expand-cons form
                     (first form) nil
                     (multiple-value-call #'%expand-cons (rest form)
                       (%expand-funmacdefs-2 (second form))
                       (let ((*fenv* (apply #'vector newfenv)))
                         (%expand-list (cddr form))))))))
              ((SYMBOL-MACROLET) ; expand body in extended environment
               (do ((L1 (second form) (cdr L1))
                    (L2 nil))
                   ((atom L1)
                    (if L1
                      (error-of-type 'source-program-error
                        (TEXT "code after SYMBOL-MACROLET contains a dotted list, ending with ~S")
                        L1)
                      (let ((*venv* (apply #'vector
                                           (nreverse (cons *venv* L2)))))
                        (let ((specials (%expand-special-declarations
                                         (cddr form))))
                          (do ((L3 (second form) (cdr L3)))
                              ((atom L3))
                            (if (memq (caar L3) specials)
                              (error-of-type 'source-program-error
                                (TEXT "~S: symbol ~S must not be declared SPECIAL and a macro at the same time")
                                'symbol-macrolet (caar L3)))))
                        (values (%expand-form (cons 'LOCALLY (cddr form)))
                                t))))
                 (let ((symdef (car L1)))
                   (if (and (consp symdef)
                            (symbolp (car symdef))
                            (consp (cdr symdef))
                            (null (cddr symdef)))
                     (let ((symbol (car symdef))
                           (expansion (cadr symdef)))
                       (if (special-variable-p symbol)
                         (error-of-type 'program-error
                           (TEXT "~S: symbol ~S is declared special and must not be declared a macro")
                           'symbol-macrolet symbol)
                         (setq L2 (cons (make-symbol-macro expansion)
                                        (cons symbol L2)))))
                     (error-of-type 'source-program-error
                       (TEXT "illegal syntax in SYMBOL-MACROLET: ~S")
                       symdef)))))
              ((%HANDLER-BIND)  ; expand handler-list and body
               (multiple-value-call #'%expand-cons form
                 (first form) nil
                 (multiple-value-call #'%expand-cons (rest form)
                   (%expand-handlers (second form))
                   (%expand-list (cddr form)))))
              (t (cond ((and (symbolp f) (special-operator-p f))
                        ;; other Special-forms,
                        ;; e.g. IF, CATCH, THROW, PROGV, UNWIND-PROTECT, PROGN,
                        ;; PROG1, PROG2, WHEN, UNLESS, MULTIPLE-VALUE-LIST,
                        ;; MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, AND, OR:
                        (multiple-value-call #'%expand-cons form
                          f nil
                          (%expand-list (rest form))))
                       ((and (symbolp f) (setq h (macro-function f)))
                        ;; global Macro-Definition
                        (values (%expand-form
                                 (funcall h form (vector *venv* *fenv*)))
                                t))
                       (t ; normal function-call
                        (multiple-value-call #'%expand-cons form
                          f nil
                          (%expand-list (rest form)))))))
            ;; f has a local definition
            (cond ((or (closurep h) (function-macro-p h) (null h))
                   ;; function to be called
                   (multiple-value-call #'%expand-cons form
                     f nil
                     (%expand-list (rest form))))
                  ((macrop h) ; macro to be expanded
                   (values (%expand-form (funcall (macro-expander h) form
                                                  (vector *venv* *fenv*)))
                           t)) ; call expander
                  (t (error-of-type 'error
                       (TEXT "bad function environment occurred in ~S: ~S")
                       '%expand-form *fenv*)))))
        (if (consp f)
          (multiple-value-call #'%expand-cons form
            (%expand-lambda f)
            (%expand-list (rest form)))
          (error-of-type 'source-program-error
            (TEXT "~S: invalid form ~S")
            '%expand-form form))))))

;; Auxiliary functions for the the expansion:

;; expands a list of forms. returns 2 values.
(defun %expand-list (l)
  (if (atom l)
    (if l
      (error-of-type 'source-program-error
        (TEXT "code contains a dotted list, ending with ~S")
        l)
      (values nil nil))
    (multiple-value-call #'%expand-cons l
                         (%expand-form (first l))
                         (%expand-list (rest l)))))

;; Adds lexical variables to *venv* .
;; (only used for shadowing symbol-macros.)
(defun %expand-lexical-variables (vars)
  (if vars
    (setq *venv*
      (apply #'vector
        (nconc (mapcan #'(lambda (v) (list v nil)) vars) (list *venv*))))))

;; Adds SPECIAL-Declarations at the beginning of a Body to *venv* .
(defun %expand-special-declarations (body)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil (vector *venv* *fenv*))
    (declare (ignore body-rest)) ; do not throw away declarations!
    (let ((specials nil))
      (mapc #'(lambda (declspec)
                (if (and (consp declspec) (null (cdr (last declspec))))
                  (if (eq (car declspec) 'SPECIAL)
                    (mapc #'(lambda (x)
                              (if (symbolp x)
                                  (setq specials (cons x specials))))
                          (cdr declspec)))))
            (nreverse declarations))
      (setq specials (nreverse specials))
      (%expand-lexical-variables specials) ; specdecl doesn't matter here
      specials)))

;; expands a function-name, that is a Cons (that must be a
;; Lambda-Expression). returns 2 values.
(defun %expand-lambda (l)
  (unless (eq (first l) 'lambda)
    (error-of-type 'source-program-error
      (TEXT "~S: ~S should be a lambda expression")
      '%expand-form l))
  (multiple-value-call #'%expand-cons l
      'lambda nil ; LAMBDA
      (%expand-lambdabody (rest l))))

;; expands the CDR of a Lambda-Expression, a (lambdalist . body).
;; returns 2 values.
(defun %expand-lambdabody (lambdabody &optional name blockp)
  (let ((body (rest lambdabody)))
    (if (and (consp body)
             (let ((form (car body)))
               (and (consp form)
                    (eq (car form) 'DECLARE)
                    (let ((declspecs (cdr form)))
                      (and (consp declspecs)
                           (let ((declspec (car declspecs)))
                             (and (consp declspec)
                                  (eq (car declspec) 'SOURCE))))))))
      (values lambdabody nil) ; already expanded -> leave untouched
      (let ((*venv* *venv*))
        (if blockp
          (setq lambdabody
                (cons (first lambdabody)
                      (add-implicit-block name (rest lambdabody)))))
        (values (list*
                 (%expand-lambdalist (first lambdabody))
                 (list 'DECLARE (list 'SOURCE lambdabody))
                 (%expand-list (rest lambdabody)))
                t)))))

;; expands a Lambda-list. returns 2 values.
(defun %expand-lambdalist (ll)
  (if (atom ll)
    (if ll
      (error-of-type 'source-program-error
        (TEXT "lambda list must not end with the atom ~S")
        ll)
      (values nil nil))
    (multiple-value-call #'%expand-cons ll
        (%expand-parspez (first ll))
        (progn
          (let ((v (first ll)))
            (if (not (memq v lambda-list-keywords))
              (setq *venv* (vector (%expand-varspec-var v) nil *venv*))))
          (%expand-lambdalist (rest ll))))))

;; expands an element of a lambda-list. returns 2 values.
;; (expands only on lists, and then only the second element.)
(defun %expand-parspez (ps)
  (if (or (atom ps) (atom (rest ps)))
    (values ps nil)
    (multiple-value-call #'%expand-cons ps
        (first ps) nil
        (multiple-value-call #'%expand-cons (rest ps)
            (%expand-form (second ps))
            (cddr ps) nil))))

;; expand a Variable-list for LET. returns 2 values.
(defun %expand-varspez (vs &optional (nvenv nil))
  (if (atom vs)
    (if vs
      (error-of-type 'source-program-error
        (TEXT "~S: variable list ends with the atom ~S")
        'let vs)
      (progn
        (setq *venv* (apply #'vector (nreverse (cons *venv* nvenv))))
        (values nil nil)))
    (multiple-value-call #'%expand-cons vs
        (%expand-parspez (first vs)) ; For List: Expand 2nd Element
        (%expand-varspez (rest vs) (list* nil (%expand-varspec-var (first vs))
                                          nvenv)))))

;; expands a Variable-list for LET*. returns 2 values.
(defun %expand-varspez* (vs)
  (if (atom vs)
    (if vs
      (error-of-type 'source-program-error
        (TEXT "~S: variable list ends with the atom ~S")
        'let* vs)
      (values nil nil))
    (multiple-value-call #'%expand-cons vs
        (%expand-parspez (first vs)) ; for list: expand 2nd Element
        (progn
          (setq *venv* (vector (%expand-varspec-var (first vs)) nil *venv*))
          (%expand-varspez* (rest vs))))))

(defun %expand-varspec-var (varspec)
  (if (atom varspec) varspec (first varspec)))

(defun %expand-varspec-val (varspec)
  (if (atom varspec) nil (eval (second varspec))))

;; expands a cond-clause-list. returns 2 values.
(defun %expand-cond (clauses)
  (if (atom clauses)
    (values clauses nil)
    (multiple-value-call #'%expand-cons clauses
        (%expand-list (first clauses))
        (%expand-cond (rest clauses)))))

;; expands a case-clause-list. returns 2 values.
(defun %expand-case (clauses)
  (if (atom clauses)
    (values clauses nil)
    (multiple-value-call #'%expand-cons clauses
      (multiple-value-call #'%expand-cons (first clauses)
        (caar clauses) nil
        (%expand-list (cdar clauses)))
      (%expand-case (rest clauses)))))

;; Apply the following to the already expanded body:
;; (%return-p name list) determines, if the form-list list contains
;; a (RETURN-FROM name ...) somewhere.
(defun %return-p (name body)
  (block return-p
    (tagbody 1
      (if (atom body) (return-from return-p nil))
      (let ((form (car body)))
        (if ; determine, if form contains a (RETURN-FROM name ...) :
         (and (consp form)
              (or (and (eq (first form) 'return-from) ; (RETURN-FROM name ...)
                       (eq (second form) name))
                  (and (consp (first form))           ; lambda-list
                       (%return-p name (first form)))
                  (and (not ; no new definition of the same block ?
                        (and (eq (first form) 'block) (eq (second form) name)))
                       (%return-p name (rest form))))) ; function call
         (return-from return-p t)))
       (setq body (cdr body))
       (go 1))))

(defun %expand-varlist-macrop (l)
  (and (consp l)
       (or (and (symbolp (car l)) (symbol-macro-p (venv-assoc (car l) *venv*)))
           (%expand-varlist-macrop (cdr l)))))

(defun %expand-setqlist-macrop (l)
  (and (consp l) (consp (cdr l))
       (or (and (symbolp (car l)) (symbol-macro-p (venv-assoc (car l) *venv*)))
           (%expand-setqlist-macrop (cddr l)))))

(defun %expand-setqlist (l)
  (if (or (atom l) (atom (cdr l)))
    (values l nil)
    (multiple-value-call #'%expand-cons l
        (first l) nil
        (multiple-value-call #'%expand-cons (rest l)
            (%expand-form (second l))
            (%expand-setqlist (cddr l))))))

;; (%expand-tagbody list) expands the elements of a list
;; and leaves atoms, that are created meanwhile, untouched.
;; (thus no new tags are created that could hide other tags).
;; returns 2 values.
(defun %expand-tagbody (body)
  (cond ((atom body) (values body nil))
        ((atom (first body))
         (multiple-value-call #'%expand-cons body
             (first body) nil
             (%expand-tagbody (rest body))))
        (t (multiple-value-bind (exp flag) (%expand-form (first body))
             (if (atom exp)
               (values (%expand-tagbody (rest body)) t) ; omit
               (multiple-value-call #'%expand-cons body
                   exp flag
                   (%expand-tagbody (rest body))))))))
;; returns a list (name1 nil ... namek nil *fenv*)
(defun %expand-fundefs-1 (fundefs)
  (if (atom fundefs)
    (if fundefs
      (error-of-type 'source-program-error
        (TEXT "FLET/LABELS: code contains a dotted list, ending with ~S")
        fundefs)
      (list *fenv*))
    (let ((fundef (car fundefs)))
      (if (and (consp fundef) (function-name-p (car fundef))
               (consp (cdr fundef)))
        (list* (car fundef) nil (%expand-fundefs-1 (cdr fundefs)))
        (error-of-type 'source-program-error
          (TEXT "illegal syntax in FLET/LABELS: ~S")
          fundef)))))
;; (%expand-fundefs-2 fundefs) expands a function-definition-list,
;; like in FLET, LABELS. returns 2 values.
(defun %expand-fundefs-2 (fundefs)
  (if (atom fundefs)
    (values fundefs nil)
    (let ((fundef (car fundefs)))
      (multiple-value-call #'%expand-cons fundefs
             (multiple-value-call #'%expand-cons fundef
                     (car fundef) nil
                     (%expand-lambdabody (cdr fundef) (car fundef) t))
             (%expand-fundefs-2 (rest fundefs))))))
;; returns a list (name1 nil ... namek nil *fenv*)
(defun %expand-funmacdefs-1 (funmacdefs)
  (if (atom funmacdefs)
    (if funmacdefs
      (error-of-type 'source-program-error
        (TEXT "FUNCTION-MACRO-LET: code contains a dotted list, ending with ~S")
        funmacdefs)
      (list *fenv*))
    (let ((funmacdef (car funmacdefs)))
      (if (and (consp funmacdef)
               (symbolp (car funmacdef))
               (consp (cdr funmacdef)) (consp (second funmacdef))
               (consp (cddr funmacdef)) (consp (third funmacdef))
               (null (cdddr funmacdef)))
        (list* (car funmacdef) nil (%expand-funmacdefs-1 (cdr funmacdefs)))
        (error-of-type 'source-program-error
          (TEXT "illegal syntax in FUNCTION-MACRO-LET: ~S")
          funmacdef)))))
;; (%expand-funmacdefs-2 funmacdefs) expands a function-macro-
;; definition-list, like in FUNCTION-MACRO-LET. returns 2 values.
(defun %expand-funmacdefs-2 (funmacdefs)
  (if (atom funmacdefs)
    (values funmacdefs nil)
    (let ((funmacdef (car funmacdefs)))
      (multiple-value-call #'%expand-cons funmacdefs
        (multiple-value-call #'%expand-cons funmacdef
          (car funmacdef) nil
          (multiple-value-call #'%expand-cons (cdr funmacdef)
            (%expand-lambdabody (cadr funmacdef))
            (multiple-value-call #'%expand-cons (cddr funmacdef)
              (%expand-lambdabody (caddr funmacdef))
              (cdddr funmacdef) nil)))
        (%expand-funmacdefs-2 (rest funmacdefs))))))
;; (%expand-handlers handlers) expands a Typ/Handler-List
;; like in %HANDLER-BIND. returns 2 values.
(defun %expand-handlers (handlers)
  (if (atom handlers)
    (values handlers nil)
    (let ((handler (car handlers)))
      (multiple-value-call #'%expand-cons handlers
        (multiple-value-call #'%expand-cons handler
          (car handler) nil
          (%expand-list (cdr handler)))
        (%expand-handlers (cdr handlers))))))

#|
;; expands a Form in a given Function-Environment
;; can be called by EVAL on demand.
(defun %expand-form-main (form *fenv*)
  (%expand-form form))
|#

;; expands (lambdalist . body) in a given function-environment.
;; Is called by GET_CLOSURE.
(defun %expand-lambdabody-main (lambdabody *venv* *fenv*)
  (%expand-lambdabody lambdabody))

(VALUES) )

;; from now on, FUNCTION is operational,
;; as long as no MACROLET occurs within it.

(PROGN

(proclaim '(special *load-paths*))
(setq *load-paths* nil)
(proclaim '(special *source-file-types*))
(setq *source-file-types* '("lisp" "lsp" "cl"))
(proclaim '(special *compiled-file-types*))
(setq *compiled-file-types* '("fas"))

;; for the time being the files don't have to be searched:
(defun search-file (filename extensions)
  (mapcan #'(lambda (extension)
              (let ((filename (merge-pathnames filename
                                     (make-pathname :type extension))))
                (if (probe-file filename) (list filename) '())))
          extensions))

(proclaim '(special *load-verbose*))
(setq *load-verbose* t)
(proclaim '(special *load-print*))
(setq *load-print* nil)
(proclaim '(special *load-echo*))
(setq *load-echo* nil)
(proclaim '(special *load-compiling*))
(setq *load-compiling* nil)
(proclaim '(special *load-pathname*))
(setq *load-pathname* nil)
(proclaim '(special *load-truename*))
(setq *load-truename* nil)
(proclaim '(special *load-input-stream*))
(setq *load-input-stream* nil)
(proclaim '(special *load-level*))
(setq *load-level* 0)
#+ffi ; the default :language for DEF-CALL-* & C-FUNCTION -- see foreign1.lisp
(proclaim '(special ffi::*foreign-language*))
#+ffi (setq ffi::*foreign-language* nil)

;; (LOAD filename [:verbose] [:print] [:if-does-not-exist] [:external-format]
;;                [:echo] [:compiling] [:extra-file-types]),
;; CLTL p. 426
(defun load (filename
             &key (verbose *load-verbose*) (print *load-print*)
                  (if-does-not-exist t) (external-format ':default)
                  (echo *load-echo*) (compiling *load-compiling*)
                  (extra-file-types '()))
  (let ((stream
         (if (streamp filename)
           filename
           (or (open (setq filename (pathname filename))
                     :direction :input-immutable
                     :element-type 'character
                     #+UNICODE :external-format
                     #+UNICODE (if (member (pathname-type filename)
                                           *compiled-file-types*
                                           :test #'string=)
                                   charset:utf-8
                                   external-format)
                     :if-does-not-exist nil)
               ;; File with precisely this name not present.
               ;; Search among the files the most recent one
               ;; with the same name and the Extensions "LISP", "FAS":
               (let ((present-files
                      (search-file filename
                                   (append extra-file-types
                                           *compiled-file-types*
                                           *source-file-types*))))
                 (if (endp present-files) nil
                   (open (setq filename (first present-files))
                         :direction :input-immutable
                         :element-type 'character
                         #+UNICODE :external-format
                         #+UNICODE (if (member (pathname-type filename)
                                               *compiled-file-types*
                                               :test #'string=)
                                       charset:utf-8
                                       external-format))))))))
    (if stream
      (let* ((input-stream
              (if echo
                  (make-echo-stream stream *standard-output*)
                  stream))
             (*load-level* (1+ *load-level*))
             (indent (if (null verbose) ""
                         (make-string *load-level* :initial-element #\Space)))
             (*load-input-stream* input-stream)
             ;; :verbose, :print, :echo and :compiling do not act recursively -
             ;; for that you have the special variables *LOAD-VERBOSE* etc.
             ;; (*load-verbose* verbose)
             ;; (*load-print* print)
             ;; (*load-echo* echo)
             ;; (*load-compiling* compiling)
             (*load-pathname* (if (pathnamep filename) filename nil))
             (*load-truename*
              (if (pathnamep filename) (truename filename) nil))
             #+ffi (ffi::*foreign-language* ffi::*foreign-language*)
             (*package* *package*) ; bind *PACKAGE*
             (*readtable* *readtable*) ; bind *READTABLE*
             (end-of-file "EOF")) ; one-time Object
        (when verbose
          (fresh-line)
          (write-string ";;")
          (write-string indent)
          (write-string (TEXT "Loading file "))
          (princ filename)
          (write-string (TEXT " ...")))
        (sys::allow-read-eval input-stream t)
        (block nil
          (unwind-protect
               (tagbody weiter
                  (when echo (fresh-line))
                  (let ((obj (read input-stream nil end-of-file)))
                    (when (eql obj end-of-file) (return-from nil))
                    (setq obj (multiple-value-list
                               (cond ((compiled-function-p obj) (funcall obj))
                                     (compiling (funcall (compile-form-in-toplevel-environment obj)))
                                     (t (eval obj)))))
                    (when print (when obj (print (first obj)))))
                  (go weiter))
            (or (eq input-stream stream)
                (sys::built-in-stream-close input-stream))
            (or (eq stream filename)
                (sys::built-in-stream-close stream))))
        (when verbose
          (fresh-line)
          (write-string ";;")
          (write-string indent)
          (write-string (TEXT "Loaded file "))
          (princ filename))
        t)
      (if if-does-not-exist
        (error-of-type 'file-error
          :pathname filename
          (TEXT "A file with name ~A does not exist")
          filename)
        nil))))

(sys::%putd 'check-symbol
  (function check-symbol
    (lambda (caller object)
      (unless (symbolp object)
        (error-of-type 'source-program-error
          (TEXT "~S: ~S is not a symbol.")
          caller object)))))

(sys::%putd 'defun              ; preliminary:
  (sys::make-macro
    (function defun
      (lambda (form env)
        (unless (and (consp (cdr form)) (consp (cddr form)))
          (error-of-type 'source-program-error
            (TEXT "~S: missing function name and/or parameter list")
            'defun))
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          (check-symbol 'defun name)
          (when (special-operator-p name)
            (error-of-type 'source-program-error
              (TEXT "~S: special operator ~S cannot be redefined.")
              'defun name))
          (multiple-value-bind (body-rest declarations docstring)
                               (sys::parse-body body t env)
            (declare (ignore docstring))
            #|
            `(PROGN
               (SYS::%PUT ',name 'SYS::DEFINITION
                 (CONS ',form (THE-ENVIRONMENT)))
               (SYS::%PUTD ',name
                 (FUNCTION ,name
                   (LAMBDA ,lambdalist
                     (DECLARE (SYS::IN-DEFUN ,name) ,@declarations)
                     (BLOCK ,name ,@body-rest))))
               ',name)
            |#
            (list 'progn
              (list 'sys::%put (list 'quote name) ''sys::definition
                    (list 'cons (list 'quote form) '(the-environment)))
              (list 'sys::%putd (list 'quote name)
                (list 'FUNCTION name
                  (list 'LAMBDA lambdalist
                        (list* 'DECLARE (list 'SYS::IN-DEFUN name)
                               declarations)
                        (list* 'BLOCK name body-rest))))
              (list 'quote name))))))))

(sys::%putd 'do               ; preliminary definition of the macro DO
  (sys::make-macro
    (function do
      (lambda (form env)
        (let ((varclauselist (second form))
              (exitclause (third form))
              (body (cdddr form)))
          (when (atom exitclause)
            (error-of-type 'source-program-error
              (TEXT "exit clause in ~S must be a list")
              'do))
          (let ((bindlist nil)
                (reinitlist nil)
                (bodytag (gensym))
                (exittag (gensym)))
            (multiple-value-bind (body-rest declarations)
                                 (sys::parse-body body nil env)
              (block do
                (tagbody 1
                  (when (atom varclauselist)
                    (return-from do
                      #|
                      `(block nil
                         (let ,(nreverse bindlist)
                           (declare ,@declarations)
                           (tagbody
                             (go ,exittag)
                             ,bodytag
                             ,@body-rest
                             (psetq ,@(nreverse reinitlist))
                             ,exittag
                             (or ,(first exitclause) (go ,bodytag))
                             (return-from nil (progn ,@(rest exitclause)))
                       ) ) )
                      |#
                      (list 'block 'nil
                        (list 'let (nreverse bindlist)
                          (cons 'declare declarations)
                          (list* 'tagbody
                            (list 'go exittag)
                            bodytag
                            (append body-rest
                              (list
                                (cons 'psetq (nreverse reinitlist))
                                exittag
                                (list 'or (first exitclause) (list 'go bodytag))
                                (list 'return-from 'nil
                                  (cons 'progn (rest exitclause))))))))))
                  (let ((varclause (first varclauselist)))
                       (setq varclauselist (rest varclauselist))
                       (cond ((atom varclause)
                              (setq bindlist
                                    (cons varclause bindlist)))
                             ((atom (cdr varclause))
                              (setq bindlist
                                    (cons (first varclause) bindlist)))
                             ((atom (cddr varclause))
                              (setq bindlist
                                    (cons varclause bindlist)))
                             (t (setq bindlist
                                      (cons (list (first varclause)
                                                  (second varclause))
                                            bindlist))
                                (setq reinitlist
                                      (list* (third varclause)
                                             (first varclause)
                                             reinitlist)))))
                   (go 1))))))))))

(sys::%putd 'dotimes       ; preliminary Definition of the Macro DOTIMES
  (sys::make-macro
    (function dotimes
      (lambda (form env)
        (let ((var (first (second form)))
              (countform (second (second form)))
              (resultform (third (second form)))
              (body (cddr form)))
          (multiple-value-bind (body-rest declarations)
                               (sys::parse-body body nil env)
            (let ((g (gensym)))
              #|
              `(DO ((,var 0 (1+ ,var))
                    (,g ,countform))
                   ((>= ,var ,g) ,resultform)
                 (declare ,@declarations)
                 ,@body-rest)
              |#
              (list* 'do (list (list var '0 (list '1+ var)) (list g countform))
                         (list (list '>= var g) resultform)
                     (cons 'declare declarations)
                     body-rest))))))))

(VALUES) )

;; from now on LOAD, DEFUN, DO, DOTIMES are operational (with limitations) .

(LOAD "defseq")                 ; Definitions of Standard-Sequences

(LOAD "backquote")              ; backquote readmacro

(PROGN

(sys::%putd 'sys::backquote
  (sys::make-macro
    (function sys::backquote
      (lambda (form &optional env) (declare (ignore env)) (third form)))))

(VALUES) )

;; from now on Backquote is operational

(LOAD "defmacro")

;; from now on FUNCTION is operational (without limitations).

(PROGN

(sys::%putd 'defmacro
  (sys::make-macro
    (function defmacro
      (lambda (form &optional env)
        (declare (ignore env))
        (multiple-value-bind (expansion name lambdalist docstring)
                             (sys::make-macro-expansion (cdr form))
          (declare (ignore lambdalist))
          `(LET ()
             (EVAL-WHEN (COMPILE LOAD EVAL)
               (SYSTEM::REMOVE-OLD-DEFINITIONS ',name)
               ,@(if docstring
                   `((SYSTEM::%SET-DOCUMENTATION ',name 'FUNCTION ',docstring))
                   '())
               (SYSTEM::%PUTD ',name (SYSTEM::MAKE-MACRO ,expansion)))
             (EVAL-WHEN (EVAL)
               (SYSTEM::%PUT ',name 'SYSTEM::DEFINITION
                 (CONS ',form (THE-ENVIRONMENT))))
             ',name))))))

#-compiler
(defmacro COMPILER::EVAL-WHEN-COMPILE (&body body) ; preliminary
  `(eval-when (compile) ,@body))

(sys::%putd 'defun
  (sys::make-macro
    (function defun
      (lambda (form env)
        (if (atom (cdr form))
          (error-of-type 'source-program-error
            (TEXT "~S: cannot define a function from that: ~S")
            'defun (cdr form)))
        (unless (function-name-p (cadr form))
          (error-of-type 'source-program-error
            (TEXT "~S: the name of a function must be a symbol, not ~S")
            'defun (cadr form)))
        (if (atom (cddr form))
          (error-of-type 'source-program-error
            (TEXT "~S: function ~S is missing a lambda list")
            'defun (cadr form)))
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          (multiple-value-bind (body-rest declarations docstring)
                               (sys::parse-body body t env)
            (let ((symbolform
                   (if (atom name)
                       `',name
                       `(LOAD-TIME-VALUE (GET-SETF-SYMBOL ',(second name)))))
                  (lambdabody
                   `(,lambdalist (DECLARE (SYS::IN-DEFUN ,name)
                                  ,@declarations)
                     (BLOCK ,(function-block-name name) ,@body-rest))))
              `(LET ()
                 (SYSTEM::REMOVE-OLD-DEFINITIONS ,symbolform)
                 ,@(if ; Is name declared inline?
                    (if (and compiler::*compiling*
                             compiler::*compiling-from-file*)
                      (member name compiler::*inline-functions* :test #'equal)
                      (eq (get (if (atom name) name
                                   (get-setf-symbol (second name))) 'inlinable)
                          'inline))
                    ;; Is the lexical environment the top-level environment?
                    ;; If yes, save the lambdabody for inline compilation.
                    (if compiler::*compiling*
                      (if (and (null compiler::*venv*)
                               (null compiler::*fenv*)
                               (null compiler::*benv*)
                               (null compiler::*genv*)
                               (eql compiler::*denv* *toplevel-denv*))
                        `((COMPILER::EVAL-WHEN-COMPILE
                           (COMPILER::C-DEFUN
                            ',name (lambda-list-to-signature ',lambdalist)
                            ',lambdabody))
                          (EVAL-WHEN (LOAD)
                            (SYSTEM::%PUT ,symbolform 'SYSTEM::INLINE-EXPANSION
                                          ',lambdabody)))
                        `((COMPILER::EVAL-WHEN-COMPILE
                           (COMPILER::C-DEFUN
                            ',name (lambda-list-to-signature ',lambdalist)))))
                      (if (and (null (svref env 0))  ; venv
                               (null (svref env 1))) ; fenv
                        `((EVAL-WHEN (EVAL)
                            (LET ((%ENV (THE-ENVIRONMENT)))
                              (IF (AND (NULL (SVREF %ENV 0)) ; venv
                                       (NULL (SVREF %ENV 1)) ; fenv
                                       (NULL (SVREF %ENV 2)) ; benv
                                       (NULL (SVREF %ENV 3)) ; genv
                                       (EQL (SVREF %ENV 4) *TOPLEVEL-DENV*)) ; denv
                                (SYSTEM::%PUT ,symbolform
                                              'SYSTEM::INLINE-EXPANSION
                                              ',lambdabody)))))
                        '()))
                    `((COMPILER::EVAL-WHEN-COMPILE
                       (COMPILER::C-DEFUN
                        ',name (lambda-list-to-signature ',lambdalist)))))
                ,@(if docstring
                    `((SYSTEM::%SET-DOCUMENTATION ,symbolform
                       'FUNCTION ',docstring))
                     '())
                 (SYSTEM::%PUTD ,symbolform
                   (FUNCTION ,name (LAMBDA ,@lambdabody)))
                 (EVAL-WHEN (EVAL)
                   (SYSTEM::%PUT ,symbolform 'SYSTEM::DEFINITION
                     (CONS ',form (THE-ENVIRONMENT))))
                 ',name))))))))

(VALUES) )

;; from now on DEFMACRO and DEFUN are operational.
(in-package "SYSTEM")

(LOAD "macros1")                ; control-structure - macros
(LOAD "macros2")                ; further macros

(LOAD "defs1")      ; definitions for symbols, numbers, characters, time
#-(or UNIX WIN32)
(LOAD "timezone")               ; Definition of the time zone

(LOAD "places")                 ; SETF-places: definitions and macros

;; from now on SETF etc. are working.

(LOAD "floatprint")             ; output of floating-points

(LOAD "type")                   ; TYPEP

(LOAD "defstruct")              ; DEFSTRUCT-macro

(LOAD "format")                 ; FORMAT

;; from now on FORMATTER is working.
(LOAD "international")          ; internationalization

(in-package "SYSTEM")

;; (default-directory) is a Synonym for (cd).
(defun default-directory () (cd))

;; (setf (default-directory) dir) is a Synonym for (cd dir).
(defsetf default-directory () (value)
  `(PROGN (CD ,value) ,value))

;; FORMAT-Control-String for output of dates,
;; applicable to a List (sec min hour day month year ...),
;; occupies 17-19 characters
(definternational date-format
  (t ENGLISH))
(deflocalized date-format ENGLISH
  (formatter
   "~1{~5@*~D-~4@*~2,'0D-~3@*~2,'0D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}"))
(defun date-format ()
  (localized 'date-format))

;; list a directory
(defun dir (&optional (pathnames #+(or AMIGA UNIX OS/2 WIN32) '("*/" "*")
                                 #+ACORN-RISCOS '("*." "*" "*.*")))
  (flet ((onedir (pathname)
           (let ((pathname-list (directory pathname :full t :circle t)))
             (if (every #'atom pathname-list)
                 (format t "~{~%~A~}"
                         (sort pathname-list #'string< :key #'namestring))
                 (let ((date-format (date-format)))
                   (dolist (l (sort pathname-list #'string< :key
                                    #'(lambda (l) (namestring (first l)))))
                     (format t "~%~A~40T~7D~52T~21<~@?~>"
                             (first l) (fourth l) date-format (third l))))))))
    (if (listp pathnames) (mapc #'onedir pathnames) (onedir pathnames)))
  (values))

;; A piece of "DO-WHAT-I-MEAN":
;; Searches for a program file.
;; We search in the current directory and then in the directories
;; listed in *load-paths*.
;; If an extension is specified in the filename, we search only for
;; files with this extension. If no extension is specified, we search
;; only for files with an extension from the given list.
;; The return value is a list of all matching files from the first directory
;; containing any matching file, sorted according to decreasing FILE-WRITE-DATE
;; (i.e. from new to old), or NIL if no matching file was found.
(defun search-file (filename extensions
                    &aux (use-extensions (null (pathname-type filename))) )
  ;; merge in the defaults:
  (setq filename (merge-pathnames filename '#"*.*"))
  ;; search:
  (let ((already-searched nil))
    (dolist (dir (cons '#""
                       ;; when filename has "..", ignore *load-paths*
                       ;; (to avoid errors with ".../../foo"):
                       (if (member #+(or AMIGA ACORN-RISCOS) :PARENT
                                   #+(or UNIX OS/2 WIN32) ".."
                                   (pathname-directory filename)
                                   :test #'equal)
                           '()
                           (mapcar #'pathname *load-paths*))))
      (let ((search-filename (merge-pathnames (merge-pathnames filename dir))))
        (unless (member search-filename already-searched :test #'equal)
          (let ((xpathnames (directory search-filename :full t :circle t)))
            (when (eq :wild (pathname-type search-filename))
              (setq xpathnames
                    (nconc xpathnames
                           (directory (make-pathname :type nil
                                                     :defaults search-filename)
                                      :full t :circle t))))
            (when (and use-extensions extensions)
              ;; filter the extensions
              (setq xpathnames
                (delete-if-not ; does xpathname have the given extensions?
                 #'(lambda (xpathname)
                     (member (pathname-type (first xpathname)) extensions
                             :test #-(or AMIGA OS/2 WIN32) #'string=
                             #+(or AMIGA OS/2 WIN32) #'string-equal))
                 xpathnames)))
            (when xpathnames
              ;; reverse sort by date:
              (dolist (xpathname xpathnames)
                (setf (rest xpathname)
                      (apply #'encode-universal-time (third xpathname))))
              (return (mapcar #'first (sort xpathnames #'> :key #'rest)))))
          (push search-filename already-searched))))))

(LOAD "room")                   ; room, space

(LOAD "savemem")                ; saveinitmem

;; At this point saveinitmem works.

;; preliminary definition of CERROR, CLtL2 p. 887
(defun cerror (continue-format-string error-format-string &rest args)
  (if *error-handler*
    (apply *error-handler*
           (or continue-format-string t) error-format-string args)
    (progn
      (terpri *error-output*)
      (write-string "** - Continuable Error" *error-output*)
      (terpri *error-output*)
      (apply #'format *error-output* error-format-string args)
      (terpri *debug-io*)
      (if (and (interactive-stream-p *debug-io*) *break-driver*)
        (progn
          (write-string (TEXT "If you continue (by typing 'continue'): ")
                        *debug-io*)
          (apply #'format *debug-io* continue-format-string args)
          (funcall *break-driver* t))
        (apply #'format *debug-io* continue-format-string args)))))

;; this should come before `compiler'
#+syscalls
(use-package '("COMMON-LISP") "POSIX")
#+syscalls
(in-package "POSIX")
#+syscalls
(let ((posix-math '(erf erfc j0 j1 jn y0 y1 yn gamma lgamma)))
  (export posix-math "POSIX")
  (import posix-math "EXT")
  (export posix-math "EXT"))
#+syscalls
(in-package "SYSTEM")

(LOAD "trace")                  ; TRACE

(load "cmacros")                ; compiler macros

(LOAD "compiler")               ; compiler

(LOAD "defs2")                  ; CLtL2-definitions, optional

(LOAD "loop")                   ; CLtL2/ANSI-CL-LOOP, optional

(LOAD "clos")                   ; CLOS

(LOAD "disassem")               ; Disassembler

(LOAD "condition")              ; Conditions

(load "loadform")               ; `make-load-form'

;; At this point the core Common Lisp is complete.

#+mt (load "threads")           ; Multi-Threading

;; Fancy streams:

(load "gray")
#+generic-streams
(LOAD "gstream")                ; generic streams, optional

(LOAD "xcharin")                ; extended character input, optional

(LOAD "keyboard")               ; keyboard stream, optional

#+(or AMIGA SCREEN)
(LOAD "screen")                 ; screen-package, optional


;; Environmental facilities:
#+sockets
(use-package '("SOCKET") "EXT")
#+sockets
(in-package "SOCKET")
#+sockets
(common-lisp:export
 '(socket-server socket-server-close socket-server-port socket-server-host
   socket-accept socket-wait socket-status socket-connect
   socket-stream-host socket-stream-port socket-stream-peer socket-stream-local
   #-win32 socket-stream-handle
   socket-service-port)
 "SOCKET")
#+sockets
(ext:re-export "SOCKET" "EXT")

(common-lisp:in-package "SYSTEM")

#+AMIGA
(LOAD "amigasock")              ; sockets, optional
#+BEOS
(LOAD "beossock")               ; sockets, optional

(LOAD "runprog")                ; run-program and friends, optional

;; User interface:

(LOAD "query")                  ; querying the user

(LOAD "reploop")                ; prompt, debugger, stepper

(LOAD "dribble")                ; dribble

(LOAD "complete")               ; completion

(load "pprint")                 ; pretty printer

(LOAD "describe")               ; apropos, describe

(LOAD "edit")                   ; edit-file, ed, uncompile

(LOAD "clhs")                   ; HyperSpec access

(load "inspect")                ; inspector

;; Random extensions:

(LOAD "macros3")                ; more macros, optional

#+FFI ; when (find-package "FFI")
(LOAD "foreign1")               ; foreign function interface, optional

#+AMIGA
(when (find-symbol "%LIBCALL" "SYSTEM")
  (LOAD "affi1"))               ; simple FFI, optional

#+AMIGA (LOAD "rexx1")          ; REXX-interface, optional

;; POSIX/SUSV2 system calls and library functions, optional
;; http://www.UNIX-systems.org/online.html
#+syscalls (load "posix")

#+GETTEXT (LOAD "german")       ; German messages
#+(and GETTEXT UNICODE) (LOAD "french") ; French messages
#+(and GETTEXT UNICODE) (LOAD "spanish") ; Spanish messages
#+GETTEXT (LOAD "dutch")        ; Dutch messages
#+(and GETTEXT UNICODE) (LOAD "russian") ; Russian messages

#+dir-key
(load "dirkey1")                ; win32 registry, LDAP, Gnome-config

(load "deprecated")             ; the deprecated functionality -- optional

(LOAD "config")    ; configuration parameters to be adjusted by the user

(setq sys::*home-package* nil ext:*command-index* 0)

(in-package "CL-USER")        ; make the default package the current one
