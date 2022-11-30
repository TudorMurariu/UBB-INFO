;;; CLtL2-kompatible Definitionen
;;; Bruno Haible 21.7.1994

;; ============================================================================

(in-package "COMMON-LISP")
(export '(nth-value function-lambda-expression defpackage
          print-unreadable-object declaim destructuring-bind complement
          constantly with-standard-io-syntax with-hash-table-iterator
          read-sequence write-sequence))
(in-package "SYSTEM")

;; ----------------------------------------------------------------------------

;; X3J13 vote <123>

;; Macro (nth-value n form) == (nth n (multiple-value-list form)), CLtL2 S. 184
(defmacro nth-value (n form)
  (if (and (integerp n) (>= n 0))
    (if (< n (1- multiple-values-limit))
      (if (= n 0)
        `(PROG1 ,form)
        (let ((resultvar (gensym)))
          (do ((vars (list resultvar))
               (ignores nil)
               (i n (1- i)))
              ((zerop i)
               `(MULTIPLE-VALUE-BIND ,vars ,form
                  (DECLARE (IGNORE ,@ignores))
                  ,resultvar
              ) )
            (let ((g (gensym))) (push g vars) (push g ignores))
      ) ) )
      `(PROGN ,form NIL)
    )
    `(NTH ,n (MULTIPLE-VALUE-LIST ,form))
) )

;; ----------------------------------------------------------------------------

;; X3J13 vote <88>

;; Interpretierte Funktion in Lambda-Ausdruck umwandeln, CLtL2 S. 682
(defun function-lambda-expression (obj)
  (cond ((compiled-function-p obj) ; SUBR oder compilierte Closure?
         (values nil t nil)
        )
        ((sys::closurep obj) ; interpretierte Closure?
         (values (cons 'LAMBDA (sys::%record-ref obj 1)) ; Lambda-Ausdruck ohne Docstring
                 (vector ; Environment
                         (sys::%record-ref obj 4) ; venv
                         (sys::%record-ref obj 5) ; fenv
                         (sys::%record-ref obj 6) ; benv
                         (sys::%record-ref obj 7) ; genv
                         (sys::%record-ref obj 8) ; denv
                 )
                 (sys::%record-ref obj 0) ; Name
        ))
        (t
         (error-of-type 'type-error
           :datum obj :expected-type 'function
           (TEXT "~S: ~S is not a function")
           'function-lambda-expression obj
) )     ))

;; ----------------------------------------------------------------------------

;; X3J13 vote <52>

;; Package-Definition und -Installation, CLtL2 S. 270
(defmacro defpackage (packname &rest options)
  (flet ((check-packname (name)
           (cond ((stringp name) name)
                 ((symbolp name) (symbol-name name))
                 (t (error-of-type 'source-program-error
                      (TEXT "~S: package name ~S should be a string or a symbol")
                      'defpackage name
         ) )     )  )
         (check-symname (name)
           (cond ((stringp name) name)
                 ((symbolp name) (symbol-name name))
                 (t (error-of-type 'source-program-error
                      (TEXT "~S ~A: symbol name ~S should be a string or a symbol")
                      'defpackage packname name
        )) )     )  )
    (setq packname (check-packname packname))
    ; Optionen abarbeiten:
    (let ((size nil) ; Flag ob :SIZE schon da war
          (documentation nil) ; Flag, ob :DOCUMENTATION schon da war
          (nickname-list '()) ; Liste von Nicknames
          (shadow-list '()) ; Liste von Symbolnamen für shadow
          (shadowing-list '()) ; Listen von Paaren (Symbolname . Paketname) für shadowing-import
          (use-list '()) ; Liste von Paketnamen für use-package
          (use-default '("COMMON-LISP")) ; default for use-list
          (case-sensitive nil) ; Flag für :CASE-SENSITIVE
          (import-list '()) ; Listen von Paaren (Symbolname . Paketname) für import
          (intern-list '()) ; Liste von Symbolnamen für intern
          (symname-list '()) ; Liste aller bisher aufgeführten Symbolnamen
          (export-list '())) ; Liste von Symbolnamen für export
      (flet ((record-symname (name)
               (if (member name symname-list :test #'string=)
                 (error-of-type 'source-program-error
                   (TEXT "~S ~A: the symbol ~A must not be specified more than once")
                   'defpackage packname name
                 )
                 (push name symname-list)
            )) )
        (dolist (option options)
          (if (listp option)
            (if (keywordp (car option))
              (case (first option)
                (:SIZE
                  (if size
                    (error-of-type 'source-program-error
                      (TEXT "~S ~A: the ~S option must not be given more than once")
                      'defpackage packname ':SIZE
                    )
                    (setq size t) ; Argument wird ignoriert
                ) )
                (:DOCUMENTATION ; ANSI-CL
                  (if documentation
                    (error-of-type 'source-program-error
                      (TEXT "~S ~A: the ~S option must not be given more than once")
                      'defpackage packname ':DOCUMENTATION
                    )
                    (setq documentation t) ; Argument wird ignoriert
                ) )
                (:NICKNAMES
                  (dolist (name (rest option))
                    (push (check-packname name) nickname-list)
                ) )
                (:SHADOW
                  (dolist (name (rest option))
                    (setq name (check-symname name))
                    (unless (member name shadow-list :test #'string=)
                      (push name shadow-list)
                      (record-symname name)
                ) ) )
                (:SHADOWING-IMPORT-FROM
                  (let ((pack (check-packname (second option))))
                    (dolist (name (cddr option))
                      (setq name (check-symname name))
                      (let ((name+pack (cons name pack)))
                        (unless (member name+pack shadowing-list :test #'equal) ; #'string= on car and cdr
                          (push name+pack shadowing-list)
                          (record-symname name)
                ) ) ) ) )
                (:USE
                  (dolist (name (rest option))
                    (push (check-packname name) use-list)
                  )
                  (setq use-default nil)
                )
                (:IMPORT-FROM
                  (let ((pack (check-packname (second option))))
                    (dolist (name (cddr option))
                      (setq name (check-symname name))
                      (let ((name+pack (cons name pack)))
                        (unless (member name+pack import-list :test #'equal) ; #'string= on car and cdr
                          (push name+pack import-list)
                          (record-symname name)
                ) ) ) ) )
                (:INTERN
                  (dolist (name (rest option))
                    (setq name (check-symname name))
                    (unless (member name intern-list :test #'string=)
                      (push name intern-list)
                      (record-symname name)
                ) ) )
                (:EXPORT
                  (dolist (name (rest option))
                    (setq name (check-symname name))
                    (unless (member name export-list :test #'string=)
                      (push name export-list)
                ) ) )
                (:CASE-SENSITIVE ; CLISP extension
                  (when (not (null (second option)))
                    (setq case-sensitive t)
                ) )
                (T (error-of-type 'source-program-error
                     (TEXT "~S ~A: unknown option ~S")
                     'defpackage packname (first option)
              ) )  )
              (error-of-type 'source-program-error
                (TEXT "~S ~A: invalid syntax in ~S option: ~S")
                'defpackage packname 'defpackage option
            ) )
            (error-of-type 'source-program-error
              (TEXT "~S ~A: not a ~S option: ~S")
              'defpackage packname 'defpackage option
        ) ) )
        ; Auf Überschneidungen zwischen intern-list und export-list prüfen:
        (setq symname-list intern-list)
        (mapc #'record-symname export-list)
      )
      ; Listen umdrehen und Default-Werte eintragen:
      (setq nickname-list (nreverse nickname-list))
      (setq shadow-list (nreverse shadow-list))
      (setq shadowing-list (nreverse shadowing-list))
      (setq use-list (or use-default (nreverse use-list)))
      (setq import-list (nreverse import-list))
      (setq intern-list (nreverse intern-list))
      (setq export-list (nreverse export-list))
      ; Expansion produzieren:
      `(EVAL-WHEN (LOAD COMPILE EVAL)
         (SYSTEM::%IN-PACKAGE ,packname :NICKNAMES ',nickname-list
                                        :USE '()
                                        ,@(when case-sensitive `(:CASE-SENSITIVE T))
         )
         ; Schritt 1
         ,@(if shadow-list
             `((SHADOW ',(mapcar #'make-symbol shadow-list) ,packname))
           )
         ,@(mapcar
             #'(lambda (pair)
                 `(SHADOWING-IMPORT-CERROR ,(car pair) ,(cdr pair) ,packname)
               )
             shadowing-list
           )
         ; Schritt 2
         ,@(if use-list `((USE-PACKAGE ',use-list ,packname)))
         ; Schritt 3
         ,@(mapcar
             #'(lambda (pair)
                 `(IMPORT-CERROR ,(car pair) ,(cdr pair) ,packname)
               )
             import-list
           )
         ,@(mapcar
             #'(lambda (symname) `(INTERN ,symname ,packname))
             intern-list
           )
         ; Schritt 4
         ,@(if export-list
             `((INTERN-EXPORT ',export-list ,packname))
           )
         (FIND-PACKAGE ,packname)
       )
) ) )
; Hilfsfunktionen:
(defun find-symbol-cerror (string packname calling-packname)
  (multiple-value-bind (sym found) (find-symbol string packname)
    (unless found
      (cerror ; 'package-error ??
              (TEXT "This symbol will be created.")
              (TEXT "~S ~A: There is no symbol ~A::~A .")
              'defpackage calling-packname packname string
      )
      (setq sym (intern string packname))
    )
    sym
) )
(defun shadowing-import-cerror (string packname calling-packname)
  (shadowing-import (find-symbol-cerror string packname calling-packname)
                    calling-packname
) )
(defun import-cerror (string packname calling-packname)
  (import (find-symbol-cerror string packname calling-packname)
          calling-packname
) )
(defun intern-export (string-list packname)
  (export (mapcar #'(lambda (string) (intern string packname)) string-list)
          packname
) )

;; ----------------------------------------------------------------------------

;; X3J13 vote <40>

(defmacro print-unreadable-object
    ((&whole args object stream &key type identity) &body body)
  (declare (ignore object stream type identity))
  `(SYSTEM::WRITE-UNREADABLE
     ,(if body `(FUNCTION (LAMBDA () ,@body)) 'NIL)
     ,@args
   )
)

;; ----------------------------------------------------------------------------

;; X3J13 vote <144>

(defmacro declaim (&rest decl-specs)
  `(PROGN
     ,@(mapcar #'(lambda (decl-spec) `(PROCLAIM (QUOTE ,decl-spec))) decl-specs)
   )
)

;; ----------------------------------------------------------------------------

;; X3J13 vote <64>

(defmacro destructuring-bind (lambdalist form &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (if declarations (setq declarations `((DECLARE ,@declarations))))
    (let ((%arg-count 0) (%min-args 0) (%restp nil)
          (%let-list nil) (%keyword-tests nil) (%default-form nil))
      (analyze1 lambdalist '<DESTRUCTURING-FORM> 'destructuring-bind '<DESTRUCTURING-FORM>)
      (let ((lengthtest (make-length-test '<DESTRUCTURING-FORM> 0))
            (mainform `(LET* ,(nreverse %let-list)
                         ,@declarations
                         ,@(nreverse %keyword-tests)
                         ,@body-rest
           ))          )
        (if lengthtest
          (setq mainform
            `(IF ,lengthtest
               (DESTRUCTURING-ERROR <DESTRUCTURING-FORM>
                                    '(,%min-args . ,(if %restp nil %arg-count))
               )
               ,mainform
        ) )  )
        `(LET ((<DESTRUCTURING-FORM> ,form)) ,mainform)
) ) ) )

(defun destructuring-error (destructuring-form min.max)
  (let ((min (car min.max))
        (max (cdr min.max)))
    (error-of-type 'error
      (TEXT "The object to be destructured should be a list with ~:[at least ~*~S~;~:[from ~S to ~S~;~S~]~] elements, not ~4@*~S.")
      max (eql min max) min max destructuring-form
) ) )

;; ----------------------------------------------------------------------------

;; X3J13 vote <87>

(defun complement (fun)
  #'(lambda (&rest arguments) (not (apply fun arguments)))
)

;; ANSI-CL

(defun constantly (object)
  #'(lambda (&rest arguments) (declare (ignore arguments)) object)
)

;; ----------------------------------------------------------------------------

;; part of X3J13 vote <40>

(defconstant *common-lisp-user-package* (find-package "COMMON-LISP-USER"))

(defmacro with-standard-io-syntax (&body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (SYSTEM::PARSE-BODY body nil env)
    ;; It would be possible to put all these bindings into a single function,
    ;; but this would force variables into closures.
    `(LET (;; printer/reader variables:
           (*PACKAGE*                   *COMMON-LISP-USER-PACKAGE*)
           ;; printer variables:
           (*PRINT-ARRAY*               T)
           (*PRINT-BASE*                10)
           (*PRINT-CASE*                ':UPCASE)
           (*PRINT-CIRCLE*              NIL)
           (*PRINT-ESCAPE*              T)
           (*PRINT-GENSYM*              T)
           (*PRINT-LENGTH*              NIL)
           (*PRINT-LEVEL*               NIL)
           (*PRINT-LINES*               NIL)
           (*PRINT-MISER-WIDTH*         NIL)
           (*PRINT-PPRINT-DISPATCH*     NIL)
           (*PRINT-PRETTY*              NIL)
           (*PRINT-RADIX*               NIL)
           (*PRINT-READABLY*            T)
           (*PRINT-RIGHT-MARGIN*        NIL)
           (*PRINT-CLOSURE*             NIL) ; CLISP specific
           (*PRINT-RPARS*               nil) ; CLISP specific
           (*PRINT-INDENT-LISTS*        1)   ; CLISP specific
           (SYSTEM::*PRIN-STREAM*       NIL) ; CLISP specific
           (SYSTEM::*PRIN-LINELENGTH*   79)  ; CLISP specific
           (SYSTEM::*PRIN-LINE-PREFIX*  NIL) ; CLISP specific
           ;; reader variables:
           (*READ-BASE*                 10)
           (*READ-DEFAULT-FLOAT-FORMAT* 'SINGLE-FLOAT)
           (*READ-EVAL*                 T)
           (*READ-SUPPRESS*             NIL)
           (*READTABLE*                 (COPY-READTABLE NIL)))
       ,@(if declarations `((DECLARE ,@declarations)))
       ,@body-rest)))

;; ----------------------------------------------------------------------------

;; part of X3J13 vote <98>

(defmacro with-hash-table-iterator ((macroname hashtable) &body body)
  (unless (symbolp macroname)
    (error (TEXT "~S: macro name should be a symbol, not ~S")
           'with-hash-table-iterator macroname))
  (let ((var (gensym)))
    `(LET ((,var (SYS::HASH-TABLE-ITERATOR ,hashtable)))
       (MACROLET ((,macroname () '(SYS::HASH-TABLE-ITERATE ,var) ))
         ,@body))))

;; ----------------------------------------------------------------------------

;; ANSI-CL

(defmacro lambda (&whole whole lambdalist &body body)
  (declare (ignore lambdalist body))
  `(FUNCTION ,whole))

;; ----------------------------------------------------------------------------

;; Make GET-MACRO-CHARACTER work on dispatch macro characters.
(let ((vector '#()))
  (declare (compile))
  ; This code must be in accordance with io.d:read_macro().
  (defun dispatch-reader (stream ch)
    (let ((arg 0)
          subch)
      (let ((flag nil))
        (loop
          (let ((nextch (read-char stream nil nil)))
            (unless nextch
              (error-of-type 'end-of-file
                :stream stream
                (TEXT "~S: input stream ~S ends within read macro beginning to ~S")
                'read stream ch
            ) )
            (unless (characterp nextch)
              (error-of-type 'stream-error
                :stream stream
                (TEXT "~S from ~S: character read should be a character: ~S")
                'read stream ch
            ) )
            (unless (char<= #\0 nextch #\9)
              (setq subch nextch)
              (return)
            )
            (setq arg (+ (* 10 arg) (digit-char-p nextch)))
            (setq flag t)
        ) )
        (unless flag (setq arg nil))
      )
      (let* ((subc (char-upcase subch))
             (macrodef
               (if (< (char-code subc) #x100)
                 (svref vector (char-code subc))
                 (gethash subc (svref vector #x100))
            )) )
        (unless macrodef
          (error-of-type 'stream-error
            :stream stream
            (TEXT "~S from ~S: After ~S is ~S an undefined dispatch macro character")
            'read stream ch subch
        ) )
        (funcall macrodef stream subch arg)
  ) ) )
  (let ((vector-index
          (do ((i 0 (1+ i)))
               (nil)
            (when (eq (%record-ref #'dispatch-reader i) vector) (return i)))))
    (%defio #'dispatch-reader vector-index)
  )
)

;; ----------------------------------------------------------------------------

;; READ-SEQUENCE and WRITE-SEQUENCE are badly specified because they assume
;; that the stream has a unique element type, either subtype of CHARACTER or
;; subtype of INTEGER. But some streams (esp. generic-streams) have a type
;; of (OR CHARACTER INTEGER).

;; This is a little hack to get the non-ambigouous cases right.

(defun stream-input-element-type (stream)
  (loop
    (typecase stream
      (SYNONYM-STREAM
        (setq stream (symbol-value (synonym-stream-symbol stream)))
      )
      (ECHO-STREAM
        (setq stream (echo-stream-input-stream stream))
      )
      (TWO-WAY-STREAM
        (setq stream (two-way-stream-input-stream stream))
      )
      (T (return))
  ) )
  (stream-element-type stream)
)

(defun stream-output-element-type (stream)
  (loop
    (typecase stream
      (SYNONYM-STREAM
        (setq stream (symbol-value (synonym-stream-symbol stream)))
      )
      (ECHO-STREAM
        (setq stream (echo-stream-output-stream stream))
      )
      (TWO-WAY-STREAM
        (setq stream (two-way-stream-output-stream stream))
      )
      (T (return))
  ) )
  (stream-element-type stream)
)

(defun read-sequence (sequence stream &rest rest &key (start 0) (end nil))
  (declare (ignore start end))
  (let ((eltype (stream-input-element-type stream)))
    (cond ((or (eq eltype 'NIL) (eq eltype 'CHARACTER))
           (apply #'read-char-sequence sequence stream rest)
          )
          ((subtypep eltype 'INTEGER)
           (apply #'read-byte-sequence sequence stream rest)
          )
          (t
           (error (TEXT "~S: ~S of ~S is ambiguous. Please use ~S or ~S.")
                  'read-sequence 'stream-element-type stream
                  'read-char-sequence 'read-byte-sequence
) ) )     ))

(defun write-sequence (sequence stream &rest rest &key (start 0) (end nil))
  (declare (ignore start end))
  (let ((eltype (stream-output-element-type stream)))
    (cond ((or (eq eltype 'NIL) (eq eltype 'CHARACTER))
           (apply #'write-char-sequence sequence stream rest)
          )
          ((subtypep eltype 'INTEGER)
           (apply #'write-byte-sequence sequence stream rest)
          )
          (t
           (error (TEXT "~S: ~S of ~S is ambiguous. Please use ~S or ~S.")
                  'write-sequence 'stream-element-type stream
                  'write-char-sequence 'write-byte-sequence
) ) )     ))

;; ----------------------------------------------------------------------------

;; ANSI-CL specifies TYPE-ERRORs in many places.
;; Here are the corresponding types.

;; (DESIGNATOR thing) is an abbreviation for many terms seen in the CLHS
;; glossary.

;; bounding index sequence    (START-INDEX sequence), (END-INDEX sequence)
;; character                  CHARACTER, BASE-CHAR
;; class                      CLASS
;; condition                  ---
;; extended function          EXTENDED-FUNCTION
;; external file format       ---
;; file position              FILE-POSITION
;; function                   FUNCTION
;; interval                   ---
;; list                       LIST
;; logical-host               LOGICAL-HOST
;; package                    PACKAGE
;; pathname                   PATHNAME
;; readtable                  READTABLE
;; restart                    RESTART
;; spreadable argument list   ---
;; stream                     STREAM
;; stream variable            ---
;; string                     STRING, (STRING length)

(deftype designator (thing)
  (cond ((symbolp thing)
         (case thing
;          (STRING
;            `(OR CHARACTER STRING SYMBOL)
;          )
           (CHARACTER
             `(OR CHARACTER
                  ,@(if (not *ansi*) `((INTEGER 0 ,(1- char-code-limit))))
                  (DESIGNATOR (STRING 1))
           )  )
           (BASE-CHAR
             `(OR BASE-CHAR
                  ,@(if (not *ansi*) `((INTEGER 0 ,(1- base-char-code-limit))))
                  #+BASE-CHAR=CHARACTER
                  (DESIGNATOR (STRING 1))
                  #-BASE-CHAR=CHARACTER
                  (AND (DESIGNATOR (STRING 1)) (SATISFIES BASE-CHAR-DESIGNATOR-P))
           )  )
;          (CLASS `(OR CLOS:CLASS (AND SYMBOL (SATISFIES CLASS-DESIGNATOR-P))))
;          (EXTENDED-FUNCTION
;            `(OR (AND (OR SYMBOL CONS) (SATISFIES FUNCTION-NAME-P)) FUNCTION)
;          )
;          (FILE-POSITION
;            `(OR (MEMBER :START :END) (INTEGER 0 *))
;          )
;          (FUNCTION
;            `(OR SYMBOL FUNCTION)
;          )
;          (LIST
;            `T
;          )
;          (LOGICAL-HOST
;            #-LOGICAL-PATHNAMES `NIL
;            #+LOGICAL-PATHNAMES `(OR STRING LOGICAL-PATHNAME)
;          )
;          (PACKAGE
;            `(OR (DESIGNATOR STRING) PACKAGE)
;          )
;          (PATHNAME
;            `(OR STRING FILE-STREAM PATHNAME)
;          )
;          (READTABLE
;            `(OR NULL READTABLE)
;          )
;          (RESTART
;            `(OR (AND SYMBOL (NOT NULL)) RESTART)
;          )
;          (STREAM
;            `(OR BOOLEAN STREAM)
;          )
           (t thing)
        ))
        ((consp thing)
         (case (first thing)
;          (START-INDEX
;            (let ((seq (second thing)))
;              (assert (typep seq 'SEQUENCE))
;              `(INTEGER 0 ,(length seq))
;          ) )
;          (END-INDEX
;            (let ((seq (second thing)))
;              (assert (typep seq 'SEQUENCE))
;              `(OR (INTEGER 0 ,(length (second thing))) NULL)
;          ) )
           (STRING
             (let ((n (second thing)))
               (assert (typep n '(INTEGER 0 *)))
               (let ((fun (intern (format nil "SYMBOL-OF-LENGTH-~D" n)
                                  (find-package "SYSTEM"))))
                 (unless (fboundp fun)
                   (setf (symbol-function fun)
                         #'(lambda (s)
                             (and (symbolp s) (eql (length (symbol-name s)) n))
                           )
                 ) )
                 `(OR ,@(if (eql n 1) '(CHARACTER) '())
                      (STRING ,n)
                      (AND SYMBOL (SATISFIES ,fun))
                  )
           ) ) )
           (t thing)
        ))
        (t (typespec-error 'designator thing))
) )

#-BASE-CHAR=CHARACTER
(defun base-char-designator-p (obj)
  (base-char-p (char (coerce obj 'string) 0))
)

;(defun class-designator-p (sym &aux f)
;  (and (setq f (get sym 'CLOS::CLOSCLASS))
;       (clos::class-p f)
;       (eq (clos:class-name f) sym)
;) )

(defun recognizable-sequence-type-p (typespec)
  (or (subtypep typespec 'LIST) (subtypep typespec 'VECTOR))
)

;; ----------------------------------------------------------------------------

