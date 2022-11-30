;;; CLISP - PLACES.LSP
;;; CLISP-spezifisch: string-concat, %rplaca, %rplacd, store, %setelt, ...

(in-package "SYSTEM")

;;;----------------------------------------------------------------------------
;;; Funktionen zur Definition und zum Ausnutzen von places:
;;;----------------------------------------------------------------------------
;; Return a symbol for SYSTEM::SETF-FUNCTION
;; the returned symbol will be interned iff the argument is.
(defun setf-symbol (symbol)
  (let* ((pack (symbol-package symbol))
         (name (string-concat "(SETF " (if pack (package-name pack) "#") ":"
                              (symbol-name symbol) ")")))
    (if pack
        (intern name pack)
        (make-symbol name))))
;;;----------------------------------------------------------------------------
;; Returns the symbol which is on the property list at SYSTEM::SETF-FUNCTION
(defun get-setf-symbol (symbol)
  (or (get symbol 'SYSTEM::SETF-FUNCTION)
      (progn
        (when (get symbol 'SYSTEM::SETF-EXPANDER)
          (warn (TEXT "The function (~S ~S) is hidden by a SETF expander.")
                'setf symbol
        ) )
        (setf (get symbol 'SYSTEM::SETF-FUNCTION) (setf-symbol symbol))
) )   )
;;;----------------------------------------------------------------------------
;; Mapping funname -> symbol
(defun get-funname-symbol (funname)
  (if (atom funname)
    funname
    (get-setf-symbol (second funname))
) )
;;;----------------------------------------------------------------------------
;; Returns 5 values:
;;   SM1  vr  variables to bind
;;   SM2  vl  values to bind to
;;   SM3  sv  variables whose values are used by the setter form
;;   SM4  se  setter form
;;   SM5  ge  getter form
(defun get-setf-expansion (form &optional (env (vector nil nil)))
  (loop
    ; 1. Schritt: nach globalen SETF-Definitionen suchen:
    (when (and (consp form) (symbolp (car form)))
      (when (global-in-fenv-p (car form) (svref env 1))
        ; Operator nicht lokal definiert
        (let ((plist-info (get (first form) 'SYSTEM::SETF-EXPANDER)))
          (when plist-info
            (return-from get-setf-expansion
              (if (symbolp plist-info) ; Symbol kommt von kurzem DEFSETF
                (do* ((storevar (gensym))
                      (tempvars nil (cons (gensym) tempvars))
                      (tempforms nil (cons (car formr) tempforms))
                      (formr (cdr form) (cdr formr)))
                     ((atom formr)
                      (setq tempforms (nreverse tempforms))
                      (values tempvars
                              tempforms
                              `(,storevar)
                              `(,plist-info ,@tempvars ,storevar)
                              `(,(first form) ,@tempvars)
                     ))
                )
                (let ((argcount (car plist-info)))
                  (if (eql argcount -5)
                    ; (-5 . fun) kommt von DEFINE-SETF-METHOD
                    (funcall (cdr plist-info) form env)
                    ; (argcount storevarcount . fun) kommt von langem DEFSETF
                    (let ((access-form form)
                          (tempvars '())
                          (tempforms '())
                          (new-access-form '()))
                      (let ((i 0)) ; Argumente-Zähler
                        ; argcount = -1 falls keine Keyword-Argumente existieren
                        ; bzw.     = Anzahl der einzelnen Argumente vor &KEY,
                        ;          = nil nachdem diese abgearbeitet sind.
                        (dolist (argform (cdr access-form))
                          (when (eql i argcount) (setq argcount nil i 0))
                          (if (and (null argcount) (evenp i))
                            (if (keywordp argform)
                              (push argform new-access-form)
                              (error-of-type 'source-program-error
                                (TEXT "The argument ~S to ~S should be a keyword.")
                                argform (car access-form)
                            ) )
                            (let ((tempvar (gensym)))
                              (push tempvar tempvars)
                              (push argform tempforms)
                              (push tempvar new-access-form)
                          ) )
                          (incf i)
                      ) )
                      (setq new-access-form
                        (cons (car access-form) (nreverse new-access-form)))
                      (let ((newval-vars (gensym-list (cadr plist-info))))
                        (values
                          (nreverse tempvars)
                          (nreverse tempforms)
                          newval-vars
                          (apply (cddr plist-info) new-access-form newval-vars)
                          new-access-form
                ) ) ) ) )
            ) )
    ) ) ) )
    ; 2. Schritt: macroexpandieren
    (when (eq form (setq form (macroexpand-1 form env)))
      (return)
  ) )
  ; 3. Schritt: Default-SETF-Methoden
  (cond ((symbolp form)
         (return-from get-setf-expansion
           (let ((storevar (gensym)))
             (values nil
                     nil
                     `(,storevar)
                     `(SETQ ,form ,storevar)
                     `,form
        )) ) )
        ((and (consp form) (symbolp (car form)))
         (return-from get-setf-expansion
           (do* ((storevar (gensym))
                 (tempvars nil (cons (gensym) tempvars))
                 (tempforms nil (cons (car formr) tempforms))
                 (formr (cdr form) (cdr formr)))
                ((atom formr)
                 (setq tempforms (nreverse tempforms))
                 (values tempvars
                         tempforms
                         `(,storevar)
                         `((SETF ,(first form)) ,storevar ,@tempvars)
                         `(,(first form) ,@tempvars)
                ))
        )) )
        (t (error-of-type 'source-program-error
             (TEXT "Argument ~S is not a SETF place.")
             form
  )     )  )
)
;;;----------------------------------------------------------------------------
(defun get-setf-method (form &optional (env (vector nil nil)))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion form env)
    (unless (and (consp stores) (null (cdr stores)))
      (error-of-type 'source-program-error
        (TEXT "SETF place ~S produces more than one store variable.")
        form
    ) )
    (values vars vals stores store-form access-form)
) )
;;;----------------------------------------------------------------------------
; In einfachen Zuweisungen wie (SETQ foo #:G0) darf #:G0 direkt ersetzt werden.
(defun simple-assignment-p (store-form stores)
  (and (= (length stores) 1)
       (consp store-form)
       (eq (first store-form) 'SETQ)
       (= (length store-form) 3)
       (symbolp (second store-form))
       (simple-use-p (third store-form) (first stores))
) )
(defun simple-use-p (form var)
  (or (eq form var)
      (and (consp form) (eq (first form) 'THE) (= (length form) 3)
           (simple-use-p (third form) var)
) )   )
;;;----------------------------------------------------------------------------
(defun documentation (symbol doctype)
  (unless (function-name-p symbol)
    (error-of-type 'error
      (TEXT "~S: first argument ~S is illegal, not a symbol")
      'documentation symbol
  ) )
  (getf (get (get-funname-symbol symbol) 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
)
(defun SYSTEM::%SET-DOCUMENTATION (symbol doctype value)
  (unless (function-name-p symbol)
    (error-of-type 'error
      (TEXT "~S: first argument ~S is illegal, not a symbol")
      'documentation symbol
  ) )
  (setq symbol (get-funname-symbol symbol))
  (if (null value)
    (when (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
      (remf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
      nil
    )
    (setf (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype) value)
) )
;;;----------------------------------------------------------------------------
(defmacro push (item place &environment env)
  (multiple-value-bind (vr vl sv se ge) (get-setf-expansion place env)
    (if (simple-assignment-p se sv)
      (subst `(CONS ,item ,ge) (car sv) se)
      (let* ((bindlist (mapcar #'list vr vl))
             (tempvars (gensym-list (length sv)))
             (ns (sublis (mapcar #'(lambda (storevar tempvar)
                                     (cons storevar
                                           `(CONS ,storevar ,tempvar)))
                                 sv tempvars)
                         se)))
        `(MULTIPLE-VALUE-BIND ,sv ,item
           (LET* ,bindlist
             (MULTIPLE-VALUE-BIND ,tempvars ,ge
               ,ns)))))))
;;;----------------------------------------------------------------------------
(eval-when (load compile eval)
  (defun check-accessor-name (accessfn)
    (unless (symbolp accessfn)
      (error-of-type 'source-program-error
        (TEXT "The name of the accessor must be a symbol, not ~S")
        accessfn))))
(defmacro define-setf-expander (accessfn lambdalist &body body
                                &environment env)
  (check-accessor-name accessfn)
  (sys::check-redefinition
   accessfn 'define-setf-expander
   (and (get accessfn 'SYSTEM::SETF-EXPANDER) "SETF expander"))
  (multiple-value-bind (body-rest declarations docstring)
      (system::parse-body body t env)
    (if (null body-rest) (setq body-rest '(NIL)))
    (let ((name (make-symbol (string-concat "SETF-" (symbol-name accessfn)))))
      (multiple-value-bind (newlambdalist envvar) (remove-env-arg lambdalist name)
        (let ((SYSTEM::%ARG-COUNT 0)
              (SYSTEM::%MIN-ARGS 0)
              (SYSTEM::%RESTP nil)
              (SYSTEM::%LET-LIST nil)
              (SYSTEM::%KEYWORD-TESTS nil)
              (SYSTEM::%DEFAULT-FORM nil)
             )
          (SYSTEM::ANALYZE1 newlambdalist '(CDR SYSTEM::%LAMBDA-LIST)
                            name 'SYSTEM::%LAMBDA-LIST
          )
          (if (null newlambdalist)
            (push `(IGNORE SYSTEM::%LAMBDA-LIST) declarations)
          )
          (let ((lengthtest (sys::make-length-test 'SYSTEM::%LAMBDA-LIST))
                (mainform
                  `(LET* ,(nreverse SYSTEM::%LET-LIST)
                     ,@(if declarations `(,(cons 'DECLARE declarations)))
                     ,@SYSTEM::%KEYWORD-TESTS
                     (BLOCK ,accessfn ,@body-rest)
                   )
               ))
            (if lengthtest
              (setq mainform
                `(IF ,lengthtest
                   (ERROR-OF-TYPE 'PROGRAM-ERROR
                     (TEXT "The SETF expander for ~S may not be called with ~S arguments.")
                     (QUOTE ,accessfn) (1- (LENGTH SYSTEM::%LAMBDA-LIST))
                   )
                   ,mainform
              )  )
            )
            `(EVAL-WHEN (LOAD COMPILE EVAL)
               (LET ()
                 (REMPROP ',accessfn 'SYSTEM::DEFSTRUCT-WRITER)
                 (DEFUN ,name (SYSTEM::%LAMBDA-LIST ,(or envvar 'SYSTEM::ENV))
                   ,@(if envvar '() '((DECLARE (IGNORE SYSTEM::ENV))))
                   ,mainform
                 )
                 (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER
                   (CONS -5 (FUNCTION ,name))
                 )
                 (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF ',docstring)
                 ',accessfn
             ) )
) ) ) ) ) )
;;;----------------------------------------------------------------------------
(defmacro defsetf (accessfn &rest args &environment env)
  (check-accessor-name accessfn)
  (sys::check-redefinition
   accessfn 'DEFSETF
   (and (get accessfn 'SYSTEM::SETF-EXPANDER) "SETF expander"))
  (cond ((and (consp args) (not (listp (first args))) (symbolp (first args)))
         `(EVAL-WHEN (LOAD COMPILE EVAL)
            (LET ()
              (REMPROP ',accessfn 'SYSTEM::DEFSTRUCT-WRITER)
              (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER ',(first args))
              (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF
                ,(if (and (null (cddr args))
                          (or (null (second args)) (stringp (second args)))
                     )
                   (second args)
                   (if (cddr args)
                     (error-of-type 'source-program-error
                       (TEXT "Too many arguments to DEFSETF: ~S")
                       (cdr args)
                     )
                     (error-of-type 'source-program-error
                       (TEXT "The doc string to DEFSETF must be a string: ~S")
                       (second args)
                 ) ) )
              )
              ',accessfn
          ) )
        )
        ((and (consp args) (listp (first args)) (consp (cdr args)) (listp (second args)))
         (when (null (second args))
           (error-of-type 'source-program-error
             (TEXT "Missing store variable in DEFSETF.")))
         (multiple-value-bind (body-rest declarations docstring)
             (system::parse-body (cddr args) t env)
           (let* ((storevars (second args))
                  arg-count
                  (setter
                    (let* ((lambdalist (first args))
                           (SYSTEM::%ARG-COUNT 0)
                           (SYSTEM::%MIN-ARGS 0)
                           (SYSTEM::%RESTP nil)
                           (SYSTEM::%LET-LIST nil)
                           (SYSTEM::%KEYWORD-TESTS nil)
                           (SYSTEM::%DEFAULT-FORM nil))
                      (SYSTEM::ANALYZE1 lambdalist '(CDR SYSTEM::%ACCESS-ARGLIST)
                                        accessfn 'SYSTEM::%ACCESS-ARGLIST
                      )
                      (setq arg-count (if (member '&KEY lambdalist) SYSTEM::%ARG-COUNT -1))
                      (when declarations (setq declarations `((DECLARE ,@declarations))))
                      `(LAMBDA (SYSTEM::%ACCESS-ARGLIST ,@storevars)
                         ,@(if (null lambdalist)
                             `((DECLARE (IGNORE SYSTEM::%ACCESS-ARGLIST)))
                           )
                         ,@declarations
                         (LET* ,(nreverse SYSTEM::%LET-LIST)
                           ,@declarations
                           ,@SYSTEM::%KEYWORD-TESTS
                           (BLOCK ,accessfn ,@body-rest)
                       ) )
                 )) )
             `(EVAL-WHEN (LOAD COMPILE EVAL)
                (LET ()
                  (REMPROP ',accessfn 'SYSTEM::DEFSTRUCT-WRITER)
                  (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER
                    (LIST* ,arg-count ,(length storevars)
                           (FUNCTION ,(concat-pnames "SETF-" accessfn) ,setter)
                  ) )
                  (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF ,docstring)
                  ',accessfn
              ) )
        )) )
        (t (error-of-type 'source-program-error
             (TEXT "Illegal syntax in DEFSETF for ~S")
             accessfn
) )     )  )
;;;----------------------------------------------------------------------------
;;; Definition of places:
;;;----------------------------------------------------------------------------
(defsetf package-lock SYSTEM::%SET-PACKAGE-LOCK)
(defsetf hash-table-weak-p SYSTEM::%SET-HASH-TABLE-WEAK-P)
;;;----------------------------------------------------------------------------
(defsetf aref (array &rest indices) (value)
  `(SYSTEM::STORE ,array ,@indices ,value))
;;;----------------------------------------------------------------------------
(defun SYSTEM::%SETNTH (index list value)
  (let ((pointer (nthcdr index list)))
    (if (null pointer)
      (error-of-type 'error
        (TEXT "(SETF (NTH ...) ...) : index ~S is too large for ~S")
        index list
      )
      (rplaca pointer value)
    )
    value
) )
(defsetf nth SYSTEM::%SETNTH)
;;;----------------------------------------------------------------------------
(defsetf elt SYSTEM::%SETELT)
;;;----------------------------------------------------------------------------
(defsetf rest SYSTEM::%RPLACD)
(defsetf first SYSTEM::%RPLACA)
(defsetf second (list) (value) `(SYSTEM::%RPLACA (CDR ,list) ,value))
(defsetf third (list) (value) `(SYSTEM::%RPLACA (CDDR ,list) ,value))
(defsetf fourth (list) (value) `(SYSTEM::%RPLACA (CDDDR ,list) ,value))
(defsetf fifth (list) (value) `(SYSTEM::%RPLACA (CDDDDR ,list) ,value))
(defsetf sixth (list) (value) `(SYSTEM::%RPLACA (CDR (CDDDDR ,list)) ,value))
(defsetf seventh (list) (value) `(SYSTEM::%RPLACA (CDDR (CDDDDR ,list)) ,value))
(defsetf eighth (list) (value) `(SYSTEM::%RPLACA (CDDDR (CDDDDR ,list)) ,value))
(defsetf ninth (list) (value) `(SYSTEM::%RPLACA (CDDDDR (CDDDDR ,list)) ,value))
(defsetf tenth (list) (value) `(SYSTEM::%RPLACA (CDR (CDDDDR (CDDDDR ,list))) ,value))

(defsetf car SYSTEM::%RPLACA)
(defsetf cdr SYSTEM::%RPLACD)
(defsetf caar (list) (value) `(SYSTEM::%RPLACA (CAR ,list) ,value))
(defsetf cadr (list) (value) `(SYSTEM::%RPLACA (CDR ,list) ,value))
(defsetf cdar (list) (value) `(SYSTEM::%RPLACD (CAR ,list) ,value))
(defsetf cddr (list) (value) `(SYSTEM::%RPLACD (CDR ,list) ,value))
(defsetf caaar (list) (value) `(SYSTEM::%RPLACA (CAAR ,list) ,value))
(defsetf caadr (list) (value) `(SYSTEM::%RPLACA (CADR ,list) ,value))
(defsetf cadar (list) (value) `(SYSTEM::%RPLACA (CDAR ,list) ,value))
(defsetf caddr (list) (value) `(SYSTEM::%RPLACA (CDDR ,list) ,value))
(defsetf cdaar (list) (value) `(SYSTEM::%RPLACD (CAAR ,list) ,value))
(defsetf cdadr (list) (value) `(SYSTEM::%RPLACD (CADR ,list) ,value))
(defsetf cddar (list) (value) `(SYSTEM::%RPLACD (CDAR ,list) ,value))
(defsetf cdddr (list) (value) `(SYSTEM::%RPLACD (CDDR ,list) ,value))
(defsetf caaaar (list) (value) `(SYSTEM::%RPLACA (CAAAR ,list) ,value))
(defsetf caaadr (list) (value) `(SYSTEM::%RPLACA (CAADR ,list) ,value))
(defsetf caadar (list) (value) `(SYSTEM::%RPLACA (CADAR ,list) ,value))
(defsetf caaddr (list) (value) `(SYSTEM::%RPLACA (CADDR ,list) ,value))
(defsetf cadaar (list) (value) `(SYSTEM::%RPLACA (CDAAR ,list) ,value))
(defsetf cadadr (list) (value) `(SYSTEM::%RPLACA (CDADR ,list) ,value))
(defsetf caddar (list) (value) `(SYSTEM::%RPLACA (CDDAR ,list) ,value))
(defsetf cadddr (list) (value) `(SYSTEM::%RPLACA (CDDDR ,list) ,value))
(defsetf cdaaar (list) (value) `(SYSTEM::%RPLACD (CAAAR ,list) ,value))
(defsetf cdaadr (list) (value) `(SYSTEM::%RPLACD (CAADR ,list) ,value))
(defsetf cdadar (list) (value) `(SYSTEM::%RPLACD (CADAR ,list) ,value))
(defsetf cdaddr (list) (value) `(SYSTEM::%RPLACD (CADDR ,list) ,value))
(defsetf cddaar (list) (value) `(SYSTEM::%RPLACD (CDAAR ,list) ,value))
(defsetf cddadr (list) (value) `(SYSTEM::%RPLACD (CDADR ,list) ,value))
(defsetf cdddar (list) (value) `(SYSTEM::%RPLACD (CDDAR ,list) ,value))
(defsetf cddddr (list) (value) `(SYSTEM::%RPLACD (CDDDR ,list) ,value))
;;;----------------------------------------------------------------------------
(defsetf svref SYSTEM::SVSTORE)
(defsetf row-major-aref system::row-major-store)
;;;----------------------------------------------------------------------------
;; Simplify a form, when its values are not needed, only its side effects.
;; Returns a list of subforms.
;;   (values x y z) --> (x y z)
;;   x --> (x)
(defun devalue-form (form)
  (if (eq (car form) 'VALUES) (cdr form) (list form))
)
;;;----------------------------------------------------------------------------
(defmacro pop (place &environment env)
  (multiple-value-bind (vr vl sv se ge) (get-setf-expansion place env)
    (if (and (symbolp ge) (simple-assignment-p se sv))
      `(PROG1 (CAR ,ge) ,(subst `(CDR ,ge) (car sv) se))
      ;; Be sure to call the CARs before the CDRs - it matters in case
      ;; not all of the places evaluate to lists.
      (let ((bindlist (mapcar #'list vr vl)))
        (if (= (length sv) 1)
          `(LET* ,(append bindlist (list (list (car sv) ge)))
             (PROG1
               (CAR ,(car sv))
               ,@(devalue-form (subst `(CDR ,(car sv)) (car sv) se))
           ) )
          `(LET* ,bindlist
             (MULTIPLE-VALUE-BIND ,sv ,ge
               (MULTIPLE-VALUE-PROG1
                 (VALUES ,@(mapcar #'(lambda (storevar) `(CAR ,storevar)) sv))
                 ,@(devalue-form
                     (sublis (mapcar #'(lambda (storevar)
                                         (cons storevar `(CDR ,storevar))
                                       )
                                     sv
                             )
                             se
                   ) )
           ) ) )
) ) ) ) )
;----------------------------------------------------------------------------
(defmacro psetf (&whole form &rest args &environment env)
  (labels ((recurse (args)
             (multiple-value-bind (vr vl sv se ge)
                 (get-setf-expansion (car args) env)
               (declare (ignore ge))
               (when (atom (cdr args))
                 (error-of-type 'source-program-error
                   (TEXT "PSETF called with an odd number of arguments: ~S")
                   form))
               `(LET* ,(mapcar #'list vr vl)
                  (MULTIPLE-VALUE-BIND ,sv ,(second args)
                    ,@(when (cddr args) (list (recurse (cddr args))))
                    ,@(devalue-form se)
                ) )
          )) )
    `(,@(recurse args) NIL)
) )
;;;----------------------------------------------------------------------------
(defmacro pushnew (item place &rest keylist &environment env)
  (multiple-value-bind (vr vl sv se ge) (get-setf-expansion place env)
    (if (simple-assignment-p se sv)
      (subst `(ADJOIN ,item ,ge ,@keylist) (car sv) se)
      (let* ((bindlist (mapcar #'list vr vl))
             (tempvars (gensym-list (length sv)))
             (ns (sublis (mapcar #'(lambda (storevar tempvar)
                                     (cons storevar `(ADJOIN ,storevar ,tempvar
                                                      ,@keylist)))
                                 sv tempvars)
                         se)))
        `(MULTIPLE-VALUE-BIND ,sv ,item
           (LET* ,bindlist
             (MULTIPLE-VALUE-BIND ,tempvars ,ge
               ,ns)))))))
;;;----------------------------------------------------------------------------
(defmacro remf (place indicator &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
    (let* ((indicatorvar (gensym))
           (bindlist
             `(,@(mapcar #'list SM1 SM2)
               (,(first SM3) ,SM5)
               (,indicatorvar ,indicator)))
           (var1 (gensym))
           (var2 (gensym)))
      `(LET* ,bindlist
         (DO ((,var1 ,(first SM3) (CDDR ,var1))
              (,var2 NIL ,var1))
             ((ATOM ,var1) NIL)
           (COND ((ATOM (CDR ,var1))
                  (ERROR-OF-TYPE 'ERROR
                    (TEXT "REMF: property list with an odd length")
                 ))
                 ((EQ (CAR ,var1) ,indicatorvar)
                  (IF ,var2
                    (RPLACD (CDR ,var2) (CDDR ,var1))
                    ,(let ((newvalform `(CDDR ,(first SM3))))
                       (if (simple-assignment-p SM4 SM3)
                         (subst newvalform (first SM3) SM4)
                         `(PROGN (SETQ ,(first SM3) ,newvalform) ,SM4)
                     ) )
                 )
                 (RETURN T)
      ) ) )     )
) ) )
;;;----------------------------------------------------------------------------
(defmacro rotatef (&rest args &environment env)
  (when (null args) (return-from rotatef NIL))
  (when (null (cdr args)) (return-from rotatef `(PROGN ,(car args) NIL)))
  (do* ((arglist args (cdr arglist))
        (res (list 'let* nil nil)) lf
        (tail (cdr res)) bindlist stores lv fv)
       ((null arglist)
        (setf (second res) (nreverse bindlist)
              (second (third res)) lv
              (cdr tail) (nconc (nreverse stores) (devalue-form lf))
              (cdr (last res)) (list nil))
        res)
    (multiple-value-bind (vr vl sv se ge)
        (get-setf-expansion (first arglist) env)
      (setq bindlist (nreconc (mapcar #'list vr vl) bindlist))
      (setf (cadr tail) (list 'MULTIPLE-VALUE-BIND lv ge nil))
      (setq tail (cddadr tail))
      (if (null fv)
        (setq fv sv)
        (setq stores (revappend (devalue-form lf) stores))
      )
      (setq lv sv lf se))))
;;;----------------------------------------------------------------------------
(defmacro define-modify-macro (name lambdalist function &optional docstring)
  (let* ((varlist nil)
         (restvar nil))
    (do* ((lambdalistr lambdalist (cdr lambdalistr))
          (next))
         ((null lambdalistr))
      (setq next (first lambdalistr))
      (cond ((eq next '&OPTIONAL))
            ((eq next '&REST)
             (if (symbolp (second lambdalistr))
               (setq restvar (second lambdalistr))
               (error-of-type 'source-program-error
                 (TEXT "In the definition of ~S: &REST variable ~S should be a symbol.")
                 name (second lambdalistr)
             ) )
             (if (null (cddr lambdalistr))
               (return)
               (error-of-type 'source-program-error
                 (TEXT "Only one variable is allowed after &REST, not ~S")
                 lambdalistr
            )) )
            ((or (eq next '&KEY) (eq next '&ALLOW-OTHER-KEYS) (eq next '&AUX))
             (error-of-type 'source-program-error
               (TEXT "Illegal in a DEFINE-MODIFY-MACRO lambda list: ~S")
               next
            ))
            ((symbolp next) (push next varlist))
            ((and (listp next) (symbolp (first next)))
             (push (first next) varlist)
            )
            (t (error-of-type 'source-program-error
                 (TEXT "lambda list may only contain symbols and lists, not ~S")
                 next
            )  )
    ) )
    (setq varlist (nreverse varlist))
    `(DEFMACRO ,name (%REFERENCE ,@lambdalist &ENVIRONMENT ENV) ,docstring
       (MULTIPLE-VALUE-BIND (DUMMIES VALS NEWVAL SETTER GETTER)
           (GET-SETF-METHOD %REFERENCE ENV)
         (DO ((D DUMMIES (CDR D))
              (V VALS (CDR V))
              (LET-LIST NIL (CONS (LIST (CAR D) (CAR V)) LET-LIST)))
             ((NULL D)
              (WHEN (SYMBOLP GETTER)
                (RETURN
                  (SUBST
                    (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                    (CAR NEWVAL)
                    SETTER
              ) ) )
              (PUSH
                (LIST
                  (CAR NEWVAL)
                  (IF (AND (LISTP %REFERENCE) (EQ (CAR %REFERENCE) 'THE))
                    (LIST 'THE (CADR %REFERENCE)
                      (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                    )
                    (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                ) )
                LET-LIST
              )
              (LIST 'LET* (NREVERSE LET-LIST) SETTER)
     ) ) ) )
) )
;;;----------------------------------------------------------------------------
(define-modify-macro decf (&optional (delta 1)) -)
;;;----------------------------------------------------------------------------
(define-modify-macro incf (&optional (delta 1)) +)
;;;----------------------------------------------------------------------------
(defmacro setf (&whole form &rest args &environment env)
  (let ((argcount (length args)))
    (cond ((eql argcount 2)
           (let* ((place (first args))
                  (value (second args)))
             (loop
               ;; 1. Schritt: nach globalen SETF-Definitionen suchen:
               (when (and (consp place) (symbolp (car place)))
                 (when (global-in-fenv-p (car place) (svref env 1))
                   ; Operator nicht lokal definiert
                   (let ((plist-info (get (first place) 'SYSTEM::SETF-EXPANDER)))
                     (when plist-info
                       (return-from setf
                         (cond ((symbolp plist-info) ; Symbol kommt von kurzem DEFSETF
                                `(,plist-info ,@(cdr place) ,value)
                               )
                               ((and (eq (first place) 'THE) (eql (length place) 3))
                                `(SETF ,(third place) (THE ,(second place) ,value))
                               )
                               ((and (eq (first place) 'VALUES-LIST) (eql (length place) 2))
                                `(VALUES-LIST
                                   (SETF ,(second place)
                                         (MULTIPLE-VALUE-LIST ,value)
                               ) ) )
                               (t
                                (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
                                    (get-setf-expansion place env)
                                  (declare (ignore SM5))
                                  (let ((bindlist (mapcar #'list SM1 SM2)))
                                    (if (= (length SM3) 1)
                                      ;; 1 store variable
                                      `(LET* ,(append bindlist
                                                 (list `(,(first SM3) ,value))
                                              )
                                         ,SM4
                                       )
                                      ;; mehrere Store-Variable
                                      (if ;; Hat SM4 die Gestalt
                                          ;; (VALUES (SETQ v1 store1) ...) ?
                                        (and (consp SM4)
                                             (eq (car SM4) 'VALUES)
                                             (do ((SM3r SM3 (cdr SM3r))
                                                  (SM4r (cdr SM4) (cdr SM4r)))
                                                 ((or (null SM3r) (null SM4r))
                                                  (and (null SM3r) (null SM4r)))
                                               (unless (simple-assignment-p (car SM4r) (list (car SM3r)))
                                                 (return nil)
                                        )    ) )
                                        (let ((vlist (mapcar #'second (rest SM4))))
                                          `(LET* ,bindlist
                                             (MULTIPLE-VALUE-SETQ ,vlist ,value)
                                             (VALUES ,@vlist)
                                           )
                                        )
                                        `(LET* ,bindlist
                                           (MULTIPLE-VALUE-BIND ,SM3 ,value
                                             ,SM4
                                         ) )
                               )) ) ) )
               ) ) ) ) ) )
               ;; 2. Schritt: macroexpandieren
               (when (eq place (setq place (macroexpand-1 place env)))
                 (return)
             ) )
             ;; 3. Schritt: Default-SETF-Methoden
             (cond ((symbolp place)
                    `(SETQ ,place ,value)
                   )
                   ((and (consp form) (symbolp (car form)))
                    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
                        (get-setf-expansion place env)
                      (declare (ignore SM5))
                      ; SM4 hat die Gestalt `((SETF ,(first place)) ,@SM3 ,@SM1).
                      ; SM3 ist überflüssig.
                      `(LET* ,(mapcar #'list SM1 SM2)
                         ,(subst value (first SM3) SM4)
                       )
                   ))
                   (t (error-of-type 'source-program-error
                        (TEXT "Illegal SETF place: ~S")
                        (first args)
             )     )  )
          ))
          ((oddp argcount)
           (error-of-type 'source-program-error
             (TEXT "~S called with an odd number of arguments: ~S")
             'setf form
          ))
          (t (do* ((arglist args (cddr arglist))
                   (L nil))
                  ((null arglist) `(LET () (PROGN ,@(nreverse L))))
               (push `(SETF ,(first arglist) ,(second arglist)) L)
          )  )
) ) )
;;;----------------------------------------------------------------------------
(defmacro shiftf (&whole form &rest args &environment env)
  (when (< (length args) 2)
    (error-of-type 'source-program-error
      (TEXT "SHIFTF called with too few arguments: ~S")
      form))
  (do* ((arglist args (cdr arglist))
        (res (list 'let* nil nil)) lf ff
        (tail (cdr res)) bindlist stores lv fv)
       ((null (cdr arglist))
        (setf (second res) (nreverse bindlist)
              (cadr tail) (list 'multiple-value-bind lv (car (last args)) nil)
              tail (cddadr tail)
              (cdr tail) (nconc (nreverse stores) (devalue-form lf))
              (third res) (list 'multiple-value-bind fv ff (third res)
                                (cons 'values fv)))
        res)
    (multiple-value-bind (vr vl sv se ge)
        (get-setf-expansion (first arglist) env)
      (setq bindlist (nreconc (mapcar #'list vr vl) bindlist))
      (if fv
          (setf stores (revappend (devalue-form lf) stores)
                (cadr tail) (list 'multiple-value-bind lv ge nil)
                tail (cddadr tail))
          (setq fv sv ff ge))
      (setq lv sv lf se))))
;;;----------------------------------------------------------------------------
;;; more places
;;;----------------------------------------------------------------------------
(defsetf GET (symbol indicator &optional default) (value)
  (let ((storeform `(SYSTEM::%PUT ,symbol ,indicator ,value)))
    (if default
      `(PROGN ,default ,storeform) ; default wird nur zum Schein ausgewertet
      `,storeform
) ) )
;;;----------------------------------------------------------------------------
;;; Schreibt zu einem bestimmten Indicator einen Wert in eine gegebene
;;; Propertyliste. Wert ist NIL falls erfolgreich getan oder die neue
;;; (erweiterte) Propertyliste.
(defun sys::%putf (plist indicator value)
  (do ((plistr plist (cddr plistr)))
      ((atom plistr) (list* indicator value plist))
    (when (atom (cdr plistr))
      (error-of-type 'error
        (TEXT "(SETF (GETF ...) ...) : property list with an odd length")
    ))
    (when (eq (car plistr) indicator)
      (rplaca (cdr plistr) value)
      (return nil)
) ) )
(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
    (let* ((storevar (gensym))
           (indicatorvar (gensym))
           (defaultvar-list (if default (list (gensym)) `()))
          )
      (values
        `(,@SM1 ,indicatorvar ,@defaultvar-list)
        `(,@SM2 ,indicator    ,@(if default `(,default) `()))
        `(,storevar)
        `(LET ((,(first SM3) (SYS::%PUTF ,SM5 ,indicatorvar ,storevar)))
           ,@defaultvar-list ; defaultvar zum Schein auswerten
           (WHEN ,(first SM3) ,SM4)
           ,storevar
         )
        `(GETF ,SM5 ,indicatorvar ,@defaultvar-list)
) ) ) )
;;;----------------------------------------------------------------------------
(defsetf GETHASH (key hashtable &optional default) (value)
  (let ((storeform `(SYSTEM::PUTHASH ,key ,hashtable ,value)))
    (if default
      `(PROGN ,default ,storeform) ; default wird nur zum Schein ausgewertet
      `,storeform
) ) )
;;;----------------------------------------------------------------------------
#| ; siehe oben:
(defun SYSTEM::%SET-DOCUMENTATION (symbol doctype value)
  (unless (function-name-p symbol)
    (error-of-type 'error
      (TEXT "first argument ~S is illegal, not a symbol")
      symbol
  ) )
  (setq symbol (get-funname-symbol symbol))
  (if (null value)
    (progn (remf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype) nil)
    (setf (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype) value)
) )
|#
(defsetf documentation SYSTEM::%SET-DOCUMENTATION)
;;;----------------------------------------------------------------------------
(defsetf fill-pointer SYSTEM::SET-FILL-POINTER)
;;;----------------------------------------------------------------------------
(defsetf readtable-case SYSTEM::SET-READTABLE-CASE)
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-VALUE SYSTEM::SET-SYMBOL-VALUE)
(sys::%putd 'SET #'SYSTEM::SET-SYMBOL-VALUE) ; deprecated alias
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-FUNCTION SYSTEM::%PUTD)
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-PLIST SYSTEM::%PUTPLIST)
;;;----------------------------------------------------------------------------
(defun SYSTEM::SET-FDEFINITION (name value)
  (setf (symbol-function (get-funname-symbol name)) value)
)
(defsetf FDEFINITION SYSTEM::SET-FDEFINITION)
;;;----------------------------------------------------------------------------
(defsetf MACRO-FUNCTION (symbol &optional env) (value)
  (declare (ignore env))
  `(PROGN
     (SETF (SYMBOL-FUNCTION ,symbol) (SYSTEM::MAKE-MACRO ,value))
     (REMPROP ,symbol 'SYSTEM::MACRO)
     ,value
   )
)
;;;----------------------------------------------------------------------------
(defsetf CHAR SYSTEM::STORE-CHAR)
(defsetf SCHAR SYSTEM::STORE-SCHAR)
(defsetf BIT SYSTEM::STORE)
(defsetf SBIT SYSTEM::STORE)
(defsetf SUBSEQ (sequence start &optional end) (value)
  `(PROGN (REPLACE ,sequence ,value :START1 ,start :END1 ,end) ,value)
)
;;;----------------------------------------------------------------------------
(define-setf-expander char-bit (char name &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method char env)
    (let* ((namevar (gensym))
           (storevar (gensym)))
      (values `(,@SM1 ,namevar)
              `(,@SM2 ,name)
              `(,storevar)
              `(LET ((,(first SM3) (SET-CHAR-BIT ,SM5 ,namevar ,storevar)))
                 ,SM4
                 ,storevar
               )
              `(CHAR-BIT ,SM5 ,namevar)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander LDB (bytespec integer &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method integer env)
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar SM1)
              (cons bytespec SM2)
              `(,storevar)
              `(LET ((,(first SM3) (DPB ,storevar ,bytespecvar ,SM5)))
                 ,SM4
                 ,storevar
               )
              `(LDB ,bytespecvar ,SM5)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander MASK-FIELD (bytespec integer &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method integer env)
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar SM1)
              (cons bytespec SM2)
              `(,storevar)
              `(LET ((,(first SM3) (DEPOSIT-FIELD ,storevar ,bytespecvar ,SM5)))
                 ,SM4
                 ,storevar
               )
              `(MASK-FIELD ,bytespecvar ,SM5)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander THE (type place &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-expansion place env)
    (values SM1 SM2 SM3
            (sublis (mapcar #'(lambda (storevar simpletype)
                                (cons storevar `(THE ,simpletype ,storevar))
                              )
                            SM3
                            (if (and (consp type) (eq (car type) 'VALUES))
                              (cdr type)
                              (list type)
                            )
                    )
                    SM4
            )
            `(THE ,type ,SM5)
) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander APPLY (fun &rest args &environment env)
  (if (and (listp fun)
           (eq (list-length fun) 2)
           (eq (first fun) 'FUNCTION)
           (symbolp (second fun))
      )
    (setq fun (second fun))
    (error-of-type 'source-program-error
      (TEXT "SETF APPLY is only defined for functions of the form #'symbol.")
  ) )
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
      (get-setf-expansion (cons fun args) env)
    (unless (eq (car (last args)) (car (last SM2)))
      (error-of-type 'source-program-error
        (TEXT "APPLY on ~S is not a SETF place.")
        fun
    ) )
    (let ((item (car (last SM1)))) ; 'item' steht für eine Argumentliste!
      (labels ((splice (arglist)
                 ; Würde man in (LIST . arglist) das 'item' nicht als 1 Element,
                 ; sondern gespliced, sozusagen als ',@item', haben wollen, so
                 ; bräuchte man die Form, die (splice arglist) liefert.
                 (if (endp arglist)
                   'NIL
                   (let ((rest (splice (cdr arglist))))
                     (if (eql (car arglist) item)
                       ; ein (APPEND item ...) davorhängen, wie bei Backquote
                       (backquote-append item rest)
                       ; ein (CONS (car arglist) ...) davorhängen, wie bei Backquote
                       (backquote-cons (car arglist) rest)
              )) ) ) )
        (flet ((call-splicing (form)
                 ; ersetzt einen Funktionsaufruf form durch einen, bei dem
                 ; 'item' nicht 1 Argument, sondern eine Argumentliste liefert
                 (let ((fun (first form))
                       (argform (splice (rest form))))
                   ; (APPLY #'fun argform) vereinfachen:
                   ; (APPLY #'fun NIL) --> (fun)
                   ; (APPLY #'fun (LIST ...)) --> (fun ...)
                   ; (APPLY #'fun (CONS x y)) --> (APPLY #'fun x y)
                   ; (APPLY #'fun (LIST* ... z)) --> (APPLY #'fun ... z)
                   (if (or (null argform)
                           (and (consp argform) (eq (car argform) 'LIST))
                       )
                     (cons fun (cdr argform))
                     (list* 'APPLY
                            (list 'FUNCTION fun)
                            (if (and (consp argform)
                                     (or (eq (car argform) 'LIST*)
                                         (eq (car argform) 'CONS)
                                )    )
                              (cdr argform)
                              (list argform)
              )) ) ) )      )
          (values SM1 SM2 SM3 (call-splicing SM4) (call-splicing SM5))
) ) ) ) )
;;;----------------------------------------------------------------------------
;;; Zusätzliche Definitionen von places
;;;----------------------------------------------------------------------------
(define-setf-expander funcall (fun &rest args &environment env)
  (unless (and (listp fun)
               (eq (list-length fun) 2)
               (let ((fun1 (first fun)))
                 (or (eq fun1 'FUNCTION) (eq fun1 'QUOTE))
               )
               (symbolp (second fun))
               (setq fun (second fun))
          )
    (error-of-type 'source-program-error
      (TEXT "SETF FUNCALL is only defined for functions of the form #'symbol.")
  ) )
  (get-setf-expansion (cons fun args) env)
)
;;;----------------------------------------------------------------------------
(define-setf-expander PROGN (&rest forms &environment env)
  (let ((last (last forms)))
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-expansion (car last) env)
      (if (eq forms last)
        (values SM1 SM2 SM3 SM4 SM5)
        (let ((dummyvar (gensym)))
          (values
            `(,dummyvar                    ,@SM1)
            `((PROGN ,@(ldiff forms last)) ,@SM2)
            SM3
            `(PROGN
               ,dummyvar ; avoid warning about unused temporary variable
               ,SM4
             )
            SM5
) ) ) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander LOCALLY (&rest body &environment env)
  (multiple-value-bind (body-rest declspecs) (system::parse-body body nil env)
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-expansion `(PROGN ,@body-rest) env)
      (if declspecs
        (let ((declarations `(DECLARE ,@declspecs)))
          (values
            SM1
            (mapcar #'(lambda (x) `(LOCALLY ,declarations ,x)) SM2)
            SM3
           `(LOCALLY ,declarations ,SM4)
           `(LOCALLY ,declarations ,SM5)
        ) )
        (values SM1 SM2 SM3 SM4 SM5)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander IF (condition t-form f-form &environment env)
  (let ((conditionvar (gensym)))
    (multiple-value-bind (T-SM1 T-SM2 T-SM3 T-SM4 T-SM5)
        (get-setf-expansion t-form env)
      (multiple-value-bind (F-SM1 F-SM2 F-SM3 F-SM4 F-SM5)
          (get-setf-expansion f-form env)
        (unless (eql (length T-SM3) (length F-SM3))
          (error-of-type 'source-program-error
            (TEXT "SETF place ~S expects different numbers of values in the true and branches (~D vs. ~D values).")
            (list 'IF condition t-form f-form) (length T-SM3) (length F-SM3)
        ) )
        (values
          `(,conditionvar
            ,@T-SM1
            ,@F-SM1
           )
          `(,condition
            ,@(mapcar #'(lambda (x) `(IF ,conditionvar ,x)) T-SM2)
            ,@(mapcar #'(lambda (x) `(IF (NOT ,conditionvar) ,x)) F-SM2)
           )
          T-SM3
          `(IF ,conditionvar ,T-SM4 ,(sublis (mapcar #'cons F-SM3 T-SM3) F-SM4))
          `(IF ,conditionvar ,T-SM5 ,F-SM5)
) ) ) ) )
;;;----------------------------------------------------------------------------
(defsetf GET-DISPATCH-MACRO-CHARACTER
         (disp-char sub-char &optional (readtable '*READTABLE*)) (value)
  `(PROGN (SET-DISPATCH-MACRO-CHARACTER ,disp-char ,sub-char ,value ,readtable) ,value)
)
;;;----------------------------------------------------------------------------
(defsetf long-float-digits SYSTEM::%SET-LONG-FLOAT-DIGITS)
;;;----------------------------------------------------------------------------
#+LOGICAL-PATHNAMES
(defsetf logical-pathname-translations set-logical-pathname-translations)
;;;----------------------------------------------------------------------------
(defsetf stream-external-format system::set-stream-external-format)
;;;----------------------------------------------------------------------------
;;; Handhabung von (SETF (VALUES place1 ... placek) form)
;;; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;;;       (SETF place1 dummy1 ... placek dummyk)
;;;       (VALUES dummy1 ... dummyk)
;;;     )
(define-setf-expander values (&rest subplaces &environment env)
  (multiple-value-bind (temps vals stores storeforms accessforms)
      (setf-VALUES-aux subplaces env)
    (values temps
            vals
            stores
            `(VALUES ,@storeforms)
            `(VALUES ,@accessforms)
) ) )
(defun setf-VALUES-aux (places env)
  (do ((temps nil)
       (vals nil)
       (stores nil)
       (storeforms nil)
       (accessforms nil)
       (placesr places))
      ((atom placesr)
       (setq temps (nreverse temps))
       (setq vals (nreverse vals))
       (setq stores (nreverse stores))
       (setq storeforms (nreverse storeforms))
       (setq accessforms (nreverse accessforms))
       (values temps vals stores storeforms accessforms)
      )
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-expansion (pop placesr) env)
      (setq temps (revappend SM1 temps))
      (setq vals (revappend SM2 vals))
      (setq stores (revappend SM3 stores))
      (setq storeforms (cons SM4 storeforms))
      (setq accessforms (cons SM5 accessforms))
) ) )
;;;----------------------------------------------------------------------------
;;; Analog zu (MULTIPLE-VALUE-SETQ (var1 ... vark) form) :
;;; (MULTIPLE-VALUE-SETF (place1 ... placek) form)
;;; --> (VALUES (SETF (VALUES place1 ... placek) form))
;;; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;;;       (SETF place1 dummy1 ... placek dummyk)
;;;       dummy1
;;;     )
(defmacro multiple-value-setf (places form &environment env)
  (multiple-value-bind (temps vals stores storeforms accessforms)
      (setf-VALUES-aux places env)
    (declare (ignore accessforms))
    `(LET* ,(mapcar #'list temps vals)
       (MULTIPLE-VALUE-BIND ,stores ,form
         ,@storeforms
         ,(first stores) ; (null stores) -> NIL -> Wert NIL
     ) )
) )
;;;----------------------------------------------------------------------------
;;;                              Symbol-macros
(define-symbol-macro *ansi* (sys::ansi))
(defsetf sys::ansi sys::set-ansi)
(system::%set-documentation '*ansi* 'variable
 "This symbol-macro modifies some variables for maximum ANSI CL compliance.
Variables affected: `custom:*floating-point-contagion-ansi*',
 `custom:*merge-pathnames-ansi*', `custom:*print-pathnames-ansi*',
 `custom:*parse-namestring-ansi*',
 `custom:*sequence-count-ansi*', `custom:*coerce-fixnum-char-ansi*'.
Invoking CLISP with `-ansi' sets this to T.
Invoking CLISP with `-traditional' sets this to NIL.")

(define-symbol-macro *current-language* (sys::current-language))
(defsetf sys::current-language sys::set-current-language)
(system::%set-documentation '*current-language* 'variable
 "This symbol-macro determines the current language used for UI.")

(define-symbol-macro *lib-directory* (sys::lib-directory))
(defsetf sys::lib-directory sys::set-lib-directory)
(system::%set-documentation '*lib-directory* 'variable
 "This symbol-macro determines the location where CLISP finds its data files.")

(define-symbol-macro *default-file-encoding*
  (system::default-file-encoding))
(defsetf system::default-file-encoding system::set-default-file-encoding)
#+UNICODE
(progn
  (define-symbol-macro *pathname-encoding* (system::pathname-encoding))
  (defsetf system::pathname-encoding system::set-pathname-encoding)
  (define-symbol-macro *terminal-encoding* (system::terminal-encoding))
  (defsetf system::terminal-encoding system::set-terminal-encoding)
  (define-symbol-macro *misc-encoding* (system::misc-encoding))
  (defsetf system::misc-encoding system::set-misc-encoding)
)
(when (fboundp 'sys::setenv)
  (defsetf ext:getenv sys::setenv))
