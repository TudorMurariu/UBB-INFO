;;;; File DEFMACRO.LISP
;;; Macro DEFMACRO und einige Hilfsfunktionen für komplizierte Macros.
;;; 1. 9. 1988
;;; Adaptiert an DEFTYPE am 10.6.1989

(in-package "SYSTEM")

;; Import aus CONTROL.Q:

#| (SYSTEM::PARSE-BODY body &optional docstring-allowed env)
   expandiert die ersten Formen in der Formenliste body (im Macroexpansions-
   Environment env), entdeckt dabei auftretende Deklarationen (und falls
   docstring-allowed=T, auch einen Docstring) und liefert drei Werte:
   1. body-rest, die restlichen Formen,
   2. declspec-list, eine Liste der aufgetretenen Decl-Specs,
   3. docstring, ein aufgetretener Docstring oder NIL.
|#
#| (SYSTEM::KEYWORD-TEST arglist kwlist)
   testet, ob arglist (eine paarige Keyword/Value-Liste) nur Keywords
   enthält, die auch in der Liste kwlist vorkommen, oder aber ein
   Keyword/Value-Paar :ALLOW-OTHER-KEYS mit Value /= NIL enthält.
   Wenn nein, wird ein Error ausgelöst.
|#
#| (keyword-test arglist kwlist) überprüft, ob in arglist (eine Liste
von Keyword/Value-Paaren) nur Keywords vorkommen, die in kwlist vorkommen,
oder ein Keyword/Value-Paar mit Keyword = :ALLOW-OTHER-KEYS und Value /= NIL
vorkommt. Sollte dies nicht der Fall sein, wird eine Errormeldung ausgegeben.

 (defun keyword-test (arglist kwlist)
  (let ((unallowed-arglistr nil)
        (allow-other-keys-flag nil))
    (do ((arglistr arglist (cddr arglistr)))
        ((null arglistr))
      (if (eq (first arglistr) ':ALLOW-OTHER-KEYS)
          (if (second arglistr) (setq allow-other-keys-flag t))
          (do ((kw (first arglistr))
               (kwlistr kwlist (cdr kwlistr)))
              ((or (null kwlistr) (eq kw (first kwlistr)))
               (if (and (null kwlistr) (null unallowed-arglistr))
                   (setq unallowed-arglistr arglistr))))))
    (unless allow-other-keys-flag
      (if unallowed-arglistr
        (cerror (TEXT "Both will be ignored.")
                (TEXT "Invalid keyword-value-pair: ~S ~S")
                (first unallowed-arglistr) (second unallowed-arglistr))))))
; Definition in Assembler siehe CONTROL.Q
|#

(defun macro-call-error (macro-form)
  (error-of-type 'source-program-error
    (TEXT "The macro ~S may not be called with ~S arguments: ~S")
    (car macro-form) (1- (length macro-form)) macro-form))

(proclaim '(special
        %restp ; gibt an, ob &REST/&BODY/&KEY angegeben wurde,
               ; also ob die Argumentanzahl unbeschränkt ist.

        %min-args ; gibt die Anzahl der notwendigen Argumente an

        %arg-count ; gibt die Anzahl der Einzelargumente an
                   ; (notwendige und optionale Argumente, zusammengezählt)

        %let-list ; umgedrehte Liste der Bindungen, die mit LET* zu machen sind

        %keyword-tests ; Liste der KEYWORD-TEST - Aufrufe, die einzubinden sind

        %default-form ; Default-Form für optionale und Keyword-Argumente,
                   ; bei denen keine Default-Form angegeben ist.
                   ; =NIL normalerweise, = (QUOTE *) für DEFTYPE.
)          )
#|
 (ANALYZE1 lambdalist accessexp name wholevar)
analysiert eine Macro-Lambdaliste (ohne &ENVIRONMENT). accessexp ist der
Ausdruck, der die Argumente liefert, die mit dieser Lambdaliste zu matchen
sind.

 (ANALYZE-REST lambdalistr restexp name)
analysiert den Teil einer Macro-Lambdaliste, der nach &REST/&BODY kommt.
restexp ist der Ausdruck, der die Argumente liefert, die mit diesem
Listenrest zu matchen sind.

 (ANALYZE-KEY lambdalistr restvar name)
analysiert den Teil einer Macro-Lambdaliste, der nach &KEY kommt.
restvar ist das Symbol, das die restlichen Argumente enthalten wird.

 (ANALYZE-AUX lambdalistr name)
analysiert den Teil einer Macro-Lambdaliste, der nach &AUX kommt.

 (REMOVE-ENV-ARG lambdalist name)
entfernt das Paar &ENVIRONMENT/Symbol aus einer Macro-Lambdaliste,
liefert zwei Werte: die verkürzte Lambdaliste und das als Environment zu
verwendende Symbol (oder die Lambdaliste selbst und NIL, falls &ENVIRONMENT
nicht auftritt).

 (MAKE-LENGTH-TEST symbol)
kreiert aus %restp, %min-args, %arg-count eine Testform, die bei Auswertung
anzeigt, ob der Inhalt der Variablen symbol als Aufruferform zum Macro
dienen kann.

 (MAKE-MACRO-EXPANSION macrodef)
liefert zu einer Macrodefinition macrodef = (name lambdalist . body)
1. den Macro-Expander als Programmtext (FUNCTION ... (LAMBDA ...)),
2. name, ein Symbol,
3. lambdalist,
4. docstring (oder NIL, wenn keiner da).

 (MAKE-MACRO-EXPANDER macrodef)
liefert zu einer Macrodefinition macrodef = (name lambdalist . body)
das fürs FENV bestimmte Objekt #<MACRO expander>.
|#

(%proclaim-constant 'macro-missing-value (list 'macro-missing-value))
; einmaliges Objekt

(%putd 'analyze-aux
  (function analyze-aux
    (lambda (lambdalistr name)
      (do ((listr lambdalistr (cdr listr)))
          ((atom listr)
           (if listr
             (cerror (TEXT "The rest of the lambda list will be ignored.")
                     (TEXT "The lambda list of macro ~S contains a dot after &AUX.")
                     name)))
        (cond ((symbolp (car listr)) (setq %let-list (cons `(,(car listr) nil) %let-list)))
              ((atom (car listr))
               (error-of-type 'source-program-error
                 (TEXT "in macro ~S: ~S may not be used as &AUX variable.")
                 name (car listr)))
              (t (setq %let-list
                   (cons `(,(caar listr) ,(cadar listr)) %let-list)
  ) ) ) )     )  )
)

(%putd 'analyze-key
  (function analyze-key
    (lambda (lambdalistr restvar name &aux (otherkeysforbidden t) (kwlist nil))
      (do ((listr lambdalistr (cdr listr))
           (next)
           (kw)
           (svar)
           (initv)
           (g))
          ((atom listr)
           (if listr
             (cerror (TEXT "The rest of the lambda list will be ignored.")
                     (TEXT "The lambda list of macro ~S contains a dot after &KEY.")
                     name)))
        (setq next (car listr))
        (cond ((eq next '&ALLOW-OTHER-KEYS) (setq otherkeysforbidden nil))
              ((eq next '&AUX) (return-from nil (analyze-aux (cdr listr) name)))
              ((or (eq next '&ENVIRONMENT) (eq next '&WHOLE) (eq next '&OPTIONAL)
                   (eq next '&REST) (eq next '&BODY) (eq next '&KEY)
               )
               (cerror (TEXT "It will be ignored.")
                       (TEXT "The lambda list of macro ~S contains a badly placed ~S.")
                       name next))
              (t
                (if %default-form
                  (cond ((symbolp next) (setq next (list next %default-form)))
                        ((and (consp next) (eql (length next) 1))
                         (setq next (list (car next) %default-form))
                ) )     )
                (cond ((symbolp next)
                       (setq kw (intern (symbol-name next) *keyword-package*))
                       (setq %let-list
                         (cons `(,next (GETF ,restvar ,kw NIL)) %let-list)
                       )
                       (setq kwlist (cons kw kwlist))
                      )
                      ((atom next)
                       (cerror (TEXT "It will be ignored.")
                               (TEXT "The lambda list of macro ~S contains the invalid element ~S")
                               name next))
                      ((symbolp (car next))
                       (setq kw (intern (symbol-name (car next)) *keyword-package*))
                       (setq initv (gensym))
                       (setq %let-list (cons `(,initv ,(cadr next)) %let-list))
                       (setq %let-list
                         (cons `(,(car next) (GETF ,restvar ,kw MACRO-MISSING-VALUE))
                               %let-list
                       ) )
                       (setq svar (if (and (cddr next) (symbolp (third next)))
                                    (third next)
                                    nil
                       )          )
                       (setq %let-list
                         (cons
                           (if svar
                             `(,svar (IF (EQ ,(car next) MACRO-MISSING-VALUE)
                                       (PROGN (SETQ ,(car next) ,initv) NIL)
                                       T
                              )      )
                             `(,(car next) (IF (EQ ,(car next) MACRO-MISSING-VALUE)
                                             ,initv
                                             ,(car next)
                              )            )
                           )
                           %let-list
                       ) )
                       (setq kwlist (cons kw kwlist))
                      )
                      ((not (and (consp (car next)) (symbolp (caar next)) (consp (cdar next))))
                       (cerror (TEXT "~0*It will be ignored.")
                               (TEXT "The lambda list of macro ~S contains an invalid keyword specification ~S")
                               name (car next)))
                      ((symbolp (cadar next))
                       (setq kw (caar next))
                       (setq initv (gensym))
                       (setq %let-list (cons `(,initv ,(cadr next)) %let-list))
                       (setq %let-list
                         (cons `(,(cadar next) (GETF ,restvar ,kw MACRO-MISSING-VALUE))
                           %let-list
                       ) )
                       (setq svar (if (and (cddr next) (symbolp (third next)))
                                    (third next)
                                    nil
                       )          )
                       (setq %let-list
                         (cons
                           (if svar
                             `(,svar (IF (EQ ,(cadar next) MACRO-MISSING-VALUE)
                                       (PROGN (SETQ ,(cadar next) ,initv) NIL)
                                       T
                              )      )
                             `(,(cadar next) (IF (EQ ,(cadar next) MACRO-MISSING-VALUE)
                                             ,initv
                                             ,(cadar next)
                              )            )
                           )
                           %let-list
                       ) )
                       (setq kwlist (cons kw kwlist))
                      )
                      (t
                       (setq kw (caar next))
                       (setq g (gensym))
                       (setq initv (gensym))
                       (setq %let-list (cons `(,initv ,(cadr next)) %let-list))
                       (setq %let-list
                         (cons `(,g (GETF ,restvar ,kw MACRO-MISSING-VALUE)) %let-list)
                       )
                       (setq svar (if (and (cddr next) (symbolp (third next)))
                                    (third next)
                                    nil
                       )          )
                       (setq %let-list
                         (cons
                           (if svar
                             `(,svar (IF (EQ ,g MACRO-MISSING-VALUE)
                                       (PROGN (SETQ ,g ,initv) NIL)
                                       T
                              )      )
                             `(,g (IF (EQ ,g MACRO-MISSING-VALUE)
                                    ,initv
                                    ,(cadar next)
                              )   )
                           )
                           %let-list
                       ) )
                       (setq kwlist (cons kw kwlist))
                       (let ((%min-args 0) (%arg-count 0) (%restp nil) (%default-form nil))
                         (analyze1 (cadar next) g name g)
                      ))
              ) )
      ) )
      (if otherkeysforbidden
        (setq %keyword-tests
          (cons `(KEYWORD-TEST ,restvar ',kwlist) %keyword-tests)
      ) )
  ) )
)

(%putd 'analyze-rest
  (function analyze-rest
    (lambda (lambdalistr restexp name)
      (if (atom lambdalistr)
        (error-of-type 'source-program-error
          (TEXT "The lambda list of macro ~S is missing a variable after &REST/&BODY.")
          name))
      (let ((restvar (car lambdalistr))
            (listr (cdr lambdalistr)))
        (setq %restp t)
        (cond ((symbolp restvar)
               (setq %let-list (cons `(,restvar ,restexp) %let-list))
              )
              ((atom restvar)
               (error-of-type 'source-program-error
                 (TEXT "The lambda list of macro ~S contains an illegal variable after &REST/&BODY: ~S")
                 name restvar))
              (t
               (let ((%min-args 0) (%arg-count 0) (%restp nil))
                 (analyze1 restvar restexp name restexp)
        )     ))
        (cond ((null listr))
              ((atom listr)
               (cerror (TEXT "The rest of the lambda list will be ignored.")
                       (TEXT "The lambda list of macro ~S contains a misplaced dot.")
                       name))
              ((eq (car listr) '&KEY) (analyze-key (cdr listr) restvar name))
              ((eq (car listr) '&AUX) (analyze-aux (cdr listr) name))
              (t (cerror (TEXT "They will be ignored.")
                         (TEXT "The lambda list of macro ~S contains superfluous elements: ~S")
                         name listr)))))))

(%putd 'cons-car
  (function cons-car
    (lambda (exp &aux h)
      (if
        (and
          (consp exp)
          (setq h
            (assoc (car exp)
              '((car . caar) (cdr . cadr)
                (caar . caaar) (cadr . caadr) (cdar . cadar) (cddr . caddr)
                (caaar . caaaar) (caadr . caaadr) (cadar . caadar) (caddr . caaddr)
                (cdaar . cadaar) (cdadr . cadadr) (cddar . caddar) (cdddr . cadddr)
                (cddddr . fifth)
        ) ) )  )
        (cons (cdr h) (cdr exp))
        (list 'car exp)
  ) ) )
)

(%putd 'cons-cdr
  (function cons-cdr
    (lambda (exp &aux h)
      (if
        (and
          (consp exp)
          (setq h
            (assoc (car exp)
              '((car . cdar) (cdr . cddr)
                (caar . cdaar) (cadr . cdadr) (cdar . cddar) (cddr . cdddr)
                (caaar . cdaaar) (caadr . cdaadr) (cadar . cdadar) (caddr . cdaddr)
                (cdaar . cddaar) (cdadr . cddadr) (cddar . cdddar) (cdddr . cddddr)
        ) ) )  )
        (cons (cdr h) (cdr exp))
        (list 'cdr exp)
  ) ) )
)

(%putd 'analyze1
  (function analyze1
    (lambda (lambdalist accessexp name wholevar)
      (do ((listr lambdalist (cdr listr))
           (withinoptional nil)
           (item)
           (g))
          ((atom listr)
           (when listr
             (unless (symbolp listr)
               (error-of-type 'source-program-error
                 (TEXT "The lambda list of macro ~S contains an illegal &REST variable: ~S")
                 name listr))
             (setq %let-list (cons `(,listr ,accessexp) %let-list))
             (setq %restp t)
          ))
        (setq item (car listr))
        (cond ((eq item '&WHOLE)
               (if (and wholevar (cdr listr) (symbolp (cadr listr)))
                 (setq %let-list (cons `(,(cadr listr) ,wholevar) %let-list)
                       listr (cdr listr))
                 (error-of-type 'source-program-error
                   (TEXT "The lambda list of macro ~S contains an invalid &WHOLE: ~S")
                   name listr)))
              ((eq item '&OPTIONAL)
               (if withinoptional
                 (cerror (TEXT "It will be ignored.")
                         (TEXT "The lambda list of macro ~S contains a superfluous ~S.")
                         name item))
               (setq withinoptional t)
              )
              ((or (eq item '&REST) (eq item '&BODY))
               (return-from nil (analyze-rest (cdr listr) accessexp name))
              )
              ((eq item '&KEY)
               (setq g (gensym))
               (setq %restp t)
               (setq %let-list (cons `(,g ,accessexp) %let-list))
               (return-from nil (analyze-key (cdr listr) g name))
              )
              ((eq item '&ALLOW-OTHER-KEYS)
               (cerror (TEXT "It will be ignored.")
                       (TEXT "The lambda list of macro ~S contains ~S before &KEY.")
                       name item))
              ((eq item '&ENVIRONMENT)
               (cerror (TEXT "It will be ignored.")
                       (TEXT "The lambda list of macro ~S contains ~S which is illegal here.")
                       name item))
              ((eq item '&AUX)
               (return-from nil (analyze-aux (cdr listr) name))
              )
              (withinoptional
               (setq %arg-count (1+ %arg-count))
               (if %default-form
                 (cond ((symbolp item) (setq item (list item %default-form)))
                       ((and (consp item) (eql (length item) 1))
                        (setq item (list (car item) %default-form))
               ) )     )
               (cond ((symbolp item)
                      (setq %let-list (cons `(,item ,(cons-car accessexp)) %let-list))
                     )
                     ((atom item)
                      #1=
                      (error-of-type 'source-program-error
                        (TEXT "The lambda list of macro ~S contains an invalid element ~S")
                        name item))
                     ((symbolp (car item))
                      (setq %let-list
                        (cons `(,(car item) (IF ,accessexp
                                              ,(cons-car accessexp)
                                              ,(if (consp (cdr item)) (cadr item) 'NIL)
                               )            )
                          %let-list
                      ) )
                      (when (and (consp (cdr item)) (consp (cddr item)))
                        (unless (symbolp (caddr item))
                          (error-of-type 'source-program-error
                            (TEXT "The lambda list of macro ~S contains an invalid supplied-variable ~S")
                            name (caddr item)))
                        (setq %let-list
                          (cons `(,(caddr item) (NOT (NULL ,accessexp))) %let-list)
                     )) )
                     (t
                      (setq g (gensym))
                      (setq %let-list
                        (cons `(,g ,(if (consp (cdr item))
                                      `(IF ,accessexp
                                         ,(cons-car accessexp)
                                         ,(cadr item)
                                       )
                                      (cons-car accessexp)
                               )    )
                          %let-list
                      ) )
                      (let ((%min-args 0) (%arg-count 0) (%restp nil))
                        (analyze1 (car item) g name g)
                      )
                      (if (consp (cddr item))
                        (setq %let-list
                          (cons `(,(caddr item) (NOT (NULL ,accessexp))) %let-list)
               )     )) )
               (setq accessexp (cons-cdr accessexp))
              )
              (t ; notwendige Argumente
               (setq %min-args (1+ %min-args))
               (setq %arg-count (1+ %arg-count))
               (cond ((symbolp item)
                      (setq %let-list (cons `(,item ,(cons-car accessexp)) %let-list))
                     )
                     ((atom item)
                      #1# ; (error-of-type ... name item), s.o.
                     )
                     (t
                      (let ((%min-args 0) (%arg-count 0) (%restp nil))
                        (analyze1 item (cons-car accessexp) name (cons-car accessexp))
               )     ))
               (setq accessexp (cons-cdr accessexp))
  ) ) ) )     )
)

(%putd 'remove-env-arg
  (function remove-env-arg
    (lambda (lambdalist name)
      (do ((listr lambdalist (cdr listr)))
          ((atom listr) (values lambdalist nil))
        (if (eq (car listr) '&ENVIRONMENT)
          (if (and (consp (cdr listr)) (symbolp (cadr listr)) (cadr listr))
            ; &ENVIRONMENT gefunden
            (return
              (values
                (do ((l1 lambdalist (cdr l1)) ; lambdalist ohne &ENVIRONMENT/Symbol
                     (l2 nil (cons (car l1) l2)))
                    ((eq (car l1) '&ENVIRONMENT)
                     (nreconc l2 (cddr l1))
                )   )
                (cadr listr)
            ) )
            (error-of-type 'source-program-error
              (TEXT "In the lambda list of macro ~S, &ENVIRONMENT must be followed by a non-NIL symbol: ~S")
              name lambdalist)))))))

(%putd 'make-length-test
  (function make-length-test
    (lambda (var &optional (header 1))
      (cond ((and (zerop %min-args) %restp) NIL)
            ((zerop %min-args) `(> (LENGTH ,var) ,(+ header %arg-count)))
            (%restp `(< (LENGTH ,var) ,(+ header %min-args)))
            ((= %min-args %arg-count) `(/= (LENGTH ,var) ,(+ header %min-args)))
            (t `(NOT (<= ,(+ header %min-args) (LENGTH ,var) ,(+ header %arg-count))))
  ) ) )
)

(%putd 'make-macro-expansion
  (function make-macro-expansion
    (lambda (macrodef &optional pre-process)
      (if (atom macrodef)
        (error-of-type 'source-program-error
          (TEXT "Cannot define a macro from that: ~S")
          macrodef))
      (unless (symbolp (car macrodef))
        (error-of-type 'source-program-error
          (TEXT "The name of a macro must be a symbol, not ~S")
          (car macrodef)))
      (if (atom (cdr macrodef))
        (error-of-type 'source-program-error
          (TEXT "Macro ~S is missing a lambda list.")
          (car macrodef)))
      (let ((name (car macrodef))
            (lambdalist (cadr macrodef))
            (body (cddr macrodef))
           )
        (multiple-value-bind (body-rest declarations docstring)
                             (parse-body body t) ; globales Environment!
          (if declarations (setq declarations (list (cons 'DECLARE declarations))))
          (multiple-value-bind (newlambdalist envvar)
                               (remove-env-arg lambdalist name)
            (let ((%arg-count 0) (%min-args 0) (%restp nil)
                  (%let-list nil) (%keyword-tests nil) (%default-form nil))
              (analyze1 newlambdalist '(CDR <MACRO-FORM>) name '<MACRO-FORM>)
              (let ((lengthtest (make-length-test '<MACRO-FORM>))
                    (mainform `(LET* ,(nreverse %let-list)
                                 ,@declarations
                                 ,@(nreverse %keyword-tests)
                                 (BLOCK ,name ,@body-rest)
                   ))          )
                (if lengthtest
                  (setq mainform
                    `(IF ,lengthtest
                       (MACRO-CALL-ERROR <MACRO-FORM>)
                       ,mainform
                ) )  )
                (values
                  `(FUNCTION ,name
                     (LAMBDA (<MACRO-FORM> &OPTIONAL ,(or envvar '<ENV-ARG>))
                       (DECLARE (CONS <MACRO-FORM>))
                       ,@(if envvar
                           declarations ; enthält evtl. ein (declare (ignore envvar))
                           '((DECLARE (IGNORE <ENV-ARG>)))
                         )
                       ,@(if docstring (list docstring))
                       ,@(if pre-process
                             `((setq <MACRO-FORM>
                                (,pre-process <MACRO-FORM>))))
                       ,mainform
                   ) )
                  name
                  lambdalist
                  docstring
  ) ) ) ) ) ) ) )
)

(%putd 'make-macro-expander
  (function make-macro-expander
    (lambda (macrodef)
      (make-macro (eval (make-macro-expansion macrodef)))
) ) )
