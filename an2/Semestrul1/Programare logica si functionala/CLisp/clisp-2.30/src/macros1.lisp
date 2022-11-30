;;;; Definitions for control structures etc.
;;;; 29.4.1988, 3.9.1988

(in-package "EXT")
(export '(mapcap maplap fcase))
(in-package "SYSTEM")

;; (DEFMACRO-SPECIAL . macrodef) is like (DEFMACRO . macrodef) except
;; that it works on a special form without replacing the special form
;; handler. ANSI CL requires that all standard macros, even when implemented
;; as special forms, have a macro expander available.
(defmacro defmacro-special (&body macrodef)
  (multiple-value-bind (expansion name) (make-macro-expansion macrodef)
    `(SYSTEM::%PUT ',name 'SYSTEM::MACRO ,expansion)))

(defmacro defvar (symbol &optional (initial-value nil svar) docstring)
  (unless (symbolp symbol)
    (error-of-type 'source-program-error
      (TEXT "~S: non-symbol ~S cannot be a variable")
      'defvar symbol))
  (if (constantp symbol)
    (error-of-type 'source-program-error
      (TEXT "~S: the constant ~S must not be redefined to be a variable")
      'defvar symbol))
  `(LET ()
     (PROCLAIM '(SPECIAL ,symbol))
     ,@(if svar
         `((UNLESS (BOUNDP ',symbol)
             (SYS::SET-SYMBOL-VALUE ',symbol ,initial-value))))
     ,@(if docstring
           `((SYS::%SET-DOCUMENTATION ',symbol 'VARIABLE ',docstring)))
     ',symbol))

(defmacro defparameter (symbol initial-value &optional docstring)
  (unless (symbolp symbol)
    (error-of-type 'source-program-error
      (TEXT "~S: non-symbol ~S cannot be a variable")
      'defparameter symbol))
  (if (constantp symbol)
    (error-of-type 'source-program-error
      (TEXT "~S: the constant ~S must not be redefined to be a variable")
      'defparameter symbol))
  `(LET ()
     (PROCLAIM '(SPECIAL ,symbol))
     (SYS::SET-SYMBOL-VALUE ',symbol ,initial-value)
     ,@(if docstring `((SYS::%SET-DOCUMENTATION ',symbol 'VARIABLE ',docstring)))
     ',symbol))

(defmacro defconstant (&whole form symbol initial-value &optional docstring)
  (unless (symbolp symbol)
    (error-of-type 'source-program-error
      (TEXT "~S: non-symbol ~S cannot be defined constant")
      'defconstant symbol))
  (let ((initial-var (gensym)))
    `(LET ()
       (COMPILER::EVAL-WHEN-COMPILE
        (COMPILER::C-PROCLAIM-CONSTANT ',symbol ',initial-value))
       (LET ((,initial-var ,initial-value))
         (IF (CONSTANTP ',symbol)
           (UNLESS (CONSTANT-EQL ,initial-value ,initial-var
                                 (SYMBOL-VALUE ',symbol))
             (CONSTANT-WARNING ',symbol ',form)))
         (SYS::%PROCLAIM-CONSTANT ',symbol ,initial-var)
         ,@(if docstring
               `((SYS::%SET-DOCUMENTATION ',symbol 'VARIABLE ',docstring)))
         ',symbol))))
; For inhibiting warnings about redefining constants when the old and the new
; value are the same string / bit vector:
(defmacro constant-eql (new-form new-value old-value)
  (declare (ignore new-form))
  `(EQL ,new-value ,old-value))
; If new-value is known to be an immutable string / bit vector and old-value
; is the same string / bit vector, this can return T by using EQUAL instead of
; EQL.
(defun loose-constant-eql (new-value old-value)
  (and (equal (type-of new-value) (type-of old-value))
       (equal new-value old-value)))
; The redefinition warning.
(defun constant-warning (symbol form)
  (warn (TEXT "~S redefines the constant ~S. Its old value was ~S.")
        form symbol (symbol-value symbol)))

(defmacro-special and (&body args)
  (cond ((null args) T)
        ((null (cdr args)) (car args))
        (t (let ((L (mapcar #'(lambda (x) `((NOT ,x) NIL) ) args)))
             (rplaca (last L) `(T ,(car (last args))))
             (cons 'COND L)))))

(defmacro-special or (&body args)
  (cond ((null args) NIL)
        ((null (cdr args)) (car args))
        (t (let ((L (mapcar #'list args)))
             (rplaca (last L) `(T ,(car (last args))))
             (cons 'COND L)))))

(defmacro-special prog1 (form1 &rest moreforms)
  (let ((g (gensym)))
    `(LET ((,g ,form1)) ,@moreforms ,g)))

(defmacro-special prog2 (form1 form2 &rest moreforms)
  (let ((g (gensym)))
    `(LET () (PROGN ,form1 (LET ((,g ,form2)) ,@moreforms ,g)))))

(defmacro-special when (test &body forms)
  `(IF ,test (PROGN ,@forms)))

(defmacro-special unless (test &body forms)
  `(IF (NOT ,test) (PROGN ,@forms)))

(defmacro return (&optional return-value)
  `(RETURN-FROM NIL ,return-value))

(defmacro loop (&body body)
  (let ((tag (gensym)))
    `(BLOCK NIL (TAGBODY ,tag ,@body (GO ,tag)))))

(defun do/do*-expand (varclauselist exitclause body env do let psetq)
  (when (atom exitclause)
    (error-of-type 'source-program-error
      (TEXT "exit clause in ~S must be a list")
      do))
  (flet ((bad-syntax (formpiece)
           (error-of-type 'source-program-error
             (TEXT "Invalid syntax in ~S form: ~S.")
             do
             formpiece ) ))
    (let ((bindlist nil)
          (reinitlist nil)
          (testtag (gensym))
          (exittag (gensym)) )
      (multiple-value-bind (body-rest declarations doc)
          (sys::parse-body body nil env)
        (declare (ignore doc))
        (when declarations
          (setq declarations (list (cons 'DECLARE declarations))))
        (loop
         (when (atom varclauselist)
           (if (null varclauselist)
               (return)
               (bad-syntax varclauselist)) )
         (let ((varclause (first varclauselist)))
           (setq varclauselist (rest varclauselist))
           (cond ((atom varclause)
                  (setq bindlist (cons varclause bindlist)) )
                 ((null (cdr varclause))
                  (setq bindlist (cons (first varclause) bindlist)) )
                 ((atom (cdr varclause))
                  (bad-syntax varclause) )
                 ((null (cddr varclause))
                  (setq bindlist (cons varclause bindlist)) )
                 ((atom (cddr varclause))
                  (bad-syntax varclause) )
                 ((null (cdddr varclause))
                  (setq bindlist
                        (cons (list (first varclause) (second varclause))
                              bindlist ) )
                  (setq reinitlist
                        (list* (third varclause) (first varclause) reinitlist) ) )
                 (t ;;(not (null (cdddr varclause)))
                  (bad-syntax varclause) ) ) ) )
        `(BLOCK NIL
           (,let ,(nreverse bindlist)
            ,@declarations
            (TAGBODY
              ,testtag
              (IF ,(first exitclause) (GO ,exittag))
              ,@body-rest
              (,psetq ,@(nreverse reinitlist))
              (GO ,testtag)
              ,exittag
              (RETURN-FROM NIL (PROGN ,@(rest exitclause))))))))))

(fmakunbound 'do)
(defmacro do (varclauselist exitclause &body body &environment env)
  (do/do*-expand varclauselist exitclause body env 'DO 'LET 'PSETQ))

(defmacro do* (varclauselist exitclause &body body &environment env)
  (do/do*-expand varclauselist exitclause body env 'DO* 'LET* 'SETQ))

(defmacro dolist ((var listform &optional resultform)
                  &body body &environment env)
  (multiple-value-bind (body-rest declarations)
                       (sys::parse-body body nil env)
    (let ((g (gensym)))
      `(DO* ((,g ,listform (CDR ,g))
             (,var NIL))
            ((ENDP ,g)
             ,(if (constantp resultform)
               ; Ist resultform konstant, so ist es /= var. Daher braucht var
               ; während Auswertung von resultform nicht an NIL gebunden zu sein:
               `,resultform
               `(LET ((,var NIL))
                  (DECLARE (IGNORABLE ,var) ,@declarations)
                  ,resultform)))
         (DECLARE (LIST ,g) ,@declarations)
         (SETQ ,var (CAR ,g))
         ,@body-rest))))

(fmakunbound 'dotimes)
(defmacro dotimes ((var countform &optional resultform)
                   &body body &environment env)
  (multiple-value-bind (body-rest declarations)
                       (sys::parse-body body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations))))
    (if (constantp countform)
      `(DO ((,var 0 (1+ ,var)))
           ((>= ,var ,countform) ,resultform)
         ,@declarations
         ,@body-rest)
      (let ((g (gensym)))
        `(DO ((,var 0 (1+ ,var))
              (,g ,countform))
             ((>= ,var ,g) ,resultform)
           ,@declarations
           ,@body-rest)))))

(defmacro-special psetq (&whole form &rest args)
  (do* ((setlist nil)
        (bindlist nil)
        (arglist args (cddr arglist)))
       ((null arglist)
        (setq setlist (cons 'NIL setlist))
        (cons 'LET (cons (nreverse bindlist) (nreverse setlist))))
    (if (null (cdr arglist))
      (error-of-type 'source-program-error
        (TEXT "~S called with an odd number of arguments: ~S")
        'psetq form))
    (let ((g (gensym)))
      (setq setlist (cons `(SETQ ,(first arglist) ,g) setlist))
      (setq bindlist (cons `(,g ,(second arglist)) bindlist)))))

(defmacro-special multiple-value-list (form)
  `(MULTIPLE-VALUE-CALL #'LIST ,form))

(defmacro-special multiple-value-bind (varlist form &body body)
  (let ((g (gensym))
        (poplist nil))
    (dolist (var varlist) (setq poplist (cons `(,var (POP ,g)) poplist)))
    `(LET* ((,g (MULTIPLE-VALUE-LIST ,form)) ,@(nreverse poplist))
       ,@body)))

(defmacro-special multiple-value-setq (varlist form)
  (let ((g (gensym))
        (poplist nil))
    (dolist (var varlist) (setq poplist (cons `(SETQ ,var (POP ,g)) poplist)))
    `(LET* ((,g (MULTIPLE-VALUE-LIST ,form)))
       ,(if poplist `(PROG1 ,@(nreverse poplist)) NIL))))

(defmacro-special locally (&body body)
  `(LET () ,@body))

(defun case-expand (form-name test keyform clauses)
  (let ((var (gensym (format nil "~a-KEY-" form-name))))
    `(let ((,var ,keyform))
      (cond
        ,@(maplist
           #'(lambda (remaining-clauses)
               (let ((clause (first remaining-clauses))
                     (remaining-clauses (rest remaining-clauses)))
                 (unless (consp clause)
                   (error-of-type 'source-program-error
                                  (TEXT "~S: missing key list")
                                  form-name))
                 (let ((keys (first clause)))
                   `(,(cond ((or (eq keys 'T) (eq keys 'OTHERWISE))
                             (if remaining-clauses
                                 (error-of-type 'source-program-error
                                                (TEXT "~S: the ~S clause must be the last one")
                                                form-name keys)
                                 't))
                            ((listp keys)
                             `(or ,@(mapcar #'(lambda (key)
                                                `(,test ,var ',key))
                                            keys)))
                            (t `(,test ,var ',keys)))
                     ,@(rest clause)))))
           clauses)))))

(defmacro fcase (test keyform &body clauses)
  (case-expand 'fcase test keyform clauses))
(defmacro-special case (keyform &body clauses)
  (case-expand 'case 'eql keyform clauses))

(defmacro prog (varlist &body body &environment env)
  (multiple-value-bind (body-rest declarations)
                       (sys::parse-body body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations))))
    `(BLOCK NIL
       (LET ,varlist
         ,@declarations
         (TAGBODY ,@body-rest)))))

(defmacro prog* (varlist &body body &environment env)
  (multiple-value-bind (body-rest declarations)
                       (sys::parse-body body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations))))
    `(BLOCK NIL
       (LET* ,varlist
         ,@declarations
         (TAGBODY ,@body-rest)))))


;;; Macro-Expander für COND:

#|
;; Dieser hier ist zwar kürzer, aber er reduziert COND auf OR,
;; das seinerseits wieder auf COND reduziert, ...
(defmacro-special cond (&body clauses)
  (ifify clauses)
)
; macht eine clauselist von COND zu verschachtelten IFs und ORs.
(defun ifify (clauselist)
  (cond ((null clauselist) NIL)
        ((atom clauselist)
         (error-of-type 'source-program-error
           (TEXT "Not a list of COND clauses: ~S")
           clauselist))
        ((atom (car clauselist))
         (error-of-type 'source-program-error
           (TEXT "The atom ~S must not be used as a COND clause.")
           (car clauselist)))
        (t (let ((ifif (ifify (cdr clauselist))))
             (if (cdar clauselist)
               ; mindestens zweielementige Klausel
               (if (constantp (caar clauselist))
                 (if (eval (caar clauselist)) ; Test zur Expansionszeit auswerten
                   (if (cddar clauselist)
                     `(PROGN ,@(cdar clauselist))
                     (cadar clauselist))
                   ifif)
                 `(IF ,(caar clauselist)
                    ,(if (cddar clauselist)
                         `(PROGN ,@(cdar clauselist)) (cadar clauselist))
                    ,ifif))
               ; einelementige Klausel
               (if (constantp (caar clauselist))
                 (if (eval (caar clauselist)) ; Test zur Expansionszeit auswerten
                   (caar clauselist)
                   ifif)
                 `(OR ,(caar clauselist) ,ifif)))))))
|#

;; Noch einfacher ginge es auch so:
#|
(defmacro-special cond (&body clauses)
  (cond ((null clauses) 'NIL)
        ((atom clauses)
         (error-of-type 'source-program-error
           (TEXT "COND code contains a dotted list, ending with ~S")
           clauses))
        (t (let ((clause (car clauses)))
             (if (atom clause)
               (error-of-type 'source-program-error
                 (TEXT "COND clause without test: ~S")
                 clause)
               (let ((test (car clause)))
                 (if (cdr clause)
                   `(IF ,test (PROGN ,@(cdr clause)) (COND ,@(cdr clauses)))
                   `(OR ,test (COND ,@(cdr clauses))))))))))
|#

;; Dieser hier reduziert COND etwas umständlicher auf IF-Folgen:
(defmacro-special cond (&body clauses)
  (let ((g (gensym)))
    (multiple-value-bind (ifif needed-g) (ifify clauses g)
      (if needed-g
        `(LET (,g) ,ifif)
        ifif))))
; macht eine clauselist von COND zu verschachtelten IFs.
; Zwei Werte: die neue Form, und ob die Dummyvariable g benutzt wurde.
(defun ifify (clauselist g)
  (cond ((null clauselist) (values NIL nil))
        ((atom clauselist)
         (error-of-type 'source-program-error
           (TEXT "Not a list of COND clauses: ~S")
           clauselist))
        ((atom (car clauselist))
         (error-of-type 'source-program-error
           (TEXT "The atom ~S must not be used as a COND clause.")
           (car clauselist)))
        (t (multiple-value-bind (ifif needed-g) (ifify (cdr clauselist) g)
             (if (cdar clauselist)
               ; mindestens zweielementige Klausel
               (if (constantp (caar clauselist))
                 (if (eval (caar clauselist)) ; Test zur Expansionszeit auswerten
                   (if (cddar clauselist)
                     (values `(PROGN ,@(cdar clauselist)) nil)
                     (values (cadar clauselist) nil))
                   (values ifif needed-g))
                 (values
                   `(IF ,(caar clauselist)
                        ,(if (cddar clauselist)
                             `(PROGN ,@(cdar clauselist)) (cadar clauselist))
                        ,ifif)
                   needed-g))
               ; einelementige Klausel
               (if (constantp (caar clauselist))
                 (if (eval (caar clauselist)) ; Test zur Expansionszeit auswerten
                   (values (caar clauselist) nil)
                   (values ifif needed-g))
                 (if (atom (caar clauselist))
                   (values ; ein Atom produziert nur einen Wert und darf
                     `(IF ,(caar clauselist) ; mehrfach hintereinander
                          ,(caar clauselist) ; ausgewertet werden!
                          ,ifif)
                     needed-g)
                   (values
                     `(IF (SETQ ,g ,(caar clauselist)) ,g ,ifif)
                     t))))))))

;;; Mapping (Chapter 7.8.4)

;; MAPCAN, but with APPEND instead of NCONC:
;; (mapcap fun &rest lists) ==  (apply #'append (apply #'mapcar fun lists))
(defun mapcap (fun &rest lists &aux (L nil))
  (loop
    (setq L
      (nconc
        (reverse
          (apply fun
            (maplist #'(lambda (listsr)
                         (if (atom (car listsr))
                           (return)
                           (pop (car listsr))))
                     lists)))
        L)))
  (sys::list-nreverse L))

;; MAPCON, but with APPEND instead of NCONC:
;; (maplap fun &rest lists) == (apply #'append (apply #'maplist fun lists))
(defun maplap (fun &rest lists &aux (L nil))
  (loop
    (setq L
      (nconc
        (reverse
          (apply fun
            (maplist #'(lambda (listsr)
                         (if (atom (car listsr))
                           (return)
                           (prog1
                             (car listsr)
                             (setf (car listsr) (cdr (car listsr))))))
                     lists)))
        L)))
  (sys::list-nreverse L))

