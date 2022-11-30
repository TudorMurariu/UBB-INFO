;;; MAKE-LOAD-FORM for CLISP
;;; Sam Steingold 2001-07-24

;; this could have been placed in in clos.lisp,
;; but `make-init-form' uses conditions

(in-package "CLOS")

(defun make-load-form-saving-slots
    (object &key environment
     (slot-names
      (let ((slots (class-slots (class-of object))))
        (etypecase object
          (standard-object
           (mapcan (lambda (slot)
                     (when (eq :instance (slotdef-allocation slot))
                       (list (slotdef-name slot))))
                   slots))
          (structure-object (mapcar #'slotdef-name slots))))))
  (declare (ignore environment))
  (values `(allocate-instance (find-class ',(class-name (class-of object))))
          `(progn
            (setf ,@(mapcan (lambda (slot)
                              (when (slot-boundp object slot)
                                `((slot-value ,object ',slot)
                                  ',(slot-value object slot))))
                            slot-names))
            (initialize-instance ,object))))

(defgeneric make-load-form (object &optional environment)
  (:method ((object standard-object) &optional environment)
    (make-load-form-saving-slots object :environment environment)))

(defun mlf-unquote (var form)
  ;; replace '(... var ...) with `(... ,var ...)
  ;; (setq v 10)
  ;; (mlf-unquote 'v ''(1 (2 (b c) v d (a)))) ==>
  ;; (LIST 1 (LIST 2 '(B C) V 'D '(A)))
  (cond ((atom form) form)
        ((eq (car form) 'quote)
         (labels ((find-var (tree)
                    (if (atom tree) (eq var tree)
                        (or (find-var (car tree)) (find-var (cdr tree)))))
                  (unquote (form)
                    (if (consp form)
                        (if (find-var form)
                            (cons 'list (mapcar #'unquote form))
                            (list 'quote form))
                        (if (eq form var) form
                            (if (constantp form) form (list 'quote form))))))
           (unquote (cadr form))))
        (t (cons (mlf-unquote var (car form))
                 (mlf-unquote var (cdr form))))))

(defun mlf-init-function (object)
  (multiple-value-bind (cre-form ini-form) (make-load-form object)
    (if ini-form
        (let ((var (gensym)))
          `(lambda ()
            (let ((,var ,cre-form))
              ,(mlf-unquote var (nsubst var object ini-form))
              ,var)))
        `(lambda () ,cre-form))))

(defun make-init-form (object)
  (when compiler::*load-forms*
    (multiple-value-bind (form found-p)
        (gethash object compiler::*load-forms*)
      (if found-p form
          (handler-case
              (setf (gethash object compiler::*load-forms*)
                    `(funcall ,(compile nil (mlf-init-function object))))
            (simple-type-error ()) ; no method defined -- ignore
            (error (err)        ; something serious -- warn
              (warn "~s[~s][~s]: ~?~%" 'make-init-form
                    (type-of object) (type-of err)
                    (simple-condition-format-control err)
                    (simple-condition-format-arguments err))))))))
