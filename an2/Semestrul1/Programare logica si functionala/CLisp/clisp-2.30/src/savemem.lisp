;;;; Saving memory images

(in-package "EXT")
(export '(saveinitmem *command-index*))
(in-package "SYSTEM")

;;---------------------------------------------------------------------------
;; Stores the current memory contents as "lispimag.mem", omitting garbage
;; collectible objects.
;; This function does not take arguments and has no local variables, since
;; otherwise in the interpreted mode the values of the variables are stored.
(defun %saveinitmem ()
  (do-all-symbols (sym) (remprop sym 'sys::definition))
  (when (fboundp 'clos::install-dispatch)
    (do-all-symbols (sym)
      (when (and (fboundp sym) (clos::generic-function-p (symbol-function sym)))
        (let ((gf (symbol-function sym)))
          (when (clos::gf-never-called-p gf)
            (clos::install-dispatch gf)
  ) ) ) ) )
  (setq - nil + nil ++ nil +++ nil * nil ** nil *** nil / nil // nil /// nil)
  (savemem "lispimag.mem")
  (room nil)
)

;; Saves the current memory contents.
;; This function works only when compiled!
(defun saveinitmem (&optional (filename "lispinit.mem")
                    &key ((:quiet *quiet*) nil) init-function verbose
                    ((:start-package *package*) *package*)
                    (locked-packages *system-package-list*))
  (let* ((old-driver *driver*)
         (fn (merge-pathnames filename #.(make-pathname :type "mem")))
         (*driver*
           #'(lambda ()
               ;(declare (special *command-index* *home-package*
               ;                  *active-restarts* *condition-restarts*))
               ;; Reset a few special variables. This must happen in the
               ;; fresh session, not in the old session, because that would
               ;; temporarily disable error handling in the old session.
               ;; Note: For GC purposes, neither is better: during savemem's
               ;; GC the old values are accessible anyway and thus not garbage
               ;; collected.
               (setq - nil
                     + nil
                     ++ nil
                     +++ nil
                     * nil
                     ** nil
                     *** nil
                     / nil
                     // nil
                     /// nil
                     *active-restarts* nil
                     *condition-restarts* nil
                     *command-index* 0
                     *home-package* nil)
               (dribble-reset)
               (setq *driver* old-driver)
               (when init-function (funcall init-function))
               (funcall *driver*))))
    (setf (package-lock locked-packages) t)
    (savemem fn)
    (when verbose
      (fresh-line)
      (write-string (TEXT "Wrote the memory image into "))
      (princ fn)
      (terpri)))
  (room nil))
