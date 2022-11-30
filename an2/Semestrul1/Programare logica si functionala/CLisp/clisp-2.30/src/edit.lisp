;;;; Ed, Edit-file, Uncompile

(in-package "EXT")
(export '(editor-name editor-tempfile edit-file uncompile))
(in-package "SYSTEM")

;;-----------------------------------------------------------------------------
;; ED

;; *editor*, editor-name und editor-tempfile sind in CONFIG.LISP definiert.
;; Hier stehen nur die Defaults.

;; Der Name des Editors:
(defparameter *editor* nil)

;; Liefert den Namen des Editors:
(defun editor-name () *editor*)

;; Das temporäre File, das LISP beim Editieren anlegt:
(defun editor-tempfile ()
  #+OS/2 "lisptemp.lisp"
  #+AMIGA "T:lisptemp.lisp"
  #+(or UNIX WIN32) (merge-pathnames "lisptemp.lisp" (user-homedir-pathname))
)

;; (edit-file file) editiert ein File.
(defun edit-file (file)
  (unless (editor-name)
    (error-of-type 'error
      (TEXT "No external editor installed.")
  ) )
  ; Damit TRUENAME keinen Fehler liefert, wenn das File noch nicht existiert,
  ; stellen wir sicher, dass das File existiert:
  #+(or UNIX AMIGA ACORN-RISCOS)
  (unless (probe-file file)
    (close (open file :direction :output))
  )
  #+(or OS/2 WIN32)
    (execute (editor-name) ; das ist der Name des Editors
             (namestring file t) ; file als String
    )
  #+UNIX
    (shell (format nil "~A ~A" (editor-name) (truename file)))
  #+AMIGA
    (shell (format nil "~A \"~A\"" (editor-name) (truename file)))
  #+ACORN-RISCOS
    (let ((pathname (truename file)))
      (shell
        (format nil "~A ~A"
                    (editor-name)
                    (if (pathname-type pathname)
                      ; swap pathname's name and type
                      (merge-pathnames
                        (make-pathname :name (pathname-type pathname)
                                       :type (pathname-name pathname)
                        )
                        pathname
                      )
                      pathname
                    )
    ) ) )
)

(defun ed (&optional arg &aux funname sym fun def)
  (if (null arg)
    (edit-file "")
    (if (or (pathnamep arg) (stringp arg))
      (edit-file arg)
      (if (and (cond ((function-name-p arg) (setq funname arg) t)
                     ((functionp arg) (function-name-p (setq funname (sys::%record-ref arg 0))))
                     (t nil)
               )
               (fboundp (setq sym (get-funname-symbol funname)))
               (or (setq fun (macro-function sym))
                   (setq fun (symbol-function sym))
               )
               (functionp fun)
               (or (function-name-p arg) (eql fun arg))
               (setq def (get sym 'sys::definition))
          )
        (let ((tempfile (editor-tempfile)))
          (with-open-file (f tempfile :direction :output)
            (pprint (car def) f)
            (terpri f) (terpri f)
          )
          (let ((date (file-write-date tempfile)))
            (edit-file tempfile)
            (when (> (file-write-date tempfile) date)
              (with-open-file (f tempfile :direction :input)
                (let ((*package* *package*) ; *PACKAGE* binden
                      (end-of-file "EOF")) ; einmaliges Objekt
                  (loop
                    (let ((obj (read f nil end-of-file)))
                      (when (eql obj end-of-file) (return))
                      (print (evalhook obj nil nil (cdr def)))
              ) ) ) )
              (when (compiled-function-p fun) (compile funname))
          ) )
          funname
        )
        (error-of-type 'error
          (TEXT "~S cannot be edited.")
          arg
) ) ) ) )

(defun uncompile (arg &aux funname sym fun def)
  (if (and (cond ((function-name-p arg) (setq funname arg) t)
                 ((functionp arg) (function-name-p (setq funname (sys::%record-ref arg 0))))
                 (t nil)
           )
           (fboundp (setq sym (get-funname-symbol funname)))
           (or (setq fun (macro-function sym))
               (setq fun (symbol-function sym))
           )
           (functionp fun)
           (or (function-name-p arg) (eql fun arg))
           (setq def (get sym 'sys::definition))
      )
    (evalhook (car def) nil nil (cdr def))
    (error-of-type 'error
      (TEXT "~S: source code for ~S not available.")
      'uncompile funname
    )
) )
