#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; Site specific definitions, to be modified on installation

(in-package "EXT")
(mapcar #'fmakunbound '(short-site-name long-site-name))

(defun short-site-name ()
  (let ((s (or
            (system::registry "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
                              "RegisteredOrganization")
            (system::registry "SOFTWARE\\Microsoft\\Windows\\CurrentVersion"
                              "RegisteredOrganization"))))
    (check-type s string)
    s))
(defun long-site-name ()
  (let ((s (or
            (system::registry "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
                              "RegisteredOwner")
            (system::registry "SOFTWARE\\Microsoft\\Windows\\CurrentVersion"
                              "RegisteredOwner"))))
    (check-type s string)
    s))

(defparameter *editor* "notepad.exe" "The name of the editor.")
(defun editor-name () (or (sys::getenv "EDITOR") *editor*))

(defun editor-tempfile ()
  "The temporary file LISP creates for editing."
  "lisptemp.lisp")

(defun edit-file (file)
  "(edit-file file) edits a file."
  (execute (editor-name) (namestring file t)))

;; Treat Ctrl-Z in files as whitespace. Some losing middle-age
;; editors insist on appending this to files.
(eval-when (load eval compile)
  (set-syntax-from-char #\Code26 #\Space))

(defparameter *load-paths*
  '(#"C:"                ; erst im Current-Directory von Laufwerk C:
    #"C:\\CLISP\\...\\") ; dann in allen Directories unterhalb C:\CLISP
  "The list of directories where programs are searched on LOAD etc.
if device and directory are unspecified:")

;; This makes screen output prettier:
(setq *print-pretty* t)

;; understand CYGWIN pathnames
(setq *device-prefix* "cygdrive")

;; This perhaps makes pathname parsing more intuitive:
;;  ".clisprc" --> #S(pathname :name ".clisprc" :type nil)
(setq *parse-namestring-dot-file* :name)

;; which browser do you use? (see `*browsers*' in clhs.lisp)
;; (setq *browser* :mozilla-remote)

;; Common Lisp HyperSpec access
(defvar *clhs-root-default*)
(defun clhs-root ()
  "This returns the root URL for the Common Lisp HyperSpec.
You can set the environment variable `CLHSROOT' or redefine this function
in ~/.clisprc.  On win32 you can also use the Registry."
  (or (sys::getenv "CLHSROOT")
      (let ((s (system::registry "SOFTWARE\\GNU\\CLISP" "CLHSROOT")))
        (check-type s (or null string))
        s)
      *clhs-root-default*))
(setq *clhs-root-default* "http://www.lisp.org/HyperSpec/")
