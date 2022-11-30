;; win32 CLISP installation
;; (c) 2001-2002  Sam Steingold, released as part of CLISP under GNU GPL.

;; load this file in the directory where your CLISP distribution is located
;;  - to set the Registry appropriately
;;  - to create CLISP.BAT & CLISP.URL on your desktop

(defvar *clisp-home* (namestring (default-directory)))
(defvar *clisp-base-cmd*
  (concatenate 'string "\"" *clisp-home* "lisp.exe\" -B \""
               (substitute #\/ #\\ *clisp-home*) "\" -M "))
(defvar *clisp-cmd*
  (concatenate 'string *clisp-base-cmd* "\"" *clisp-home* "lispinit.mem\""))

(defvar *eflags*
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 0 0 0)))

(defparameter *lisp-type-map*  '((".lisp" . "lispfile")
                                 (".lsp" . "lispfile")
                                 (".cl" . "lispfile")
                                 (".fas" . "fasfile")
                                 (".mem" . "memfile")))

(format t "~& * Installing CLISP to run from ~a~%" *clisp-home*)

(defun add-lisp-file (dkey)
  (format t "associating CLISP with Lisp files under ~s~%" (dir-key-path dkey))
  (with-dir-key-open (lf dkey "lispfile" :direction :output)
    (setf (dir-key-value lf "") "Lisp source file"
          (dir-key-value lf "EditFlags") *eflags*)
    (with-dir-key-open (ic lf "DefaultIcon" :direction :output)
      (setf (dir-key-value ic "") "%SystemRoot%\\system32\\SHELL32.dll,41"))
    (with-dir-key-open (cc lf "Shell\\Compile_with_CLISP" :direction :output)
      (setf (dir-key-value cc "") "Compile with CLISP")
      (with-dir-key-open (cmd cc "command" :direction :output)
        (setf (dir-key-value cmd "")
              (concatenate 'string *clisp-cmd* " -c \"%1\""))))))

(defun add-fas-file (dkey)
  (format t "associating CLISP with FAS files under ~s~%" (dir-key-path dkey))
  (with-dir-key-open (ff dkey "fasfile" :direction :output)
    (setf (dir-key-value ff "") "CLISP compiled file"
          (dir-key-value ff "EditFlags") *eflags*)
    (with-dir-key-open (ic ff "DefaultIcon" :direction :output)
      (setf (dir-key-value ic "") "%SystemRoot%\\system32\\SHELL32.dll,21"))
    (with-dir-key-open (sh ff "Shell" :direction :output)
      (setf (dir-key-value sh "") "Execute_with_CLISP")
      (with-dir-key-open (ex sh "Execute_with_CLISP" :direction :output)
        (setf (dir-key-value ex "") "Execute with CLISP")
        (with-dir-key-open (cmd ex "command" :direction :output)
          (setf (dir-key-value cmd "")
                (concatenate 'string *clisp-cmd* " \"%1\""))))
      (with-dir-key-open (lo sh "Load_into_CLISP" :direction :output)
        (setf (dir-key-value lo "") "Load into CLISP")
        (with-dir-key-open (cmd lo "command" :direction :output)
          (setf (dir-key-value cmd "")
                (concatenate 'string *clisp-cmd* " -i \"%1\"")))))))

(defun add-mem-file (dkey)
  (format t "associating CLISP with MEM files under ~s~%" (dir-key-path dkey))
  (with-dir-key-open (mf dkey "memfile" :direction :output)
    (setf (dir-key-value mf "") "CLISP memory image"
          (dir-key-value mf "EditFlags") *eflags*)
    (with-dir-key-open (ic mf "DefaultIcon" :direction :output)
      (setf (dir-key-value ic "") "%SystemRoot%\\system32\\SHELL32.dll,21"))
    (with-dir-key-open (sh mf "Shell" :direction :output)
      (setf (dir-key-value sh "") "Run_with_CLISP")
      (with-dir-key-open (ex sh "Run_with_CLISP" :direction :output)
        (setf (dir-key-value ex "") "Run with CLISP")
        (with-dir-key-open (cmd ex "command" :direction :output)
          (setf (dir-key-value cmd "")
                (concatenate 'string *clisp-base-cmd* " \"%1\"")))))))

(when (y-or-n-p "Associate types~:{ <~a>,~} with CLISP?" *lisp-type-map*)
  (with-dir-key-open (c1 :win32 "HKEY_CLASSES_ROOT")
    (loop :for (type . key) :in *lisp-type-map* :do
       (with-dir-key-open (lf c1 type :direction :output)
         (setf (dir-key-value lf "") key)))
    (add-lisp-file c1)
    (add-fas-file c1)
    (add-mem-file c1)))

(let* ((desktop (dir-key-single-value :win32 "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders" "Common Desktop"))
       (bat-file (concatenate 'string desktop "\\clisp.bat"))
       (url-file (concatenate 'string desktop "\\clisp home.url")))
  (when (y-or-n-p "Create CLISP bat file on your desktop <~s>?" bat-file)
    (with-open-file (bat bat-file :direction :output)
      (format t "~&writing <~a>..." bat-file) (force-output)
      (format bat "@echo off~%~a %1 %2 %3 %4 %5 %6 %7 %8 %9~%" *clisp-cmd*)
      (format t "done~%")))
  (when (y-or-n-p "Create CLISP URL file on your desktop <~s>?" url-file)
    (with-open-file (url (substitute #\/ #\\ url-file) :direction :output)
      (format t "~&writing <~a>..." url-file) (force-output)
      (format url "[Internetshortcut]~%URL=http://clisp.cons.org
IconFile=~adoc\\clisp.ico~%" *clisp-home*)
      (format t "done~%"))))
