;;; Copyright (C) 2000-2002 by Sam Steingold
;;; This file is a part of CLISP (http://clisp.cons.org), and, as such,
;;; is distributed under the GNU GPL (http://www.gnu.org/copyleft/gpl.html)

(in-package "EXT")

(export '(clhs clhs-root read-from-file browse-url))

(in-package "SYSTEM")

(defvar *clhs-table* nil)       ; the hash table

(defvar *browsers*
  '((:netscape "netscape" "~a")
    (:netscape-remote "netscape" "-remote" "openURL(~a,new-window)")
    (:mozilla "mozilla" "~a")
    (:mozilla-remote "mozilla" "-remote" "openURL(~a,new-window)")
    (:konqueror "kfmclient" "openURL" "~a")
    (:lynx "lynx" "~a")
    #+unix (:lynx-xterm "xterm" "-e" "lynx" "~a")
    (:links "links" "~a")
    #+unix (:links-xterm "xterm" "-e" "links" "~a")
    (:w3m "w3m" "~a")
    #+unix (:w3m-xterm "xterm" "-e" "w3m" "~a")
    (:mmm "mmm" "-external" "~a")
    (:mosaic "xmosaic" "~a")
    (:emacs-w3 "gnudoit" "-q" "(w3-fetch \"~a\")"))
  "Alist of browsers and commands that invoke them.
`~a' will be replaced with the URL to view.")
(defvar *browser* nil
  "The default browser - a key in `*browsers*' or a list of strings.")

(defun read-from-file (file &key (out *standard-output*)
                       (package (find-package "KEYWORD")))
  "Read an object from a file.
The keyword argument KEYWORD specifies the package to read in.
The keyword argument OUT specifies the output for log messages."
  (let ((beg-real (get-internal-real-time)))
    (prog1 (with-open-file (str file :direction :input)
             (when out
               (format out "~&;; Reading `~a' [~:d bytes]..."
                       file (file-length str))
               (force-output (if (eq out t) *standard-output* out)))
             (with-standard-io-syntax
               (let ((*package* (etypecase package
                                  (package package)
                                  ((or string symbol)
                                   (find-package package)))))
                 (read str))))
      (when out
        (format out "done [~,2f sec]~%"
                (/ (- (get-internal-real-time) beg-real)
                   internal-time-units-per-second))))))

(defun browse-url (url &key (browser *browser*) (out *standard-output*))
  "Run the browser (a keyword in `*browsers*' or a list) on the URL."
  (let* ((command
          (etypecase browser
            (list browser)
            (symbol (or (cdr (assoc browser *browsers* :test #'eq))
                        (error "unknown browser: `~s' (must be a key in `~s')"
                               browser '*browsers*)))))
         (args (mapcar (lambda (arg) (format nil arg url)) (cdr command))))
    (cond (command
           (when out
             (format out "~&;; running [~s~{ ~s~}]..." (car command) args)
             (force-output (if (eq out t) *standard-output* out)))
           (run-program (car command) :arguments args :wait nil)
           (when out
             (format out "done~%")))
          ((format t "~s: no browser specified; please point your browser at
 --> <URL:~a>~%" 'browse-url url)))))

(defun clhs (symbol-string &key (browser *browser*) (out *standard-output*))
  "Dump the CLHS doc for the symbol."
  (unless *clhs-table*
    ;; read in the COMMON-LISP package: the CLHS symbols are supposed to be
    ;; there, but unlock it in case some symbols are still not implemented
    (without-package-lock ("COMMON-LISP")
      (setq *clhs-table* (read-from-file (clisp-data-file "clhs.txt")
                                         :out out :package "COMMON-LISP"))))
  (let* ((clhs-root (clhs-root))
         (slash (if (and (> (length clhs-root) 0)
                         (eql (char clhs-root (- (length clhs-root) 1)) #\/))
                  ""
                  "/")))
    (do* ((symbol (etypecase symbol-string
                    (symbol symbol-string)
                    (string
                      (let ((pack (find-package "COMMON-LISP")))
                        (multiple-value-bind (symb found-p)
                            (find-symbol (string-upcase symbol-string) pack)
                          (unless (eq found-p ':external)
                            (error "no symbol named ~s exported from ~s"
                                   symbol-string pack))
                          symb)))))
          (path-list (or (gethash symbol *clhs-table*)
                         (error "No HyperSpec doc for `~s'" symbol))
                     (cdr path-list)))
         ((endp path-list))
      (browse-url
       (concatenate 'string clhs-root slash "Body/" (car path-list))
       :browser browser :out out))))
