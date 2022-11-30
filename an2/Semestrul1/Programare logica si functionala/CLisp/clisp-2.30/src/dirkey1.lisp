;;; Copyright (C) 2000-2002 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See <http://www.gnu.org/copyleft/gpl.html>

(use-package '("COMMON-LISP" "EXT") "LDAP")
(in-package "LDAP")

(export
 '(dir-key-type dir-key-path dir-key-direction dir-key-open-p dir-key-open
   dir-key-close dir-key-subkeys dir-key-attributes dir-key-value
   dir-key-subkey-delete dir-key-value-delete dir-key
   dir-key-single-value with-dir-key-open dir-key-copy
   with-dir-key-search dir-key-children dir-key-values dir-key-dump-tree
   dir-key-info))

(use-package '("LDAP") "EXT")
(ext:re-export "LDAP" "EXT")

;;; utilities

(defmacro with-dir-key-open ((var key subkey &rest opts) &body body)
  `(let ((,var (dir-key-open ,key ,subkey ,@opts)))
    (unwind-protect (progn ,@body)
      (dir-key-close ,var))))

(defun dir-key-single-value (type path name)
  (with-dir-key-open (dk type path)
    (dir-key-value dk name)))

(defun dir-key-copy (dkey)
  (dir-key-open (dir-key-type dkey) (dir-key-path dkey)
                :direction (dir-key-direction dkey)))

;;; iterations

(defmacro with-dir-key-search ((key-iter att-iter dkey path
                                &key (scope :level))
                               &body body)
  "Iterate over the subtree of the given `dir-key'.
Bind `key-iter' to a macro (using `macrolet'), returning the next key
 and a flag indicating whether the key was opened successfully;
 returns `nil' when no more keys is available.
Bind `att-iter' (if non-nil)  to a macro (using `macrolet'),
 returning the next attribute and its value;
 returns `nil' when no more attributes is available.
The search is done according to the `scope', in the sub-`path' of `dkey'."
  (unless (symbolp key-iter)
    (error (TEXT "~S: macro name should be a symbol, not ~S")
           'with-dir-key-search key-iter))
  (unless (symbolp att-iter)
    (error (TEXT "~S: macro name should be a symbol, not ~S")
           'with-dir-key-search att-iter))
  (let ((k-it (gensym "WDKS-")))
    `(let ((,k-it (dkey-search-iterator ,dkey ,path ,scope)))
      (macrolet ((,key-iter () '(dkey-search-next-key ,k-it)) .
                 ,(if att-iter
                      `((,att-iter () '(dkey-search-next-att ,k-it)))))
        ,@body))))

;; the following two functions are re-implementations of
;; `dir-key-attributes' and `dir-key-subkeys' respectively,
;; using `with-dir-key-search'.
(defun dir-key-values (dkey path)
  (with-dir-key-search (k-iter a-iter dkey path :scope :self)
    (let ((kk (k-iter)) vals)
      (loop (multiple-value-bind (att val) (a-iter)
              (unless att (return))
              (push (cons att val) vals)))
    (cons kk (nreverse vals)))))

(defun dir-key-children (dkey path)
  (with-dir-key-search (k-iter nil dkey path :scope :level)
    (do* ((kk (k-iter) (k-iter)) res)
         ((null kk) (nreverse res))
      (push kk res))))

(defun dir-key-dump-tree (dkey path &key (test #'identity)
                          (out *standard-output*) (collect t))
  "Dump the whole subtree to OUT.
If collect is non-nil, collect all the keys into an a-list."
  (with-dir-key-search (k-iter v-iter dkey path :scope :tree)
    (loop (multiple-value-bind (kk denied-p keys) (k-iter)
            (unless kk (return (nreverse keys)))
            (when (funcall test kk)
              (when out (format out "~%[~s ~s~:[~; access denied!~]]~2%"
                                (dir-key-path dkey) kk denied-p))
              (let (vals)
                (loop (multiple-value-bind (att val) (v-iter)
                        (unless att (return))
                        (when collect (push (cons att val) vals))
                        (when out (format out "~s=~s~%" att val))))
                (when collect (push (cons kk vals) keys)))
              (when out (terpri out)))))))

;;; info

(defstruct dir-key-info
  type path
  class-name
  n-sub-keys max-sub-key-len max-sub-key-class-len
  n-values max-value-name-len max-value-data-len
  security
  write-time)

(defun dir-key-info (dkey)
  (multiple-value-bind
        (class-name n-sub-keys max-sub-key-len max-sub-key-class-len
         n-values max-value-name-len max-value-data-len
         security write-time)
      (dkey-info dkey)
    (make-dir-key-info
     :type (dir-key-type dkey) :path (dir-key-path dkey) :class-name class-name
     :n-sub-keys n-sub-keys :max-sub-key-len max-sub-key-len
     :max-sub-key-class-len max-sub-key-class-len
     :n-values n-values :max-value-name-len max-value-name-len
     :max-value-data-len max-value-data-len
     :security security :write-time write-time)))

(defsetf dir-key-value (key name &optional default) (value)
  ;; just like gethash
  (let ((storeform `(ldap::set-dkey-value ,key ,name ,value)))
    (if default
        `(progn ,default ,storeform)
        `,storeform)))
