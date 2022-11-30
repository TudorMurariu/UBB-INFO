;; multithreading for CLISP

(defpackage "THREADS"
  (:nicknames "MT")
  (:use "COMMON-LISP" "EXT")
  (:export "MAKE-THREAD" "THREAD-WAIT" "THREAD-WAIT-WITH-TIMEOUT"
           "WITHOUT-INTERRUPTS" "THREAD-YIELD" "THREAD-KILL"
           "THREAD-INTERRUPT" "THREAD-RESTART" "THREADP" "THREAD-NAME"
           "THREAD-ACTIVE-P" "THREAD-STATE" "CURRENT-THREAD" "LIST-THREADS"
           "MAKE-LOCK" "THREAD-LOCK" "THREAD-UNLOCK" "WITH-LOCK"
           "Y-OR-N-P-TIMEOUT" "WITH-TIMEOUT"))

(in-package "MT")

(use-package '("MT") "EXT")
(re-export "MT" "EXT")

;; definitions

(defun with-timeout-f (timeout bodyf timeoutf)
  (block timeout
    (let ((done nil) (thr (current-thread)))
      (make-thread (format nil "Timeout monitor for ~A" thr)
                   (lambda ()
                     (sleep timeout)
                     (unless done
                       (thread-interrupt thr (lambda ()
                                               (return-from timeout
                                                 (funcall timeoutf)))))))
      (unwind-protect (funcall bodyf)
        (setf done t)))))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds,
terminate and evaluate TIMEOUT-FORMS."
  (let ((bodyf (gensym "WT-")) (timeoutf (gensym "WT-")))
    `(flet ((,bodyf () ,@body)
            (,timeoutf () ,@timeout-forms))
      (with-timeout-f ,seconds #',bodyf #',timeoutf))))

(defun y-or-n-p-timeout (seconds default &rest args)
  "Y-OR-N-P with timeout."
  (declare (ignorable seconds default))
  (with-timeout (seconds (format t "[Timed out] ~:[NO~;YES~]~%" default)
                         default)
    (apply #'y-or-n-p args)))

(defmacro with-lock ((lock) &body body)
  "Execute BODY with LOCK locked."
  (let ((lk (gensym "WL-")))
    `(let ((,lk ,lock))
      (unwind-protect (progn (thread-lock ,lk) ,@body)
        (thread-unlock ,lk)))))
