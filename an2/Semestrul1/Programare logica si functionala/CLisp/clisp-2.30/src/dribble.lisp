;;;; Dribble

(in-package "SYSTEM")

;;-----------------------------------------------------------------------------
;; DRIBBLE

;; The use of an intermediate synonym-stream is for robustness.
;; (Just try dribbling to a file on a full disk partition...)
(defvar *dribble-stream* nil)

(let ((dribble-file nil) (dribbled-input nil) (dribbled-output nil)
      (dribbled-error-output nil) (dribbled-trace-output nil)
      (dribbled-query-io nil) (dribbled-debug-io nil))
  (defun dribble-reset ()
    (setq dribble-file nil dribbled-input nil dribbled-output nil
          dribbled-error-output nil dribbled-trace-output nil
          dribbled-query-io nil dribbled-debug-io nil
          *dribble-stream* nil))
  (defun dribble (&optional file)
    (if file
      (progn
        (if dribble-file
          (warn (TEXT "Already dribbling to ~S") *dribble-stream*)
          ;; Dribbling means to redirect all screen output to the file.
          ;; We redirect all standard streams. More precisely, those
          ;; which are #<SYNONYM-STREAM *TERMINAL-IO*>. Those which are
          ;; synonyms to other standard streams indirectly referring
          ;; to #<SYNONYM-STREAM *TERMINAL-IO*> are not redirected,
          ;; because that would cause each output to this stream to
          ;; be written twice to the dribble-file.
          (labels ((goes-to-terminal (stream) ; this is a hack
                     (and (typep stream 'synonym-stream)
                          (eq (synonym-stream-symbol stream) '*terminal-io*)))
                   (goes-indirectly-to-terminal (stream) ; an even bigger hack
                     (and (typep stream 'synonym-stream)
                          (let ((sym (synonym-stream-symbol stream)))
                            (and (boundp sym)
                                 (let ((stream (symbol-value sym)))
                                   (or (goes-to-terminal stream)
                                       (goes-indirectly-to-terminal
                                        stream))))))))
            (setq *dribble-stream* (open file :direction :output
                                              :if-exists :append
                                              :if-does-not-exist :create)
                  dribble-file (make-synonym-stream '*dribble-stream*))
            (write-string (TEXT ";; Dribble started ") *dribble-stream*)
            (funcall (date-format) *dribble-stream*
                     (multiple-value-list (get-decoded-time)))
            (terpri *dribble-stream*)
            (macrolet ((save (glo loc type)
                         `(if (goes-indirectly-to-terminal ,glo)
                            (setq ,loc nil)
                            (setq ,loc ,glo
                                  ,glo
                                  ,(ecase type
                                    (:in `(make-echo-stream
                                           ,glo dribble-file))
                                    (:out `(make-broadcast-stream
                                            ,glo dribble-file))
                                    (:io `(make-two-way-stream
                                           (make-echo-stream ,glo dribble-file)
                                           (make-broadcast-stream
                                            ,glo dribble-file))))))))
              (save *standard-input*  dribbled-input        :in)
              (save *standard-output* dribbled-output       :out)
              (save *error-output*    dribbled-error-output :out)
              (save *trace-output*    dribbled-trace-output :out)
              (save *query-io*        dribbled-query-io     :io)
              (save *debug-io*        dribbled-debug-io     :io))
            *dribble-stream*)))
      (if dribble-file
        (macrolet ((restore (loc glo) `(when ,loc (setq ,glo ,loc ,loc nil))))
          (restore dribbled-input        *standard-input*)
          (restore dribbled-output       *standard-output*)
          (restore dribbled-error-output *error-output*)
          (restore dribbled-trace-output *trace-output*)
          (restore dribbled-query-io     *query-io*)
          (restore dribbled-debug-io     *debug-io*)
          (setq dribble-file nil)
          (write-string (TEXT ";; Dribble finished ") *dribble-stream*)
          (funcall (date-format) *dribble-stream*
                   (multiple-value-list (get-decoded-time)))
          (terpri *dribble-stream*)
          (close *dribble-stream*)
          (prog1 *dribble-stream* (setq *dribble-stream* nil)))
        (warn (TEXT "Currently not dribbling."))))))
