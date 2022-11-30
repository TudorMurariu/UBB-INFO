;;; Auxiliary socket related functions for BeOS

(in-package "SYSTEM")

(defstruct (servent (:type vector))  ;; see getservent(3)
  (name nil    :type string)
  (aliases nil :type list) ; list of strings
  (port nil    :type integer)
  (proto nil   :type string)
)

; List of all servent structures known to the system,
; initialized upon the first call to socket-service-port.
(defvar *servents*)

(defun socket-service-port (&optional service-name protocol)
  ; Like the implementation of SOCKET-SERVICE-PORT in socket.d.
  (unless protocol (setq protocol "tcp"))
  (assert (typep protocol 'string))
  (unless service-name (setq service-name ':default))
  (unless (boundp '*servents*)
    (setq *servents*
      (let ((filename "/boot/beos/etc/services")
            (servents '()))
        (with-open-file (s filename
                           :direction :input
                           #+UNICODE :external-format #+UNICODE 'charset:UTF-8)
          (loop
            ; Read a line.
            (let ((line (read-line s nil nil)))
              (unless line (return))
              ; Remove trailing comments.
              (let ((i (position #\# line)))
                (when i (setq line (subseq line 0 i))))
              ; Split into whitespace separated fields.
              (let ((fields '()))
                (let ((i 0))
                  (loop
                    (let ((i1 (position-if-not #'whitespacep line :start i)))
                      (unless i1 (return))
                      (let ((i2 (or (position-if #'whitespacep line :start i1)
                                    (length line))))
                        (push (subseq line i1 i2) fields)
                        (setq i i2)
                ) ) ) )
                (setq fields (nreverse fields))
                (when fields
                  ; Split second field, of the form "port/proto".
                  (let* ((port+proto (second fields))
                         (i (position #\/ port+proto)))
                    (when i
                      (push
                        (make-servent
                          :name (first fields)
                          :aliases (cddr fields)
                          :port (parse-integer (subseq port+proto 0 i))
                          :proto (subseq port+proto (1+ i)))
                        servents
                      )
        ) ) ) ) ) ) )
        (nreverse servents)
  ) ) )
  (etypecase service-name
    ((eql :default)
      (copy-list *servents*))
    (string
      (values-list
        (coerce
          (or (find-if #'(lambda (se)
                           (and (string= (servent-name se) service-name)
                                (string= (servent-proto se) protocol)))
                       *servents*)
              (error (TEXT "service does not exist: ~A/~A")
                     service-name protocol))
          'list)))
    (integer
      (values-list
        (coerce
          (or (find-if #'(lambda (se)
                           (and (= (servent-port se) service-name)
                                (string= (servent-proto se) protocol)))
                       *servents*)
              (error (TEXT "service does not exist: ~A/~A")
                     service-name protocol))
          'list)))
) )
