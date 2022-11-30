;; SCREEN is actually conditionally defined in constpack.d,
;; but the condition (in lispbibl.d) is too hairy
;; to duplicate it in makemake.in, so this file is always compiled
;; (even when it is not subsequently loaded by init.lisp),
;; thus we have to use DEFPACKAGE here just in case

(defpackage "SCREEN"
  (:use "COMMON-LISP" "EXT"))

(in-package "SCREEN")

#|
; Re-Export von importierten Symbolen kann man nicht mit der
; P I S E R U I - Regel erreichen. Diese Symbole muss man zuerst importieren.
(in-package "SYSTEM")
(import '(make-window window-size
          window-cursor-position set-window-cursor-position
          clear-window clear-window-to-eot clear-window-to-eol
          delete-window-line insert-window-line
          highlight-on highlight-off window-cursor-on window-cursor-off
         )
        "SCREEN"
)
(in-package "SCREEN")
|#

(export '(; exported functions and macros:
          make-window window-size
          window-cursor-position set-window-cursor-position
          clear-window clear-window-to-eot clear-window-to-eol
          delete-window-line insert-window-line
          highlight-on highlight-off window-cursor-on window-cursor-off
          with-window *window*
          read-keyboard-char
          ; user-settable things:
          *new-window*
)        )

(proclaim '(special *window*))

#-AMIGA
(defun read-keyboard-char (stream)
  (declare (ignore stream))
  (read-char *keyboard-input*)
)

#-AMIGA
(defconstant *new-window* nil)


;;;; SCREEN-Package for Amiga
;;;; Jörg Höhle, 23.7.1996
;;;; TODO: Use Gray streams instead of old generic-streams.

#+AMIGA (use-package "CLOS")
#+AMIGA
(progn

; Determines the "new window" policy.
(defvar *new-window* "RAW:0/11/581/231/CLISP Window"
  "This variables determines the behaviour of SCREEN:MAKE-WINDOW.
If NIL, it uses *TERMINAL-IO*. If non-NIL, it should be the specification
string of a special file to be OPENed, e.g. \"RAW:0/11/581/231/Window Title\"."
)

;;; Why is this so complex? Because applications (Punimax) need to use
;;; the raw mode functions but nevertheless expect to read input in
;;; cooked mode. Cooked mode is also nicer if you happen to fall into
;;; the debugger. Thus I provide special streams that switch modes
;;; automatically.


;; The class of all data present in SCREEN's generic streams.
(defclass screen-controller (generic-stream-controller)
  ((stream :reader controller-stream
           :type stream
) ))

; The screen's mode: either T (raw) or NIL (line editing enabled)
(defgeneric controller-mode (controller))
(defgeneric (setf controller-mode) (mode controller))


;; Two subclasses:

; terminal-controller generic streams refer to *terminal-io*.
(defclass terminal-controller (screen-controller)
  ((stream :initform *terminal-io*) ; cache so that *terminal-io* can be rebound
   ; The terminal's mode is cached in stream.d, no need to cache it here.
) )

; window-controller generic streams refer to a special device stream.
(defclass window-controller (screen-controller)
  ((stream :initarg :stream)
   (mode :accessor controller-mode
         :initform 'unknown ; the initial mode is unknown
  ))
)


;; (raw-mode stream mode) puts the stream into the given mode (T or NIL)
;; and returns the old mode.
(defun raw-mode (stream mode)
  (if (generic-stream-p stream)
    (generic-raw-mode (generic-stream-controller stream) mode)
    ; handle low-level streams here
    (sys::terminal-raw stream mode t)
) )
(defgeneric generic-raw-mode (controller mode))
(defmethod generic-raw-mode ((controller screen-controller) mode)
  (raw-mode (controller-stream controller) mode)
)
(defmethod generic-raw-mode ((controller window-controller) mode)
  (let ((old-mode (controller-mode controller)))
    ; compare against the cached current mode
    (if (eq mode old-mode)
      old-mode
      (prog1
        (raw-mode (controller-stream controller) mode)
        (setf (controller-mode controller) mode)
) ) ) )


;; Return a new window stream.
(defun make-window (&optional (*new-window* *new-window*))
  (let ((stream
          (make-generic-stream
            (if *new-window*
              (make-instance 'window-controller
                :stream (etypecase *new-window*
                          (STREAM *new-window*)
                          ((OR PATHNAME STRING) (open *new-window* :direction :io))
              )         )
              (make-instance 'terminal-controller)
       )) ) )
    ; (raw-mode stream t) ; Don't need this because modes are switched automatically.
    stream
) )


;; Operations on SCREEN streams.

(defmethod generic-stream-read-char ((controller screen-controller))
  (generic-raw-mode controller nil) ;; want to switch to cooked mode
  (read-char (controller-stream controller))
)

(defmethod generic-stream-read-char-will-hang-p ((controller screen-controller))
  (generic-raw-mode controller nil) ;; want to switch to cooked mode
  (null (listen (controller-stream controller)))
)

(defmethod generic-stream-clear-input ((controller screen-controller))
  (generic-raw-mode controller nil) ;; want to switch to cooked mode
  (clear-input (controller-stream controller))
  T
)

(defmethod generic-stream-write-char ((controller screen-controller) ch)
  (write-char ch (controller-stream controller))
)

;; for speed only
(defmethod generic-stream-write-string ((controller screen-controller) string start len)
  (write-string (substring string start (+ start len))
                (controller-stream controller)
) )

(defmethod generic-stream-finish-output ((controller screen-controller))
  (finish-output (controller-stream controller))
)

(defmethod generic-stream-force-output ((controller screen-controller))
  (force-output (controller-stream controller))
)

(defmethod generic-stream-clear-output ((controller screen-controller))
  (clear-output (controller-stream controller))
)

(defmethod generic-stream-close ((controller screen-controller))
  (raw-mode (controller-stream controller) nil)
  T
)
(defmethod generic-stream-close ((controller window-controller))
  ; Don't need to call raw-mode on this window since it will go away anyway.
  (close (controller-stream controller))
)

; Return a list of all characters immediately available on stream
(defun stream-chars (stream)
  (let ((res '()))
    (loop
      (let ((c (read-char-no-hang stream)))
        (unless c (return))
        (push c res)
    ) )
    (nreverse res)
) )

; Parse an ANSI Control String:
; { #\CSI | #\ESC #\[ } { digits #\; }* [ digits [ #\; ] ] { rest }
; Return (rest . ... num2 num1)
(defun parse-csi (string)
  (let ((res '())
        num
        (start (cond ((eq (aref string 0) #\CSI)      1)
                     ((and (eq (aref string 0) #\ESC)
                           (> (length string) 1)
                           (eq (aref string 1) #\[))  2)
                     (t (error "Not a CSI sequence: ~S" string))
       ))      )
    (loop
      (multiple-value-setq (num start) (parse-integer string :start start :junk-allowed t))
      (when (null num) (return))
      (push num res)
      (when (and (< start (length string)) (eq (aref string start) #\;))
        (incf start)                 ; skip ANSI separator
    ) )
    (cons (subseq string start) res) ; push rest
) )

; Send a CSI sequence to the terminal and read the response, an ANSI sequence.
; Return a reversed list of numbers.
; (Note: As a side effect, a (clear-input stream) is done, which throws away
; characters.)
(defun read-csi-response (stream send expected)
  (clear-input stream)
  (write-string send stream)
  (let* ((chars
           (or (stream-chars stream)
               (error "Got no response from ~S." stream)
         ) )
         (response (parse-csi (coerce chars 'string))))
    (unless (string= expected (first response))
      (error (TEXT "Got bad response from ~S: ~S")
             stream chars
    ) )
    (cdr response)
) )

(defun window-size (stream)
  "Reports window size.
Will flush pending characters!"
  ;; (window-checks stream)
  (when (and (generic-stream-p stream)
             (typep (generic-stream-controller stream) 'screen-controller))
    (raw-mode stream t)
    (setq stream (controller-stream (generic-stream-controller stream)))
  )
  (let ((response
          (read-csi-response
            stream
            (load-time-value (coerce '(#\CSI #\0 #\Space #\q) 'string))
            "r"               ; parse-integer ate the space
       )) )
    (let ((width (first response))
          (height (second response)))
      ; Decrement width to avoid problems with wrapping/scrolling of the last line.
      (values height (- width 1))
) ) )

(defun window-cursor-position (stream)
  "Reports cursor position, report origin as 0;0.
Will flush pending characters!"
  ;; (window-checks stream)
  (when (and (generic-stream-p stream)
             (typep (generic-stream-controller stream) 'screen-controller))
    (raw-mode stream t)
    (setq stream (controller-stream (generic-stream-controller stream)))
  )
  (let ((response
          (read-csi-response
            stream
            (load-time-value (coerce '(#\CSI #\6 #\n) 'string))
            "R"
       )) )
    (values (1- (second response)) (1- (first response))) ; line;column
) )

(defun set-window-cursor-position (stream line column)
  ;; ANSI position origin is 1;1, but SCREEN uses 0;0
  (format stream "~a~d;~dH" #\CSI (1+ line) (1+ column))
  (values)
)

(defun clear-window (stream)
  (write-char '#\FF stream)
  (values)
)

(defun clear-window-to-eot (stream)
  (write-string (load-time-value (coerce '(#\CSI #\J) 'string)) stream)
  (values)
)

(defun clear-window-to-eol (stream)
  (write-string (load-time-value (coerce '(#\CSI #\K) 'string)) stream)
  (values)
)

(defun delete-window-line (stream)
  (write-string (load-time-value (coerce '(#\CSI #\M) 'string)) stream)
  (values)
)

(defun insert-window-line (stream)
  (write-string (load-time-value (coerce '(#\CSI #\L) 'string)) stream)
  (values)
)

(defun highlight-on (stream)
  (write-string (load-time-value (coerce '(#\CSI #\1 #\m) 'string)) stream)
  (values)
)

(defun highlight-off (stream)
  (write-string (load-time-value (coerce '(#\CSI #\m) 'string)) stream)
  (values)
)

(defun window-cursor-on (stream)
  (write-string (load-time-value (coerce '(#\CSI #\Space #\p) 'string)) stream)
  (values)
)

(defun window-cursor-off (stream)
  (write-string (load-time-value (coerce '(#\CSI #\0 #\Space #\p) 'string)) stream)
  (values)
)


#|
;; Read characters in raw mode
(defun read-raw-char (stream)
  (raw-mode stream t)
  (when (generic-stream-p stream)
    (setq stream (controller-stream (generic-stream-controller stream))))
  (read-char stream)
)
|#

;; This function does a simple mapping from CSI-sequences as reported
;; by the Amiga keyboard to characters with HYPER (even SUPER or CONTROL) bit
;; set. Furthermore, most codes between 1 and 26 get the CONTROL bit set.
;; key   codes  shift   character
;; f1    CSI0~  CSI10~  #\f1, #\s-f1
;; f10   CSI9~  CSI19~  #\f10, #\s-f10
;; Help  CSI?~  CSI?~   #\Help
;; Up    CSIA   CSIT    #\Up,    #\S-Up
;; Down  CSIB   CSIS    #\Down,  #\S-Down
;; Left  CSID   CSI A   #\Left,  #\S-Left
;; Right CSIC   CSI @   #\Right, #\S-Right
(defun read-keyboard-char (stream)
  ; In order to minimize mode switches, switch once then read from low-level stream
  (raw-mode stream t)
  (when (generic-stream-p stream)
    (setq stream (controller-stream (generic-stream-controller stream))))
  (let ((c (read-char stream)))
    (if (char= c '#\CSI)
      (let ((chars '()) c)
        (loop
          (setq c (read-char stream))
          (unless (char<= #\Space c #\?) (return))
          (push c chars)
        )
        (cond ((char/= c '#\~) ; arrow keys
               (or (cdr (assoc c (if chars
                                   '((#\A . #\S-Left)
                                     (#\@ . #\S-Right)
                                    )
                                   '((#\A . #\Up)
                                     (#\B . #\Down)
                                     (#\C . #\Right)
                                     (#\D . #\Left)
                                     (#\S . #\S-Down)
                                     (#\T . #\S-Up)
                                    )
                   )    )        )
                   '#\CSI
              ))
              ((null chars) '#\CSI) ; don't parse this...
              ((eq (first chars) '#\?) '#\Help) ; Help key
              ((not (digit-char-p (first chars))) '#\CSI) ; don't parse this...
              ((null (rest chars)) ; f1 ... f10
               (int-char (+ (char-int '#\f1) (digit-char-p (first chars))))
              )
              ((eq '#\1 (second chars)) ; F1 ... F10
               (int-char (+ (char-int '#\s-f1) (digit-char-p (first chars))))
              )
              (t '#\CSI) ; don't parse this...
      ) )
      (if (and (<= 1 (char-int c) 26) ; Ctrl-A ... Ctrl-Z
               (not (or (eql c #\Newline) (eql c #\Backspace) (eql c #\Tab)
                        (eql c #\Return)
          )    )    )
        (set-char-bit (int-char (+ 64 (char-int c))) :CONTROL t)
        c
) ) ) )


;; Support for WITH-KEYBOARD and *KEYBOARD-INPUT*

;; The mode is switched to raw when the stream is created and switched back
;; when the stream is closed.

(defclass keyboard-controller (terminal-controller)
  ((orig-mode :initform (raw-mode *terminal-io* t))
   ; *terminal-io* is cached by terminal-controller
) )

(defmethod generic-stream-read-char ((controller keyboard-controller))
  ; make some cursor and function keys mappings, see above
  (read-keyboard-char (controller-stream controller))
)

(defmethod generic-stream-read-char-will-hang-p ((controller keyboard-controller))
  (generic-raw-mode controller t) ;; need to switch to raw mode
  (null (listen (controller-stream controller)))
)

(defmethod generic-stream-clear-input ((controller keyboard-controller))
  (generic-raw-mode controller t) ;; need to switch to raw mode
  (clear-input (controller-stream controller))
  T
)

(defmethod generic-stream-close ((controller keyboard-controller))
  (with-slots (stream orig-mode) controller
    (raw-mode stream orig-mode)
) )

; Redefine WITH-KEYBOARD expansion from KEYBOARD.LSP
(defun system::exec-with-keyboard (fun)
  (let ((*keyboard-input*
          (make-generic-stream (make-instance 'keyboard-controller)) ))
    (unwind-protect
      (funcall fun)
      (close *keyboard-input*)
) ) )

) ; #+AMIGA


(defmacro with-window (&body body)
  `(LET ((*WINDOW* (MAKE-WINDOW)))
     (UNWIND-PROTECT (PROGN ,@body) (CLOSE *WINDOW*))
   )
)
