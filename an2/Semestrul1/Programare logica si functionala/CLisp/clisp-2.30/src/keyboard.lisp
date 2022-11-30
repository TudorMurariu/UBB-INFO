;; Keyboard stream

(in-package "EXT")
(export '(with-keyboard *keyboard-input*))
(in-package "SYSTEM")

;;;--------------------------------------------------------------------------

(defvar *keyboard-input*)
(defmacro with-keyboard (&body body)
  `(SYS::EXEC-WITH-KEYBOARD (FUNCTION (LAMBDA () (PROGN ,@body))))
)
(defun exec-with-keyboard (fun)
  #+(or OS/2 WIN32) ; *keyboard-input* existiert schon
    (funcall fun)
  #+(or UNIX ACORN-RISCOS)
    (let ((mode nil))
      (unwind-protect
        (progn
          (unless *keyboard-input*
            (setq *keyboard-input* (sys::make-keyboard-stream))
          )
          (setq mode (sys::terminal-raw *terminal-io* t))
          (funcall fun)
        )
        (sys::terminal-raw *terminal-io* mode)
    ) )
  #+AMIGA
    (let ((*keyboard-input* *terminal-io*))
      (unwind-protect
        (progn (sys::terminal-raw *terminal-io* t) (funcall fun))
        (sys::terminal-raw *terminal-io* nil)
    ) )
    #| ;; redefined after SCREEN is loaded:
    (let ((*keyboard-input* (screen::make-generic-stream ...)))
      (unwind-protect
        (funcall fun)
        (close *keyboard-input*)
    ) )
    |#
)

; Used by spvw.d.
(defun wait-keypress ()
  (with-keyboard (read-char *keyboard-input*))
)

