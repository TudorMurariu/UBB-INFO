; 1.
; or is a special operator, not a function!
;

(SET 'L '(t nil t))

(defun list_or(L)
    (cond
        ((null L) nil)
        ((car L) t)
        (t
            (list_or (cdr L))
        )
    )
)

(print (list_or L))
