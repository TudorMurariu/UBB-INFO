;;;; Command-line completion hook

(in-package "SYSTEM")

;;-----------------------------------------------------------------------------
;; Completing routine for the GNU Readline library.
;; Input: string (the input line), and the boundaries of the text to be
;; completed:  (subseq string start end)
;; Output: a list of simple-strings. empty, when no meaningful completions.
;; otherwise, CDR = list of all meaningful completions,
;; CAR = the immediate replacement
#+(or UNIX OS/2)
(let ((state nil))
(defun completion (string start end)
  (let* ((quotedp (and (>= start 1) ; quoted completion?
                       (member (char string (- start 1)) '(#\" #\|))))
         (start1 (if quotedp (1- start) start))
         ;; vars for collecting the symbols
         known-part known-len (return-list '())
         (functionalp1 (and (>= start1 1)
                            (equal (subseq string (- start1 1) start1) "(")))
         (functionalp2 (and (>= start1 2)
                            (equal (subseq string (- start1 2) start1) "#'")))
         ;; completion of a function
         (functionalp (or (= start end) functionalp1 functionalp2))
         (gatherer
          (if functionalp
              (lambda (sym)
                (when (fboundp sym)
                  (let ((name (symbol-name sym)))
                    (when (and (>= (length name) known-len)
                               (string-equal name known-part :end1 known-len))
                      (push name return-list)))))
              (lambda (sym)
                (let ((name (symbol-name sym)))
                  (when (and (>= (length name) known-len)
                             (string-equal name known-part :end1 known-len))
                    (push name return-list))))))
         (package *package*)
         (mapfun #'sys::map-symbols)
         (prefix nil)
         (new-state (list* string start end))
         (void-completion
          ;; special case: nothing was entered to be completed,
          ;; so we try to DESCRIBE the last function symbol entered
          (and (= start end)
               (or (>= start (length string))
                   (whitespacep (schar string start))))))
    (when void-completion
      (do ((pos (min end (1- (length string))) (1- pos)) (depth 0) (white end))
          ((or (minusp pos) (plusp depth))
           (setq start (+ pos 2) end white))
        (cond ((char= #\( (schar string pos)) (incf depth))
              ((char= #\) (schar string pos)) (decf depth))
              ((whitespacep (schar string pos)) (setq white pos)))))
    ;; get the package name:
    (unless quotedp
      (let ((colon (position #\: string :start start :end end)))
        (when colon
          (unless (setq package (find-package (string-upcase
                                               (subseq string start colon))))
            (return-from completion nil))
          (incf colon)
          (if (and (< colon end) (eql (char string colon) #\:))
              (incf colon)
              (setq mapfun #'sys::map-external-symbols))
          (setq prefix (subseq string start colon))
          (setq start colon))))
    (setq known-part (subseq string start end))
    (setq known-len (length known-part))
    (funcall mapfun gatherer package)
    (when (null return-list) (return-from completion nil))
    (when void-completion
      (let ((sym (find-symbol (car return-list) package)))
        (return-from completion
          (when (and sym (fboundp sym)) ; (null (cdr return-list))
            (cond ((equalp state new-state)
                   (describe sym) (terpri) (terpri))
                  ((setq state new-state)))
            0))))
    ;; for a function without arguments, append the closing paren
    (when (and functionalp1
               (null (cdr return-list))
               (let ((sym (find-symbol (car return-list) package)))
                 (and sym (fboundp sym) (functionp (symbol-function sym))
                      (multiple-value-bind (name req-anz opt-anz rest-p key-p)
                          (function-signature (symbol-function sym))
                        (declare (ignore name))
                        (and (eql req-anz 0) (eql opt-anz 0)
                             (not rest-p) (not key-p))))))
      (setf (car return-list) (string-concat (car return-list) ")")))
    (unless quotedp             ; downcase
      (setq return-list (mapcar #'string-downcase return-list)))
    (setq return-list (sort return-list #'string<))
    ;; look for the largest common initial piece
    (let ((imax (reduce #'min return-list :key #'length)))
      (do ((i 0 (1+ i)))
          ((or (eql i imax)
               (let ((c (char (first return-list) i)))
                 (dolist (s (rest return-list) nil)
                   (unless (eql (char s i) c) (return t)))))
           (push (subseq (first return-list) 0 i) return-list))))
    ;; reattach prefix
    (when prefix
      (mapl #'(lambda (l) (setf (car l) (string-concat prefix (car l))))
            return-list))
    return-list))
)
