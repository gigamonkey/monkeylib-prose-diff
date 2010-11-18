(in-pacakge :com.gigamonkeys.prose-diff)

;;; Bits of utility code that perhaps should be moved into
;;; com.gigamonkeys.utilities or replaced with calls to equivalent
;;; bits o fsome standard utility library.

(defun take (list n)
  "Return a list of of the first n values of list and the left-over
tail as a secondary value."
  (let ((tail (nthcdr n list)))
    (values (ldiff list tail) tail)))

(defun vector-push-extend* (list v)
  "Push all the elements of `list' onto v as if by vector-push-extend"
  (loop for item in list do (vector-push-extend item v)))

(defun split-list (list tail)
  (let ((rest (nthcdr (or (search tail list) (error "~a not found in ~a" tail list)) list)))
    (values (ldiff list rest) rest)))

(defun longer (list-a list-b)
  (cond
    ((endp list-b)
     (not (endp list-a)))
    ((endp list-a)
     nil)
    (t (longer (cdr list-a) (cdr list-b)))))


