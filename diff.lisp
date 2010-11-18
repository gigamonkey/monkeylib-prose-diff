(in-package :com.gigamonkeys.prose-diff)

;;;
;;; Generic functions for diffing vectors of objects.
;;;

(defun diff-vectors (old new)
  "Diff two vectors returning a vector with the elements of old and
new wrapped in conses whose CAR is either :LCS, :DELETE, or :ADD."
  (loop with output = (make-array (length new) :adjustable t :fill-pointer 0)
     with old-i = 0
     with old-length = (length old)
     with new-i = 0
     with new-length = (length new)
     for next-lcs across (lcs old new) 
     do
       (setf old-i (emit-diffs next-lcs old old-i old-length :delete output))
       (setf new-i (emit-diffs next-lcs new new-i new-length :add output))
       (vector-push-extend (cons :lcs next-lcs) output)

     finally
       (setf old-i (emit-diffs (cons nil nil) old old-i old-length :delete output))
       (setf new-i (emit-diffs (cons nil nil) new new-i new-length :add output))
       (return output)))

(defun emit-diffs (next-lcs v i max-i marker output)
  (cond
    ((< i max-i) 
     (let ((idx (or (position next-lcs v :start i) max-i)))
       (cond
         ((> idx i)
          (loop for j from i below idx do (vector-push-extend (cons marker (aref v j)) output))
          (1+ idx))
         (t
          (1+ i)))))
    (t i)))
