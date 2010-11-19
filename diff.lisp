(in-package :com.gigamonkeys.prose-diff)

;;;
;;; Generic functions for diffing vectors of objects.
;;;

(defun diff-vectors (old new &optional (lcs-frobber #'identity))
  "Diff two vectors returning a vector with the elements of old and
new wrapped in conses whose CAR is either :LCS, :DELETE, or :ADD.
Optionally frob the computed LCS before computing the diff."
  (loop with output = (make-array (length new) :adjustable t :fill-pointer 0)
     with old-i = 0
     with old-length = (length old)
     with new-i = 0
     with new-length = (length new)
     for next-lcs across (funcall lcs-frobber (lcs old new))
     do
       (setf old-i (emit-diffs next-lcs old old-i old-length :delete output))
       (setf new-i (emit-diffs next-lcs new new-i new-length :add output))
       (vector-push-extend (cons :lcs next-lcs) output)

     finally
       (emit-diffs (cons nil nil) old old-i old-length :delete output)
       (emit-diffs (cons nil nil) new new-i new-length :add output)
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

(defun diff-textified-markup (a b)
  (let ((diff (diff-vectors (textify-markup a) (textify-markup b) #'collapse-spaces-in-lcs)))
    (clean-diff-vector (map-into diff #'translate-textified diff))))

(defun collapse-spaces-in-lcs (lcs)
  ;; Since spaces are quite common in text, the LCS of any two bits of
  ;; text will include a lot of them. However when there are no words
  ;; between them in the LCS it is better to collapse them so that
  ;; instead of diffing: 'a b' and 'd e' and ((:delete a) (:add d) " "
  ;; (:delete b) (:add e))' we get ((:delete "a b") (:add "d e")) We also
  ;; get rid of any leading spaces.
  (let ((just-saw-space t))
    (remove-if (lambda (x)
                 (cond
                   ((string= (text x) " ")
                    (prog1 just-saw-space
                      (setf just-saw-space t)))
                   (t (setf just-saw-space nil)))) lcs)))

(defun translate-textified (x)
  (destructuring-bind (label . thing) x
    (ecase label
      (:lcs thing)
      ((:add :delete) (add-property thing label)))))

(defun clean-diff-vector (v)
  (remove-if #'diff-junk v))

(defun diff-junk (text)
  "Empty text elements that are being deleted will just mess up the
detextification algorithm. Empty :adds, on the other hand, are still
needed to separate elements."
  (and (string= (text text) "") (eql (first (properties text)) :delete)))
