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

(defun diff-pair (pair)
  (diff-textified (textified (original pair)) (textified (edited pair))))

(defun diff-textified (a b)
  (flet ((translate-textified (x)
           (destructuring-bind (label . thing) x
             (ecase label
               (:lcs thing)
               ((:add :delete) (add-property thing label)))))

         (empty-delete? (x)
           ;; Empty text elements that are being deleted will just
           ;; mess up the detextification algorithm. Empty :adds, on
           ;; the other hand, are still needed to separate elements.
           (and (string= (text x) "") (eql (first (properties x)) :delete)))

         (collapse-spaces-in-lcs (lcs)
           ;; Since spaces are quite common in text, the LCS of any
           ;; two bits of text will include a lot of them. However,
           ;; when there are no words between them in the LCS it is
           ;; better to collapse them so that instead of diffing: 'a
           ;; b' and 'd e' as ((:delete a) (:add d) " " (:delete b)
           ;; (:add e))' we get ((:delete "a b") (:add "d e")) We
           ;; also get rid of any leading spaces for a similar
           ;; reason.
           (let ((just-saw-space t))
             (flet ((collapsable? (x)
                      (if (string= (text x) " ")
                          (prog1 just-saw-space (setf just-saw-space t))
                          (setf just-saw-space nil))))
               (remove-if #'collapsable? lcs)))))
    
    (let ((diff (diff-vectors a b #'collapse-spaces-in-lcs)))
      (remove-if #'empty-delete? (map-into diff #'translate-textified diff)))))

(defun split-positions (one-chunk parts)
  "Find the positions where one-chunk should be split to get pieces
  corresponding to the given parts, which are derived from one-chunk.
  For instance, if one-chunk is a piece of text that was split up and
  the pieces inserted at various places in the new document, it will
  show up as a single deletion in the diff and multiple additions. Or
  conversely, if a bunch of separate pieces (from different
  paragraphs) in the original document were combined into contiguous
  text in the edited document, we would have a single addition and
  multiple deletions."
  (multiple-value-bind (one-chunk-lcs-indices combined-lcs-indices) 
      (lcs-positions one-chunk (concatenate-vectors parts))
    (setf one-chunk-lcs-indices (coerce one-chunk-lcs-indices 'vector))
    (setf combined-lcs-indices (coerce combined-lcs-indices 'vector))
    (loop for s in (part-starts parts)
       for prev-x = 0 then x
       for x = (position-if (lambda (x) (>= x s)) combined-lcs-indices :start prev-x)
       collect (aref one-chunk-lcs-indices x))))

(defun split-vector (one-chunk parts)
  (loop for (start end) on (split-positions one-chunk parts) 
     collect (subseq one-chunk start end)))

(defun part-starts (parts)
  (loop for (p . rest) on parts
     summing (length p) into total
     when rest collect total into starts
     finally (return (cons 0 starts))))


