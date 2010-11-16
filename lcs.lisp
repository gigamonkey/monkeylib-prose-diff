(in-package :com.gigamonkeys.prose-diff)

(defun lcs (a b)
  (extract-lcs (%lcs-table a b) a b))

(defun lcs-length (a b)
  (multiple-value-bind (table m n) (%lcs-table a b)
    (aref table n m)))

(defun %lcs-table (a b)
    
  (let* ((m (length a))
         (n (length b))
         (table (make-array (list (1+ n) (1+ m)) :initial-element 0)))

    (flet ((lcs-length (j i)
             (cond
               ((eql (aref a (1- i)) (aref b (1- j)))
                (+ 1 (aref table (1- j) (1- i))))
               (t
                (max (aref table (1- j) i) (aref table j (1- i)))))))

      (loop for j from 1 to n do
           (loop for i from 1 to m do
              (setf (aref table j i) (lcs-length j i)))))

    (values table m n)))

(defun extract-lcs (table a b)
  (let ((lcs ())
        (i (length a))
        (j (length b)))

    (loop while (> (aref table j i) 0) do
         (let* ((current (aref table j i))
                (previous (1- current)))
           
           (cond
             ((and (eql previous (aref table (1- j) (1- i)))
                   (eql previous (aref table j (1- i)))
                   (eql previous (aref table (1- j) i)))
              (push (aref a (1- i)) lcs)
              (decf j)
              (decf i))
             ((eql current (aref table (1- j) i))
              (decf j))
             ((eql current (aref table j (1- i)))
              (decf i))
             (t
              (format t "what! ~s, ~s"j i)))))
    (coerce lcs 'vector)))


(defun diff-vectors (old new)

  (loop with output = (make-array (length new) :adjustable t :fill-pointer 0)
     with old-i = 0
     with old-length = (length old)
     with new-i = 0
     with new-length = (length new)
     for next-lcs across (lcs old new) 
     do
       (setf old-i (emit-diffs next-lcs old old-i old-length :del output))
       (setf new-i (emit-diffs next-lcs new new-i new-length :add output))
       (vector-push-extend next-lcs output)

     finally (return output)))

(defun emit-diffs (next-lcs v i max-i marker-name output)
  (cond
    ((< i max-i) 
     (let ((idx (or (position next-lcs v :start i) max-i)))
       (cond
         ((> idx i)
          (vector-push-extend (open-marker marker-name) output)
          (loop for j from i below idx do (vector-push-extend (aref v j) output))
         (vector-push-extend (close-marker marker-name) output)
          (1+ idx))
         (t
          (1+ i)))))
    (t i)))