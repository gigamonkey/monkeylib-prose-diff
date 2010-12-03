(in-package :com.gigamonkeys.prose-diff)

(defun lcs (a b)
  "Compute the longest common subsequence of vectors `a' and `b'"
  (map 'vector (lambda (i) (aref a i)) (lcs-positions a b)))

(defun lcs-length (a b)
  "Compute the length of the longest common subsequence of vectors `a' and `b'"
  (multiple-value-bind (table m n) (%lcs-table a b)
    (aref table n m)))

(defun lcs-positions (a b)
  "Find the indices in a and b of the elements of the LCS."
  (multiple-value-bind (table m n) (%lcs-table a b)
    (let* ((len (aref table n m))
           (a-indices (make-array len))
           (b-indices (make-array len))
           (idx (1- len))
           (i (length a))
           (j (length b)))
    
    (loop while (> (aref table j i) 0) do
         (let* ((current (aref table j i))
                (previous (1- current)))
           
           (cond
             ((and (= previous (aref table (1- j) (1- i)))
                   (= previous (aref table j (1- i)))
                   (= previous (aref table (1- j) i)))
              (decf j)
              (decf i)
              (setf (aref a-indices idx) i)
              (setf (aref b-indices idx) j)
              (decf idx))
             ((= current (aref table (1- j) i)) (decf j))
             ((= current (aref table j (1- i))) (decf i))
             (t (error "Assertion gone haywire: ~s ~s" j i)))))
    (values a-indices b-indices))))

(defun %lcs-table (a b)
  "Compute the MxN table from which we can extract the LCS, and a
bunch of other good stuff."
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

(defun similarity (a b)
  "Compute the similarity of vectors `a' and `b' in terms of the
average of the ratios of the length of the LCS to their length."
  (let ((lcs-length (lcs-length a b)))
    (/ (+ (/ lcs-length (length a)) (/ lcs-length (length b))) 2.0d0)))

(defun one-way-similarity (a b)
  "Like `similarity' but in only one direction."
  (float (/ (lcs-length a b) (length a)) 0d0))

