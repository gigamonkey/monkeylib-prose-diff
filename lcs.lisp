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

(defun diff-linearized-markup (a b)
  (diff-vectors (linearize-markup a t) (linearize-markup b t)))

(defun diff-textified-markup (a b)
  (clean-diff-vector
   (diff-textified-vectors (textify-markup a t) (textify-markup b t))))

(defun diff-textified-vectors (old new)

  (loop with output = (make-array (length new) :adjustable t :fill-pointer 0)
     with old-i = 0
     with old-length = (length old)
     with new-i = 0
     with new-length = (length new)
     for next-lcs across (lcs old new) 
     do
       (setf old-i (emit-textified-diffs next-lcs old old-i old-length :del output))
       (setf new-i (emit-textified-diffs next-lcs new new-i new-length :add output))
       (vector-push-extend next-lcs output)

     finally (return output)))

(defun emit-textified-diffs (next-lcs v i max-i marker-name output)
  (cond
    ((< i max-i) 
     (let ((idx (or (position next-lcs v :start i) max-i)))
       (cond
         ((> idx i)
          (loop for j from i below idx do
               (vector-push-extend (add-property (aref v j) marker-name) output))
          (1+ idx))
         (t
          (1+ i)))))
    (t i)))

(defun clean-diff-vector (v)
  (remove-if #'diff-junk v))

(defun diff-junk (text)
  "Empty text elements that are being deleted will just mess up the
detextification algorithm."
  (and (string= (text text) "") (eql (first (properties text)) :del)))

(defun foo-test ()
  (list
   (rewrite-adds-and-deletes
    (detextify-markup
     (diff-textified-markup
      '(:p "foo " (:i "quux") " baz")
      '(:p "foo " (:b "quux") " baz"))))

  (rewrite-adds-and-deletes
   (detextify-markup
    (diff-textified-markup
     '(:p "foo " (:i "quux bar biff") " baz")
     '(:p "foo " (:i "quux baz boom") " baz"))))))

(defun full-file-diff (old new)
  (remove-empties
   (rewrite-adds-and-deletes
    (detextify-markup
     (diff-textified-markup (parse-file old) (parse-file new))))))

(defun remove-empties (tree)
  (labels ((helper (x)
             (cond
               ((and (consp x) (null (rest x))) nil)
               ((consp x) (list (mapcan #'helper x)))
               (t (list x)))))
    (first (helper tree))))

(defun rewrite-adds-and-deletes (tree)
  (cond
    ((consp tree)
     (let ((add-or-delete (has-nested-add-or-delete-p tree)))
       (when add-or-delete
         (setf tree (promote-tag add-or-delete tree)))
       (mapcar #'rewrite-adds-and-deletes tree)))
    (t tree)))

(defun has-nested-add-or-delete-p (tree)
  "Tree has a nested :add or :del tag and it's not the only tag.
Returns which one it is since there should only be one or the other."
  (let ((tags (nested-tags tree)))
    (and (not (null (cdr tags)))
         (or (find :add tags) (find :del tags)))))

(defun promote-tag (tag tree)
  (list tag (remove-tag tag tree)))

(defun remove-tag (tag tree)
  (cond 
    ((consp tree)
     (cond 
       ((consp (second tree))
        (destructuring-bind (t1 (t2 . rest)) tree
          (cond
            ((eql t1 tag) `(,t2 ,@rest))
            ((eql t2 tag) `(,t1 ,@rest))
            (t `(,t1 ,(remove-tag tag `(,t2 ,@rest)))))))
       (t (destructuring-bind (t1 . rest) tree
            (cond
              ((eql t1 tag) rest)
              (t `(,t1 ,@rest)))))))
    (t tree)))



(defun remove-tag-test ()
  (and 
   (equalp
    (remove-tag :add '(:i (:b (:add "foo" (:x "abc")))))
    '(:i (:b "foo" (:x "abc"))))

   (equalp
    (remove-tag :b '(:i (:b (:add "foo" (:x "abc")))))
    '(:i (:add "foo" (:x "abc"))))

   (equalp
    (remove-tag :i '(:i (:b (:add "foo" (:x "abc")))))
    '(:b (:add "foo" (:x "abc"))))))

(defun nested-tags (tree)
  (if (consp tree)
      (if (null (cddr tree))
          (cons (car tree) (nested-tags (second tree)))
          (list (car tree)))))
