(in-package :com.gigamonkeys.prose-diff)

(defclass chunk ()
  ((markup :initarg :markup :accessor markup)
   (textified :reader textified)
   (most-similar :initform nil :accessor most-similar)
   (pair :initform nil :accessor pair)))

(defclass pair ()
  ((original :initarg :original :reader original)
   (edited :initarg :edited :reader edited)))

(defmethod initialize-instance :after ((chunk chunk) &key &allow-other-keys)
  (with-slots (markup textified) chunk
    (setf textified (textify-markup markup))))

(defun make-chunk (markup)
  (make-instance 'chunk :markup markup))

(defun make-empty-chunk ()
  (make-chunk ()))

(defun empty-chunk-p (chunk)
  (null (markup chunk)))

(defun combine-chunks (&rest chunks)
  (make-instance 'chunk :markup (apply #'append (mapcar #'markup chunks))))

(defun paragraphs (file)
  "Split file into chunks, one per paragraph."
  (loop for p in (rest (parse-file file))
       for i from 0
       for markup = (list p)
       collect (make-chunk markup)))

(defmethod print-object ((o chunk) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~s identical: ~a; symmetric: ~a" (markup o) (identical-p o) (symmetrical-p o))))

(defmethod print-object ((o pair) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~&original: ~s~&edited: ~s" (original o) (edited o))))

(defun identical-p (p)
  (and (most-similar p) (equal (markup p) (markup (most-similar p)))))

(defun symmetrical-p (p)
  (and (most-similar p) (eql (most-similar (most-similar p)) p)))

(defun establish-pairs (original edited)
  "Establish the pairing between two lists of chunks."
  (pair-identical original edited)
  (pair-symmetrical original edited)
  (repair-asymmetrical original t)
  (repair-asymmetrical edited nil))

(defun pair-identical (original edited)
  (let ((identical (make-hash-table :test #'equal)))
    (loop for o in original do (setf (gethash (markup o) identical) o))
    (loop for e in edited 
       for o = (gethash (markup e) identical)
       do 
         (when o
           (setf (most-similar e) o)
           (setf (most-similar o) e)))))

(defun pair-symmetrical (original edited)
  (set-most-similar original edited)
  (loop for chunk in original
     when (symmetrical-p chunk) do
       (pair-chunks chunk (most-similar chunk))))

(defun set-most-similar (original edited)
  "Make each chunk in original and edited point to the most similar
chunk on the other side unless they have already been paired with an
identical chunk."
  (loop for c in edited unless (most-similar c) do (find-most-similar c original))
  (loop for c in original unless (most-similar c) do (find-most-similar c edited)))

(defun find-most-similar (p others)
  (let ((textified (textified p)))
    (flet ((score (o) (similarity textified (textified o))))
      (setf (most-similar p) (maximum others :key #'score)))))

(defun pair-chunks (original edited)
  (setf (pair original)
        (setf (pair edited)
              (make-instance 'pair :original original :edited edited))))

(defun repair-asymmetrical (chunks original-p)
  (loop for chunk in chunks
     when (not (symmetrical-p chunk)) do
       (let ((empty (make-empty-chunk)))
         (setf (most-similar chunk) empty)
         (if original-p
             (pair-chunks chunk empty)
             (pair-chunks empty chunk)))))
                                  



(defun diff-to-markup (original-file edited-file)
  (let ((original (paragraphs original-file))
        (edited (paragraphs edited-file)))
    (establish-pairs original edited)
    (loop for (label . pair) across (diff-vectors (as-pairs original) (as-pairs edited))
       for diff = (cleaned-diff-output (diff-pair pair))
       nconc 
         (cond
           ((and (eql label :add) (not (empty-chunk-p (original pair))))
            `(((:div :class "moved") ,@diff)))
           ((and (eql label :delete) (not (empty-chunk-p (edited pair))))
            `(((:div :class "moved-away") ,@diff)))
           (t diff)))))

(defun diff-to-markup/no-moves (original-file edited-file)
  (let ((original (paragraphs original-file))
        (edited (paragraphs edited-file)))
    (establish-pairs original edited)
    (loop for (label . pair) across (diff-vectors (as-pairs original) (as-pairs edited))
       for diff = (cleaned-diff-output (diff-pair pair))
       nconc diff)))

(defun show-pairing (label add)
  (let* ((ta (textified add))
         (tb (textified (most-similar add)))
         (diff (diff-textified tb ta))
         #+(or)(undiffed (undiffed (detextify-markup diff))))
    (when (finer-grained-p diff ta)
      (format t "~2&~a: ~s => ~s~&diff: ~s~&identical: ~a; symmetric: ~a; similarity: ~f; one-way: ~f)"
              label
              (markup add)
              (markup (most-similar add))
              (detextify-markup diff)
              (identical-p add)
              (symmetrical-p add)
              (similarity ta tb)
              (one-way-similarity ta tb)))))

(defun finer-grained-p (new-diff orig-diff)
  "Is the new-diff finer-grained than the original?"
  (< (length (remove-if-not #'part-of-diff-p new-diff)) (length orig-diff)))

(defun part-of-diff-p (text)
  (or (has-property-p :add text) (has-property-p :delete text)))

(defun pair-adds-and-deletes (original-file edited-file)
  (let ((markup (diff-to-markup/no-moves original-file edited-file)))
    (flet ((chunkify (things)
             (mapcar (lambda (x) (make-chunk (rest x))) things)))
      (let ((adds (chunkify (extract markup :add)))
            (deletes (chunkify (extract markup :delete))))
        (pair-identical adds deletes)
        (pair-symmetrical (remove-if #'identical-p adds) (remove-if #'identical-p deletes))
        (loop for x in deletes do (show-pairing "DELETE" x))
        (loop for x in adds do (show-pairing "ADD" x))
        ))))

(defun refine-diffs (original-file edited-file)
  (let ((markup (diff-to-markup/no-moves original-file edited-file)))
    (flet ((chunkify (things)
             (mapcar (lambda (x) (make-chunk (rest x))) things)))
      (let ((adds (chunkify (extract markup :add)))
            (deletes (chunkify (extract markup :delete))))
        (loop for add in adds do
             (multiple-value-bind (delete refinement) (find-most-refining add deletes)
               (when (plusp refinement)
                 (format t "~2&~s vs ~s (~f)~&~s"
                         (markup add)
                         (markup delete)
                         refinement
                         (cleaned-diff-output (diff-textified (dediff (textified delete)) (dediff (textified add))))))))))))

(defun dediff (textified)
  (map 'vector (lambda (x) (remove-properties x '(:add :delete))) textified))

(defun refinement (delete add)
  (let* ((refined-diff (diff-textified (dediff (textified delete)) (dediff (textified add))))
         (diff-length (length (remove-if-not #'part-of-diff-p refined-diff)))
         (original-add-length (length (textified add))))
    (* original-add-length (/ (- original-add-length diff-length) original-add-length))))

(defun find-most-refining (add deletes)
  (flet ((score (delete) (refinement delete add)))
    (maximum deletes :key #'score)))

(defun extract (markup what)
  "Find all the what elements in markup."
  (cond
    ((consp markup)
     (if (eql (car markup) what)
         (list markup)
         (mapcan (lambda (x) (extract x what)) markup)))
    (t nil)))

(defun undiffed (markup)
  (cond
    ((consp markup) 
     (if (or (eql (car markup) :add)
             (eql (car markup) :delete))
         nil
         (mapcan #'undiffed markup)))
    (t (list markup))))

(defun as-pairs (chunks) (map 'vector #'pair chunks))

(defun cleaned-diff-output (diff)
  (mapcar #'rewrite-adds-and-deletes
          (remove nil
                  (mapcar #'remove-empties (detextify-markup diff)))))

(defun remove-empties (tree)
  "Remove empty sub-trees from tree."
  (labels ((helper (x)
             (cond
               ((and (consp x) (null (rest x))) nil)
               ((consp x) (list (mapcan #'helper x)))
               (t (list x)))))
    (first (helper tree))))

(defun rewrite-adds-and-deletes (tree)
  "Rewrite the Markup tree so that :add and :delete tags are moved out
as far as possible. E.g. given (:p (:add something)) we tranform it
to: (:add (:p something))."
  (cond
    ((consp tree)
     (let ((add-or-delete (has-nested-add-or-delete-p tree)))
       (mapcar
        #'rewrite-adds-and-deletes
        (if add-or-delete (promote-tag add-or-delete tree) tree))))
    (t tree)))

(defun has-nested-add-or-delete-p (tree)
  "Tree has a nested :add or :delete tag and it's not the only tag.
Returns which one it is since there should only be one or the other."
  (let ((tags (nested-tags tree)))
    (and (not (null (cdr tags)))
         (or (find :add tags) (find :delete tags)))))

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

(defun nested-tags (tree)
  "Give a tree like '(:a (:b (:c ...))) the nested tags are (:a :b :c)"
  (if (consp tree)
      (if (null (cddr tree))
          (cons (car tree) (nested-tags (second tree)))
          (list (car tree)))))