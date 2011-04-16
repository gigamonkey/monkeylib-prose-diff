(in-package :com.gigamonkeys.prose-diff)

(defparameter *short-length* 5)

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

(defun paragraphs (parsed)
  "Split file into chunks, one per paragraph."
  (loop for p in (rest parsed) collect (make-chunk (list p))))

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

(defun establish-pairs (original edited &key unpair-short)
  "Establish the pairing between two lists of chunks."
  (pair-identical original edited)
  ;; XXX -- should perhaps remove elements from original and edited
  ;; that have most-similar set by pair-identical.
  (pair-symmetrical original edited)
  (unpair-asymmetrical original t)
  (unpair-asymmetrical edited nil)
  (when unpair-short
    (unpair-short original t)
    (unpair-short edited nil)))

(defun pair-identical (original edited)
  (let ((identical (make-hash-table :test #'equal)))
    (loop for o in original do (push o (gethash (markup o) identical nil)))
    ;; Reverse the lists so we can pop them off in the right order below.
    (loop for k being the hash-keys of identical do
         (setf (gethash k identical) (nreverse (gethash k identical))))
    (loop for e in edited 
       for o = (pop (gethash (markup e) identical nil))
       do 
         (when o
           (setf (most-similar e) o)
           (setf (most-similar o) e)
           (pair-chunks o e)))))
  
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

(defun find-most-similar (chunk others)
  (let ((textified (textified chunk)))
    (flet ((score (other) (similarity textified (textified other))))
      (setf (most-similar chunk) (maximum others :key #'score)))))

(defun pair-chunks (original edited)
  (setf (pair original)
        (setf (pair edited)
              (make-instance 'pair :original original :edited edited))))

(defun unpair-asymmetrical (chunks original-p)
  (loop for chunk in chunks when (not (symmetrical-p chunk)) do (unpair chunk original-p)))

(defun unpair-short (chunks original-p)
  "Unpair very short chunks."
  (loop for chunk in chunks when (< (length (textified chunk)) *short-length*)
     do (unpair chunk original-p)))

(defun unpair (chunk original-p)
  (let ((empty (make-empty-chunk)))
    (setf (most-similar chunk) empty)
    (if original-p
        (pair-chunks chunk empty)
        (pair-chunks empty chunk))))
                                  
(defun diff-pair (pair)
  (diff-textified (textified (original pair)) (textified (edited pair))))

(defun diff-to-markup (original edited)
  (clean-adds-and-deletes (mark-moves (diff-to-markup/no-moves original edited))))

(defun diff-to-markup/no-moves (original-parsed edited-parsed)
  (let ((original (paragraphs original-parsed))
        (edited (paragraphs edited-parsed)))
    (establish-pairs original edited)
    (loop for (label . pair) across (diff-vectors (as-pairs original) (as-pairs edited))
       for diff = (clean-empties (diff-pair pair))
       nconc
       (cond
         ((and (eql label :add) (not (empty-chunk-p (original pair))))
          `((:add ,@diff)))
         ((and (eql label :delete) (not (empty-chunk-p (edited pair))))
          `((:delete ,@diff)))
         (t diff)))))

(defun mark-moves (markup)
  "Take the output of diff-to-markup/no-moves and find the adds and
deletes which are actually moves."
  (let ((chunks (make-hash-table)))
    (flet ((chunkify (thing)
             (setf (gethash thing chunks) (make-chunk (rest thing)))))
      (let ((adds (mapcar #'chunkify (extract markup :add)))
            (deletes (mapcar #'chunkify (extract markup :delete))))
        (establish-pairs deletes adds :unpair-short t)

        ;; Walk the tree. For each :delete find the chunk. If chunk is
        ;; unpaired then leave the :delete as is. Otherwise replace
        ;; :delete with :moved-away containing the diff of the :delete
        ;; and the :add (original and edited chunks of the pair.) For
        ;; each :add similarly, if the chunk is unpaired, leave as is,
        ;; otherwise replace with :moved-to containing diff of the
        ;; :delete and :add. Also leave things as they are if the new
        ;; diff is actually more complex than the originals.
        (flet ((rewrite (x)
                 (let ((label (car x)))
                   (case label
                     ((:add :delete)
                      (let* ((chunk (gethash x chunks))
                             (pair (pair chunk))
                             (new-diff (diff-pair pair))
                             (other (if (eql label :add) (original pair) (edited pair)))
                             (new-diff-length (length new-diff))
                             (old-diff-length (length (textified chunk)))
                             (other-old-diff-length (length (textified other)))
                             (move-label (if (eql label :add) :moved-to :moved-from))
                             (more-complex (and (> new-diff-length old-diff-length) (> new-diff-length other-old-diff-length))))
                        (cond
                          ((and (symmetrical-p chunk) (not more-complex))
                           `(,move-label ,@(detextify-markup new-diff)))
                          (t `(,label ,@(rest x))))))
                     (t x)))))
          (map-tree #'rewrite markup))))))

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
  (clean-adds-and-deletes (clean-empties diff)))

(defun clean-adds-and-deletes (diff)
  (mapcar #'rewrite-adds-and-deletes diff))

(defun clean-empties (diff)
  (remove nil (mapcar #'remove-empties (detextify-markup diff))))

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