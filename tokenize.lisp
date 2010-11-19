(in-package :com.gigamonkeys.prose-diff)

;;; Tokenize text into a vector of symbols. Each word becomes a single
;;; symbol as does each punctuation character. A series of whitespace
;;; other than newlines 

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

(defun chunk-similarity (a b)
  (similarity (textified a) (textified b)))





(defun establish-pairs (original edited)
  (set-most-similar original edited)
  (pair-symmetrical original)
  (repair-asymmetrical original t)
  (repair-asymmetrical edited nil))

(defun set-most-similar (original edited)
  "Make each chunk in original and edited point to the most similar
chunk on the other side."
  (loop for p in edited do (find-most-similar p original))
  (loop for p in original do (find-most-similar p edited)))

(defun find-most-similar (p others)
  (loop with max = -1
     with best = nil
     for o in others
     for similarity = (similarity (textified p) (textified o)) do
       (when (> similarity max)
         (setf max similarity)
         (setf best o))
     finally (setf (most-similar p) best)))

(defun pair-symmetrical (original)
  (loop for chunk in original
     when (symmetrical-p chunk) do
       (pair-chunks chunk (most-similar chunk))))

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
                                  
;;;
;;; HTML generation -- given two Markup files generate an HTML file of the diff.
;;;

(defun diff-to-html (original edited output)
  (with-output-to-file (out output)
    (let ((com.gigamonkeys.markup3.html::*tag-mappings* com.gigamonkeys.markup3.html::*tag-mappings*))
      (push '(:add . wrap-add-delete) com.gigamonkeys.markup3.html::*tag-mappings*)
      (push '(:delete . wrap-add-delete) com.gigamonkeys.markup3.html::*tag-mappings*)
      (com.gigamonkeys.markup3.html::render-sexp
       (cons :body (diff-to-markup original edited)) out :stylesheet "diff.css"))))

(defun block-element-p (x)
  (member x 
          '(:body :colgroup :div :dl :fieldset :form :head :html :map :noscript
            :object :ol :optgroup :pre :script :select :style :table :tbody
            :tfoot :thead :tr :ul
            :area :base :blockquote :br :button :caption :col :dd :div :dt :h1
            :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
            :td :textarea :th :title)))

(defun wrap-add-delete (sexp)
  (destructuring-bind (which &rest wrapped) sexp
    (let ((class (format nil "~(~aed~)" which)))
      (cond
        ((and (consp (first wrapped)) (block-element-p (car (first wrapped))))
         `((:div :class ,class) ,@wrapped))
        (t `((:span :class ,class) ,@wrapped))))))



(defun diff-to-markup (original-file edited-file)
  (let ((original (paragraphs original-file))
        (edited (paragraphs edited-file)))
    (establish-pairs original edited)
    (loop for x across (diff-vectors (as-pairs original) (as-pairs edited))
       for pair = (cdr x)
       for diff = (diff-pair pair)
       nconc (cleaned-diff-output diff))))

(defun as-pairs (chunks) (map 'vector #'pair chunks))

(defun diff-pair (pair)
  (clean-diff-vector
   (diff-textified-vectors (textified (original pair)) (textified (edited pair)))))

(defun cleaned-diff-output (diff)
  (mapcar #'rewrite-adds-and-deletes
          (remove nil
                  (mapcar #'remove-empties (detextify-markup diff)))))

(defun rewrite-adds-and-deletes (tree)
  "Rewrite the Markup trees so that :add and :delete tags "
  (cond
    ((consp tree)
     (let ((add-or-delete (has-nested-add-or-delete-p tree)))
       (when add-or-delete
         (setf tree (promote-tag add-or-delete tree)))
       (mapcar #'rewrite-adds-and-deletes tree)))
    (t tree)))

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

(defun remove-empties (tree)
  (labels ((helper (x)
             (cond
               ((and (consp x) (null (rest x))) nil)
               ((consp x) (list (mapcan #'helper x)))
               (t (list x)))))
    (first (helper tree))))

(defun has-nested-add-or-delete-p (tree)
  "Tree has a nested :add or :delete tag and it's not the only tag.
Returns which one it is since there should only be one or the other."
  (let ((tags (nested-tags tree)))
    (and (not (null (cdr tags)))
         (or (find :add tags) (find :delete tags)))))

(defun nested-tags (tree)
  (if (consp tree)
      (if (null (cddr tree))
          (cons (car tree) (nested-tags (second tree)))
          (list (car tree)))))