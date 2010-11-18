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

(defun diff-to-markup (original-file edited-file)
  (let ((original (paragraphs original-file))
        (edited (paragraphs edited-file)))
    (establish-pairs original edited)
    (loop for x across (diff-vectors (as-pairs original) (as-pairs edited))
       for pair = (cdr x)
       for diff = (diff-pair pair)
       nconc (cleaned-diff-output diff))))

(defun cleaned-diff-output (diff)
  (mapcar #'rewrite-adds-and-deletes
          (remove nil
                  (mapcar #'remove-empties (detextify-markup diff)))))

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

(defun possible-split (p)
  (let ((original (most-similar p)))
    (when (symmetrical-p original)
      (let* ((edited (most-similar original))
             (current-simmilarity (chunk-similarity original edited))
             (new-similarity-before (similarity (textified original)
                                                (concatenate 'vector
                                                             (textified p)
                                                             (textified edited))))
             (new-similarity-after
              (similarity (textified original)
                          (concatenate 'vector
                                       (textified edited)
                                       (textified p)))))

        (format t "~&POSSIBLE SPLIT?")
        (format t "~&Original/edited similarity: ~f" current-simmilarity)
        (format t "~&Combined similarity before: ~f; after: ~f"
                new-similarity-before
                new-similarity-after)))))
                                  
(defun find-most-similar (p others)
  (loop with max = -1
     with best = nil
     for o in others
     for similarity = (similarity (textified p) (textified o)) do
       (when (> similarity max)
         (setf max similarity)
         (setf best o))
     finally (setf (most-similar p) best)))

(defun show-pairings (original-file edited-file)
  (let ((original (paragraphs original-file))
        (edited (paragraphs edited-file)))
    (establish-pairs original edited)
    (let ((*print-right-margin* nil)
          (*print-pretty* nil))
      (loop for (label . pair) across (diff-vectors (as-pairs original) (as-pairs edited)) 
         for diff = (diff-pair pair)
         do
           (format t "~2&~a: original: ~s~&edited: ~s" label (textified (original pair)) (textified (edited pair)))
           (format t "~&diff: ~s~&~s" diff (cleaned-diff-output diff))))))

(defun show-similarities (original-file edited-file)
  (let ((original (paragraphs original-file))
        (edited  (paragraphs edited-file)))

    (set-most-similar original edited)

    (let ((identical (remove-if-not #'identical-p edited))
          (symmetrical (remove-if-not #'symmetrical-p edited)))

      (format t "~&~:d original paragraph~:p" (length original))
      (format t "~&~:d edited paragraph~:p" (length edited))
      (format t "~&~:d identical paragraph~:p" (length identical))
      (format t "~&~:d symmetrical paragraphs." (length symmetrical))
      
      (format t "~2&EDITED:")
      (loop for p in edited
           for o = (most-similar p) do
           (format t "~2&original: ~s~&edited: ~s~&similarity: ~f (~f one-way); identical: ~a; symmetrical: ~a"
                   (markup o)
                   (markup p)
                   (chunk-similarity p o)
                   (one-way-similarity (textified p) (textified o))
                   (identical-p p)
                   (symmetrical-p p)))

      (format t "~2&ORIGINAL:")
      (loop for p in original
           for o = (most-similar p) do
           (format t "~2&original: ~s~&edited: ~s~&similarity: ~f (~f one-way); identical: ~a; symmetrical: ~a"
                   (markup o)
                   (markup p)
                   (chunk-similarity p o)
                   (one-way-similarity (textified p) (textified o))
                   (identical-p p)
                   (symmetrical-p p))))))

(defun block-element-p (x)
  (member x 
          '(:body :colgroup :div :dl :fieldset :form :head :html :map :noscript
            :object :ol :optgroup :pre :script :select :style :table :tbody
            :tfoot :thead :tr :ul
            :area :base :blockquote :br :button :caption :col :dd :div :dt :h1
            :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
            :td :textarea :th :title)))

(defun diff-to-html (original edited)
  (with-output-to-file (out "test-diff.html")
    (let ((com.gigamonkeys.markup3.html::*tag-mappings* com.gigamonkeys.markup3.html::*tag-mappings*))
      (push '(:add . wrap-add-delete) com.gigamonkeys.markup3.html::*tag-mappings*)
      (push '(:del . wrap-add-delete) com.gigamonkeys.markup3.html::*tag-mappings*)
      (com.gigamonkeys.markup3.html::render-sexp
       (cons :body (diff-to-markup original edited)) out :stylesheet "diff.css"))))

(defun wrap-add-delete (sexp)
  (destructuring-bind (which &rest wrapped) sexp
    (let ((class (ecase which (:add "added") (:del "deleted"))))
      (cond
        ((and (consp (first wrapped)) (block-element-p (car (first wrapped))))
         `((:div :class ,class) ,@wrapped))
        (t `((:span :class ,class) ,@wrapped))))))




(defun diff-pair (pair)
  (clean-diff-vector
   (diff-textified-vectors (textified (original pair)) (textified (edited pair)))))


(defun as-pairs (chunks)
  (map 'vector #'pair chunks))