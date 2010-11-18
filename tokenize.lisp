(in-package :com.gigamonkeys.prose-diff)

;;; Tokenize text into a vector of symbols. Each word becomes a single
;;; symbol as does each punctuation character. A series of whitespace
;;; other than newlines 

(defparameter *just-word-scanner* (cl-ppcre:create-scanner "(\\w+)"))
(defparameter *everything-scanner* (cl-ppcre:create-scanner "(\\w+|\\s+|[\\W\\S])"))

(defclass paragraph ()
  ((markup :initarg :markup :reader markup)
   (document :initarg :document :reader document)
   (index :initarg :index :reader index)
   (textified-all :initarg :textified-all :reader textified-all)
   (textified-no-ws :initarg :textified-no-ws :reader textified-no-ws)
   (most-similar :initform nil :accessor most-similar)))

(defun tokenize-text (text all-text)
  "Split a text string into a list of tokens containing either all the
text split into words, whitespace, and punctuation or just the words."
  (cl-ppcre:all-matches-as-strings 
   (if all-text *everything-scanner* *just-word-scanner*)
   text))

(defun paragraphs (file) 
  (loop for p in (rest (parse-file file))
       for i from 0
       collect (make-instance 'paragraph
                 :document file
                 :index 0
                 :markup p
                 :textified-all (textify-markup p t)
                 :textified-no-ws (textify-markup p nil))))

(defmethod print-object ((o paragraph) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~s identical: ~a; symmetric: ~a" (markup o) (identical-p o) (symmetrical-p o))))

(defun identical-p (p)
  (and (most-similar p)
       (equal (markup p) (markup (most-similar p)))))

(defun symmetrical-p (p)
  (and (most-similar p)
       (eql (most-similar (most-similar p)) p)))

(defun similarity/all (a b)
  (similarity (textified-all a) (textified-all b)))

(defun similarity/no-ws (a b)
  (similarity (textified-no-ws a) (textified-no-ws b)))

(defun similarity-table (original-file edited-file)
  (let ((original (paragraphs original-file))
        (edited  (paragraphs edited-file)))

    (loop for p in edited do (find-most-similar p original))
    (loop for p in original do (find-most-similar p edited))

    (let ((identical (remove-if-not #'identical-p edited))
          (symmetrical (remove-if-not #'symmetrical-p edited)))

      (format t "~&~:d original paragraph~:p" (length original))
      (format t "~&~:d edited paragraph~:p" (length edited))
      (format t "~&~:d identical paragraph~:p" (length identical))
      (format t "~&~:d symmetrical paragraphs." (length symmetrical))
      
      (loop for p in edited
           for o = (most-similar p) do
           (format t "~2&original: ~s~&edited: ~s~&similarity: ~f (~f one-way); identical: ~a; symmetrical: ~a"
                   (markup o)
                   (markup p)
                   (similarity/no-ws p o)
                   (one-way-similarity (textified-no-ws p) (textified-no-ws o))
                   (identical-p p)
                   (symmetrical-p p))
           (when (not (symmetrical-p p))
             (possible-split p))
))))

(defun possible-split (p)
  (let ((original (most-similar p)))
    (when (symmetrical-p original)
      (let* ((edited (most-similar original))
             (current-simmilarity (similarity/no-ws original edited))
             (new-similarity-before (similarity (textified-no-ws original)
                                                (concatenate 'vector
                                                             (textified-no-ws p)
                                                             (textified-no-ws edited))))
             (new-similarity-after
              (similarity (textified-no-ws original)
                          (concatenate 'vector
                                       (textified-no-ws edited)
                                       (textified-no-ws p)))))

        (format t "~&POSSIBLE SPLIT?")
        (format t "~&Original/edited similarity: ~f" current-simmilarity)
        (format t "~&Combined similarity before: ~f; after: ~f"
                new-similarity-before
                new-similarity-after)))))

(defun possible-join (p)
  (let ((original (most-similar p)))
    (when (symmetrical-p original)
      (let* ((edited (most-similar original))
             (current-simmilarity (similarity/no-ws original edited))
             (new-similarity-before (similarity (textified-no-ws original)
                                                (concatenate 'vector
                                                             (textified-no-ws p)
                                                             (textified-no-ws edited))))
             (new-similarity-after
              (similarity (textified-no-ws original)
                          (concatenate 'vector
                                       (textified-no-ws edited)
                                       (textified-no-ws p)))))

        (format t "~&POSSIBLE SPLIT?")
        (format t "~&Original/edited similarity: ~f" current-simmilarity)
        (format t "~&Combined similarity before: ~f; after: ~f"
                new-similarity-before
                new-similarity-after)))))
                                  
        
          



(defun in-both (old-paragraphs new-paragraphs)
  (intersection old-paragraphs new-paragraphs :test #'paragraph-equal))

(defun paragraph-equal (a b)
  (equal (markup a) (markup b)))

(defun similarity (a b)
  (let ((lcs-length (lcs-length a b)))
    (/ (+ (/ lcs-length (length a)) (/ lcs-length (length b))) 2.0d0)))

(defun one-way-similarity (a b)
  (float (/ (lcs-length a b) (length a))))

(defun find-most-similar (p others)
  (loop with max = -1
     with best = nil
     for o in others
     for lcs = (lcs-length (textified-no-ws p) (textified-no-ws o)) do
       (when (> lcs max)
         (setf max lcs)
         (setf best o))
     finally (setf (most-similar p) best)))


(defun foo (orig edited output)
  (values
   (with-output-to-file (out (format nil "~a.sexp" output))
     (print (similarity-table orig edited) out))
   #+(or)(with-output-to-file (out (format nil "~a-reversed.sexp" output))
     (print (similarity-table edited orig) out))))

