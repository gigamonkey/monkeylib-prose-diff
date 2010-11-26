(in-package :com.gigamonkeys.prose-diff)

(defvar *interned-text* (make-hash-table :test #'equal))

(defparameter *token-scanner* (cl-ppcre:create-scanner "(\\w+|\\s+|[\\W\\S])"))

(defclass propertied-text ()
  ((text :initarg :text :accessor text)
   (properties :initarg :properties :accessor properties)))

(defun clear-interned-text ()
  (setf *interned-text* (make-hash-table :test #'equal)))

(defun intern-text (text &optional properties)
  (let ((key (cons text properties)))
    (let ((existing (gethash key *interned-text*)))
      (or
       existing
       (setf
        (gethash key *interned-text*)
        (make-instance 'propertied-text
          :text text 
          :properties properties))))))

(defun add-property (text new-prop)
  (intern-text (text text) (cons new-prop (properties text))))

(defun remove-properties (text props)
  (intern-text (text text) (set-difference (properties text) props)))

(defun has-property-p (prop text)
  (member prop (properties text)))

(defmethod print-object ((object propertied-text) stream)
  (when *print-readably*
    (error 'print-not-readable object))
  (format stream "\"~a\"~@[[~{~(~a~)~^:~}]~]" (text object) (properties object)))

(defun textify-markup (markup-list)
  "Convert a list of Markup sexps into a vector of interned
propertied-text objects."
  (let ((v (make-array 10 :adjustable t :fill-pointer 0)))
    (loop for markup in markup-list do (vector-push-extend* (%textify-markup markup ()) v))
    v))

(defun detextify-markup (v)
  "Convert a vector of propertied-text object (such as produced by
textify-markup into a list of Markup sexps."
  (values (%detextify-markup v 0 (length v) ())))

(defgeneric %textify-markup (markup properties))

(defmethod %textify-markup ((markup cons) properties)
  (destructuring-bind (tag &rest body) markup
    (let ((props (cons tag properties))
          (results ()))
      (loop for element in body do 
           (loop for x in (%textify-markup element props) do (push x results)))
      ;;; This next line is a bit of a kludge to keep adjacent
      ;;; elements with the same tag, e.g. adjacent :P's, from
      ;;; merging.
      (push (intern-text "" properties) results)
      (nreverse results))))

(defmethod %textify-markup ((markup string) properties)
  (mapcar (lambda (tok) (intern-text tok properties)) (tokenize-text markup)))

(defun tokenize-text (text)
  "Split a text string into a list of tokens containing either all the
text split into words, whitespace, and punctuation or just the words."
  (cl-ppcre:all-matches-as-strings *token-scanner* text))

(defun %detextify-markup (v start end open-props)
  (assert (not (null start)))
  (let ((result ())
        (i start))
    (with-output-to-string (s)
      (flet ((save-text ()
               (let ((str (get-output-stream-string s)))
                 (when (plusp (length str)) (push str result)))))
        (loop while (< i end) do
             (let* ((text (aref v i))
                    (text-props (properties text)))
               (cond
                 ((equal text-props open-props)
                  (write-string (text text) s)
                  (incf i))

                 ((new-open-p text-props open-props)
                  (save-text)
                  (let* ((unopen (unopen text-props open-props))
                         (tag (first (last unopen)))
                         (now-open (cons tag open-props)))
                    (multiple-value-bind (thing idx) (%detextify-markup v i end now-open)
                      (push (cons tag thing) result)
                      (setf i idx))))
                 (t (decf i)
                    (save-text)
                    (loop-finish)))))
        (save-text)))
    (values (nreverse result) (1+ i))))


(defun new-open-p (text-props open-props)
  (equal (last text-props (length open-props)) open-props))

(defun unopen (text-props open-props)
  (multiple-value-bind (unopen open) 
      (take text-props (- (length text-props) (length open-props)))
    (assert (equal open-props open) () 
            "Remaining props ~s not equal to open props ~s" open open-props)
    unopen))

;; used?
(defun tag (tags rest)
  (cond
    ((null tags) rest)
    ((null (cdr tags)) (cons (car tags) rest))
    (t (list (car tags) (tag (cdr tags) rest)))))
    

