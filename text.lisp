(in-package :com.gigamonkeys.prose-diff)

(defparameter *interned-text* (make-hash-table :test #'equal))

(defclass propertied-text ()
  ((text :initarg :text :accessor text)
   (properties :initarg :properties :accessor properties)))

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

(defmethod print-object ((object propertied-text) stream)
  (when *print-readably*
    (error 'print-not-readable object))
  (format stream "\"~a\"~@[[~{~(~a~)~^:~}]~]" (text object) (properties object)))

(defun textify-markup (markup all-text)
  "Convert a Markup sexp into a vector of interned text objects."
  (coerce (%textify-markup markup () all-text) 'vector))

(defun detextify-markup (v)
  (or (first (%detextify-markup v 0 (length v) ())) ""))

(defgeneric %textify-markup (markup properties all-text))

(defmethod %textify-markup ((markup cons) properties all-text)
  (destructuring-bind (tag &rest body) markup
    (let ((props (cons tag properties))
          (results ()))
      (loop for element in body do 
           (loop for x in (%textify-markup element props all-text) do (push x results)))
      ;;; This next line is a bit of a kludge to keep adjacent
      ;;; elements with the same tag from merging. Potentially it
      ;;; could be construed as a feature if certain elements would
      ;;; merge (i.e. if you have (:i "foo") (:i "bar") that should
      ;;; perhaps turn into (:i "foobar")
      (push (intern-text "" properties) results)
      (nreverse results))))

(defmethod %textify-markup ((markup string) properties all-text)
  (mapcar (lambda (tok) (intern-text tok properties)) (tokenize-text markup all-text)))

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

(defun take (list n)
  (let ((tail (nthcdr n list)))
    (values (ldiff list tail) tail)))

(defun new-open-p (text-props open-props)
  (equal (last text-props (length open-props)) open-props))

(defun unopen (text-props open-props)
  (multiple-value-bind (unopen open) 
      (take text-props (- (length text-props) (length open-props)))
    (assert (equal open-props open) () 
            "Remaining props ~s not equal to open props ~s" open open-props)
    unopen))

(defun split-list (list tail)
  (let ((rest (nthcdr (or (search tail list) (error "~a not found in ~a" tail list)) list)))
    (values (ldiff list rest) rest)))

(defun tag (tags rest)
  (cond
    ((null tags) rest)
    ((null (cdr tags)) (cons (car tags) rest))
    (t (list (car tags) (tag (cdr tags) rest)))))
    
            

(defun longer (list-a list-b)
  (cond
    ((endp list-b)
     (not (endp list-a)))
    ((endp list-a)
     nil)
    (t (longer (cdr list-a) (cdr list-b)))))
