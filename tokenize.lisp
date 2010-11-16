(in-package :com.gigamonkeys.prose-diff)

;;; Tokenize text into a vector of symbols. Each word becomes a single
;;; symbol as does each punctuation character. A series of whitespace
;;; other than newlines 

(defparameter *just-word-scanner* (cl-ppcre:create-scanner "(\\w+)"))
(defparameter *everything-scanner* (cl-ppcre:create-scanner "(\\w+|\\s+|[\\W\\S])"))

(defun linearize-markup (markup include-whitespace)
  "Convert a Markup sexp into a vector of symbols with paired open and
  close markers surrounding tagged sections and symbols for each chunk
  of text."
  (coerce (linearize-markup/list markup include-whitespace) 'vector))

(defun linearize-markup/list (markup all-text)
  (cond
    ((consp markup)
     (destructuring-bind (tag &rest body) markup
       (append (list (open-marker tag))
               (mapcan #'(lambda (x) (linearize-markup/list x all-text)) body)
               (list (close-marker tag)))))
    ((stringp markup)
     (mapcar (lambda (x) (intern x :keyword)) (tokenize-text markup all-text)))
    (t (list markup))))

#+(or)(defun tokenize-text (text include-whitespace)
  (if include-whitespace
      (cl-ppcre:split "(\\s+)" text :with-registers-p t)
      (cl-ppcre:split "\\s+" text)))

(defun tokenize-text (text all-text)
  (cl-ppcre:all-matches-as-strings 
   (if all-text *everything-scanner* *just-word-scanner*)
   text))

(defun delinearize-markup (v)
  (first (%delinearize-markup v 0 (length v))))

(defun %delinearize-markup (v start end)
  (let ((result ()))
    (loop with i = start
       while (< i end)
       do
         (multiple-value-bind (what next-idx)
             (cond
               ((open-marker-p (aref v i))
                (delinearize-markup/cons v i))
               (t
                (delinearize-markup/text v i end)))
           (push what result)
           (setf i next-idx)))
    (nreverse result)))
    
(defun delinearize-markup/cons (v start)
  (let* ((tag (marker-tag (aref v start)))
         (end (position (close-marker tag) v)))
    (unless end
      (error "No closing tag for ~a at ~d" tag start))
    (values
     (cons tag (%delinearize-markup v (1+ start) end))
     (1+ end))))

(defun delinearize-markup/text (v start end)
  (let ((end (or (position-if #'open-marker-p v :start start :end end) end)))
    (values
     (with-output-to-string (s)
       (loop for i from start below end do (princ (aref v i) s)))
     end)))
         

(defpackage :open-markers (:use))
(defpackage :close-markers (:use))

(defun open-marker (tag) (intern (symbol-name tag) :open-markers))

(defun close-marker (tag) (intern (symbol-name tag) :close-markers))

(defun marker-tag (marker) (values (intern (symbol-name  marker) :keyword)))

(defun marker-p (x)
  (and (symbolp x)
       (let ((p (symbol-package x)))
         (or (eql p (find-package :open-markers))
             (eql p (find-package :close-markers))))))

(defun closes-p (open x)
  (and (close-marker-p x) (eql (marker-tag x) (marker-tag open))))

(defun open-marker-p (x)
  (and (marker-p x) (eql (symbol-package x) (find-package :open-markers))))

(defun close-marker-p (x)
  (and (marker-p x) (eql (symbol-package x) (find-package :close-markers))))


(defun paragraph-vectors (file)
  (let ((paragraphs (rest (parse-file file))))
    (mapcar (lambda (x) (coerce (linearize-markup x nil) 'vector)) paragraphs)))


;;; 1. Vectorize both documents.

;;; 2. Remove all the paragraphs that exist in both documents.

;;; 3. For each paragraph only in edited, find most similar paragraph
;;; only in original. This may result in multiple edited paragraphs
;;; being similar to the same original paragraph. For instance,
;;; suppose a paragraph was added in the edited version. It will be
;;; most similar to some original paragraph that doesn't appear in
;;; edited. But assuming that paragraph wasn't deleted, it should be
;;; more similar to its edited version. So after finding the most
;;; similar original paragraph we want to sort by the average
;;; similarity and then walk through the list 

;;; 4. Sort by average |LCS|/|paragraph|. 

;;; Paragraphs in both (perfect matches)
;;; Symmetrical matches
;;; Paragraphs 


;;; If there are multiple original paragraphs with the same best
;;; matching edited paragraph and they all have a high one-way
;;; lcs-length to length ratio, they were likely joined to create the
;;; edited paragraph.

;;; If there are multiple edited paragraphs with the same best
;;; matching original paragraph and they all have a high one-way
;;; lcs-length to length ratio, they were likely created by splitting
;;; the original paragraph.

;;; In the case of a join, we want to recognize that most of the text
;;; of the joined paragraph actually occurs in the original version.
;;; So we can take one of the original paragraphs and find the LCS
;;; with the new, joined paragraph. Then temporarily remove the text
;;; covered by the LCS



(defun similarity-table (original edited)
  (let* ((original-ps (paragraph-vectors original))
         (edited-ps  (paragraph-vectors edited))
         (in-both (intersection original-ps edited-ps :test #'equalp))
         (new-original-ps (set-difference original-ps in-both :test #'equalp))
         (new-edited-ps (set-difference edited-ps in-both :test #'equalp))
         (best-match (best-match-table new-edited-ps new-original-ps))
         (symmetrical (symmetrical-best-matches edited-ps best-match))
         (rsymmetrical (loop for p in symmetrical collect (gethash p best-match)))
         (left-over-edited (set-difference new-edited-ps symmetrical))
         (left-over-original (set-difference new-original-ps rsymmetrical)))

    (format t "~&~:d original paragraph~:p" (length original-ps))
    (format t "~&~:d edited paragraph~:p" (length edited-ps))
    (format t "~&~:d identical paragraph~:p" (length in-both))
    (format t "~&~:d paragraph~:p only in ~a" (length new-original-ps) original)
    (format t "~&~:d paragraph~:p only in ~a" (length new-edited-ps) edited)
    (format t "~&~:d symmetrical best matching paragraph~:p" (length symmetrical))
    (format t "~&~:d left-over edited paragraph~:p" (length left-over-edited))
    (format t "~&~:d left-over original paragraph~:p" (length left-over-original))

    (flet ((sorted-ps (ps x)
             (list x
                   (sort (loop for p in ps
                            for best = (gethash p best-match)
                            collect (list (delinearize-markup p)
                                          (delinearize-markup best)
                                          (similarity p best)
                                          (one-way-similarity p best)
                                          (one-way-similarity best p)
                                          (lcs p best)
                                          x))
                         #'> :key #'third))))
             
    
      `(
        ;; Paragraphs that occur in identical form in both versions
        (:both ,in-both)

        ;; Paragraphs who are each others best match according to SIMILARITY
        ,(sorted-ps symmetrical :symmetrical)

        ;; Paragraphs from original not in either of the two
        ;; categories above. These are likely paragraphs that have
        ;; been completely deleted in the edited version.
        ,(sorted-ps left-over-original :left-over-original)

        ;; Paragraphs from the edited version that are not in either
        ;; :both or :symmetrical. These are likely new paragraphs that
        ;; have been added during editing.
        ,(sorted-ps left-over-edited :left-over-edited)

        (:best-matches
         ,(sorted-ps new-edited-ps :edited-best-matches)
         ,(sorted-ps new-original-ps :original-best-matches)
        )))))


(defun similarity (a b)
  (let ((lcs-length (lcs-length a b)))
    (/ (+ (/ lcs-length (length a)) (/ lcs-length (length b))) 2.0d0)))

(defun one-way-similarity (a b)
  (float (/ (lcs-length a b) (length a))))


(defun best-match-table (as bs)
  (let ((table (make-hash-table :test #'equalp)))
    (loop for p in as do (setf (gethash p table) (most-similar p bs)))
    (loop for p in bs do (setf (gethash p table) (most-similar p as)))
    table))

(defun symmetrical-best-matches (ps matches)
  (loop for p in ps when (equalp (gethash (gethash p matches) matches) p) collect p))


(defun foo (orig edited output)
  (values
   (with-output-to-file (out (format nil "~a.sexp" output))
     (print (similarity-table orig edited) out))
   #+(or)(with-output-to-file (out (format nil "~a-reversed.sexp" output))
     (print (similarity-table edited orig) out))))

(defun most-similar (p others)
  (loop with max = -1
     with best = nil
     for o in others
     for lcs = (lcs-length p o) do
       (when (> lcs max)
         (setf max lcs)
         (setf best o))
     finally (return best)))




;;; OLD
(defun delinearize-to-text (v)
  (with-output-to-string (s)
    (loop for x across v do
         (cond
           ((open-marker-p x) 
            (format s "\\~(~a~){" (marker-tag x)))
           ((close-marker-p x)
            (format s "}"))
           (t (format s "~a" x))))))