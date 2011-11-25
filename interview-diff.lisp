(in-package :com.gigamonkeys.prose-diff)

;;; This code provides a specialized form of diff that is useful when
;;; editing transcripts of interviews. The basic idea is that we have
;;; say, an original transcript of an interview with all the text in
;;; the order it was actually said, and an edited version of the
;;; interview in which things may have been moved around. We want to
;;; be able to determine roughly what text from the original has made
;;; it into the edited version and what has been left behind. Three
;;; views are useful: 1) a list of the chunks of text from the
;;; original that do not appear in the edited version, sorted by
;;; length. 2) an HTML rendering of the original with all the text
;;; that appears in the edited version rendered distinctively from the
;;; text that does not. 3) an HTML rendering of the edited version,
;;; with any text that does not come from the original (i.e. editorial
;;; additions) rendered distinctively.

(defparameter *word-regexp* (create-scanner "((?:\\\\n{.*?})|(?:\\w*’\\w+)|(?:\\w+))"))
(defparameter *old-mode-regexp* (create-scanner "-\\*- mode: .*; -\\*-"))
(defparameter *new-mode-line* "-*- mode: coders-at-work-editing; -*-")
(defparameter *minimum-match* 5)

(defun words (text)
  "Convert a string to a vector of keyword symbols representing the
  words in the text."
  (let (words)
    (do-matches (start end *word-regexp* text (coerce (nreverse words) 'vector))
      (push (intern (string-upcase (subseq text start end)) :keyword) words))))

(defun word-starts (text)
  (let (positions)
    (do-matches (start end *word-regexp* text (coerce (nreverse positions) 'vector))
      (push start positions))))

(defun positions-table (words)
  "Return a hash table mapping the words in a vector to their
  positions in the vector, with the lists of positions in order from
  lowest to highest."
  (let ((table (make-hash-table)))
    (loop for position from (1- (length words)) downto 0
       do (push position (gethash (aref words position) table ())))
    table))

(defun dump-table (table)
  (loop for k being the hash-keys of table
       do (format t "~a => ~a~%" k (gethash k table))))

;; Maybe for section that has ties, we should remember the position in
;; edited but leave the word vectors alone. Then after we've gone
;; through all of the edited words, go back and try the remembered
;; positions again. This will deal with the case where a short phrase
;; matches a number of places and the first one happens to be the
;; wrong one. If we don't take it out, then that instance of the short
;; phrase will hopefully be taken as part of some bigger piece of text
;; used later in the edited text. As it stands now, we'll take the
;; short phrase out of the middle of the longer text which will then
;; get broken into two bits surrounding the short phrase. (Which I
;; guess will then be taken from somewhere else.)

;;; Possible variant algorithm:

;;; For each unique word in edited find all the matches in original
;;; greater or equal in length to *minimum-match*. Each match will be
;;; represented by a triple of (original-index, edited-index, length).
;;; Sort all the matches by length. Then loop through the matches,
;;; nulling out the words corresponding to the match and removing all
;;; overlapping matches 

;;; If another match overlaps the just processed match, it can be
;;; reduced. If we just removed (o, e, l) and we're reducing (o', e',
;;; l') if o < o' < o+l then part of the front of the match being
;;; reduced has been chopped off so o' and e' have to be increased and
;;; l' decreased. And if o' < o < o'+l' then l' goes to o - o' and
;;; likewise if e' < e < e'+l'. Discard matches whose length is
;;; reduced to less than *minimum-match* and resort by the new
;;; lengths.

;;; This algorithm will ensure that shorter matches don't break up
;;; longer ones. But may be more expensive. Need to figure out whether
;;; it's worth it.


(defun interview-diff (original edited)
  "Given word vectors `original' and `edited' return them with strings
  of words occuring in both texts replaced by nil. Thus the words that
  remain in original are the ones not used in the edited version and
  the words that remain in edited are the ones added during editing.
  Note that this is quite different from finding the least common
  subsequence of the two word vectors because the strings can occur in
  different orders in the two vectors. For instance if the original
  was: 'a b c e f g h' and the edited was 'x y e f c a b', the result
  would be (with a *minimum-match* of 1) an original with only the g
  and h left and an edited with only the x and y since the 'a b' will
  match at the end, the 'c' will match by itself, and the 'e f' near
  the beginning"
  (loop
     with positions-table = (positions-table original)
     with edited-idx      = 0

     for word      = (aref edited edited-idx)
     for positions = (gethash word positions-table)

     do
       (loop
	  with longest-match = 0
	  with longest-match-starts = ()
            
	  for original-idx in positions
	  for match = (match-length original edited original-idx edited-idx longest-match)
            
	  do (when (>= match longest-match)
	       (when (> match longest-match)
		 (setf longest-match-starts ())
		 (setf longest-match match))
	       (push original-idx longest-match-starts))

	  finally (cond
		    ((>= longest-match *minimum-match*)
		     (null-words original (first longest-match-starts) longest-match)
		     (null-words edited edited-idx longest-match)
		     (incf edited-idx longest-match))
		    (t (incf edited-idx))))

     while (< edited-idx (length edited))
     finally (return (values original edited))))

(defun match-length (original edited o-start e-start longest-match)
  "Given two word vectors, `original' and `edited', a starting index
  into each, `o-start' and `e-start', as well as the length of the
  previous longest match, find the length of the matching run of words
  in the two vectors starting at the two indices or zero if we
  determine that the match can't be at least as long as longest-match,
  because the element at the end doesn't match. (This will quickly
  catch the case where some other match has already nulled out words
  somewhere beyond our start but close enough to keep us from getting
  long enough to be longer than longest-match.)"
  (flet ((same (i)
	   (let ((e (+ e-start i))
		 (o (+ o-start i)))
	     (and (< e (length edited))
		  (< o (length original))
		  (eql (aref edited e) (aref original o))))))
    (if (and (same 0) (or (zerop longest-match) (same (1- longest-match))))
	(loop for i from 1 when (not (same i)) return i)
	0)))

(defun null-words (words start count)
  (loop repeat count for i from start do (setf (aref words i) nil)))

(defun map-in-and-out (words word-starts in-fn out-fn)
  "Given a vector `words' with some words nulled out and a parallel
  vector `word-starts' containing the starting positions of the words
  in a text (not given), map over the chunks of text, calling `in-fn'
  for each chunk of words present in `words' and `out-fn' for each
  chunk of words missing from `words'. The functions are called with a
  bounding index designator of the underlying text."
  (loop with text-idx = 0
     with word-idx = 0
     while word-idx
     do (let ((word (aref words word-idx))
	      (end (aref word-starts word-idx)))
	  (unless (zerop word-idx)
	    (funcall (if word out-fn in-fn) text-idx end)
	    (setf text-idx end))
	  (setf word-idx (position nil words :start (1+ word-idx) :key (if word #'identity #'not))))
     finally (funcall (if (aref words (1- (length words))) in-fn out-fn) text-idx nil)))

(defun find-original-text (original-text edited-text)
  (interview-diff (words original-text) (words edited-text)))

(defun textified-to-words (textified)
  (loop for x across textified
     for i from 0
     for text = (text x)
     when (cl-ppcre:scan "\\w+" text) 
     collect (intern (string-upcase text) :keyword) into words and
     collect i into positions
     finally (return (values (coerce words 'vector) (coerce positions 'vector)))))

;;; Plan
;;;
;;; 1. Textify markup into vector of propertied-text objects.
;;;
;;; 2. Map propertied-text objects into words vector. (Skipping some
;;; of the text elements)
;;;
;;; 3. Map propertied-text words into positions in the
;;; propertied-text vector.
;;;
;;; 4. Do interview-diff on words vector.
;;;
;;; 5. Use map-in-and-out to map back from words/word-starts to
;;; propertied, text, applying :kept or :tossed properties the
;;; propertied-text words between start and end.
;;;
;;; 6. Retextify propertied-text.
;;;
;;; 7. Generate HTML.

(defun interview-diff/markup (original-file edited-file)
  "Given two markup files, convert to propertied text and feed into
  interview-diff/textified which will return the frobbed word
  vectors."
  (let ((o-text (textify-markup (rest (parse-file original-file :parse-links-p nil))))
        (e-text (textify-markup (rest (parse-file edited-file :parse-links-p nil)))))
    (interview-diff/textified o-text e-text)))


(defun show-original (original-file edited-file output)
  (multiple-value-bind (o-text e-text o-words e-words o-positions e-positions)
      (interview-diff/markup original-file edited-file)
    (declare (ignore e-text e-words e-positions))
    (map-in-and-out 
     o-words
     o-positions
     ;; Words only original.
     (lambda (start end)
       (loop for i from start below (or end (length o-text)) do
            (setf (aref o-text i) (add-property (aref o-text i) :unused))))
     ;; Words in both
     (lambda (start end)
       (declare (ignore start end))
       #+(or)(loop for i from start below (or end (length o-text)) do
            (setf (aref o-text i) (add-property (aref o-text i) :used)))))
    (with-output-to-file (out output)
      (render-sexps-to-stream
       (cons :body (detextify-markup o-text))
       out
       :stylesheets '("interview-diff.css")
       :rewriter (make-retagger '((:unused . (:span :class "unused"))
                                  (:n . (:span :class "name"))))))))
      

(defun interview-diff/textified (o-text e-text)
  (multiple-value-bind (o-words o-positions) (textified-to-words o-text)
    (multiple-value-bind (e-words e-positions) (textified-to-words e-text)
      (setf (values o-words e-words) (interview-diff o-words e-words))
      (values o-text e-text o-words e-words o-positions e-positions))))


  



(defun show-cuts (&key master edited output)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (let ((text (file-text master)))
      (flet ((make-emitter (label)
	       (lambda (start end)
		 (let ((text (subseq text start end)))
		   (when (zerop start)
		     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
		   (format out "{{~a}}~a" label text)))))
	(let ((words (find-original-text text (file-text edited))))
	  (map-in-and-out words (word-starts text) (make-emitter "toss") (make-emitter "keep")))))))

(defun show-leftovers (&key master edited output)
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((text (file-text master)))
      (flet ((make-emitter (label)
	       (lambda (start end)
		 (let ((text (subseq text start end)))
		   (when (zerop start)
		     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
		   (format out "{{~a}}~a" label text)))))
	(let ((words (find-original-text text (file-text edited))))
	  (map-in-and-out words (word-starts text) (make-emitter "keep") (make-emitter "book")))))))

(defun show-unused (&key master edited output)
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (file-text master))
	  (edited-text (file-text edited)))
      (flet ((make-emitter ()
	       (lambda (start end)
		 (let ((text (subseq master-text start end)))
		   (write-string text out))))
	     (make-elipsator ()
	       (lambda (start end)
		 (let ((text (subseq master-text start end)))
		   (if (find #\Newline text)
		       (format out "~2&§~2%")
		       (format out " … "))))))
	(let ((words (find-original-text master-text edited-text)))
	  (map-in-and-out words (word-starts master-text) (make-emitter) (make-elipsator)))))))

(defun show-possible-cuts (&key master edited output (minimum 0))
  "Emit all sections left in `master' after `edited' text has been
  removed, longer than `minimum' characters. Sections are output in
  the order they appear in `master'."
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (file-text master))
	  (edited-text (file-text edited)))
      (let ((chunks-found 0))
	(flet ((make-emitter ()
		 (lambda (start end)
		   (let ((text (subseq master-text start end)))
		     (when (> (length text) minimum)
		       (when (zerop start)
			 (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
		       (incf chunks-found)
		       (format t "~&Found chunk of ~:d characters." (length text))
		       (format out "~2&{{~a}}" text))))))
	  (let ((words (find-original-text master-text edited-text)))
	    (map-in-and-out words (word-starts master-text) (make-emitter) (constantly nil))))
	(format t "~&~:d chunks of more than ~:d characters found." chunks-found minimum)))))

(defun used-sections (&key master edited output (minimum 0))
  "Emit all sections from `edited' found in `master' in the order they
  appear in `master'."
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (file-text master))
	  (edited-text (file-text edited)))
      (let ((chunks-found 0))
	(flet ((make-emitter ()
		 (lambda (start end)
		   (let ((text (subseq master-text start end)))
		     (when (> (length text) minimum)
		       (when (zerop start)
			 (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
		       (incf chunks-found)
		       (format t "~&Found chunk of ~:d characters." (length text))
		       (format out "~2&{{~a}}" text))))))
	  (let ((words (find-original-text master-text edited-text)))
	    (map-in-and-out words (word-starts master-text) (constantly nil) (make-emitter))))
	(format t "~&~:d chunks of more than ~:d characters found." chunks-found minimum)))))

(defun show-sorted-cuts (&key master edited output)
  "Emit all sections left in `master' after `edited' text has been
  removed, sorted by size, longer sections first."
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (file-text master))
	  (edited-text (file-text edited)))
      (let ((words (find-original-text master-text edited-text))
	    (chunks ()))
	(flet ((collector (start end)
		 (let ((text (subseq master-text start end)))
		   (push (list (length text) text start end) chunks))))
	  (map-in-and-out words (word-starts master-text) #'collector (constantly nil)))
	(loop for (length text start end) in (sort chunks #'> :key #'first) do
	     (format out "~2&** ~:d characters (~:d-~:d)~2%" length start end)
	     (write-string text out))))))
      
  
(defun show-additions (&key master edited output)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (let ((text (file-text edited)))
      (flet ((make-emitter (label)
	       (lambda (start end)
		 (let ((text (subseq text start end)))
		   (when (zerop start)
		     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
		   (format out "{{~a}}~a" label text)))))
	(let ((words (nth-value 1 (find-original-text (file-text master) text))))
	  (map-in-and-out words (word-starts text) (make-emitter "book") (make-emitter "keep")))))))
  


	   
;;; Scrap

(defun words-with-text-positions (text)
  (let (words positions)
    (do-matches (start end *word-regexp* text
		       (values (coerce (nreverse words) 'vector)
			       (nreverse (cons (length text) positions))))
      (push (intern (string-upcase (subseq text start end)) :keyword) words)
      (push start positions))))

(defun reconstitute-text (text)
  (multiple-value-bind (words positions) (words-with-text-positions text)
    (declare (ignore words))
    (string= text
	     (with-output-to-string (s)
	       (loop for (start end) on (cons 0 positions )
		  while end
		  do (format s "~a" (subseq text start end)))))))



