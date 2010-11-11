;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.prose-diff)

(defparameter *word-regexp* (create-scanner "((?:\\\\n{.*?})|(?:\\w*’\\w+)|(?:\\w+))"))
(defparameter *old-mode-regexp* (create-scanner "-\\*- mode: .*; -\\*-"))
(defparameter *new-mode-line* "-*- mode: coders-at-work-editing; -*-")
(defparameter *minimum-match* 4)

(defun text (file)
  (with-output-to-string (s)
    (with-open-file (in file)
      (loop for line = (read-line in nil nil)
	 while line do (write-line line s)))))

(defun words (text)
  (let (words)
    (do-matches (start end *word-regexp* text (coerce (nreverse words) 'vector))
      (push (intern (string-upcase (subseq text start end)) :keyword) words))))


(defun word-starts (text)
  (let (positions)
    (do-matches (start end *word-regexp* text (coerce (nreverse positions) 'vector))
      (push start positions))))

(defun positions-table (words)
  (let ((table (make-hash-table)))
    (loop for position from (1- (length words)) downto 0
       do (push position (gethash (aref words position) table ())))
    table))

(defun dump-table (table)
  (loop for k being the hash-keys of table
       do (format t "~a => ~a~%" k (gethash k table))))

;; XXX -- the policy of removing the first match if there are ties (by
;; length) is not necessarily quite right since you could have
;; something like: 'a b c d a b c e' in the original and then in the
;; edited: 'a b c x a b c d' in which case the first match attempt on
;; 'a' would find the two 'a b c' sequences and then would take the
;; first one leaving '- - - d a b c e'. Then when matching starting at
;; the second 'a' in the edited text the 'a b c d' has been broken up
;; so it will only match 'a b c' leaving an extra 'e'. Don't know how
;; much this will matter in practice. (Hmmm, maybe could do something
;; like: for each occurence of word W in edited find the longest match
;; in original. Then assign the longest overall to which ever
;; occurence of W it goes with and then axe any matches that overlap
;; with it and assign the longest remaining to it's W and so on. Then
;; move to the next word in edited.)
;;
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

(defun find-original-text (original-text edited-text)
  "Given `original-text' and `edited-text' return two word vectors
  representing the given texts with the sequences of words occuring in
  both texts replaced by nil. Thus the sequences of words that remain
  in original are the ones not used in the edited version and the
  words that remain in edited are the ones added during editing."
  (loop
     with original        = (words original-text)
     with edited          = (words edited-text)
     with positions-table = (positions-table original)
     with edited-idx      = 0

     for word = (aref edited edited-idx)
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
		    ((> longest-match *minimum-match*)
		     (null-words original (first longest-match-starts) longest-match)
		     (null-words edited edited-idx longest-match)
		     (incf edited-idx longest-match))
		    (t (incf edited-idx))))
       
     while (< edited-idx (length edited))
     finally (return (values original edited))))

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

(defun show-cuts (&key master edited output)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (let ((text (text master)))
      (flet ((make-emitter (label)
	       (lambda (start end)
		 (let ((text (subseq text start end)))
		   (when (zerop start)
		     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
		   (format out "{{~a}}~a" label text)))))
	(let ((words (find-original-text text (text edited))))
	  (map-in-and-out words (word-starts text) (make-emitter "toss") (make-emitter "keep")))))))

(defun show-leftovers (&key master edited output)
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((text (text master)))
      (flet ((make-emitter (label)
	       (lambda (start end)
		 (let ((text (subseq text start end)))
		   (when (zerop start)
		     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
		   (format out "{{~a}}~a" label text)))))
	(let ((words (find-original-text text (text edited))))
	  (map-in-and-out words (word-starts text) (make-emitter "keep") (make-emitter "book")))))))

(defun show-unused (&key master edited output)
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (text master))
	  (edited-text (text edited)))
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
    (let ((master-text (text master))
	  (edited-text (text edited)))
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
    (let ((master-text (text master))
	  (edited-text (text edited)))
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
    (let ((master-text (text master))
	  (edited-text (text edited)))
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
    (let ((text (text edited)))
      (flet ((make-emitter (label)
	       (lambda (start end)
		 (let ((text (subseq text start end)))
		   (when (zerop start)
		     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
		   (format out "{{~a}}~a" label text)))))
	(let ((words (nth-value 1 (find-original-text (text master) text))))
	  (map-in-and-out words (word-starts text) (make-emitter "book") (make-emitter "keep")))))))
  

(defun match-length (original edited o-start e-start longest-match)
  (flet ((same (i)
	   (let ((e (+ e-start i))
		 (o (+ o-start i)))
	     (and (< e (length edited))
		  (< o (length original))
		  (eql (aref edited e) (aref original o))))))
    ;; Check the zeroth element in case the original got nulled out
    ;; due to a previous match. Then check that this match could at
    ;; least in theory be as long as the previous longest match, i.e.
    ;; the last element matches, before we go to the bother of
    ;; checking all the intervening elements.
    (if (and (same 0) (or (zerop longest-match) (same (1- longest-match))))
	(loop for i from 1 when (not (same i)) return i)
	0)))
	   
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

