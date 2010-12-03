(in-package :com.gigamonkeys.prose-diff)

;;;
;;; Code for splitting chunks into parts corresponding to a bunch of
;;; other chunks derived from them. This could, in theory, be used to
;;; further refine diffs and recognize more moved text. At the moment,
;;; however, it is not used.
;;;

(defun split-positions (one-chunk parts)
  "Find the positions where one-chunk should be split to get pieces
  corresponding to the given parts, which are derived from one-chunk.
  For instance, if one-chunk is a piece of text that was split and the
  pieces inserted at various places in the new document, it will show
  up in the diff as a single deletion and multiple additions. Or,
  conversely, if a bunch of separate pieces (from different
  paragraphs) in the original document were combined into contiguous
  text in the edited document, we would have a single addition and
  multiple deletions."
  (multiple-value-bind (one-chunk-lcs-indices combined-lcs-indices) 
      (lcs-positions one-chunk (concatenate-vectors parts))
    (loop for s in (part-starts parts)
       for prev-x = 0 then x
       for x = (position-if (lambda (x) (>= x s)) combined-lcs-indices :start prev-x)
       collect (aref one-chunk-lcs-indices x))))

(defun split-vector (one-chunk parts)
  (loop for (start end) on (split-positions one-chunk parts) 
     collect (subseq one-chunk start end)))

(defun part-starts (parts)
  (loop for (p . rest) on parts
     summing (length p) into total
     when rest collect total into starts
     finally (return (cons 0 starts))))


