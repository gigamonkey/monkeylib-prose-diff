(in-package :com.gigamonkeys.prose-diff)

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