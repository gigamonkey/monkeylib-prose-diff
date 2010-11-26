(in-package :com.gigamonkeys.prose-diff)

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

;; For experimenting. Probably a dead end.
(defun diff-to-html/no-paragraphs (original edited output)
  (with-output-to-file (out output)
    (let ((com.gigamonkeys.markup3.html::*tag-mappings* com.gigamonkeys.markup3.html::*tag-mappings*))
      (push '(:add . wrap-add-delete) com.gigamonkeys.markup3.html::*tag-mappings*)
      (push '(:delete . wrap-add-delete) com.gigamonkeys.markup3.html::*tag-mappings*)
      (com.gigamonkeys.markup3.html::render-sexp
       (cons :body (diff-to-markup/no-paragraphs original edited)) out :stylesheet "diff.css"))))

(defun diff-to-markup/no-paragraphs (original-file edited-file)
  (let ((original (make-chunk (list (parse-file original-file))))
        (edited (make-chunk (list (parse-file edited-file)))))
    (cleaned-diff-output (diff-textified (textified original) (textified edited)))))

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
    (let ((class (string-downcase which)))
      (cond
        ((and (consp (first wrapped)) (block-element-p (car (first wrapped))))
         `((:div :class ,class) ,@wrapped))
        (t `((:span :class ,class) ,@wrapped))))))
