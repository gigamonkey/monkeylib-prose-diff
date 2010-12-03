(in-package :com.gigamonkeys.prose-diff)

;;;
;;; HTML generation -- given two Markup files generate an HTML file of the diff.
;;;

(defun diff-to-html (original edited output)
  (with-output-to-file (out output)
    (let ((com.gigamonkeys.markup.html::*tag-mappings* com.gigamonkeys.markup.html::*tag-mappings*))
      (push '(:add . wrap-add-delete) com.gigamonkeys.markup.html::*tag-mappings*)
      (push '(:delete . wrap-add-delete) com.gigamonkeys.markup.html::*tag-mappings*)
      (push '(:moved-to . wrap-add-delete) com.gigamonkeys.markup.html::*tag-mappings*)
      (push '(:moved-from . wrap-add-delete) com.gigamonkeys.markup.html::*tag-mappings*)
      (com.gigamonkeys.markup.html::render-sexps-to-stream 
       `(:body
         ,@(diff-to-markup original edited)
         ((:ul :id "buttons")
          (:li ((:button :id "show_diff") "Diff"))
          (:li ((:button :id "show_original") "Original"))
          (:li ((:button :id "show_new") "New"))))
       out :stylesheets '("diff.css") :scripts '("jquery-1.4.4.js" "diff.js")))))

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
    (let ((class (string-downcase which))
          (wrapped (mapcar #'com.gigamonkeys.markup.html::remap-tags wrapped)))
      ;; Recursive wrapping necessary since :moved-to and moved-from
      ;; elements can contain :adds and :deletes
      (cond
        ((and (consp (first wrapped)) (block-element-p (car (first wrapped))))
         `((:div :class ,class) ,@wrapped))
        (t `((:span :class ,class) ,@wrapped))))))
