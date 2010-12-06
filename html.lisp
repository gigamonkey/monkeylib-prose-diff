(in-package :com.gigamonkeys.prose-diff)

;;;
;;; HTML generation -- given two Markup files generate an HTML file of the diff.
;;;

(defparameter *retagger* 
  (make-retagger 
   (mapcar (lambda (x) (cons x 'wrap-add-delete)) '(:add :delete :moved-to :moved-from))))

(defun diff-to-html (original edited output)
  (with-output-to-file (out output)
    (render-sexps-to-stream
     `(:body
       ,@(diff-to-markup original edited)
       ((:ul :id "buttons")
        (:li ((:button :id "show_diff") "Diff"))
        (:li ((:button :id "show_original") "Original"))
        (:li ((:button :id "show_new") "New"))))
     out
     :stylesheets '("diff.css")
     :scripts '("jquery-1.4.4.js" "diff.js")
     :rewriter (compose 
                (lambda (x) (footnotes :comments x))
                (lambda (x) (footnotes :notes x))
                *retagger*))))

(defun wrap-add-delete (sexp)
  (destructuring-bind (which &rest wrapped) sexp
    (let ((class (string-downcase which))
          (wrapped (mapcar *retagger* wrapped)))
      ;; Recursive wrapping necessary since :moved-to and moved-from
      ;; elements can contain :adds and :deletes
      (cond
        ((and (consp (first wrapped)) (block-element-p (car (first wrapped))))
         `((:div :class ,class) ,@wrapped))
        (t `((:span :class ,class) ,@wrapped))))))

(defun block-element-p (x)
  (member x 
          '(:body :colgroup :div :dl :fieldset :form :head :html :map :noscript
            :object :ol :optgroup :pre :script :select :style :table :tbody
            :tfoot :thead :tr :ul
            :area :base :blockquote :br :button :caption :col :dd :div :dt :h1
            :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
            :td :textarea :th :title)))
