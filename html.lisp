(in-package :com.gigamonkeys.prose-diff)

;;;
;;; HTML generation -- given two Markup files generate an HTML file of the diff.
;;;

(defparameter *retagger*
  (make-retagger
   (mapcar (lambda (x) (cons x 'wrap-add-delete)) '(:add :delete :moved-to :moved-from))))

(defun diff-to-html (original-file edited-file output &key (css-dir "") (js-dir "") parse-links-p)
  "Diff two files of Markup and write the diff as HTML to the file output."
  (let ((original (remove-comments (parse-file original-file :parse-links-p parse-links-p)))
        (edited (remove-comments (parse-file edited-file :parse-links-p parse-links-p))))
    (with-output-to-file (out output)
      (%diff-to-html original edited out css-dir js-dir))))

(defun diff-to-html/text (original-text edited-text &key (css-dir "") (js-dir "") parse-links-p)
  "Diff two strings of Markup text and return a string of the diff as HTML."
  (let ((original (remove-comments (parse-text original-text :parse-links-p parse-links-p)))
        (edited (remove-comments (parse-text edited-text :parse-links-p parse-links-p))))
    (with-output-to-string (out)
      (%diff-to-html original edited out css-dir js-dir))))

(defun %diff-to-html (original edited out css-dir js-dir)
  (render-sexps-to-stream
   `(:body
     ,@(diff-to-markup original edited)
     ((:ul :id "buttons")
      (:li ((:button :id "show_diff") "Diff"))
      (:li ((:button :id "show_original") "Original"))
      (:li ((:button :id "show_new") "New"))))
   out
   :stylesheets (loop for x in '("diff.css") collect (format nil "~a~a" css-dir x))
   :scripts (loop for x in '("jquery-1.4.4.js" "diff.js") collect (format nil "~a~a" js-dir x))
   :rewriter (compose  (lambda (x) (footnotes :note x)) *retagger*)))

(defun extract-comments (sexp)
  (let ((num 0)
        (comments ()))
    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((numberp x) x)
                 ((symbolp x) x)
                 ((eql (car x) :comment)
                  (push x comments)
                  `(:commentref ,(princ-to-string (incf num))))
                 (t  `(,(car x) ,@(mapcar #'walker (cdr x)))))))
      (let ((walked (walker sexp)))
        (values walked (nreverse comments))))))

(defun remove-comments (sexp)
  (labels ((walker (x)
             (cond
               ((stringp x) (list x))
               ((numberp x) (list x))
               ((symbolp x) (list x))
               ((eql (car x) :comment) nil)
               (t
                (let ((body (mapcan #'walker (cdr x))))
                  (if body `((,(car x) ,@body)) nil))))))

    (first (walker sexp))))

(defun commentref->html (sexp)
  (destructuring-bind (tag string) sexp
    (assert (eql tag :commentref))
    (let ((num (parse-integer string)))
      `((:span :href (:format "~(#comment_~d~)" ,num)
               :id (:format "commentref_~d" ,num)
               :class "comment_ref")
        (:character 9997)))))


(defun comments->html (sexp)
  (destructuring-bind (tag &rest comments) sexp
    (assert (eql tag :comments))
    `(:progn ,@(loop for comment in comments
                  for num from 1
                  collect
                    (destructuring-bind (tag &rest body) comment
                      (assert (eql tag :comment))
                      `((:div :id (:format "~(~a_~d~)" ,tag ,num) :class "comment") ,@body))))))

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
