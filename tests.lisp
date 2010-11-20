(in-package :com.gigamonkeys.prose-diff)


(defun remove-tag-test ()
  (and 
   (equalp
    (remove-tag :add '(:i (:b (:add "foo" (:x "abc")))))
    '(:i (:b "foo" (:x "abc"))))

   (equalp
    (remove-tag :b '(:i (:b (:add "foo" (:x "abc")))))
    '(:i (:add "foo" (:x "abc"))))

   (equalp
    (remove-tag :i '(:i (:b (:add "foo" (:x "abc")))))
    '(:b (:add "foo" (:x "abc"))))))

(defun diff-textified-markup (a b)
  (diff-textified (textify-markup a) (textify-markup b)))

(defun foo-test ()
  (list
   (rewrite-adds-and-deletes
    (detextify-markup
     (diff-textified-markup
      '(:p "foo " (:i "quux") " baz")
      '(:p "foo " (:b "quux") " baz"))))

  (rewrite-adds-and-deletes
   (detextify-markup
    (diff-textified-markup
     '(:p "foo " (:i "quux bar biff") " baz")
     '(:p "foo " (:i "quux baz boom") " baz"))))))