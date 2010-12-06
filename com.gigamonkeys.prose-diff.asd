;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.prose-diff
  :name "com.gigamonkeys.prose-diff"
  :components
  ((:file "packages")
   (:file "lcs" :depends-on ("packages"))
   (:file "tokenize" :depends-on ("packages"))
   (:file "text" :depends-on ("packages"))
   (:file "utilities" :depends-on ("packages"))
   (:file "diff" :depends-on ("packages"))
   (:file "html" :depends-on ("packages")))
  :depends-on (:cl-ppcre
               :com.gigamonkeys.pathnames
               :com.gigamonkeys.utilities
               :com.gigamonkeys.macro-utilities
               :com.gigamonkeys.markup
               :monkeylib-markup-html))
