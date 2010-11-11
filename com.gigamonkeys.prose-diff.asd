;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.prose-diff
  :name "com.gigamonkeys.prose-diff"
  :components
  ((:file "packages")
   (:file "prose-diff" :depends-on ("packages"))))