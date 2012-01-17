;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.prose-diff
  (:use :common-lisp
        :cl-ppcre
        :com.gigamonkeys.pathnames
        :com.gigamonkeys.utilities
        :com.gigamonkeys.markup
        :com.gigamonkeys.markup.html)
  (:import-from :alexandria :compose)
  (:export
   :show-cuts))
