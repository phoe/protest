;;;; cl-protest.asd

(asdf:defsystem #:cl-protest
  :description "Common Lisp PROtocol and TESTcase Manager"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "MIT 2-Clause"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop
               #:cl-who
               #:ningle
               #:clack
               #:named-readtables
               #:uiop)
  :components (;; General data
               (:file "util/package")
               (:file "util/util")
               ;; Protocol management
               (:file "pro/vars")
               (:file "pro/parse/function")
               (:file "pro/parse/macro")
               (:file "pro/parse/variable")
               (:file "pro/parse/class")
               (:file "pro/parse/form")
               (:file "pro/macros")
               ;; Test case management
               (:file "test/vars")
               (:file "test/failures")
               (:file "test/readtable")
               (:file "test/functions")
               (:file "test/macros")
               ;; HTML generation
               (:file "web/web")))
