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
               #:uiop)
  :components ((:file "package")
               (:file "utils")
               (:file "variables")
               (:file "parse-function")
               (:file "parse-macro")
               (:file "parse-variable")
               (:file "parse-class")
               (:file "choose")
               (:file "macros")
               (:file "web")))
