;;;; cltms.asd

(asdf:defsystem #:cl-protest
  :description "Common Lisp PROtocol and TESTcase Manager"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "MIT 2-Clause"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop
               #:uiop)
  :components ((:file "package")
               (:file "cl-protest")))
