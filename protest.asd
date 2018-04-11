;;;; protest.asd

(asdf:defsystem #:protest
  :description "Common Lisp PROtocol and TESTcase Manager"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:protest/base
               #:protest/protocol
               #:protest/test-case
               #:protest/parachute
               ;; #:protest/web
               )
  :components ((:file "package")))

(asdf:defsystem #:protest/base
  :description "Base macros and utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop)
  :components ((:file "base/base")
               (:file "base/test")))

(asdf:defsystem #:protest/protocol
  :description "Protocol defining utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:protest/base)
  :components ((:file "protocol/package")
               (:file "protocol/elements")
               (:file "protocol/elements/function")
               (:file "protocol/elements/macro")
               (:file "protocol/elements/class")
               (:file "protocol/elements/condition-type")
               (:file "protocol/elements/variable")
               (:file "protocol/elements/category")
               (:file "protocol/elements/config")
               (:file "protocol/protocol")
               (:file "protocol/test")))

(asdf:defsystem #:protest/test-case
  :description "Test case defining utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:protest/base)
  :components ((:file "test-case/package")
               (:file "test-case/test-step")
               (:file "test-case/test-case")
               (:file "test-case/test")))

(asdf:defsystem #:protest/parachute
  :description "PROTEST integration with Parachute library"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "Artistic"
  :serial t
  :depends-on (#:alexandria
               #:named-readtables
               #:parachute
               #:protest/test-case)
  :components ((:file "parachute/package")
               (:file "parachute/base")
               (:file "parachute/macros")
               (:file "parachute/parachute-modification")
               (:file "parachute/test")))
