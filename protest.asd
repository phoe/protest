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
  :components ((:file "src/package")))

(asdf:defsystem #:protest/base
  :description "Base macros and utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop)
  :components ((:file "src/base/base")))

(asdf:defsystem #:protest/protocol
  :description "Protocol defining utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:protest/base)
  :components ((:file "src/protocol/package")
               (:file "src/protocol/elements")
               (:file "src/protocol/elements/function")
               (:file "src/protocol/elements/macro")
               (:file "src/protocol/elements/class")
               (:file "src/protocol/elements/condition-type")
               (:file "src/protocol/elements/variable")
               (:file "src/protocol/elements/category")
               (:file "src/protocol/elements/config")
               (:file "src/protocol/protocol")))

(asdf:defsystem #:protest/test-case
  :description "Test case defining utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:protest/base)
  :components ((:file "src/test-case/package")
               (:file "src/test-case/test-step")
               (:file "src/test-case/test-case")))

(asdf:defsystem #:protest/parachute
  :description "PROTEST integration with Parachute library"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "Artistic"
  :serial t
  :depends-on (#:alexandria
               #:named-readtables
               #:parachute
               #:protest/test-case)
  :components ((:file "src/parachute/package")
               (:file "src/parachute/base")
               (:file "src/parachute/macros")
               (:file "src/parachute/parachute-modification")
               (:file "src/parachute/test")))

(asdf:defsystem #:protest/test
  :description "PROTEST test package"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:protest)
  :components ((:file "t/1am")
               (:file "t/base")
               (:file "t/protocol")
               (:file "t/test-case")))
