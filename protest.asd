;;;; protest.asd

(asdf:defsystem #:protest
  :description "Common Lisp PROtocol and TESTcase Manager"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:protest/base
               #:protest/protocol
               #:protest/test-case
               ;; #:protest/web
               )
  :components ((:file "src/package")))

(defmethod perform ((o test-op) (c (eql (find-system :protest))))
  (load-system :protest/test)
  (symbol-call :protest/test :run-all-tests))

(asdf:defsystem #:protest/base
  :description "Base macros and utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop)
  :components ((:file "src/base/package")
               (:file "src/base/base")))

(asdf:defsystem #:protest/protocol
  :description "Protocol defining utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop
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
               (:file "src/protocol/definition")
               (:file "src/protocol/validation")
               (:file "src/protocol/macro")))

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
  :description "PROTEST integration with Parachute testing library"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "Artistic"
  :serial t
  :depends-on (#:alexandria
               #:named-readtables
               #:parachute
               #:protest/base
               #:protest/test-case)
  :components ((:file "src/parachute/package")
               (:file "src/parachute/base")
               (:file "src/parachute/macros")
               (:file "src/parachute/parachute-modification")
               (:file "src/parachute/test")))

(asdf:defsystem #:protest/1am
  :description "PROTEST integration with 1AM testing library"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:named-readtables
               #:1am
               #:protest/base
               #:protest/test-case)
  :components ((:file "src/1am/package")
               (:file "src/1am/formatter")
               (:file "src/1am/macro")
               (:file "src/1am/test")))

(asdf:defsystem #:protest/test
  :description "Tests for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:protest)
  :components ((:file "t/test")
               (:file "t/framework")
               (:file "t/base")
               (:file "t/protocol")
               (:file "t/test-case")))
