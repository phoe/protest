;;;; protest.asd

(asdf:defsystem #:protest
  :description "Common Lisp PROtocol and TESTcase Manager"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:protest/base
               #:protest/protocol
               ;; #:protest/test-case
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
               #:named-readtables)
  :components ((:file "protocol/package")
               (:file "protocol/elements")
               (:file "protocol/elements/function")
               (:file "protocol/elements/macro")
               (:file "protocol/elements/class")
               (:file "protocol/elements/condition-type")
               (:file "protocol/elements/variable")
               (:file "protocol/protocol")))

;; (asdf:defsystem #:protest
;;   :description "Common Lisp PROtocol and TESTcase Manager"
;;   :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
;;   :license "MIT 2-Clause"
;;   :serial t
;;   :depends-on (#:alexandria
;;                #:closer-mop
;;                #:cl-who
;;                #:ningle
;;                #:clack
;;                #:1am
;;                #:named-readtables
;;                #:uiop)
;;   :components (;; General data
;;                (:file "_old/util/package")
;;                (:file "_old/util/util")
;;                ;; Protocol management
;;                (:file "_old/pro/vars")
;;                (:file "_old/pro/verify-class")
;;                (:file "_old/pro/parse/function")
;;                (:file "_old/pro/parse/macro")
;;                (:file "_old/pro/parse/variable")
;;                (:file "_old/pro/parse/class")
;;                (:file "_old/pro/parse/option")
;;                (:file "_old/pro/parse/config")
;;                (:file "_old/pro/parse/form")
;;                (:file "_old/pro/macros")
;;                ;; Test case management
;;                (:file "_old/test/vars")
;;                (:file "_old/test/failures")
;;                (:file "_old/test/readtable")
;;                (:file "_old/test/functions")
;;                (:file "_old/test/macros")
;;                ;; HTML generation
;;                (:file "_old/web/web")))
