;;;; protest.asd

(asdf:defsystem #:protest
  :description "Common Lisp PROtocol and TESTcase Manager"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
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
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop)
  :components ((:file "src/base/package")
               (:file "src/base/base")))

(asdf:defsystem #:protest/ftype
  :description "FTYPE generation for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "src/ftype/package")
               (:file "src/ftype/ftype")))

(asdf:defsystem #:protest/protocol
  :description "Protocol defining utilities for PROTEST"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop
               #:protest/base
               #:protest/ftype)
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
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:alexandria
               #:protest/base)
  :components ((:file "src/test-case/package")
               (:file "src/test-case/test-step")
               (:file "src/test-case/test-case")))

(asdf:defsystem #:protest/parachute
  :description "PROTEST integration with Parachute testing library"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
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
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
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
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "LLGPL"
  :serial t
  :depends-on (#:protest)
  :components ((:file "t/test")
               (:file "t/framework")
               (:file "t/base")
               (:file "t/ftype")
               (:file "t/protocol")
               (:file "t/test-case")))

;;; PROTEST common protocols

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *%protest-commons* '()))

(defmacro define-protest-common
    (name description &key (subdirectory "") depends-on)
  (let ((filename (string-downcase (string name)))
        (symbol (make-symbol (uiop:strcat (string '#:protest/common/)
                                          (string name)))))
    `(progn
       (asdf:defsystem ,symbol
         :description ,description
         :author "Michał \"phoe\" Herda <phoe@disroot.org>"
         :license "LLGPL"
         :serial t
         :depends-on
         (#:protest/protocol
          ,@depends-on)
         :components
         ((:file ,(uiop:strcat "src/common/" subdirectory filename))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (pushnew ',symbol *%protest-commons*)))))

(define-protest-common #:date
    "Date protocol from PROTEST"
    :depends-on (#:protest/common/serializable))
(define-protest-common #:addressed
    "Addressed protocol from PROTEST"
    :subdirectory "mixin/")
(define-protest-common #:handling
    "Handling protocol from PROTEST"
    :subdirectory "mixin/")
(define-protest-common #:killable
    "Killable protocol from PROTEST"
    :subdirectory "mixin/")
(define-protest-common #:named
    "Named protocol from PROTEST"
    :subdirectory "mixin/")
(define-protest-common #:serializable
    "Serializable protocol from PROTEST"
    :subdirectory "mixin/")

(asdf:defsystem #:protest/common
  :description "Common protocols and examples of PROTEST"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "LLGPL"
  :serial t
  :depends-on #.*%protest-commons*)
