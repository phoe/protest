;;;; t/protocol.lisp

(defpackage #:protest/test/protocol
  (:use #:cl
        #:protest
        #:protest/test))

(in-package #:protest/test/protocol)

(register-test-package)

(defmacro with-fresh-state (&body body)
  `(let ((*protocols* (make-hash-table))
         (protest/protocol::*protocol-documentation-store* (make-hash-table)))
     ,@body
     (values)))

(defun kill-class (class-name)
  (c2mop:remove-direct-subclass (find-class 'standard-object)
                                (find-class class-name))
  (setf (find-class class-name) nil))

;;; BASIC PROTOCOLS

(define-protest-test test-protocol-define-empty
  (with-fresh-state
    (unwind-protect
         (progn
           (define-protocol #1=#.(gensym) ())
           (let ((protocol (find-protocol '#1#)))
             (is (null (documentation protocol 'protocol)))
             (is (null (tags protocol)))
             (is (null (dependencies protocol)))
             (is (null (exports protocol)))
             (is (null (elements protocol)))
             (validate-implementations protocol)))
      (remove-protocol '#1#))))

(define-protest-test test-protocol-define-detailed
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #1=#.(gensym) (:export ()))
                (define-protocol #2=#.(gensym) (:dependencies (#1#)
                                                :tags (#3=#.(gensym))
                                                :attachments (#4="haha")
                                                :documentation "asdf"
                                                :export t))
                (let ((protocol (find-protocol '#2#)))
                  (is (string= "asdf"
                               (documentation protocol 'protocol)))
                  (is (equal '(#3#) (tags protocol)))
                  (is (equal '(#1#) (dependencies protocol)))
                  (is (string= "haha" (first (attachments protocol))))
                  (is (null (exports protocol)))
                  (is (null (elements protocol)))
                  (validate-implementations protocol)))
      (mapc #'remove-protocol '(#1# #2#))
      (is (null (documentation '#2# 'protocol))))))

(define-protest-test test-protocol-define-dependencies
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #1=#.(gensym) ())
                (define-protocol #2=#.(gensym) (:dependencies (#1#)))
                (define-protocol #3=#.(gensym) (:dependencies (#1#)))
                (define-protocol #4=#.(gensym) (:dependencies (#2# #3#)))
                (define-protocol #5=#.(gensym) (:dependencies (#2#)))
                (define-protocol #6=#.(gensym) (:dependencies (#3#)))
                (define-protocol #7=#.(gensym) (:dependencies (#2#)))
                (define-protocol #8=#.(gensym)
                  (:dependencies (#1# #2# #3# #4# #5# #6# #7#))))
      (mapc #'remove-protocol '(#1# #2# #3# #4# #5# #6# #7# #8#)))))

(define-protest-test test-protocol-define-circular-dependency
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #1=#.(gensym) ())
                (define-protocol #2=#.(gensym) (:dependencies (#1#)))
                (define-protocol #3=#.(gensym) (:dependencies (#2#)))
                (define-protocol #4=#.(gensym) (:dependencies (#3#)))
                (signals protocol-error
                  (define-protocol #1# (:dependencies (#4#)))))
      (mapc #'remove-protocol '(#1# #2# #3# #4#)))))

(define-protest-test test-protocol-define-self-dependency
  (with-fresh-state
    (signals protocol-error
      (define-protocol #1=#.(gensym) (:dependencies (#1#))))))

(define-protest-test test-protocol-define-invalid-name
  (with-fresh-state
    (signals protocol-error (define-protocol 2 ()))
    (signals protocol-error (define-protocol "PROTOCOL" ()))
    (signals protocol-error (define-protocol '(#.(gensym) #.(gensym)) ()))
    (signals protocol-error (define-protocol nil ()))))

(define-protest-test test-protocol-define-invalid-dependencies-1
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies (2))))))

(define-protest-test test-protocol-define-invalid-dependencies-2
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies ("ABC"))))))

(define-protest-test test-protocol-define-invalid-dependencies-3
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies ((#.(gensym))))))))

(define-protest-test test-protocol-define-invalid-dependencies-4
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies ((1 2 3 4)))))))

(define-protest-test test-protocol-define-invalid-dependencies-5
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies (nil))))))

(define-protest-test test-protocol-define-duplicate-elements-1
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:variable #1=#.(gensym))
        (:variable #1#)))))

(define-protest-test test-protocol-define-duplicate-elements-2
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:function #2=#.(gensym) ())
        (:macro #2# ())))))

(define-protest-test test-protocol-define-duplicate-elements-3
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:class #3=#.(gensym) () ())
        (:condition-type #3# () ())))))

(define-protest-test test-protocol-define-duplicate-elements-4
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:category (:category))
        (:config (:category))))))

(define-protest-test test-protocol-define-duplicate-elements-5
  (with-fresh-state
    (signals protocol-error
      (define-protocol #1=#.(gensym) ()
        (:category (:category #1#))
        (:config (:category #1#))))))

(define-protest-test test-protocol-define-duplicate-elements-inheritance
  (with-fresh-state
    (unwind-protect
         (progn
           (define-protocol #1=#.(gensym) () (:variable #2=#.(gensym)))
           (signals protocol-error
             (define-protocol #.(gensym) (:dependencies (#1#))
               (:variable #2#))))
      (remove-protocol '#1#))))

(define-protest-test test-protocol-invalid-redefinition
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #1=#.(gensym) ())
                (define-protocol #2=#.(gensym) (:dependencies (#1#))
                  (:variable #3=#.(gensym)))
                (signals protocol-error
                  (handler-bind ((warning (lambda (c) (declare (ignore c))
                                            (muffle-warning))))
                    (define-protocol #1# ()
                      (:variable #3#)))))
      (mapc #'remove-protocol '(#1# #2#)))))

;;; ELEMENTS

(define-protest-test test-protocol-define-category-1
  (with-fresh-state
    (let* ((variable nil)
           (*category-callback*
             (lambda (x) (declare (ignore x)) (setf variable t))))
      (unwind-protect
           (progn (define-protocol #3=#.(gensym) ()
                    (:category #1=(:foo :bar #3#)) #2="qwer")
                  (eval '(execute-protocol #3#))
                  (is (string= #2# (documentation '#1# 'category)))
                  (is (eq variable t))
                  (let ((category (first (elements (find-protocol '#3#)))))
                    (is (equal (canonical-name category) '(:foo :bar nil)))))
        (let* ((elements (elements (find-protocol '#3#)))
               (category (find 'protocol-category elements :key #'type-of)))
          (remove-protocol-element category)
          (is (null (documentation '#1# 'category))))))))

(define-protest-test test-protocol-define-category-2
  (with-fresh-state
    (let* ((*variable* nil))
      (declare (special *variable*))
      (unwind-protect
           (progn (define-protocol #6=#.(gensym)
                    (:bindings ((*category-callback*
                                 (lambda (x)
                                   (declare (ignore x)
                                            (special *variable*))
                                   (setf *variable* t)))))
                    (:category #4=(:foo :bar)) #5="qwer")
                  (eval '(execute-protocol #6#))
                  (is (string= #5# (documentation '#4# 'category)))
                  (is (eq *variable* t)))
        (let* ((elements (elements (find-protocol '#6#)))
               (category (find 'protocol-category elements :key #'type-of)))
          (remove-protocol-element category)
          (is (null (documentation '#4# 'category))))))))

(define-protest-test test-protocol-define-class
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:class #1=#.(gensym) () ())
                  #2="qwer")
                (eval '(execute-protocol #3#))
                (is (find-class '#1#))
                (is (string= #2# (documentation '#1# 'type))))
      (let* ((elements (elements (find-protocol '#3#)))
             (class (find 'protocol-class elements :key #'type-of)))
        (remove-protocol-element class)
        (is (null (documentation '#1# 'type)))
        (is (null (find-class '#1# nil)))))))

(define-protest-test test-protocol-define-class-instantiate
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:class #1=#.(gensym) () ()))
                (eval '(execute-protocol #3#))
                (signals protocol-error
                  (make-instance (find-class '#1#))))
      (let* ((elements (elements (find-protocol '#3#)))
             (class (find 'protocol-class elements :key #'type-of)))
        (remove-protocol-element class)
        (is (null (documentation '#1# 'type)))
        (is (null (find-class '#1# nil)))))))

(define-protest-test test-protocol-define-condition-type
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:condition-type #1=#.(gensym) () ())
                  #2="qwer")
                (eval '(execute-protocol #3#))
                (is (find-class '#1#))
                (is (string= #2# (documentation '#1# 'type))))
      (let* ((elements (elements (find-protocol '#3#)))
             (condition-type (find 'protocol-condition-type elements
                                   :key #'type-of)))
        (remove-protocol-element condition-type)
        (is (null (documentation '#1# 'type)))
        (is (null (find-class '#1# nil)))))))

(define-protest-test #2=test-protocol-define-condition-type-instantiate
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:condition-type #1=#.(gensym) () ()))
                (eval '(execute-protocol #3#))
                (signals protocol-error
                  (make-instance'#1#)))
      (let* ((elements (elements (find-protocol '#3#)))
             (condition-type (find 'protocol-condition-type elements
                                   :key #'type-of)))
        (remove-protocol-element condition-type)
        (is (null (documentation '#1# 'type)))
        (is (null (find-class '#1# nil)))))))

(define-protest-test test-protocol-define-config-1
  (with-fresh-state
    (let* ((variable nil)
           (*config-callback*
             (lambda (w x y &optional z)
               (declare (ignore w x y z)) (setf variable t))))
      (unwind-protect
           (progn (define-protocol #3=#.(gensym) ()
                    (:config #1=(:foo :bar #3#) string :mandatory "a")
                    #2="qwer")
                  (eval '(execute-protocol #3#))
                  (is (string= #2# (documentation '#1# 'config)))
                  (is (eq variable t))
                  (let ((config (first (elements (find-protocol '#3#)))))
                    (is (equal (canonical-name config) '(:foo :bar nil)))))
        (let* ((elements (elements (find-protocol '#3#)))
               (config (find 'protocol-config elements :key #'type-of)))
          (remove-protocol-element config)
          (is (null (documentation '#1# 'config))))))))

(define-protest-test test-protocol-define-config-2
  (with-fresh-state
    (let* ((*variable* nil))
      (declare (special *variable*))
      (unwind-protect
           (progn (define-protocol #6=#.(gensym)
                    (:bindings ((*config-callback*
                                 (lambda (w x y &optional z)
                                   (declare (ignore w x y z)
                                            (special *variable*))
                                   (setf *variable* t)))))
                    (:config #4=(:foo :bar) string :mandatory "a")
                    #5="qwer")
                  (eval '(execute-protocol #6#))
                  (is (string= #5# (documentation '#4# 'config)))
                  (is (eq *variable* t)))
        (let* ((elements (elements (find-protocol '#6#)))
               (config (find 'protocol-config elements :key #'type-of)))
          (remove-protocol-element config)
          (is (null (documentation '#4# 'config))))))))

(define-protest-test test-protocol-define-config-boundp
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #2=#.(gensym) ()
                  (:config #1=(:foo :bar) t :mandatory 42))
                (let* ((protocol (find-protocol '#2#))
                       (elements (elements protocol))
                       (element (find '#1# elements :key #'name :test #'equal)))
                  (is (protocol-element-boundp element))
                  (is (= 42 (initial-value element)))
                  (protocol-element-makunbound element)
                  (is (not (protocol-element-boundp element)))))
      (let* ((elements (elements (find-protocol '#2#)))
             (config (find 'protocol-config elements :key #'type-of)))
        (remove-protocol-element config)
        (is (null (documentation '#1# 'config)))))))

(define-protest-test test-protocol-define-config-wrong-type-1
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:config (:foo :bar) number :mandatory "42")))))

(define-protest-test test-protocol-define-config-wrong-type-2
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:config (:foo :bar) string :mandatory 42)))))

;; TODO examine the following warning
#|
CL-USER> (protest/test/protocol::test-protocol-define-function)
PROTEST/TEST/PROTOCOL::TEST-PROTOCOL-DEFINE-FUNCTION
STYLE-WARNING:
Generic function #:G50 clobbers an earlier FTYPE proclamation
(FUNCTION (T T) (VALUES &OPTIONAL (QUOTE STRING) &REST T)) for the same name
with (FUNCTION (T T) *).
....
Success: 1 test, 4 checks.
|#
(define-protest-test test-protocol-define-function
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #5=#.(gensym) ()
                  (:function #1=#.(gensym) (#.(gensym) #.(gensym)) string)
                  #4="qwer")
                (eval '(execute-protocol #5#))
                (is (typep (fdefinition '#1#) 'generic-function))
                (is (string= #4# (documentation '#1# 'function))))
      (let* ((elements (elements (find-protocol '#5#)))
             (function (find 'protocol-function elements :key #'type-of)))
        (remove-protocol-element function)
        (is (not (fboundp'#1#)))
        (is (null (documentation '#1# 'function)))))))

(define-protest-test test-protocol-define-function-setf
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #5=#.(gensym) ()
                  (:function #1=(setf #.(gensym)) (#.(gensym) #.(gensym))
                             string)
                  #4="qwer"
                  (:function #2=(setf #.(gensym)) (#.(gensym) #.(gensym))
                             string)
                  #6="asdf")
                (eval '(execute-protocol #5#))
                (is (typep (fdefinition '#1#) 'generic-function))
                (is (string= #4# (documentation '#1# 'function)))
                (is (typep (fdefinition '#2#) 'generic-function))
                (is (string= #6# (documentation '#2# 'function)))
                (let ((function (first (elements (find-protocol '#5#)))))
                  (is (equal (name function) (canonical-name function)))
                  (is (equal (name function) '#1#))))
      (let* ((elements (elements (find-protocol '#5#)))
             (functions (remove 'protocol-function elements
                                :test-not #'eql :key #'type-of)))
        (mapc #'remove-protocol-element functions)
        (is (not (fboundp'#1#)))
        (is (null (documentation '#1# 'function)))
        (is (not (fboundp'#2#)))
        (is (null (documentation '#2# 'function)))))))

(define-protest-test test-protocol-define-macro
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #5=#.(gensym) ()
                  (:macro #1=#.(gensym) (#.(gensym) #.(gensym)))
                  #4="qwer")
                (eval '(execute-protocol #5#))
                (is (string= #4# (documentation '#1# 'function)))
                (is (not (fboundp'#1#))))
      (let* ((elements (elements (find-protocol '#5#)))
             (macro (find 'protocol-macro elements :key #'type-of)))
        (remove-protocol-element macro)
        (is (null (documentation '#1# 'function)))))))

(define-protest-test test-protocol-define-variable
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #4=#.(gensym) ()
                  (:variable #1=#.(gensym) string #2="asdf")
                  #3="qwer")
                (eval '(execute-protocol #4#))
                (is (string= (symbol-value '#1#) #2#))
                (is (string= #3# (documentation '#1# 'variable))))
      (let* ((elements (elements (find-protocol '#4#)))
             (variable (find 'protocol-variable elements :key #'type-of)))
        (remove-protocol-element variable)
        (is (not (boundp '#1#)))
        (is (null (documentation '#1# 'function)))))))

(define-protest-test test-protocol-define-variable-boundp
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #2=#.(gensym) ()
                  (:variable #1=#.(gensym) t 42))
                (let* ((protocol (find-protocol '#2#))
                       (elements (elements protocol))
                       (element (find '#1# elements :key #'name)))
                  (is (protocol-element-boundp element))
                  (is (= 42 (initial-value element)))
                  (protocol-element-makunbound element)
                  (is (not (protocol-element-boundp element)))))
      (let* ((elements (elements (find-protocol '#2#)))
             (variable (find 'protocol-variable elements :key #'type-of)))
        (remove-protocol-element variable)))))

(define-protest-test test-protocol-define-variable-wrong-type-1
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:variable #1=#.(gensym) number "42")))))

(define-protest-test test-protocol-define-variable-wrong-type-2
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:variable #2=#.(gensym) string 42)))))

;;; COMPLEX PROTOCOLS

(define-protest-test test-protocol-define-complex
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #8=#.(gensym) ()
                  (:category #1=(:foo :bar))
                  (:class #2=#.(gensym) () ())
                  (:condition-type #3=#.(gensym) () ())
                  (:config #4=(:foo :bar :baz) t :mandatory "a")
                  (:function #5=#.(gensym) ())
                  (:macro #6=#.(gensym) ())
                  (:variable #7=#.(gensym) t "asdf"))
                (eval '(execute-protocol #8#))
                (is (find-class '#2#))
                (is (find-class '#3#))
                (is (fdefinition '#5#))
                (is (string= "asdf" (symbol-value '#7#)))
                (validate-implementations '#8#))
      (remove-protocol '#8#)
      (is (null (find-protocol '#8#)))
      (is (null (find-class '#2# nil)))
      (is (null (find-class '#3# nil)))
      (is (not (fboundp '#5#)))
      (is (not (boundp '#6#))))))

(define-protest-test test-protocol-compute-effective-protocol-elements
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #1=#.(gensym) ()
                  (:variable #2=#.(gensym)))
                (define-protocol #3=#.(gensym) (:dependencies (#1#))
                  (:variable #4=#.(gensym)))
                (define-protocol #5=#.(gensym) (:dependencies (#1#))
                  (:variable #6=#.(gensym)))
                (define-protocol #7=#.(gensym) (:dependencies (#3# #5#))
                  (:variable #8=#.(gensym)))
                (let ((elements (compute-effective-protocol-elements
                                 (find-protocol '#7#))))
                  (is (find '#2# elements :key #'name))
                  (is (find '#4# elements :key #'name))
                  (is (find '#6# elements :key #'name))
                  (is (find '#8# elements :key #'name))))
      (mapc #'remove-protocol (mapcar #'find-protocol '(#1# #3# #5# #7#))))))

;;; VALIDATE-IMPLEMENTATIONS - ONE ARG

(defmacro with-fresh-state-and-unwind (body assertions unwind)
  `(with-fresh-state (unwind-protect (progn ,@body ,@assertions) ,@unwind)))

(define-protest-test test-protocol-validate-implementations-one-arg-1
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #3=#.(make-symbol "FUNCTION") ((#2# #2#))))
     (eval '(execute-protocol #1#)))
    ((is (null (validate-implementations '#1#))))
    ((remove-protocol '#1#))))

(define-protest-test test-protocol-validate-implementations-one-arg-2
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #3=#.(make-symbol "FUNCTION") ((#2# #2#))))
     (eval '(execute-protocol #1#))
     (defclass #4=#.(gensym "CONCRETE-CLASS-1-") (#2#) ()))
    ((let ((results (validate-implementations '#1#)))
       (is (= 1 (length results)))
       (is (equal (first results)
                  (list :missing-method (fdefinition '#3#) 0
                        (find-class '#2#) (find-class '#4#))))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#4#)))))

(define-protest-test test-protocol-validate-implementations-one-arg-3
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #3=#.(make-symbol "FUNCTION") ((#2# #2#))))
     (eval '(execute-protocol #1#))
     (defclass #4=#.(gensym "CONCRETE-CLASS-1-") (#2#) ())
     (defmethod #3# ((#2# #4#))))
    ((is (null (validate-implementations '#1#))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#4#)))))

(define-protest-test test-protocol-validate-implementations-one-arg-4
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #3=#.(make-symbol "FUNCTION") ((#2# #2#))))
     (eval '(execute-protocol #1#))
     (defclass #4=#.(gensym "CONCRETE-CLASS-1-") (#2#) ())
     (defmethod #3# ((#2# #4#)))
     (defclass #5=#.(gensym "CONCRETE-CLASS-2-") (#2#) ()))
    ((let ((results (validate-implementations '#1#)))
       (is (= 1 (length results)))
       (let ((result (first results))
             (expected (list :missing-method (fdefinition '#3#) 0
                             (find-class '#2#) (find-class '#5#))))
         (is (equal result expected)))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#4# #5#)))))

(define-protest-test test-protocol-validate-implementations-one-arg-5
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #3=#.(make-symbol "FUNCTION") ((#2# #2#))))
     (eval '(execute-protocol #1#))
     (defclass #4=#.(gensym "CONCRETE-CLASS-1-") (#2#) ())
     (defmethod #3# ((#2# #4#)))
     (defclass #5=#.(gensym "CONCRETE-CLASS-2-") (#2#) ())
     (defmethod #3# ((#2# #5#))))
    ((is (null (validate-implementations '#1#))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#4# #5#)))))

(define-protest-test test-protocol-validate-implementations-one-arg-6
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #3=#.(make-symbol "FUNCTION") ((#2# #2#))))
     (eval '(execute-protocol #1#))
     (defclass #4=#.(gensym "CONCRETE-CLASS-1-") (#2#) ())
     (defmethod #3# ((#2# #4#)))
     (defclass #5=#.(gensym "CONCRETE-CLASS-2-") (#2#) ())
     (defmethod #3# ((#2# #5#)))
     (defclass #6=#.(gensym "CONCRETE-CLASS-3-") (#5#) ()))
    ((is (null (validate-implementations '#1#)))
     (let ((results (validate-implementations '#1# :successp t)))
       (is (= 3 (length results)))
       (is (member (list :success (fdefinition '#3#) 0
                         (find-class '#2#) (find-class '#4#)
                         (find-class '#4#))
                   results :test #'equal))
       (is (member (list :success (fdefinition '#3#) 0
                         (find-class '#2#) (find-class '#5#)
                         (find-class '#5#))
                   results :test #'equal))
       (is (member (list :success (fdefinition '#3#) 0
                         (find-class '#2#) (find-class '#6#)
                         (find-class '#5#))
                   results :test #'equal))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#4# #5# #6#)))))

;;; TODO :declaim-types-p nil everywhere to avoid SBCL warnings?

;;; VALIDATE-IMPLEMENTATIONS - TWO ARG

(define-protest-test test-protocol-validate-implementations-two-arg-1
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS-1") () ())
       (:class #3=#.(make-symbol "PROTOCOL-CLASS-2") () ())
       (:function #4=#.(gensym "FUNCTION-") ((#2# #2#) (#3# #3#))))
     (eval '(execute-protocol #1#)))
    ((is (null (validate-implementations '#1#))))
    ((remove-protocol '#1#))))

(define-protest-test test-protocol-validate-implementations-two-arg-2
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS-1") () ())
       (:class #3=#.(make-symbol "PROTOCOL-CLASS-2") () ())
       (:function #4=#.(gensym "FUNCTION-") ((#2# #2#) (#3# #3#))))
     (eval '(execute-protocol #1#))
     (defclass #5=#.(make-symbol "CONCRETE-CLASS-1") (#2#) ()))
    ((let ((results (validate-implementations '#1#)))
       (is (= 1 (length results)))
       (is (member (list :missing-method (fdefinition '#4#) 0
                         (find-class '#2#) (find-class '#5#))
                   results :test #'equal))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#5#)))))

(define-protest-test test-protocol-validate-implementations-two-arg-3
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS-1") () ())
       (:class #3=#.(make-symbol "PROTOCOL-CLASS-2") () ())
       (:function #4=#.(gensym "FUNCTION-") ((#2# #2#) (#3# #3#))))
     (eval '(execute-protocol #1#))
     (defclass #5=#.(make-symbol "CONCRETE-CLASS-1") (#2#) ())
     (defclass #6=#.(make-symbol "CONCRETE-CLASS-2") (#3#) ()))
    ((let ((results (validate-implementations '#1#)))
       (is (= 2 (length results)))
       (is (member (list :missing-method (fdefinition '#4#) 0
                         (find-class '#2#) (find-class '#5#))
                   results :test #'equal))
       (is (member (list :missing-method (fdefinition '#4#) 1
                         (find-class '#3#) (find-class '#6#))
                   results :test #'equal))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#5# #6#)))))

(define-protest-test test-protocol-validate-implementations-two-arg-4
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS-1") () ())
       (:class #3=#.(make-symbol "PROTOCOL-CLASS-2") () ())
       (:function #4=#.(gensym "FUNCTION-") ((#2# #2#) (#3# #3#))))
     (eval '(execute-protocol #1#))
     (defclass #5=#.(make-symbol "CONCRETE-CLASS-1") (#2#) ())
     (defclass #6=#.(make-symbol "CONCRETE-CLASS-2") (#3#) ())
     (defmethod #4# ((#2# #5#) (#3# #5#))))
    ((let ((results (validate-implementations '#1#)))
       (is (= 2 (length results)))
       (is (member (list :missing-method (fdefinition '#4#) 0
                         (find-class '#2#) (find-class '#5#))
                   results :test #'equal))
       (is (member (list :missing-method (fdefinition '#4#) 1
                         (find-class '#3#) (find-class '#6#))
                   results :test #'equal))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#5# #6#)))))

(define-protest-test test-protocol-validate-implementations-two-arg-5
  (with-fresh-state-and-unwind
    ((define-protocol #1=#.(make-symbol "PROTOCOL") ()
       (:class #2=#.(make-symbol "PROTOCOL-CLASS-1") () ())
       (:class #3=#.(make-symbol "PROTOCOL-CLASS-2") () ())
       (:function #4=#.(gensym "FUNCTION-") ((#2# #2#) (#3# #3#))))
     (eval '(execute-protocol #1#))
     (defclass #5=#.(make-symbol "CONCRETE-CLASS-1") (#2#) ())
     (defclass #6=#.(make-symbol "CONCRETE-CLASS-2") (#3#) ())
     (defmethod #4# ((#2# #5#) (#3# #5#)))
     (defmethod #4# ((#2# #5#) (#3# #6#))))
    ((is (null (validate-implementations '#1#)))
     (let ((results (validate-implementations '#1# :successp t)))
       (is (= 2 (length results)))
       (is (member (list :success (fdefinition '#4#) 0
                         (find-class '#2#) (find-class '#5#)
                         (find-class '#5#))
                   results :test #'equal))
       (is (member (list :success (fdefinition '#4#) 1
                         (find-class '#3#) (find-class '#6#)
                         (find-class '#6#))
                   results :test #'equal))))
    ((remove-protocol '#1#)
     (mapc #'kill-class '(#5# #6#)))))

;;; VALIDATE-IMPLEMENTATIONS - MIXED ARG

(define-protest-test test-protocol-validate-implementations-mixed-arg-1
  (with-fresh-state-and-unwind
    ((defclass #1=#.(make-symbol "CLASS") () ())
     (define-protocol #2=#.(make-symbol "PROTOCOL") ()
       (:class #3=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #4=#.(make-symbol "FUNCTION") ((#3# #3#) (#1# #1#))))
     (eval '(execute-protocol #2#)))
    ((is (null (validate-implementations '#2#))))
    ((remove-protocol '#2#)
     (mapc #'kill-class '(#1#)))))

(define-protest-test test-protocol-validate-implementations-mixed-arg-2
  (with-fresh-state-and-unwind
    ((defclass #1=#.(make-symbol "CLASS") () ())
     (define-protocol #2=#.(make-symbol "PROTOCOL") ()
       (:class #3=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #4=#.(make-symbol "FUNCTION") ((#3# #3#) (#1# #1#))))
     (eval '(execute-protocol #2#))
     (defclass #5=#.(make-symbol "CONCRETE-CLASS-1") (#3#) ()))
    ((let ((results (validate-implementations '#2# :successp t)))
       (is (= 1 (length results)))
       (is (member (list :missing-method (fdefinition '#4#) 0
                         (find-class '#3#) (find-class '#5#))
                   results :test #'equal))))
    ((remove-protocol '#2#)
     (mapc #'kill-class '(#1# #5#)))))

(define-protest-test test-protocol-validate-implementations-mixed-arg-3
  (with-fresh-state-and-unwind
    ((defclass #1=#.(make-symbol "CLASS") () ())
     (define-protocol #2=#.(make-symbol "PROTOCOL") ()
       (:class #3=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #4=#.(make-symbol "FUNCTION") ((#3# #3#) (#1# #1#))))
     (eval '(execute-protocol #2#))
     (defclass #5=#.(make-symbol "CONCRETE-CLASS-1") (#3#) ())
     (defmethod #4# ((#3# #5#) (#1# #1#))))
    ((is (null (validate-implementations '#2#)))
     (let ((results (validate-implementations '#2# :successp t)))
       (is (= 1 (length results)))
       (is (member (list :success (fdefinition '#4#) 0
                         (find-class '#3#) (find-class '#5#)
                         (find-class '#5#))
                   results :test #'equal))))
    ((remove-protocol '#2#)
     (mapc #'kill-class '(#1# #5#)))))

(define-protest-test test-protocol-validate-implementations-mixed-arg-4
  (with-fresh-state-and-unwind
    ((defclass #1=#.(make-symbol "CLASS") () ())
     (define-protocol #2=#.(make-symbol "PROTOCOL") ()
       (:class #3=#.(make-symbol "PROTOCOL-CLASS") () ())
       (:function #4=#.(make-symbol "FUNCTION") ((#3# #3#) (#1# #1#))))
     (eval '(execute-protocol #2#))
     (defclass #5=#.(make-symbol "CONCRETE-CLASS-1") (#3#) ())
     (defmethod #4# ((#3# #5#) (#1# #1#)))
     (defclass #6=#.(make-symbol "CONCRETE-CLASS-2") (#5#) ()))
    ((is (null (validate-implementations '#2#)))
     (let ((results (validate-implementations '#2# :successp t)))
       (is (= 2 (length results)))
       (is (member (list :success (fdefinition '#4#) 0
                         (find-class '#3#) (find-class '#5#)
                         (find-class '#5#))
                   results :test #'equal))
       (is (member (list :success (fdefinition '#4#) 0
                         (find-class '#3#) (find-class '#6#)
                         (find-class '#5#))
                   results :test #'equal))))
    ((remove-protocol '#2#)
     (mapc #'kill-class '(#1# #5# #6#)))))

;; https://bugs.launchpad.net/sbcl/+bug/1808654
;; Class objects are not removable at the moment. Wait for SBCL 1.4.15.
