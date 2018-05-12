;;;; t/protocol.lisp

(defpackage #:protest/test/protocol
  (:use #:cl
        #:protest
        #:protest/test))

(in-package #:protest/test/protocol)

(register-test-package)

(defmacro with-fresh-state (&body body)
  `(let ((*protocols* (make-hash-table)))
     ,@body
     (values)))

(define-protest-test test-protocol-define-empty
  (with-fresh-state
    (define-protocol #1=#.(gensym) ())
    (let ((protocol (gethash '#1# *protocols*)))
      (is (null (documentation protocol 'protocol)))
      (is (null (tags protocol)))
      (is (null (dependencies protocol)))
      (is (null (exports protocol)))
      (is (null (elements protocol))))))

(define-protest-test test-protocol-define-detailed
  (with-fresh-state
    (define-protocol #1=#.(gensym) (:export ()))
    (define-protocol #2=#.(gensym) (:dependencies (#1#)
                                    :tags (#3=#.(gensym))
                                    :attachments (#4="haha")
                                    :documentation "asdf"
                                    :export t))
    (let ((protocol (gethash '#2# *protocols*)))
      (is (string= "asdf"
                   (documentation protocol 'protocol)))
      (is (equal '(#3#) (tags protocol)))
      (is (equal '(#1#) (dependencies protocol)))
      (is (string= "haha" (first (attachments protocol))))
      (is (null (exports protocol)))
      (is (null (elements protocol))))))

(define-protest-test test-protocol-define-dependencies
  (with-fresh-state
    (define-protocol #1=#.(gensym) ())
    (define-protocol #2=#.(gensym) (:dependencies (#1#)))
    (define-protocol #3=#.(gensym) (:dependencies (#1#)))
    (define-protocol #4=#.(gensym) (:dependencies (#2# #3#)))
    (define-protocol #5=#.(gensym) (:dependencies (#2#)))
    (define-protocol #6=#.(gensym) (:dependencies (#3#)))
    (define-protocol #7=#.(gensym) (:dependencies (#2#)))
    (define-protocol #.(gensym) (:dependencies (#1# #2# #3# #4# #5# #6# #7#)))))

(define-protest-test test-protocol-define-circular-dependency
  (with-fresh-state
    (define-protocol #1=#.(gensym) ())
    (define-protocol #2=#.(gensym) (:dependencies (#1#)))
    (define-protocol #3=#.(gensym) (:dependencies (#2#)))
    (define-protocol #4=#.(gensym) (:dependencies (#3#)))
    (signals protocol-error
      (define-protocol #1# (:dependencies (#4#))))))

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

(define-protest-test test-protocol-define-invalid-dependencies
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies (2)))))
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies ("ABC")))))
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies ((#.(gensym)))))))
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies ((1 2 3 4))))))
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) (:dependencies (nil))))))

(define-protest-test test-protocol-define-duplicate-elements
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:variable #1=#.(gensym))
        (:variable #1#))))
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:category (:category))
        (:config (:category))))))

(define-protest-test test-protocol-define-duplicate-elements-inheritance
  (with-fresh-state
    (signals protocol-error
      (define-protocol #1=#.(gensym) ()
        (:variable #2=#.(gensym)))
      (define-protocol #.(gensym) (:dependencies (#1#))
        (:variable #2#)))))

(define-protest-test test-protocol-define-category
  (with-fresh-state
    (let* ((variable nil)
           (*category-callback*
             (lambda (x) (declare (ignore x)) (setf variable t))))
      (unwind-protect
           (progn (define-protocol #3=#.(gensym) ()
                    (:category #1=(:foo :bar)) #2="qwer")
                  (eval '(execute-protocol #3#))
                  (is (string= #2# (documentation '#1# 'category)))
                  (is (eq variable t)))
        (setf (documentation '#1# 'category) nil)
        (is (null (documentation '#1# 'category)))))))

(define-protest-test test-protocol-define-class
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:class #1=#.(gensym) () ())
                  #2="qwer")
                (eval '(execute-protocol #3#))
                (is (find-class '#1#))
                (is (string= #2# (documentation '#1# 'type))))
      (setf (documentation '#1# 'type) nil)
      (is (null (documentation '#1# 'type)))
      (setf (find-class '#1#) nil)
      (is (null (find-class '#1# nil))))))

(define-protest-test test-protocol-define-class-instantiate
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:class #1=#.(gensym) () ()))
                (eval '(execute-protocol #3#))
                (signals protocol-error
                  (make-instance (find-class '#1#))))
      (setf (find-class '#1#) nil)
      (is (null (find-class '#1# nil))))))

(define-protest-test test-protocol-define-condition-type
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:condition-type #1=#.(gensym) () ())
                  #2="qwer")
                (eval '(execute-protocol #3#))
                (is (find-class '#1#))
                (is (string= #2# (documentation '#1# 'type))))
      (setf (documentation '#1# 'type) nil)
      (is (null (documentation '#1# 'type)))
      (setf (find-class '#1#) nil)
      (is (null (find-class '#1# nil))))))

(define-protest-test #2=test-protocol-define-condition-type-instantiate
  ;; https://bugs.launchpad.net/sbcl/+bug/1761950
  #+sbcl (format t "~&~A broken on SBCL; skipping.~&" '#2#)
  #-sbcl (with-fresh-state
           (unwind-protect
                (progn (define-protocol #3=#.(gensym) ()
                         (:condition-type #1=#.(gensym) () ()))
                       (eval '(execute-protocol #3#))
                       (signals protocol-error
                         (make-condition '#1#)))
             (setf (find-class '#1#) nil)
             (is (null (find-class '#1# nil))))))

(define-protest-test test-protocol-define-config
  (with-fresh-state
    (let* ((variable nil)
           (*config-callback*
             (lambda (w x y &optional z)
               (declare (ignore w x y z)) (setf variable t))))
      (unwind-protect
           (progn (define-protocol #3=#.(gensym) ()
                    (:config #1=(:foo :bar) string :mandatory "a")
                    #2="qwer")
                  (eval '(execute-protocol #3#))
                  (is (string= #2# (documentation '#1# 'config)))
                  (is (eq variable t)))
        (setf (documentation '#1# 'config) nil)
        (is (null (documentation '#1# 'config)))))))

(define-protest-test test-protocol-define-config-boundp
  (with-fresh-state
    (define-protocol #2=#.(gensym) ()
      (:config #1=(:foo :bar) t :mandatory 42))
    (let* ((protocol (gethash '#2# *protocols*))
           (elements (elements protocol))
           (element (find '#1# elements :key #'name :test #'equal)))
      (is (protocol-element-boundp element))
      (is (= 42 (initial-value element)))
      (protocol-element-makunbound element)
      (is (not (protocol-element-boundp element))))))

(define-protest-test test-protocol-define-config-wrong-type
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:config (:foo :bar) number :mandatory "42"))))
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:config (:foo :bar) string :mandatory 42)))))

(define-protest-test test-protocol-define-function
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #5=#.(gensym) ()
                  (:function #1=#.(gensym) (#.(gensym) #.(gensym)) 'string)
                  #4="qwer")
                (eval '(execute-protocol #5#))
                (is (typep (fdefinition '#1#) 'generic-function))
                (is (string= #4# (documentation '#1# 'function))))
      (fmakunbound '#1#)
      (is (not (fboundp'#1#)))
      (setf (documentation '#1# 'function) nil)
      (is (null (documentation '#1# 'function))))))

(define-protest-test test-protocol-define-macro
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #5=#.(gensym) ()
                  (:macro #1=#.(gensym) (#.(gensym) #.(gensym)))
                  #4="qwer")
                (eval '(execute-protocol #5#))
                (is (string= #4# (documentation '#1# 'function))))
      (fmakunbound '#1#)
      (is (not (fboundp'#1#)))
      (setf (documentation '#1# 'function) nil)
      (is (null (documentation '#1# 'function))))))

(define-protest-test test-protocol-define-variable
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #4=#.(gensym) ()
                  (:variable #1=#.(gensym) string #2="asdf")
                  #3="qwer")
                (eval '(execute-protocol #4#))
                (is (string= (symbol-value '#1#) #2#))
                (is (string= #3# (documentation '#1# 'variable))))
      (makunbound '#1#)
      (is (not (boundp '#1#)))
      (setf (documentation '#1# 'variable) nil)
      (is (null (documentation '#1# 'variable))))))

(define-protest-test test-protocol-define-variable-boundp
  (with-fresh-state
    (define-protocol #2=#.(gensym) ()
      (:variable #1=#.(gensym) t 42))
    (let* ((protocol (gethash '#2# *protocols*))
           (elements (elements protocol))
           (element (find '#1# elements :key #'name)))
      (is (protocol-element-boundp element))
      (is (= 42 (initial-value element)))
      (protocol-element-makunbound element)
      (is (not (protocol-element-boundp element))))))

(define-protest-test test-protocol-define-variable-wrong-type
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:variable #1=#.(gensym) number "42"))))
  (with-fresh-state
    (signals protocol-error
      (define-protocol #.(gensym) ()
        (:variable #2=#.(gensym) string 42)))))

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
                (is (string= "asdf" (symbol-value '#7#))))
      (setf (find-class '#2#) nil
            (find-class '#3#) nil)
      (fmakunbound '#5#)
      (makunbound '#7#))))
