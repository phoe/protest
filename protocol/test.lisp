;;;; protocol/test.lisp

(in-package #:protest/protocol)

;; (defmacro with-test ((success-expected-p) &body body)
;;   (with-gensyms (function warnp failp)
;;     (once-only (success-expected-p)
;;       `(multiple-value-prog1 (values)
;;          (handler-case
;;              (let ((*error-output* (make-string-output-stream))
;;                    (*protocols* (make-hash-table))
;;                    (*compile-time-protocols* (make-hash-table)))
;;                (multiple-value-bind (,function ,warnp ,failp)
;;                    (compile nil '(lambda () ,@body))
;;                  (when (and ,success-expected-p
;;                             (or ,warnp ,failp))
;;                    (format t "Errors/warnings when compiling tests:~%~A"
;;                            (get-output-stream-string *error-output*)))
;;                  (when (and (null ,warnp) (null ,failp))
;;                    (funcall ,function)
;;                    (when (not ,success-expected-p)
;;                      (error "Test failure: unexpected success.")))))
;;            (protocol-error (e)
;;              (declare (ignorable e))
;;              (when ,success-expected-p
;;                (error "Test failure: unexpected failure of type ~S:~%~A"
;;                       (type-of e) e))))))))

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
                                      :documentation "asdf"
                                      :export t))
      (let ((protocol (gethash '#2# *protocols*)))
        (is (string= "asdf"
                     (documentation protocol 'protocol)))
        (is (equal '(#3#) (tags protocol)))
        (is (equal '(#1#) (dependencies protocol)))
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
  (with-fresh-state (define-protocol #.(gensym) (:dependencies (2))))
  (with-fresh-state (define-protocol #.(gensym) (:dependencies ("ABC"))))
  (with-fresh-state (define-protocol #.(gensym) (:dependencies ((#.(gensym))))))
  (with-fresh-state (define-protocol #.(gensym) (:dependencies ((1 2 3 4)))))
  (with-fresh-state (define-protocol #.(gensym) (:dependencies (nil)))))

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
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:category #1=(:foo :bar)) #2="qwer")
                (eval '(execute-protocol #3#))
                (is (string= #2# (documentation '#1# 'category))))
      (setf (documentation '#1# 'category) nil)
      (is (eq (documentation '#1# 'category) nil)))))

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
      (is (eq (documentation '#1# 'type) nil))
      (setf (find-class '#1#) nil)
      (is (eq (find-class '#1# nil) nil)))))

(define-protest-test test-protocol-define-class-instantiate
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #3=#.(gensym) ()
                  (:class #1=#.(gensym) () ()))
                (eval '(execute-protocol #3#))
                (signals protocol-error
                  (make-instance (find-class '#1#))))
      (setf (find-class '#1#) nil)
      (is (eq (find-class '#1# nil) nil)))))

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
      (is (eq (documentation '#1# 'type) nil))
      (setf (find-class '#1#) nil)
      (is (eq (find-class '#1# nil) nil)))))

(define-protest-test #2=test-protocol-define-condition-type-instantiate
  ;; https://bugs.launchpad.net/sbcl/+bug/1761950
  #+sbcl (format t "~&~A broken on SBCL; skipping.~&" '#2#)
  #-sbcl (with-fresh-state
           (unwind-protect
                (progn (define-protocol #.(gensym) ()
                         (:condition-type #1=#.(gensym) () ()))
                       (make-condition (find-class '#1#)))
             (setf (find-class '#1#) nil))))

(define-protest-test test-protocol-define-config
  (with-fresh-state
    (let* ((variable nil)
           (*configuration-setter*
             (lambda (x y) (declare (ignore x y)) (setf variable t))))
      (unwind-protect
           (progn (define-protocol #.(gensym) ()
                    (:config #1=(:foo :bar) string :mandatory "a")
                    #2="qwer")
                  (assert (string= #2# (documentation '#1# 'config))))
        (setf (documentation '#1# 'config) nil)))))

(define-protest-test test-protocol-define-function
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #.(gensym) ()
                  (:function #1=#.(gensym) (#2=#.(gensym) #3=#.(gensym)) 'string)
                  #4="qwer")
                (assert (fdefinition '#1#))
                (assert (string= #4# (documentation '#1# 'function))))
      (fmakunbound '#1#)
      (setf (documentation '#1# 'function) nil))))

(define-protest-test test-protocol-define-macro
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #.(gensym) ()
                  (:macro #1=#.(gensym) (#2=#.(gensym) #3=#.(gensym)))
                  #4="qwer")
                (assert (string= #4# (documentation '#1# 'function))))
      (fmakunbound '#1#)
      (setf (documentation '#1# 'function) nil))))

(define-protest-test test-protocol-define-variable
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #.(gensym) ()
                  (:variable #1=#.(gensym) string #2="asdf")
                  #3="qwer")
                (assert (string= (symbol-value '#1#) #2#))
                (assert (string= #3# (documentation '#1# 'variable))))
      (makunbound '#1#)
      (setf (documentation '#1# 'variable) nil))))

(define-protest-test test-protocol-define-complex
  (with-fresh-state
    (unwind-protect
         (progn (define-protocol #.(gensym) ()
                  (:category #1=(:foo :bar))
                  (:class #2=#.(gensym) () ())
                  (:condition-type #3=#.(gensym) () ())
                  (:config #4=(:foo :bar :baz) t :mandatory "a")
                  (:function #5=#.(gensym) ())
                  (:macro #6=#.(gensym) ())
                  (:variable #7=#.(gensym) t "asdf"))
                (assert (find-class '#2#))
                (assert (find-class '#3#))
                (assert (fdefinition '#5#))
                (assert (string= "asdf" (symbol-value '#7#))))
      (setf (find-class '#2#) nil
            (find-class '#3#) nil)
      (fmakunbound '#5#)
      (makunbound '#7#))))
