;;;; t/base.lisp

(defpackage #:protest/test/base
  (:use #:cl
        #:protest
        #:protest/test))

(in-package #:protest/test/base)

(register-test-package)

(define-protest-test test-protocol-class-define
  (unwind-protect
       (define-protocol-class #2=#.(gensym) () ())
    (setf (find-class '#2#) nil)))

(define-protest-test test-protocol-class-instantiate
  (unwind-protect
       (progn
         (define-protocol-class #2=#.(gensym) () ())
         (signals protocol-error (make-instance '#2#)))
    (setf (find-class '#2#) nil)))

(define-protest-test #1=test-protocol-condition-type-define
  ;; https://bugs.launchpad.net/sbcl/+bug/1761950
  #+sbcl (format t "~&~A broken on SBCL; skipping.~&" '#1#)
  #-sbcl
  (unwind-protect
       (define-protocol-condition-type #2=#.(gensym) () ())
    (setf (find-class '#2#) nil)))

(define-protest-test #1=test-protocol-condition-type-instantiate
  ;; https://bugs.launchpad.net/sbcl/+bug/1761950
  #+sbcl (format t "~&~A broken on SBCL; skipping.~&" '#1#)
  #-sbcl
  (unwind-protect
       (progn
         (define-protocol-condition-type #2=#.(gensym) () ())
         (signals protocol-error (make-condition '#2#)))
    (setf (find-class '#2#) nil)))
