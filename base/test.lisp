;;;; base/test.lisp

(in-package #:protest/base)

(defun #1=test-protocol-class-instantiate ()
  (unwind-protect (progn
                    (define-protocol-class #2=#.(gensym) () ())
                    (handler-case
                        (progn (make-instance '#2#)
                               (error "Test failure in ~A." '#1#))
                      (protocol-error ())))
    (setf (find-class '#2#) nil))
  (values))

(defun #1=test-protocol-condition-type-define ()
  ;; https://bugs.launchpad.net/sbcl/+bug/1761950
  #+sbcl (format t "~&~A broken on SBCL; skipping.~&" '#1#)
  #-sbcl
  (unwind-protect
       (define-protocol-condition-type #2=#.(gensym) () ())
    (setf (find-class '#2#) nil))
  (values))

(defun #1=test-protocol-condition-type-instantiate ()
  ;; https://bugs.launchpad.net/sbcl/+bug/1761950
  #+sbcl (format t "~&~A broken on SBCL; skipping.~&" '#1#)
  #-sbcl
  (unwind-protect (progn
                    (define-protocol-condition-type #2=#.(gensym) () ())
                    (handler-case
                        (progn (make-condition '#2#)
                               (error "Test failure in ~A." '#1#))
                      (protocol-error ())))
    (setf (find-class '#2#) nil))
  (values))
