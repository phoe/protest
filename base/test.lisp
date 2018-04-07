;;;; base/test.lisp

(in-package #:protest/base)

(defun test ()
  (mapc #'funcall
        '(test-protocol-class-define
          test-protocol-class-instantiate
          test-protocol-condition-type-define
          test-protocol-condition-type-instantiate))
  (values))

(defun test-protocol-class-define ()
  (unwind-protect
       (define-protocol-class #1=#.(gensym) () ())
    (setf (find-class '#1#) nil))
  (values))

(defun #1=test-protocol-class-instantiate ()
  (unwind-protect (progn
                    (define-protocol-class #2=#.(gensym) () ())
                    (handler-case
                        (progn (make-instance '#2#)
                               (error "Test failure in ~A." '#1#))
                      (protocol-error ())))
    (setf (find-class '#2#) nil))
  (values))

;; https://bugs.launchpad.net/sbcl/+bug/1761950
(defun #1=test-protocol-condition-type-define ()
  #+sbcl (format t "~A broken on SBCL; skipping.~%" '#1#)
  #-sbcl
  (unwind-protect
       (define-protocol-condition-type #2=#.(gensym) () ())
    (setf (find-class '#2#) nil))
  (values))

;; https://bugs.launchpad.net/sbcl/+bug/1761950
(defun #1=test-protocol-condition-type-instantiate ()
  #+sbcl (format t "~A broken on SBCL; skipping.~%" '#1#)
  #-sbcl
  (unwind-protect (progn
                    (define-protocol-condition-type #2=#.(gensym) () ())
                    (handler-case
                        (progn (make-condition '#2#)
                               (error "Test failure in ~A." '#1#))
                      (protocol-error ())))
    (setf (find-class '#2#) nil))
  (values))
