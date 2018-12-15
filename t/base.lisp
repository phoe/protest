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
    (remove-protocol-object '#2#)
    (setf (find-class '#2#) nil)))

(define-protest-test test-protocol-class-instantiate
  (unwind-protect
       (progn
         (define-protocol-class #2=#.(gensym) () ())
         (signals protocol-error (make-instance '#2#)))
    (remove-protocol-object '#2#)
    (setf (find-class '#2#) nil)))

(define-protest-test test-protocol-class-remove
  (flet ((count-methods () (length (c2mop:generic-function-methods
                                    #'c2mop:ensure-class-using-class))))
    (let ((method-count (count-methods)))
      (unwind-protect
           (progn
             (define-protocol-class #2=#.(gensym) () ())
             (is (= (1+ method-count) (count-methods)))
             (define-protocol-class #2# () ())
             (is (= (1+ method-count) (count-methods)))
             (define-protocol-class #2# () ())
             (is (= (1+ method-count) (count-methods))))
        (remove-protocol-object '#2#)
        (is (= method-count (count-methods)))
        (setf (find-class '#2#) nil)))))

(define-protest-test test-protocol-class-redefine
  (flet ((count-methods () (length (c2mop:generic-function-methods
                                    #'c2mop:ensure-class-using-class))))
    (let ((method-count (count-methods)))
      (unwind-protect
           (progn
             (define-protocol-class #2=#.(gensym) () ())
             (signals protocol-error (make-instance '#2#))
             (defclass #2# () ())
             (is (= method-count (count-methods)))
             (make-instance '#2#))
        (setf (find-class '#2#) nil)))))

(define-protest-test #1=test-protocol-condition-type-define
  (unwind-protect
       (define-protocol-condition-type #2=#.(gensym) () ())
    (remove-protocol-object '#2#)
    (setf (find-class '#2#) nil)))

(define-protest-test #1=test-protocol-condition-type-instantiate
  ;; https://bugs.launchpad.net/sbcl/+bug/1761735
  ;; For quasi-portability, use MAKE-INSTANCE instead of MAKE-CONDITION
  ;; in client code.
  (unwind-protect
       (progn
         (define-protocol-condition-type #2=#.(gensym) () ())
         (signals protocol-error (make-instance '#2#)))
    (remove-protocol-object '#2#)
    (setf (find-class '#2#) nil)))

(define-protest-test test-protocol-condition-type-remove
  (flet ((count-methods () (length (c2mop:generic-function-methods
                                    #'c2mop:ensure-class-using-class))))
    (let ((method-count (count-methods)))
      (unwind-protect
           (progn
             (define-protocol-condition-type #2=#.(gensym) () ())
             (is (= (1+ method-count) (count-methods)))
             (define-protocol-condition-type #2# () ())
             (is (= (1+ method-count) (count-methods)))
             (define-protocol-condition-type #2# () ())
             (is (= (1+ method-count) (count-methods))))
        (remove-protocol-object '#2#)
        (is (= method-count (count-methods)))
        (setf (find-class '#2#) nil)))))

(define-protest-test test-protocol-condition-type-redefine
  (flet ((count-methods () (length (c2mop:generic-function-methods
                                    #'c2mop:ensure-class-using-class))))
    (let ((method-count (count-methods)))
      (unwind-protect
           (progn
             (define-protocol-condition-type #2=#.(gensym) () ())
             (signals protocol-error (make-instance '#2#))
             (define-condition #2# () ())
             (is (= method-count (count-methods)))
             (make-instance '#2#))
        (setf (find-class '#2#) nil)))))
