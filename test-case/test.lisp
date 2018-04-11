;;;; test-case/test.lisp

(in-package #:protest/test-case)

(defmacro with-test ((success-expected-p) &body body)
  (with-gensyms (function warnp failp)
    (once-only (success-expected-p)
      `(multiple-value-prog1 (values)
         (handler-case
             (let ((*error-output* (make-broadcast-stream))
                   (*test-cases* (make-hash-table)))
               (multiple-value-bind (,function ,warnp ,failp)
                   (compile nil '(lambda () ,@body))
                 (declare (ignore ,warnp))
                 (when (null ,failp)
                   (funcall ,function)
                   (when (not ,success-expected-p)
                     (error "Test failure: unexpected success.")))))
           (protocol-error (e)
             (declare (ignorable e))
             (when ,success-expected-p
               (error "Test failure: unexpected failure of type ~S:~%~A"
                      (type-of e) e))))))))

(defun test-test-case-define-empty ()
  (with-test (t)
    (define-test-case #1=#.(gensym) ())
    (let ((test-case (gethash '#1# *test-cases*)))
      (assert (null (description test-case)))
      (assert (null (tags test-case)))
      (assert (null (attachments test-case)))
      (assert (null (hash-table-alist (steps test-case))))))
  (with-test (t) (define-test-case #.(gensym) #.(gensym))))

(defun test-test-case-define-detailed ()
  (with-test (t)
    (define-test-case #1=#.(gensym) (:attachments (#2="haha")
                                     :tags (#3=#.(gensym))
                                     :description "asdf"
                                     :export t))
    (let ((test-case (gethash '#1# *test-case*)))
      (assert (string= "asdf" (description test-case)))
      (assert (equal '(#3#) (tags test-case)))
      (assert (= 1 (length (attachments test-case))))
      (assert (string= "haha" (first (attachments test-case))))
      (assert (null (exports test-case)))
      (assert (null (elements test-case))))))

(defun test-test-case-define-invalid-name ()
  (with-test (nil) (define-test-case 2 ()))
  (with-test (nil) (define-test-case "TEST-CASE" ()))
  (with-test (nil) (define-test-case '(#.(gensym) #.(gensym)) ()))
  (with-test (nil) (define-test-case nil ())))

(defun test-test-case-define-invalid-contents ()
  (with-test (nil) (define-test-case #.(gensym) #(1 2 3 4)))
  (with-test (nil) (define-test-case #.(gensym) 1 #(1 2 3 4)))
  (with-test (nil) (define-test-case #.(gensym) 1))
  (with-test (nil) (define-test-case #.(gensym) "yu" #(1 2 3 4))))

(defun test-test-case-define-duplicate-ids ()
  (with-test (nil) (define-test-case #.(gensym) 1 "a" 1 "b")))

(defun test-test-case-define-duplicate-phases ()
  (with-test (nil) (define-test-case #.(gensym) #1=#.(gensym) 1 "a" #1# 2 "b")))

(defun test-test-case-define-ids-not-in-order ()
  (with-test (nil) (define-test-case #.(gensym) 2 "a" 1 "b")))

(defun test-test-case-define-complex ()
  ;; TODO implement
  )
