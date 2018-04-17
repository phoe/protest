;;;; test-case/test.lisp

(in-package #:protest/test-case)

;; TODO write a better test framework, this one doesn't work well
(defmacro with-test ((success-expected-p) &body body)
  (with-gensyms (function warnp failp)
    (once-only (success-expected-p)
      `(multiple-value-prog1 (values)
         (handler-case
             (let ((*error-output* (make-string-output-stream))
                   (*test-cases* (make-hash-table)))
               (multiple-value-bind (,function ,warnp ,failp)
                   (compile nil '(lambda () ,@body))
                 (when (and ,success-expected-p
                            (or ,warnp ,failp))
                   (format t "Errors/warnings when compiling tests:~%~A"
                           (get-output-stream-string *error-output*)))
                 (when (and (null ,warnp) (null ,failp))
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
      (assert (null (documentation test-case 'test-case)))
      (assert (null (tags test-case)))
      (assert (null (attachments test-case)))
      (assert (null (hash-table-alist (steps test-case))))))
  (with-test (t) (define-test-case #.(gensym) ())))

(defun test-test-case-define-detailed ()
  (with-test (t)
    (define-test-case #1=#.(gensym) (:attachments (#2="haha")
                                     :tags (#3=#.(gensym))
                                     :documentation "asdf"))
    (let ((test-case (gethash '#1# *test-cases*)))
      (assert (string= "asdf" (documentation test-case 'test-case)))
      (assert (equal '(#3#) (tags test-case)))
      (assert (= 1 (length (attachments test-case))))
      (assert (string= "haha" (first (attachments test-case))))
      (assert (null (steps-list test-case))))))

(defun test-test-case-define-invalid-name ()
  (with-test (nil) (define-test-case 2 ()))
  (with-test (t) (define-test-case "TEST-CASE" ()))
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
  (with-test (t)
    (define-test-case #1=#.(gensym) ()
      #2=#.(gensym)
      10 "ten"
      20 "twenty"
      #3=#.(gensym)
      30 "thirty")
    (let* ((test-case (gethash '#1# *test-cases*))
           (steps (steps-list test-case))
           (first (first steps))
           (second (second steps))
           (third (third steps)))
      (assert (= 10 (id first)))
      (assert (= 20 (id second)))
      (assert (= 30 (id third)))
      (assert (eq '#2# (test-phase first)))
      (assert (eq '#2# (test-phase second)))
      (assert (eq '#3# (test-phase third)))
      (assert (string= "ten" (description first)))
      (assert (string= "twenty" (description second)))
      (assert (string= "thirty" (description third))))))
