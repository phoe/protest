;;;; test-case/test.lisp

(defpackage #:protest/test/test-case
  (:use #:cl
        #:protest
        #:protest/test))

(in-package #:protest/test/test-case)

(register-test-package)

(defmacro with-fresh-state (&body body)
  `(let ((*test-cases* (make-hash-table :test #'equal)))
     ,@body
     (values)))

(define-protest-test test-test-case-define-empty
  (with-fresh-state
    (define-test-case #1=#.(gensym) ())
    (let ((test-case (find-test-case '#1#)))
      (is (null (documentation test-case 'test-case)))
      (is (null (tags test-case)))
      (is (null (attachments test-case)))
      (is (null (steps-list test-case)))))
  (with-fresh-state
    (define-test-case "TEST-CASE" ()))
  (with-fresh-state
    (define-test-case #.(gensym) ())))

(define-protest-test test-test-case-define-detailed
  (with-fresh-state
    (define-test-case #1=#.(gensym) (:attachments (#2="haha")
                                     :tags (#3=#.(gensym))
                                     :documentation "asdf"))
    (let ((test-case (find-test-case '#1#)))
      (is (string= "asdf" (documentation test-case 'test-case)))
      (is (equal '(#3#) (tags test-case)))
      (is (= 1 (length (attachments test-case))))
      (is (string= "haha" (first (attachments test-case))))
      (is (null (steps-list test-case))))))

(define-protest-test test-test-case-define-invalid-name
  (with-fresh-state
    (signals protocol-error (define-test-case 2 ())))
  (with-fresh-state
    (signals protocol-error (define-test-case '(#.(gensym) #.(gensym)) ())))
  (with-fresh-state
    (signals protocol-error (define-test-case nil ()))))

(define-protest-test test-test-case-define-invalid-contents ()
  (with-fresh-state
    (signals protocol-error (define-test-case #.(gensym) () #(1 2 3 4))))
  (with-fresh-state
    (signals protocol-error (define-test-case #.(gensym) () 1 #(1 2 3 4))))
  (with-fresh-state
    (signals protocol-error (define-test-case #.(gensym) () 1)))
  (with-fresh-state
    (signals protocol-error (define-test-case #.(gensym) () "yu" #(1 2 3 4)))))

(define-protest-test test-test-case-define-duplicate-ids
  (with-fresh-state
    (signals protocol-error (define-test-case #.(gensym) () 1 "a" 1 "b"))))

(define-protest-test test-test-case-define-duplicate-phases
  (with-fresh-state
    (signals protocol-error
      (define-test-case #.(gensym) () #1=#.(gensym) () 1 "a" #1# 2 "b"))))

(define-protest-test test-test-case-define-ids-not-in-order
  (with-fresh-state
    (signals protocol-error (define-test-case #.(gensym) () 2 "a" 1 "b"))))

(define-protest-test test-test-case-define-complex
  (with-fresh-state
    (define-test-case #1=#.(gensym) ()
      #2=#.(gensym)
      10 "ten"
      20 "twenty"
      #3=#.(gensym)
      30 "thirty")
    (let* ((test-case (find-test-case '#1#))
           (steps (steps-list test-case))
           (first (first steps))
           (second (second steps))
           (third (third steps)))
      (is (= 10 (id first)))
      (is (= 20 (id second)))
      (is (= 30 (id third)))
      (is (eq '#2# (test-phase first)))
      (is (eq '#2# (test-phase second)))
      (is (eq '#3# (test-phase third)))
      (is (string= "ten" (description first)))
      (is (string= "twenty" (description second)))
      (is (string= "thirty" (description third))))))
