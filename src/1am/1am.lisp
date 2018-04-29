;;;; src/1am/1am.lisp

(defpackage #:protest/1am
  (:use #:cl
        #:named-readtables
        #:protest/base
        #:protest/test-case)
  (:import-from #:1am #:is #:signals #:run #:*tests*)
  (:export #:define-test #:test #:is #:signals #:run #:*tests*))

(in-package #:protest/1am)

(defvar *current-test* nil)

(defvar *current-step-id* nil)

(defun failure-before (test-case condition)
  (let ((string "Test failure in test ~S, before step 1:~%~A")
        (test-name (name test-case)))
    (format nil string test-name condition)))

(defun failure-during (test-case test-step condition)
  (let ((string "Test failure in test ~A, phase ~A, step ~D:~%~A~%~A")
        (test-name (name test-case)) (phase (test-phase test-step))
        (id (id test-step)) (description (description test-step)))
    (format nil string test-name phase id description condition)))

(defun failure-after (test-case test-step condition)
  (let ((string "Test failure in test ~A, phase ~A, after step ~D:~%~A~%~A")
        (test-name (name test-case)) (phase (test-phase test-step))
        (id (id test-step)) (description (description test-step)))
    (format nil string test-name phase id description condition)))

(defun failure-internal (step-id e)
  (format nil "*CURRENT-TEST-STEP* has an illegal value: ~S
This is an internal PROTEST/TEST-CASE error.~%~A" step-id e))

(defun test-step-macro-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((form (read stream)))
    `(progn
       (assert *current-step-id* ()
               "The #? macro must be used inside a test definition.")
       (setf *current-step-id* ,arg)
       (multiple-value-prog1 ,form
         (setf *current-step-id* ,(+ arg 1/2))))))

(defreadtable protest/1am
  (:merge :standard)
  (:dispatch-macro-char #\# #\? 'test-step-macro-reader))

(defun make-test-function (test-case-name package test-body)
  (let ((package-name (package-name (find-package package))))
    `(let* ((test-case (find-test-case name ,package-name))
            (*current-step-id* 0))
       (assert test-case ()
               "Test case with name ~S was not found." ,test-case-name)
       (flet ((current-step ()
                (gethash (truncate *current-step-id*) (steps test-case))))
         (handler-bind
             ((error (lambda (e)
                       (cond
                         ((= *current-step-id* 0)
                          (error (failure-before test-case e)))
                         ((= 1/2 (nth-value 1 (truncate *current-step-id*)))
                          (error (failure-after test-case (current-step) e)))
                         ((typep *current-step-id* 'unsigned-byte)
                          (error (failure-during test-case (current-step) e)))
                         (t
                          (error (failure-internal *current-step-id* e)))))))
           (progn ,@test-body))))))

(progn
  (defmacro test (name &body body)
    (let* ((string (string name)))
      (assert (find-test-case name *package*) ()
              "Test case with name ~S was not found. ~
Use DEFINE-TEST-CASE first." string)
      (let ((symbol (intern string *package*)))
        `(1am:test ,symbol ,(make-test-function *package* string body)))))
  (setf (macro-function 'define-test) (macro-function 'test)))

;; TODO document this
