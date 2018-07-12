;;;; src/1am/macro.lisp

(in-package #:protest/1am)

(defvar *current-step-id* nil)

(defun test-step-macro-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((form (read stream t nil t)))
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
    `(let* ((test-case (find-test-case ,test-case-name ,package-name))
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
        `(1am:test ,symbol ,(make-test-function string *package* body)))))
  (setf (macro-function 'define-test) (macro-function 'test)))
