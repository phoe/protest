;;;; test/readtable.lisp

(in-package #:protest)

(defun test-step-macro-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((form (read stream)))
    `(progn
       (assert *current-step* ()
               "The #? macro must be used inside DEFINE-TEST.")
       (setf *current-step* ,arg)
       (multiple-value-prog1 ,form
         (setf *current-step* ,(+ arg 1/2))))))

(defreadtable protest
  (:merge :standard)
  (:dispatch-macro-char #\# #\? 'test-step-macro-reader))
