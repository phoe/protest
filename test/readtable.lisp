;;;; test/readtable.lisp

(in-package #:cl-protest)

(defun test-step-macro-reader (stream subchar arg)
  (declare (ignore subchar))
  (assert *current-step* () "The #? macro must be used inside DEFINE-TEST.")
  (let ((form (read stream)))
    `(progn
       (setf *current-step* ,arg)
       ,form
       (setf *current-step* ,(+ arg 1/2)))))

(defreadtable cl-protest
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'test-step-macro-reader))
