;;;; test/functions.lisp

(in-package #:cl-protest)

(defun analyze-test-steps (steps)
  (let ((result ())
        (step-number 0)
        (phase nil))
    (do ((elt (pop steps) (pop steps)))
        ((null steps)
         (nreverse result))
      (etypecase elt
        (keyword (setf phase elt))
        (unsigned-byte
         (when (/= elt (incf step-number))
           (error "Test step ~D is out of order." elt))
         (let ((description (pop steps)))
           (unless (stringp description)
             (error "The description for step ~D is not a string." elt))
           (push (list elt description phase) result)))))))

(defun make-test-function (test-case test-body)
  (compile
   nil
   `(lambda ()
      (let ((step-data ',(analyze-test-steps (cddr test-case)))
            (*current-step* 0)
            (test-name ',(car test-case)))
        (handler-case
            (progn ,@test-body)
          (error (e)
            (cond
              ((= *current-step* 0)
               (failure-before test-name e))
              ((= 1/2 (nth-value 1 (truncate *current-step*)))
               (failure-after *current-step* step-data test-name e))
              ((typep *current-step* 'unsigned-byte)
               (failure-during *current-step* step-data test-name e))
              (t
               (failure-internal *current-step*)))))))))
