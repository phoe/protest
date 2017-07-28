;;;; test/macros.lisp

(in-package #:cl-protest)

(defmacro define-test-case
    (&whole whole test-case-name options &body steps)
  (declare (ignore options steps))
  `(let ((data (cdr ',whole))
         (value (find ',test-case-name *test-cases* :key #'car)))
     (unless (equal data value)
       (when value
         (warn "Redefining ~S in DEFINE-TEST-CASE" ',test-case-name))
       (setf *test-cases*
             (cons data
                   (if value
                       (remove ',test-case-name *test-cases* :key #'car)
                       *test-cases*))))))

(defmacro define-test (test-name &body body)
  (let ((test-case (find test-name *test-cases* :key #'car)))
    (unless test-case
      (error "Test case named ~A not found. Use DEFINE-TEST-CASE first."
             test-name))
    `(setf (gethash ',test-name *tests*)
           ,(make-test-function test-case body))))
