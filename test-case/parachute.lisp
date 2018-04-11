;;;; test-case/parachute.lisp

(in-package #:protest/test-case)

(defmacro define-test (name &body arguments-and-body)
  (unless (gethash name *test-cases*)
    (protocol-error "Test case named ~S was not found. ~
Use DEFINE-TEST-CASE first." name))
  `(let ((,*define-test-closure-symbol* ,name))
     (declare (ignorable ,*define-test-closure-symbol*))
     (parachute:define-test ,name ,@arguments-and-body)))

(defun test-step-macro-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((form (read stream t nil t))
        (name *define-test-closure-symbol*))
    `(let* ((*current-test-case* (gethash ,name *test-cases*))
            (*current-test-step*
              (when *current-test-case*
                (gethash ,arg (steps *current-test-case*)))))
       ,form)))

(defreadtable protest
  (:merge :standard)
  (:dispatch-macro-char #\# #\? 'test-step-macro-reader))

(uiop:define-package #:protest/parachute
  (:use)
  (:mix #:protest/test-case #:parachute)
  (:reexport #:protest/test-case #:parachute))
