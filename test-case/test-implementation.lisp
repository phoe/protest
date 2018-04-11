;;;; test-case/test-implementation.lisp

(in-package #:protest/test-case)

(defvar *define-test-closure-symbol* (gensym)
  "A symbol used for generating lexical bindings for closures inside
DEFINE-TEST. Must NOT be proclaimed special.")

(defvar *current-test-case* nil
  "The test case that is currently being executed.")

(defvar *current-test-step* nil
  "The test step that is currently being executed.")

(defclass test-case-result (parachute:value-result)
  ((test-case-name :initarg :test-case-name :accessor test-case-name)
   (id :initarg :id :accessor id)
   (test-phase :initarg :test-phase :accessor test-phase))
  (:default-initargs
   :test-case-name (if *current-test-case* (name *current-test-case*) "unknown")
   :id (if *current-test-step* (id *current-test-step*) "unknown")
   :description (if *current-test-step* (description *current-test-step*) nil)
   :test-phase (if *current-test-step* (test-phase *current-test-step*)
                   "unknown")))

(defmethod parachute:format-result
    ((result test-case-result) (type (eql :extensive)))
  (concatenate 'string
               (format nil "In test case ~A, phase ~A, step ~A:~%"
                       (test-case-name result) (test-phase result) (id result))
               (call-next-method)))

(defclass test-case-comparison-result
    (test-case-result parachute:comparison-result) ())

(defclass test-case-multiple-value-comparison-result
    (test-case-result parachute:multiple-value-comparison-result) ())

(defclass test-case-finishing-result
    (test-case-result parachute:finishing-result) ())

(defmacro define-test (name &body arguments-and-body)
  (unless (gethash name *test-cases*)
    (protocol-error "Test case named ~S was not found. ~
Use DEFINE-TEST-CASE first." name))
  `(let ((,*define-test-closure-symbol* ,name))
     (parachute:define-test ,name ,@arguments-and-body)))

(defun test-step-macro-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((form (read stream t nil t))
        (symbol *define-test-closure-symbol*))
    ;; (handler-case (let ((*error-output* (make-broadcast-stream)))
    ;;                 (funcall (compile nil `(lambda () ,symbol))))
    ;;   (unbound-variable ()
    ;;     (protocol-error
    ;;      "The #? reader macro must be used inside DEFINE-TEST.")))
    (with-gensyms (test-case test-step foundp)
      `(multiple-value-bind (,test-case ,foundp)
           (gethash ,symbol *test-cases*)
         (unless ,foundp (protocol-error "Test case ~S was not found. ~
Use DEFINE-TEST-CASE first." ,symbol))
         (multiple-value-bind (,test-step ,foundp)
             (gethash ,arg (steps ,test-case))
           (unless ,foundp (protocol-error "Step with ID ~D was not found ~
in test case ~S." ,arg (name ,test-case)))
           (let ((*current-test-case* ,test-case)
                 (*current-test-step* ,test-step))
             ,form))))))

(defreadtable protest
  (:merge :standard)
  (:dispatch-macro-char #\# #\? 'test-step-macro-reader))
