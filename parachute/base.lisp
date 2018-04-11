;;;; parachute/base.lisp

(in-package #:protest/for-parachute)

(defvar *define-test-closure-symbol*
  (gensym "PROTEST-SYNTAX-USED-OUTSIDE-DEFINE-TEST")
  "A symbol used for generating lexical bindings for closures inside
DEFINE-TEST. Must NEVER be proclaimed special.")

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
  (with-slots (test-case-name test-phase id) result
    (let ((prologue (format nil "In test case ~A, phase ~A, step ~A:~%"
                            test-case-name test-phase id)))
      (concatenate 'string prologue (call-next-method)))))

(defvar *last-printed-phase* nil)

(defvar *printing-protest-report* nil)

(defmethod parachute:eval-in-context :around
    ((report parachute:plain) (result parachute:parent-result))
  (let* ((*printing-protest-report* t))
    (call-next-method)))

(defmethod parachute:report-on :before
    ((result test-case-result) (report parachute:plain))
  (when *printing-protest-report*
    (alexandria:when-let ((phase (test-phase result)))
      (unless (eq phase *last-printed-phase*)
        (setf *last-printed-phase* phase)
        (format (parachute:output report)
                "             #~v@{  ~} Phase ~S~%"
                parachute::*level* phase)))
    (format (parachute:output report) "~4D " (id result))))

(defmethod parachute:report-on :around
    ((result parachute:result) (report parachute:plain))
  (when *printing-protest-report*
    (unless (typep result 'test-case-result)
      (format (parachute:output report) "     ")))
  (call-next-method))

(defclass test-case-comparison-result
    (test-case-result parachute:comparison-result) ())

(defclass test-case-multiple-value-comparison-result
    (test-case-result parachute:multiple-value-comparison-result) ())

(defclass test-case-finishing-result
    (test-case-result parachute:finishing-result) ())

(defun test-step-macro-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((form (read stream t nil t))
        (name *define-test-closure-symbol*))
    `(let* ((*current-test-case* (gethash ,name *test-cases*))
            (*current-test-step*
              (when *current-test-case*
                (gethash ,arg (steps *current-test-case*)))))
       ,form)))

(defreadtable protest/parachute
  (:merge :standard)
  (:dispatch-macro-char #\# #\? 'test-step-macro-reader))

(defmacro define-test (name &body arguments-and-body)
  (unless (gethash name *test-cases*)
    (protocol-error "Test case named ~S was not found. ~
Use DEFINE-TEST-CASE first." name))
  `(let ((,*define-test-closure-symbol* ',name))
     (declare (ignorable ,*define-test-closure-symbol*))
     (parachute:define-test ,name ,@arguments-and-body)))
