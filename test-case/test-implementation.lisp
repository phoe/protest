;;;; test-case/test-implementation.lisp

(in-package #:protest/test-case)

;; TODO split /TEST-CASE into /TEST-CASE and /PARACHUTE
(defparameter *define-test-closure-symbol*
  (gensym "PROTEST-SYNTAX-USED-OUTSIDE-DEFINE-TEST")
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

(defmacro true (form &optional description &rest format-args)
  `(parachute:eval-in-context
    parachute:*context*
    (make-instance
     'test-case-comparison-result
     :expression '(true ,form)
     :value-form ',form
     :body (lambda () ,form)
     :expected 'T
     :comparison 'geq
     ,@(when description
         `(:description (format NIL ,description ,@format-args))))))

(defmacro false (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance
     'test-case-comparison-result
     :expression '(false ,form)
     :value-form ',form
     :body (lambda () ,form)
     :expected 'NIL
     :comparison 'geq
     ,@(when description
         `(:description (format NIL ,description ,@format-args))))))

(defmacro is (comp expected form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance
     'test-case-comparison-result
     :expression '(is ,comp ,expected ,form)
     :value-form ',form
     :body (lambda () ,form)
     :expected ,expected
     :comparison ,(parachute::maybe-quote comp)
     ,@(when description
         `(:description (format NIL ,description ,@format-args))))))

(defmacro isnt (comp expected form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance
     'test-case-comparison-result
     :expression '(is ,comp ,expected ,form)
     :value-form ',form
     :body (lambda () ,form)
     :expected ,expected
     :comparison ,(parachute::maybe-quote comp)
     :comparison-geq NIL
     ,@(when description
         `(:description (format NIL ,description ,@format-args))))))

(defmacro is-values (form &body body)
  (multiple-value-bind (comp-expected expected comparison description format-args)
      (parachute::destructure-is-values-body body)
    `(eval-in-context
      *context*
      (make-instance
       'multiple-value-test-case-comparison-result
       :expression '(is-values ,form ,@comp-expected)
       :value-form ',form
       :body (lambda () ,form)
       :expected (list ,@expected)
       :comparison (list ,@comparison)
       ,@(when description
           `(:description (format NIL ,description ,@format-args)))))))

(defmacro isnt-values (form &body body)
  (multiple-value-bind (comp-expected expected comparison description format-args)
      (parachute::destructure-is-values-body body)
    `(eval-in-context
      *context*
      (make-instance
       'multiple-value-test-case-comparison-result
       :expression '(isnt-values ,form ,@comp-expected)
       :value-form ',form
       :body (lambda () ,form)
       :expected (list ,@expected)
       :comparison (list ,@comparison)
       :comparison-geq NIL
       ,@(when description
           `(:description (format NIL ,description ,@format-args)))))))

(defmacro fail (form &optional (type 'error) description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance
     'test-case-comparison-result
     :expression '(fail ,form ,type)
     :value-form '(capture-error ,form)
     :body (lambda () (capture-error ,form ,(parachute::maybe-unquote type)))
     :expected ',(parachute::maybe-unquote type)
     :comparison 'typep
     ,@(when description
         `(:description (format NIL ,description ,@format-args))))))

(defmacro of-type (type form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'test-case-comparison-result
                   :expression '(of-type ,type ,form)
                   :value-form ',form
                   :body (lambda () ,form)
                   :expected ',(parachute::maybe-unquote type)
                   :comparison 'typep
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))

(defmacro finish (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'test-case-finishing-result
                   :expression '(finish ,form)
                   :body (lambda () ,form)
                   ,@(when description
                       `(:description (format NIL ,description ,@format-args))))))











(defmacro define-test (name &body arguments-and-body)
  (unless (gethash name *test-cases*)
    (protocol-error "Test case named ~S was not found. ~
Use DEFINE-TEST-CASE first." name))
  `(let ((,*define-test-closure-symbol* ,name))
     ;;(declare (ignorable ,*define-test-closure-symbol*))
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
