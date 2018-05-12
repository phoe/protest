;;;; src/parachute/macros.lisp

(in-package #:protest/for-parachute)

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
       'test-case-multiple-value-comparison-result
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
       'test-case-multiple-value-comparison-result
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
    (make-instance
     'test-case-comparison-result
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
    (make-instance
     'test-case-finishing-result
     :expression '(finish ,form)
     :body (lambda () ,form)
     ,@(when description
         `(:description (format NIL ,description ,@format-args))))))
