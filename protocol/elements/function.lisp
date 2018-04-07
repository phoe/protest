;;;; protocol/elements/function.lisp

(in-package #:protest/protocol)

(defclass protocol-function (protocol-operation)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%lambda-list :accessor lambda-list
                 :initarg :lambda-list
                 :initform (error "Must provide LAMBDA-LIST."))
   (%return-type :accessor return-type
                 :initarg :return-type
                 :initform t))
  (:documentation "Describes a generic function that is a part of a protocol."))

(defmethod generate-element ((type (eql :function)) &rest form)
  (destructuring-bind (name lambda-list . rest) form
    (declare (ignore rest))
    (assert (or (and (not (null name)) (symbolp name))
                (and (listp name) (= (length name) 2) (eq (first name) 'setf)))
            () "Wrong thing to be a function name: ~S" name)
    (parse-ordinary-lambda-list lambda-list :allow-specializers t)
    (let ((element (make-instance 'protocol-function :name name
                                                     :lambda-list lambda-list)))
      (when (<= 3 (length form))
        (let ((return-type (third form)))
          (setf (return-type element) return-type)))
      element)))

(defmethod embed-documentation ((element protocol-function) (string string))
  (setf (documentation (name element) 'function) string))

(defmethod generate-forms ((element protocol-function))
  (let* ((name (name element))
         (return-type (return-type element))
         (documentation (documentation name 'function)))
    `((:function ,name ,(lambda-list element)
                 ,@(unless (eq t return-type)
                     `(,return-type)))
      ,@(when documentation `(,documentation)))))
