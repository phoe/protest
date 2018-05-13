;;;; src/protocol/elements/function.lisp

(in-package #:protest/protocol)

(defclass protocol-function (protocol-operation)
  ((%name :reader name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%lambda-list :reader lambda-list
                 :initarg :lambda-list
                 :initform (error "Must provide LAMBDA-LIST."))
   (%return-type :accessor return-type
                 :initarg :return-type
                 :initform '*)
   (%keyword-types :accessor keyword-types
                   :initarg :keyword-types
                   :initform '())
   (%declaim-type-p :accessor declaim-type-p
                    :initform t))
  (:documentation "Describes a generic function that is a part of a protocol.
\
The form for a protocol function consists of the following subforms:
* NAME - mandatory, must be a symbol or a (SETF symbol) form. Denotes the name
  of the function.
* LAMBDA-LIST - mandatory, must be a valid lambda list.
* RETURN-TYPE - optional, must be a valid return type for a function. If not
  specified, defaults to the symbol CL:*.
* KEYWORD-TYPES - optional, must be a valid plist containing some or all of the
  &KEY arguments used in LAMBDA-LIST along with their respective types."))
;;; TODO maybe add means of passing default methods/arguments to DEFGENERIC

(defmethod generate-element
    ((type (eql :function)) details &optional (declaim-type-p t))
  (destructuring-bind (name lambda-list . rest) details
    (declare (ignore rest))
    (assert (or (and (not (null name)) (symbolp name))
                (and (listp name) (= (length name) 2) (eq (first name) 'setf)))
            () "Wrong thing to be a function name: ~S" name)
    (parse-ordinary-lambda-list lambda-list :allow-specializers t)
    (let ((element (make-instance 'protocol-function :name name
                                                     :lambda-list lambda-list)))
      (setf (declaim-type-p element) declaim-type-p)
      (when (<= 3 (length details))
        (let ((return-type (third details)))
          (setf (return-type element) return-type)))
      (when (<= 4 (length details))
        (let ((keyword-types (fourth details)))
          (setf (keyword-types element) keyword-types)))
      element)))

(defmethod generate-forms ((element protocol-function))
  (let* ((name (name element))
         (return-type (return-type element))
         (documentation (docstring element)))
    `((:function ,name ,(lambda-list element)
                 ,@(unless (eq t return-type)
                     `(,return-type)))
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-function))
  (with-accessors
        ((name name) (lambda-list lambda-list)
         (return-type return-type) (keyword-types keyword-types))
      element
    (let ((ftype-args (ftype-args lambda-list keyword-types))
          (documentation (docstring element)))
      `(,@(when (declaim-type-p element)
            `((declaim (ftype (function ,ftype-args ,return-type) ,name))))
        (defgeneric? ,name ,(c2mop:extract-lambda-list lambda-list)
          ,@(when documentation `((:documentation ,documentation))))))))

(defmacro defgeneric? (name lambda-list &body options)
  (if (or (not (fboundp name))
          (not (typep (fdefinition name) 'generic-function)))
      `(defgeneric ,name ,lambda-list ,@options)
      `(progn)))

(defun ftype-args (lambda-list keyword-types)
  ;; TODO test the hell out of this
  ;; TODO this shit is broken
  (loop with keyword = nil
        with repeatp = t
        for elt = (pop lambda-list)
        while repeatp
        if (null lambda-list)
          do (setf repeatp nil)
        if (eq elt '&key)
          collect elt
          and do (setf keyword '&key)
        if (eq elt '&allow-other-keys)
          collect elt
        else if (eq elt '&rest)
               collect '&rest
        ;;and collect 't
               and do (pop lambda-list)
        else if (eq elt '&optional)
               collect '&optional
               and do (setf keyword '&optional)
        else if (member elt lambda-list-keywords)
               do (setf keyword elt)
        else if (eq keyword '&key)
               collect (let* ((keyword (make-keyword elt)))
                         (multiple-value-bind (key value tail)
                             (get-properties keyword-types (list keyword))
                           (declare (ignore key))
                           (list keyword (if tail value 't))))
        else if (symbolp elt)
               collect 't
        else if (listp elt)
               collect (second elt)
        else do (protocol-error "ftype-args internal error")))
