;;;; src/ftype/ftype.lisp

(defun function-ftype-declaration-form
    (typed-lambda-list &optional (result-type t) (keyword-types '()))
  "Given a lambda list annotated with types and a function result type, produces
a form suitable for usage inside FTYPE declarations."
  (check-type typed-lambda-list proper-list)
  (check-type result-type (or symbol proper-list))
  `(function ,(lambda-list-argument-types typed-lambda-list keyword-types)
             ,result-type))

(defun lambda-list-argument-types (lambda-list keyword-types)
  (loop with keyword = nil
        with temp = nil
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
               and do (setf temp (pop lambda-list))
               and collect (if (listp temp)
                               (second temp)
                               t)
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
        else if (and elt (symbolp elt))
               collect 't
        else if (and elt (listp elt))
               collect (second elt)
        else when repeatp do (error "ftype-args internal error")))
