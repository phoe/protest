;;;; The above protocol, when evaluated, has the following side effects:

;;;; Protocol FUELABLE
(defgeneric fuel (object)
  (:documentation "Returns the current amount of fuel in the fuelable."))
(declaim (ftype (function (fuelable) real) fuel))

(defgeneric (setf fuel) (new-value object)
  (:documentation "Sets the current amount of fuel in the fuelable."))
(declaim (ftype (function (real fuelable) real) (setf fuel)))

;;;; Protocol AUTOMOBILE
(define-protocol-class automobile (fuelable) nil
  (:documentation "An automobile object. Objects participaring in this protocol
must subclass this protocol class."))

(defgeneric brand (object)
  (:documentation "Returns the brand of a automobile."))
(declaim (ftype (function (automobile) keyword) brand))

(defgeneric (setf brand) (new-value object)
  (:documentation "Sets the brand of a automobile."))
(declaim (ftype (function (keyword automobile) keyword) (setf brand)))

(defgeneric drive (object distance &optional env)
  (:documentation "Drives a given distance with the given automobile. If ENV
is supplied, the drive occurs in that environment."))
(declaim (ftype (function (automobile real t) (values)) drive))

(setf (documentation 'with-snow-tires 'function)
      "Executes the body with snow tires on all automobiles, therefore
diminishing chances of an accident in snow environment.")

(defvar *accident-chance* 0.005)
(declaim (type (float 0.0 1.0) *accident-chance*))
(setf (documentation '*accident-chance* 'variable)
      "The chance of an accident per 100 kilometers. This variable is expected
to be rebound when the environment changes.")

(setf (documentation '(:automobile) 'category)
      "Describes configuration entries related to all automobiles in general.")

(funcall *configuration-setter*
         '(:automobile :maximum-lead-per-100-kilometers) 5.0e-4)
(setf (documentation '(:automobile :maximum-lead-per-100-kilometers) 'config)
      "Describes how many grams of lead an engine is allowed to output after
driving for 100 kilometers.")

(setf (documentation '(:automobile :steering) 'category)
      "Describes configuration entries related to the automobiles' steering.")

(setf (documentation '(:automobile :steering :wheel-side) 'config)
      "Describes if the automobiles must have steering wheels on left or right
side.")