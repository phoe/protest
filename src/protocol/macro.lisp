;;;; src/protocol/macro.lisp

(in-package :protest/protocol)

(defun ensure-protocol (name options whole)
  (let* ((protocol (apply #'make-instance 'protocol
                          :name name :whole whole options)))
    (validate-protocol protocol)
    (let ((value (find-protocol name)))
      (when (and (find-protocol name)
                 (not (equalp (whole value) (whole protocol))))
        (warn "Redefining ~A in DEFINE-PROTOCOL" name)))
    (let ((okp nil)
          (old-protocol (find-protocol name)))
      (unwind-protect
           (progn (setf (find-protocol name) protocol)
                  (validate-all-protocols)
                  (setf okp t))
        (unless okp (setf (find-protocol name) old-protocol))))
    name))

(defmacro define-protocol (&whole whole name (&rest options) &body forms)
  "Defines the protocol with the provided NAME and OPTIONS, instantiating all
its elements based on FORMS."
  (declare (ignore forms))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-protocol ',name ',options ',whole)))

(defmacro execute-protocol (name)
  "Executes all the side effects of the protocol with the provided NAME."
  (check-type name symbol)
  (let ((protocol (find-protocol name)))
    (if protocol
        (progn (validate-protocol protocol)
               (generate-code protocol))
        (error "Protocol ~S was not found." name))))

;; TODO check that all exported symbols are documented

(defmethod remove-protocol-element ((element protocol-macro))
  (let ((name (name element)))
    (setf (documentation name 'function) nil)
    (fmakunbound name)))
