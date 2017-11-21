;;;; protest-function.lisp

(in-package #:protest)

(defun parse-gfn-args (args)
  (let ((acc ())
        (list (copy-list args)))
    (do ((elt (pop list) (pop list)))
        ((and (null list) (null elt)) (nreverse acc))
      (etypecase elt
        (list (push (first elt) acc))
        (symbol
         (case elt
           (&rest
            (push '&rest acc)
            (let ((elt2 (pop list)))
              (if (listp elt2)
                  (push (first elt2) acc)
                  (push elt2 acc))))
           (&optional
            (push '&optional acc))
           (&key
            (push '&key acc)
            (loop for elt2 = (pop list)
                  if (null elt2)
                    return nil
                  else if (eq elt2 '&allow-other-keys)
                         do (push '&allow-other-keys acc)
                            (return)
                  else if (listp elt2)
                         do (push (first elt2) acc)
                  else do (push elt2 acc)))
           (otherwise
            (push elt acc))))))))

(defun parse-gfn-result (result)
  (if (listp result)
      (if (eq (car result) 'values)
          result
          (second result))
      t))

(defun parse-function (form docstring)
  `(progn
     ,@(when docstring
         `((setf (documentation ',(first form) 'function)
                 ,(format nil docstring))))
     ;; ,@(when (>= (length form) 3)
     ;;     `((declaim (ftype (function * ,(parse-gfn-result (third form)))
     ;;                       ,(first form)))))
     (defgeneric? ,(first form) ,(parse-gfn-args (second form)))))

(pushnew '(:function #'parse-function) *categories* :test #'equal)
