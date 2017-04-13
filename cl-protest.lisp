;;;; cltms.lisp

(in-package #:cl-protest)

(defmacro define-protocol-class (name superclasses slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,slots ,@options)
     (defmethod initialize-instance :before ((object ,name) &key)
       (when (eq (class-of object) (find-class ',name))
         (error "~S is a protocol class and thus cannot be instantiated."
                ',name)))
     ',name))

(defmacro defgeneric? (fun-name lambda-list &body options)
  `(when (or (not (fboundp ',fun-name))
             (not (typep (fdefinition ',fun-name) 'generic-function))
             ;;(null (generic-function-methods (fdefinition ',fun-name)))
             )
     (defgeneric ,fun-name ,lambda-list ,@options)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *protocols* '())
  (defvar *test-cases* '())

  (defun keywordize (symbol)
    (intern (symbol-name symbol) :keyword))

  (defun choose-function (keyword)
    (ecase keyword
      (:class #'parse-class)
      (:variable #'parse-variable)
      (:macro #'parse-macro)
      (:function #'parse-function)
      (:generic #'parse-generic)))

  (defun parse-form (original-form docstring)
    (destructuring-bind (keyword . form) original-form
      (let ((function (choose-function keyword)))
        (funcall function form docstring))))

  (defun parse-fn-args (args)
    (let ((acc ())
          (list (copy-list args)))
      (do ((elt (pop list) (pop list)))
          ((and (null list) (null elt)) (nreverse acc))
        (cond
          ;; typed arguments
          ((listp elt)
           (push (second elt) acc))
          ;; &REST
          ((eq elt '&rest)
           (push '&rest acc)
           (let ((elt2 (pop list)))
             (if (listp elt2)
                 (push (second elt2) acc)
                 (push t acc))))
          ;; &OPTIONAL
          ((eq elt '&optional)
           (push '&optional acc))
          ;; &KEY
          ((eq elt '&key)
           (push '&key acc)
           (loop for elt2 = (pop list)
                 if (null elt2)
                   return nil
                 else if (eq elt2 '&allow-other-keys)
                        do (push '&allow-other-keys acc)
                           (return)
                 else do (push (list (keywordize (first elt2))
                                     (second elt2)) acc)))
          ;; untyped arguments
          (t
           (push t acc))))))

  (defun parse-gfn-args (args)
    (let ((acc ())
          (list (copy-list args)))
      (do ((elt (pop list) (pop list)))
          ((and (null list) (null elt)) (nreverse acc))
        (cond
          ;; typed arguments
          ((listp elt)
           (push (first elt) acc))
          ;; &REST
          ((eq elt '&rest)
           (push '&rest acc)
           (let ((elt2 (pop list)))
             (if (listp elt2)
                 (push (first elt2) acc)
                 (push elt2 acc))))
          ;; &OPTIONAL
          ((eq elt '&optional)
           (push '&optional acc))
          ;; &KEY
          ((eq elt '&key)
           (push '&key acc)
           (loop for elt2 = (pop list)
                 if (null elt2)
                   return nil
                 else if (eq elt2 '&allow-other-keys)
                        do (push '&allow-other-keys acc)
                           (return)
                 else do (push (first elt2) acc)))
          ;; untyped arguments
          (t
           (push t acc))))))

  (defun parse-fn-result (result)
    (if (listp result)
        (if (eq (car result) 'values)
            result
            (second result))
        t))

  (defun parse-class (form docstring)
    `(progn ;;unless (find-class ',(first form) nil)
       (define-protocol-class ,@form)
       (setf (documentation ',(first form) 'type)
             ,(format nil docstring))))

  (defun parse-variable (form docstring)
    `(progn
       (setf (documentation ',(first form) 'variable) ,(format nil docstring))
       ,@(when (>= (length form) 2)
           `((declaim (type ,(second form) ,(first form)))))
       (defvar ,(first form)
         ,@(when (>= (length form) 3) `(,(third form))))))

  (defun parse-macro (form docstring)
    `(progn
       (setf (documentation ',(first form) 'variable) ,(format nil docstring))))

  (defun parse-function (form docstring)
    `(progn
       (setf (documentation ',(first form) 'function) ,(format nil docstring))
       ,@(when (>= (length form) 2)
           `((declaim (ftype (function
                              ,(parse-fn-args (second form))
                              ;; ,@(when (>= (length form) 3)
                              ;;     `(,(parse-fn-result (third form))))
                              )
                             ,(first form)))))))

  (defun parse-generic (form docstring)
    `(progn
       (setf (documentation ',(first form) 'function) ,(format nil docstring))
       (defgeneric? ,(first form) ,(parse-gfn-args (second form)))
       ,@(when (>= (length form) 3)
           `((declaim (ftype (function *
                                       ,(parse-fn-result (third form)))
                             ,(first form))))))))

(defmacro define-protocol
    (&whole whole
       protocol-name options &body forms)
  (declare (ignore options))
  `(progn
     ;; TODO make docstrings optional, modify the loop to achieve this
     ,@(loop for (form docstring) on forms by #'cddr
             collect (parse-form form docstring))
     (let ((data (cdr ',whole))
           (value (find ',protocol-name *protocols* :key #'car)))
       (unless (equal data value)
         (when value
           (warn "Redefining ~S in DEFINE-PROTOCOL" ',protocol-name))
         (setf *protocols*
               (cons data
                     (if value
                         (remove ',protocol-name *protocols* :key #'car)
                         *protocols*)))))))

(defmacro define-test-case
    (&whole whole test-case-name options description &body steps)
  (declare (ignore options description steps))
  `(let ((data (cdr ',whole))
         (value (find ',test-case-name *test-cases* :key #'car)))
     ;; update *test-cases*
     (unless (equal data value)
       (when value
         (warn "Redefining ~S in DEFINE-TEST-CASE" ',test-case-name))
       (setf *test-cases*
             (cons data
                   (if value
                       (remove ',test-case-name *test-cases* :key #'car)
                       *test-cases*))))))
