;;;; util/util.lisp

(in-package #:protest)

(defmacro define-protocol-class (name superclasses slots &rest options)
  (let ((superclasses (if (member 'standard-object superclasses)
                          superclasses
                          (append superclasses '(standard-object)))))
    `(progn
       (defclass ,name ,superclasses ,slots ,@options)
       (defmethod initialize-instance :before ((object ,name) &key)
         (when (eq (class-of object) (find-class ',name))
           (error "~S is a protocol class and thus cannot be instantiated."
                  ',name)))
       ',name)))

(defmacro defgeneric? (fun-name lambda-list &body options)
  `(when (or (not (fboundp ',fun-name))
             (not (typep (fdefinition ',fun-name) 'generic-function)))
     (defgeneric ,fun-name ,lambda-list ,@options)))

(defun keywordize (symbol)
  (intern (symbol-name symbol) :keyword))

(defun remove-strings (list)
  (remove-if #'stringp list))

(defun cat (&rest strings)
  (apply #'concatenate 'string strings))
