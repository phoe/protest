;;;; util/util.lisp

(in-package #:cl-protest)

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

(defun verify-class (class-name superclass-names slot-names docstring)
  (if-let ((class (find-class class-name nil)))
    (let* ((superclasses (class-direct-superclasses class))
           (names-1 (cons 'standard-object superclass-names))
           (names-2 (mapcar #'class-name superclasses))
           (slots (class-direct-slots class))
           (snames-1 slot-names)
           (snames-2 (mapcar #'slot-definition-name slots))
           (diff1 (set-difference names-1 names-2))
           (diff2 (set-difference names-2 names-1))
           (sdiff1 (set-difference snames-1 snames-2))
           (sdiff2 (set-difference snames-2 snames-1))
           (docstring2 (documentation class-name 'type)))
      (unless (and (or (null docstring)
                       (string= docstring docstring2))
                   (null diff1) (null diff2)
                   (null sdiff1) (null sdiff2))
        (error *class-mismatch-format*
               class-name docstring docstring2
               names-1 names-2 snames-1 snames-2)))))

(defun keywordize (symbol)
  (intern (symbol-name symbol) :keyword))

(defun remove-strings (list)
  (remove-if #'stringp list))
