;;;; cl-protest-macros.lisp

(in-package #:cl-protest)

(defmacro define-protocol
    (&whole whole
       protocol-name options &body forms)
  (declare (ignore options))
  `(progn
     ,@(loop for (form docstring) on forms
             if (and (listp form)
                     (keywordp (car form))
                     (stringp docstring))
               collect (parse-form form docstring)
             else if (and (listp form)
                          (keywordp (car form)))
                    collect (parse-form form nil))
     (let ((data (remove-strings (cdr ',whole)))
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
    (&whole whole test-case-name options &body steps)
  (declare (ignore options steps))
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
