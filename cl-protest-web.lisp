;;;; cl-protest-web.lisp

(in-package #:cl-protest-web)

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/")
      "Welcome to ningle!")

(setf (ningle:route *app* "/login" :method :POST)
      #'(lambda (params)
          (if (authorize (cdr (assoc "username" params :test #'string=))
                         (cdr (assoc "password" params :test #'string=)))
              "Authorized!"
              "Failed...Try again.")))

(defun begin-protest ()
  (clack:clackup *app*))

(defun stop-protest ()
  (clack:stop *app*))
