;;;; cl-protest-web.lisp

(in-package #:cl-protest-web)

(defvar *app* (make-instance 'ningle:<app>))

;; (setf (ningle:route *app* "/")
;;       "Welcome to ningle!")

;; (setf (ningle:route *app* "/login" :method :POST)
;;       #'(lambda (params)
;;           (if (authorize (cdr (assoc "username" params :test #'string=))
;;                          (cdr (assoc "password" params :test #'string=)))
;;               "Authorized!"
;;               "Failed...Try again.")))

(defun print-protocol (stream protocol)
  (destructuring-bind (protocol-name options &rest entries) protocol
    (destructuring-bind (&key description tags attachments) options
      (cl-who:with-html-output (stream nil :indent t)
        (:div :class "protocol-wrapper"
              (:h1 "Protocol " (str protocol-name))
              (protocol-description stream description)
              (protocol-tags stream tags)
              (protocol-attachments stream attachments)
              (protocol-entries stream entries)
              )))))

(defun protocol-description (stream description)
  (when description
    (with-html-output (stream nil :indent t)
      (:div :class "protocol-description"
            (:strong "Description: ")
            (str description)))))

(defun protocol-tags (stream tags)
  (when tags
    (with-html-output (stream nil :indent t)
      (:div :class "protocol-tags"
            (:strong "Tags: ")
            (fmt "~{~A ~}" tags)))))

(defun protocol-attachments (stream attachments)
  (when attachments
    (with-html-output (stream nil :indent t)
      (:div :class "protocol-attachments"
            (:strong "Attachments: ")
            (loop for elt in attachments
                  for link = (first elt)
                  for name = (or (second elt) link)
                  do (htm (:a :href link
                              (str name))))))))

(defun protocol-entries (stream entries)
  (when entries
    (with-html-output (stream nil :indent t)
      (:div :class "protocol-entries-wrapper"
            (loop for entry in entries
                  for fn = (process-kind (first entry))
                  do (funcall fn stream entry))))))

(defun process-kind (entry)
  (ecase entry
    (:variable nil)
    (:function nil)
    (:macro nil)
    (:class nil)
    (:generic nil)))

(defun begin-protest ()
  (clack:clackup *app*))

(defun stop-protest ()
  (clack:stop *app*))
