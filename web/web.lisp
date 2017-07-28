;;;; protest-web.lisp

(in-package #:protest-web)

(defvar *app* (make-instance 'ningle:<app>))

;; (setf (ningle:route *app* "/")
;;       "Welcome to ningle!")

;; (setf (ningle:route *app* "/login" :method :POST)
;;       #'(lambda (params)
;;           (if (authorize (cdr (assoc "username" params :test #'string=))
;;                          (cdr (assoc "password" params :test #'string=)))
;;               "Authorized!"
;;               "Failed...Try again.")))

(defun print-protocol (protocol &optional (stream *standard-output*))
  (destructuring-bind (protocol-name options &rest entries) protocol
    (destructuring-bind (&key description tags attachments) options
      (cl-who:with-html-output (stream nil :indent t)
        (:div :class "protocol-wrapper"
              (:h1 "Protocol " (str protocol-name))
              (protocol-description description stream)
              (protocol-tags tags stream)
              (protocol-attachments attachments stream)
              (protocol-entries entries stream))))))

(defun protocol-description (desc &optional (stream *standard-output*))
  (when desc
    (with-html-output (stream nil :indent t)
      (:div :class "protocol-description"
            (:p :class "protocol-description-list"
                (:strong "Description: ")
                (:span (str desc)))))))

(defun protocol-tags (tags &optional (stream *standard-output*))
  (when tags
    (with-html-output (stream nil :indent t)
      (:div :class "protocol-tags"
            (:p :class "protocol-tags-list"
                (:strong "Tags: ")
                (:span (fmt "~{~A ~}" tags)))))))

(defun protocol-attachments (atts &optional (stream *standard-output*))
  (when atts
    (with-html-output (stream nil :indent t)
      (:div :class "protocol-attachments"
            (:p :class "protocol-attachments-list"
                (:strong "Attachments: ")
                (:ul (loop for elt in atts
                           for link = (first elt)
                           for name = (or (second elt) link)
                           do (htm (:li (:a :href link
                                            (str name)))))))))))

(defun protocol-entries (entries &optional (stream *standard-output*))
  (when entries
    (with-html-output (stream nil :indent t)
      (:div :class "protocol-entries-wrapper"
            (loop for entry in entries
                  for fn = (process-kind (first entry))
                  do (funcall fn (rest entry) stream))))))

(defun process-kind (entry)
  (ecase entry
    (:variable #'process-variable)
    (:function #'process-function)
    (:macro #'process-macro)
    (:class #'process-class)
    (:generic #'process-generic)))

(defun process-variable (entry &optional (stream *standard-output*))
  (with-html-output (stream nil :indent t)
    (:div :class "protocol-variable-wrapper"
          (:h2 "Variable " (str (first entry)))
          (when (>= (length entry) 2)
            (htm (:p :class "protocol-variable-type"
                     (:strong "Type: ") (:span (str (second entry))))))
          (when (>= (length entry) 3)
            (htm (:p :class "protocol-variable-initval"
                     (:strong "Initial value: ")
                     (:span (str (princ-to-string (third entry)))))))
          (when (documentation (first entry) 'variable)
            (htm (:p :class "protocol-variable-documentation"
                     (:span (str (br (documentation (first entry)
                                                    'variable))))))))))

(defun process-function (entry &optional (stream *standard-output*))
  (with-html-output (stream nil :indent t)
    (:div :class "protocol-function-wrapper"
          (:h2 "Function " (str (first entry)))
          (:p :class "protocol-function-lambda-list"
              (:em (str (function-lambda-list entry))))
          (:p :class "protocol-function-args-vals"
              (:strong "Arguments and return values: ")
              (:span (fn-arg-rets entry stream)))
          (when (documentation (first entry) 'function)
            (htm (:p :class "protocol-function-documentation"
                     (:span (str (br (documentation (first entry)
                                                    'function))))))))))

(defun process-generic (entry &optional (stream *standard-output*))
  (with-html-output (stream nil :indent t)
    (:div :class "protocol-generic-wrapper"
          (:h2 "Generic Function " (str (first entry)))
          (:p :class "protocol-generic-lambda-list"
              (:em (str (function-lambda-list entry))))
          (:p :class "protocol-generic-args-vals"
              (:strong "Arguments and return values: ")
              (:span (fn-arg-rets entry stream)))
          (when (documentation (first entry) 'function)
            (htm (:p :class "protocol-generic-documentation"
                     (:span (str (br (documentation (first entry)
                                                    'function))))))))))

(defun process-macro (entry &optional (stream *standard-output*))
  (with-html-output (stream nil :indent t)
    (:div :class "protocol-macro-wrapper"
          (:h2 "Macro " (str (first entry)))
          (when (documentation (first entry) 'function)
            (htm (:p :class "protocol-macro-documentation"
                     (:span (str (br (documentation (first entry)
                                                    'function))))))))))

(defun process-class (entry &optional (stream *standard-output*))
  (with-html-output (stream nil :indent t)
    (:div :class "protocol-class-wrapper"
          (:h2 "Class " (str (first entry)))
          (when (second entry)
            (htm (:p :class "protocol-variable-superclasses"
                     (:strong "Superclasses: ") (:span (str (second entry))))))
          (when (third entry)
            (htm (:p :class "protocol-variable-slots"
                     (:strong "Slots: ")
                     (:span (str (princ-to-string (third entry)))))))
          (when (documentation (first entry) 'type)
            (htm (:p :class "protocol-class-documentation"
                     (:span (str (br (documentation (first entry)
                                                    'type))))))))))

(defun fn-arg-rets (entry stream)
  (with-html-output (stream nil :indent t)
    (flet ((typed-object (elt)
             (htm (:li (:strong (str (first elt)))
                       " - object of type "
                       (:b (let ((*print-case* :upcase))
                             (str (second elt))
                             (str "."))))))
           (untyped-object (elt)
             (htm (:li (:strong (str elt))
                       " - any object.")))
           (generalized-boolean ()
             (htm (:li (:strong
                        (str 'generalized-boolean))
                       " - a generalized boolean."))))
      (htm (:ul
            (let ((*print-case* :downcase))
              (dolist (elt (second entry))
                (cond ((listp elt)
                       (typed-object elt))
                      ((member elt lambda-list-keywords))
                      (t
                       (untyped-object elt))))
              (cond ((eq (third entry) :generalized-boolean)
                     (generalized-boolean))
                    ((equal (third entry) '(values)))
                    (t
                     (typed-object (third entry))))))))))

(defun function-lambda-list (entry)
  (with-output-to-string (s)
    (let ((*print-case* :downcase)
          (lambda-list (loop for elt in (second entry)
                             if (listp elt) collect (first elt)
                               else collect elt))
          (retval (cond ((equal (third entry) '(values))
                         "(no value)")
                        ((listp (third entry))
                         (first (third entry)))
                        (t
                         (third entry)))))
      (format s "~{~A ~}"
              (append (cons (first entry) lambda-list)
                      (list "→" retval))))))

(defun br (string)
  (replace-all string #.(format nil "~%") "<br />"))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun begin-protest ()
  (clack:clackup *app*))

(defun stop-protest ()
  (clack:stop *app*))

;; (with-output-to-file (s "/tmp/foo.html" :if-exists :overwrite
;;                                         :if-does-not-exist :create)
;;   (mapc (lambda (x) (print-protocol x s)) (reverse *protocols*))
;;   nil)
