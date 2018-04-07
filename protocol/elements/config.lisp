
(defmethod generate-element ((type (eql :config)) &rest form)
  )

(defmethod embed-documentation
    ((element protocol-config) (string string))
  )

(defmethod generate-forms ((element protocol-config))
  )

(defmethod generate-code ((element protocol-config))
  )
