;;;; src/protocol/validation.lisp

(in-package :protest/protocol)

(defun validate-protocol (protocol)
  (check-dependencies-valid protocol)
  (check-dependencies-not-circular protocol)
  (check-duplicate-element-forms protocol)
  (check-duplicate-effective-elements protocol)
  (check-exports protocol))

(defun validate-all-protocols ()
  (let ((protocols (hash-table-values *protocols*)))
    (mapcar #'validate-protocol protocols)))

(defun check-dependencies-valid (protocol)
  (let ((name (name protocol))
        (dependencies (dependencies protocol)))
    (unless (every (conjoin #'identity #'symbolp) dependencies)
      (protocol-error "Dependency is not a non-null symbol: ~A"
                      (find-if-not #'symbolp dependencies)))
    (when (member name dependencies)
      (protocol-error "Protocol ~A must not depend on itself." name))
    (loop for dependency in dependencies
          unless (find-protocol dependency)
            do (protocol-error "Unknown protocol ~A passed as a dependency."
                               dependency))
    (unless (setp dependencies)
      (protocol-error "Duplicate protocol dependencies detected: ~A"
                      (retain-duplicates dependencies)))))

(defun check-dependencies-not-circular (protocol)
  ;; The following function is meant to return a list of all effective
  ;; dependencies of a protocol, but as a side effect it does error-checking.
  (protocol-effective-dependencies protocol))

(defun check-duplicate-element-forms (protocol)
  (let ((forms (cdddr (whole protocol))))
    (setf forms (remove-if #'stringp forms))
    (loop with hash-table = (make-hash-table :test #'equal)
          for form in forms
          for type = (first form)
          for name = (second form)
          for list = (list type name)
          when (member type '(:category :config))
            do (setf (first list) :category-or-config)
          if (gethash list hash-table)
            do (protocol-error "Duplicate element form for ~{~S ~S~}." list)
          else do (setf (gethash list hash-table) t))))

(defun check-duplicate-effective-elements (protocol)
  (let* ((symbols (protocol-effective-dependencies protocol))
         (protocols (mapcar #'find-protocol symbols))
         (protocols (cons protocol protocols))
         (hash-table (make-hash-table :test #'equal)))
    (dolist (protocol protocols)
      (dolist (form (mapcar (compose #'first #'generate-forms)
                            (elements protocol)))
        (let* ((type (first form)) (name (second form)) (list (list type name)))
          (setf (first list)
                (case type
                  ((:class :condition-type) :class-or-condiiton-type)
                  ((:function :macro) :function-or-macro)
                  ((:category :config) :category-or-config)
                  (t (first list))))
          (if (gethash list hash-table)
              (protocol-error "Duplicate element form for ~{~S ~S~} found ~
in protocol ~A." list (name protocol))
              (setf (gethash list hash-table) t)))))))

(defun check-exports (protocol)
  (let ((exports (exports protocol)))
    (assert (or (eq exports t) (and (listp exports) (every #'symbolp exports)))
            () "Incorrect export list: ~S" exports)
    (unless (eq exports t)
      (loop for export in exports
            for element = (find export (elements protocol) :key #'name)
            unless element do (error "Export not found: ~S" export)))))
