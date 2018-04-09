;;;; base/test.lisp

(in-package #:protest/base)

(defun run-tests (&optional (package *package*))
  (mapc (compose #'funcall #'print)
        (uiop:while-collecting (collect)
          (do-symbols (symbol package)
            (let ((name (symbol-name symbol)))
              (when (and (<= 5 (length name))
                         (string= "TEST-" (subseq name 0 5))
                         (fboundp symbol))
                (collect symbol))))))
  (values))

(defun test-protocol-class-define ()
  (unwind-protect
       (define-protocol-class #1=#.(gensym) () ())
    (setf (find-class '#1#) nil))
  (values))

(defun #1=test-protocol-class-instantiate ()
  (unwind-protect (progn
                    (define-protocol-class #2=#.(gensym) () ())
                    (handler-case
                        (progn (make-instance '#2#)
                               (error "Test failure in ~A." '#1#))
                      (protocol-error ())))
    (setf (find-class '#2#) nil))
  (values))

(defun #1=test-protocol-condition-type-define ()
  ;; https://bugs.launchpad.net/sbcl/+bug/1761950
  #+sbcl (warn "~A broken on SBCL; skipping.~%" '#1#)
  #-sbcl
  (unwind-protect
       (define-protocol-condition-type #2=#.(gensym) () ())
    (setf (find-class '#2#) nil))
  (values))

(defun #1=test-protocol-condition-type-instantiate ()
  ;; https://bugs.launchpad.net/sbcl/+bug/1761950
  #+sbcl (warn "~A broken on SBCL; skipping.~%" '#1#)
  #-sbcl
  (unwind-protect (progn
                    (define-protocol-condition-type #2=#.(gensym) () ())
                    (handler-case
                        (progn (make-condition '#2#)
                               (error "Test failure in ~A." '#1#))
                      (protocol-error ())))
    (setf (find-class '#2#) nil))
  (values))
