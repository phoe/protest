;;;; protocol/test.lisp

(in-package #:protest/protocol)

;; TODO undoing variables/functions/classes
(defmacro with-test ((success-expected-p) &body body)
  (with-gensyms (function)
    `(tagbody
      :start
        (multiple-value-prog1 (values)
          (handler-case
              (let* ((*error-output* (make-broadcast-stream))
                     (*protocols* (make-hash-table))
                     (*compile-time-protocols* (make-hash-table))
                     (,function (compile nil '(lambda () ,@body))))
                (funcall ,function)
                (go ,(if success-expected-p :end :fail)))
            (protocol-error (e)
              (declare (ignorable e))
              ,(if success-expected-p
                   `(error "Test failure: unexpected failure:~%~A" e)
                   `(go :end)))
            (error (e)
              (error "Test failure: unexpected failure:~%~A" e))))
      :fail
        (error "Test failure: unexpected success.")
      :end)))

(defun test-protocol-define-empty ()
  (with-test (t)
    (define-protocol #1=#.(gensym) ())
    (let ((protocol (gethash '#1# *protocols*)))
      (assert (null (description protocol)))
      (assert (null (tags protocol)))
      (assert (null (dependencies protocol)))
      (assert (null (exports protocol)))
      (assert (null (elements protocol))))))

(defun test-protocol-define-detailed ()
  (with-test (t)
    (define-protocol #1=#.(gensym) (:export ()))
    (define-protocol #2=#.(gensym) (:dependencies (#1#)
                                    :tags (#3=#.(gensym))
                                    :description "asdf"
                                    :export t))
    (let ((protocol (gethash '#2# *protocols*)))
      (assert (string= "asdf" (description protocol)))
      (assert (equal '(#3#) (tags protocol)))
      (assert (equal '(#1#) (dependencies protocol)))
      (assert (null (exports protocol)))
      (assert (null (elements protocol))))))

(defun test-protocol-define-dependencies ()
  (with-test (t)
    (define-protocol #1=#.(gensym) ())
    (define-protocol #2=#.(gensym) (:dependencies (#1#)))
    (define-protocol #3=#.(gensym) (:dependencies (#1#)))
    (define-protocol #4=#.(gensym) (:dependencies (#2# #3#)))
    (define-protocol #5=#.(gensym) (:dependencies (#2#)))
    (define-protocol #6=#.(gensym) (:dependencies (#3#)))
    (define-protocol #7=#.(gensym) (:dependencies (#2#)))
    (define-protocol #.(gensym) (:dependencies (#1# #2# #3# #4# #5# #6# #7#)))))

(defun #5=test-protocol-define-circular-dependency ()
  (with-test (nil)
    (define-protocol #1=#.(gensym) ())
    (define-protocol #2=#.(gensym) (:dependencies (#1#)))
    (define-protocol #3=#.(gensym) (:dependencies (#2#)))
    (define-protocol #4=#.(gensym) (:dependencies (#3#)))
    (define-protocol #1# (:dependencies (#4#)))))

(defun #2=test-protocol-define-self-dependency ()
  (with-test (nil)
    (define-protocol #1=#.(gensym) (:dependencies (#1#)))))

(defun #2=test-protocol-define-invalid-name ()
  (with-test (nil) (define-protocol 2 ()))
  (with-test (nil) (define-protocol "PROTOCOL" ()))
  (with-test (nil) (define-protocol '(#.(gensym) #.(gensym)) ()))
  (with-test (nil) (define-protocol nil ())))

(defun #2=test-protocol-define-invalid-dependencies ()
  (with-test (nil) (define-protocol #.(gensym) (:dependencies (2))))
  (with-test (nil) (define-protocol #.(gensym) (:dependencies ("ABC"))))
  (with-test (nil) (define-protocol #.(gensym) (:dependencies ((#.(gensym))))))
  (with-test (nil) (define-protocol #.(gensym) (:dependencies ((1 2 3 4)))))
  (with-test (nil) (define-protocol #.(gensym) (:dependencies (nil)))))

(defun test-protocol-define-duplicate-elements ()
  (with-test (nil) (define-protocol #.(gensym) ()
                     (:variable #1=#.(gensym))
                     (:variable #1#)))
  (with-test (nil) (define-protocol #.(gensym) ()
                     (:config (#2=#.(gensym)))
                     (:config (#2#)))))

(defun test-protocol-define-duplicate-elements-inheritance ()
  (with-test (nil)
    (define-protocol #1=#.(gensym) ()
      (:variable #2=#.(gensym)))
    (define-protocol #.(gensym) (:dependencies (#1#))
      (:variable #2#))))

(defun test-protocol-define-category ()
  (with-test (t)
    (unwind-protect
         (progn (define-protocol #.(gensym) ()
                  (:category #1=(:foo :bar)) #2="qwer")
                (assert (string= #2# (documentation '#1# 'category))))
      (setf (documentation '#1# 'category) nil))))

(defun test-protocol-define-class ()
  (warn "Test not implemented yet."))

(defun test-protocol-define-condition-type ()
  (warn "Test not implemented yet."))

(defun test-protocol-define-config ()
  (warn "Test not implemented yet."))

(defun test-protocol-define-function ()
  (warn "Test not implemented yet."))

(defun test-protocol-define-macro ()
  (warn "Test not implemented yet."))

(defun test-protocol-define-variable ()
  (warn "Test not implemented yet."))
