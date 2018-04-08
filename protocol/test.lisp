;;;; protocol/test.lisp

(in-package #:protest/protocol)

(defun test-protocol-define-empty ()
  (let ((*protocols* (make-hash-table)))
    (define-protocol #1=#.(gensym) ())
    (let ((protocol (gethash '#1# *protocols*)))
      (assert (null (description protocol)))
      (assert (null (tags protocol)))
      (assert (null (dependencies protocol)))
      (assert (null (exports protocol)))
      (assert (null (elements protocol))))
    (values)))

(defun test-protocol-define-detailed ()
  (let ((*protocols* (make-hash-table)))
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
      (assert (null (elements protocol))))
    (values)))

(defun test-protocol-define-dependencies ()
  (let ((*protocols* (make-hash-table)))
    (define-protocol #1=#.(gensym) ())
    (define-protocol #2=#.(gensym) (:dependencies (#1#)))
    (define-protocol #3=#.(gensym) (:dependencies (#1#)))
    (define-protocol #4=#.(gensym) (:dependencies (#2# #3#)))
    (define-protocol #5=#.(gensym) (:dependencies (#2#)))
    (define-protocol #6=#.(gensym) (:dependencies (#3#)))
    (define-protocol #7=#.(gensym) (:dependencies (#2#)))
    (define-protocol #.(gensym) (:dependencies (#1# #2# #3# #4# #5# #6# #7#)))
    (values)))

(defun #5=test-protocol-define-circular-dependency ()
  (let ((*protocols* (make-hash-table)))
    (define-protocol #1=#.(gensym) ())
    (define-protocol #2=#.(gensym) (:dependencies (#1#)))
    (define-protocol #3=#.(gensym) (:dependencies (#2#)))
    (define-protocol #4=#.(gensym) (:dependencies (#3#)))
    (handler-case
        (progn (define-protocol #1# (:dependencies (#4#)))
               (error "Test failure in ~A" '#5#))
      (protocol-error ()))
    (values)))

(defun #2=test-protocol-define-self-dependency ()
  (let ((*protocols* (make-hash-table)))
    (handler-case
        (progn (define-protocol #1=#.(gensym) (:dependencies (#1#)))
               (error "Test failure in ~A" '#2#))
      (protocol-error ()))
    (values)))

(defun #2=test-protocol-define-wrong-name ()
  (let ((*protocols* (make-hash-table)))
    (macrolet
        ((test (x) `(handler-case (progn (define-protocol ,x ())
                                         (error "Test failure in ~A" '#2#))
                      (protocol-error ()))))
      (test 2)
      (test "PROTOCOL")
      (test '(#.(gensym) #.(gensym)))
      (test nil))
    (values)))

(defun #2=test-protocol-define-invalid-dependencies ()
  (let ((*protocols* (make-hash-table)))
    (macrolet ((test (&rest x)
                 `(handler-case (progn (define-protocol #.(gensym)
                                         (:dependencies ,x))
                                       (error "Test failure in ~A" '#2#))
                    (protocol-error ()))))
      (test 2)
      (test "PROTOCOL")
      (test '(#.(gensym) #.(gensym)))
      (test '(1 2 3 4))
      (test nil))
    (values)))

(defun test-protocol-define-duplicate-elements ()
  (warn "Test not implemented yet."))

(defun test-protocol-define-duplicate-elements-inheritance ()
  (warn "Test not implemented yet."))

(defun test-protocol-define-documentation ()
  (warn "Test not implemented yet."))

(defun test-protocol-define-category ()
  (warn "Test not implemented yet."))

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

(define-protocol foo ())
