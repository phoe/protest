;;;; t/ftype.lisp

(defpackage #:protest/test/ftype
  (:use #:cl
        #:protest/ftype
        #:protest/test))

(in-package #:protest/test/ftype)

(register-test-package)

(defmacro test (lambda-list return-type keyword-types result)
  `(is (equal (function-ftype-declaration-form
               ',lambda-list ',return-type ',keyword-types)
              ',result)))

(define-protest-test test-ftype-null
  (test ()
        t ()
        (function () t)))

(define-protest-test test-ftype-no-types
  (test (foo bar baz)
        t ()
        (function (t t t) t)))

(define-protest-test test-ftype-basic-types
  (test ((foo string) (bar (or number symbol)) (baz t) quux)
        rational ()
        (function (string (or number symbol) t t) rational)))

(define-protest-test test-ftype-&rest
  (test (foo bar &rest baz)
        t ()
        (function (t t &rest t) t)))

(define-protest-test test-ftype-&rest-typed
  (test ((foo number) bar &rest (baz string))
        stream ()
        (function (number t &rest string) stream)))

(define-protest-test test-ftype-&optional
  (test (foo bar &optional)
        t ()
        (function (t t &optional) t))
  (test (foo bar &optional baz)
        t ()
        (function (t t &optional t) t))
  (test (foo bar &optional baz quux fred)
        t ()
        (function (t t &optional t t t) t)))

(define-protest-test test-ftype-&optional-typed
  (test ((foo number) bar &optional (baz string) (quux class))
        stream ()
        (function (number t &optional string class) stream)))

(define-protest-test test-ftype-&key
  (test (foo bar &key baz quux)
        t ()
        (function (t t &key (:baz t) (:quux t)) t))
  (test (foo bar &key baz quux &allow-other-keys)
        t ()
        (function (t t &key (:baz t) (:quux t) &allow-other-keys) t)))

(define-protest-test test-complex
  (test (foo &rest bar &optional baz &key quux)
        t ()
        (function (t &rest t &optional t &key (:quux t)) t))
  (test ((foo string) &rest (bar symbol) &optional (baz number) &key quux)
        class (:quux stream)
        (function (string &rest symbol &optional number &key (:quux stream))
                  class)))
