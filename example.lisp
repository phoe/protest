;;;; Example test case and protocol.

(define-protocol wiggler ()
  (:class wiggler () ())
  "An object that is able to wiggle."
  (:variable *wiggler* t nil)
  "A dynamic variable denoting the current active wiggler."
  (:macro with-wiggler)
  "(with-wiggler wiggler ...)
A wrapper macro that binds *WIGGLER* to the value of WIGGLER."
  (:function make-wiggler ((type (or class symbol))) (wiggler wiggler))
  "A constructor function that makes a wiggler of given type."
  (:generic wiggle ((wiggler wiggler) (object fist)) (values))
  "Wiggles inside target object.")

(define-protocol killable ()
  (:class killable () ())
  "A killable object is something that lives and can therefore be killed."
  (:generic alivep ((object killable)) :generalized-boolean)
  "Returns true if the object is alive (was not killed) and false otherwise."
  (:generic kill ((object killable)) (values))
  "Kills the object.")

(define-protocol fist (:description "This is a sample description."
                       :tags (tag1 tag2 foo bar baz)
                       :attachments (("fist.png")
                                     ("fist2.png" "Alternate fist")))
  (:class fist (killable) ())
  "A fist is something that can squeeze around objects and hold them ~
despite any wiggling. If a fist dies, then it is possible to wiggle ~
out of it."
  (:generic squeeze ((object fist)) :generalized-boolean)
  "Squeezes the fist.
Returns true if the first was not previously squeezed and false otherwise."
  (:generic unsqueeze ((object fist)) :generalized-boolean)
  "Unsqueezes the fist.
Returns true if the first was previously squeezed and false otherwise."
  (:generic wigglep ((object fist)) :generalized-boolean)
  "Checks if anything wiggled inside the fist since its last squeeze.")

;; (define-test-case fist-wiggle
;;     (:description "This is a sample test case."
;;      :tags (fist wiggle)
;;      :attachments (("fist-wiggle.png")))
;;   1 "Enter the fist and let it close."
;;   2 "Wiggle inside."
;;   3 "Assert that the fist is still closed.")

;; (define-test-case fist-wiggle-death
;;     (:description "This is another sample test case."
;;      :tags (fist wiggle killable)
;;      :attachments (("fist-wiggle-death.png")))
;;   1 "Enter the fist and let it close."
;;   2 "Kill the fist."
;;   3 "Wiggle inside."
;;   4 "Assert that you can wiggle out of the fist.")
