(ns dbc.core
  (:use clojure.test))


;; Commentary
;; Contracts consist of two parts: pre and post
;; conditions, stored in the contract map as dom and rnf respectively.
;;
;; All contracts pertain to the arguments to this function so if the
;; argument is a high order function, then the contract states: "takes
;; a function which returns positive numbers" or some such.

;; So how do we describe that contract?

;; "takes a positive number": (pos ?)
;; "takes a function which returns a positive number": ?

(defn make-ho-contract [pre post]
  "Return a contract for a high order function"
  [pre post])


(defn make-contract [pre post]
  "Returns a contract with dom and rng set to pre and post
respectively."
  {:dom pre :rng post})

(defn dom [contract]
  (:dom contract))

(defn rng [contract]
  (:rng contract))

(defn flat? [x]
  (not (map? x)))

(defn lenient [_]
  true)

(defn strict [_]
  false)

(defn gt0 [x]
  (and
   (number? x)
   (pos? x)))


(deftest contract-construction
  (testing "Flat predicate"
    (is (flat? :foo))
    (is (not (flat? (make-contract :foo :bar)))))
  (testing "Contract construction"
    (is (= 2 (count (make-contract :foo :bar))))
    (is ((dom (make-contract lenient lenient)) 42))))





(defn pred [contract value]
  "CONTRACT must be a function which accepts a single value to check."
  (contract value))



; TODO look at using clojure-contrib.condition/raise here
(defn contract-error [position]
  ;(print "Contract failed: " position)
  (assert (and false position)))



;;; Mutual recursion, as in ho-warp and wrap, probably ought to
;;; use trampolining:
;;; http://groups.google.com/group/clojure/msg/3addf875319c5c10

(declare fo-wrap ho-wrap)

(defn wrap [contract value p n]
  (if (flat? contract)
    (fo-wrap contract value p n)
    (ho-wrap contract value p n)))

(defn fo-wrap  [contract value p n]
  (print "\nfo-wrap: " contract " value: " value)
  (if (pred contract value)
      value
      (contract-error p)))

(defn ho-wrap [ct x p n]
  (let [d (dom ct)
	r (rng ct)]
    (print "\nho-wrap: " ct " value: " x)
    (fn [y] (wrap r
		  (x (wrap d y n p))
		  p
		  n))))









(defn test-wrap []
  (wrap (partial > 42) 43 "pos" "neg"))

(defn test-ho-wrap []
  "Validate that a function parameter accepts +ve values"
  (wrap {:rng (partial < 0) :dom #(true)} #(42) "p" "n"))


(deftest contracts
  (testing "FO Contracts"
    (is (= 42 (wrap lenient 42 "pos" "neg")))
    (is (thrown? java.lang.AssertionError (wrap strict 42 "pos" "neg")))
    (is (= 41 (wrap (partial > 42) 41 "pos" "neg")))
    (is (thrown? java.lang.AssertionError
		 (wrap (partial > 42) 43 "pos" "neg")))))




(defn plus1 []
  (partial + 1))

(defn a-ho-fn [fn number]
  (fn number))



;; so now we want to say that a-ho-fn takes a function which accepts
;; numbers > 0



(defn plus1-contract [param]
  (and (number? param) (< 0 param)))



(deftest ho-contract
  (testing "Sanity check on a-ho-fn"
    (is (= 7 (a-ho-fn (plus1) 6)))))




;; Idea based on section 2.2 of F&F


(def saved (ref (fn [_] 50)))



;;; (bigger-than-0 -> bigger-than-0) -> any
(defn save [f] (dosync (ref-set saved f) ))


(defn save-contract []
  (make-contract (make-contract gt0 gt0) gt0))
;;; bigger-than-0 -> bigger-than-0
(defn use [n] (saved n))


