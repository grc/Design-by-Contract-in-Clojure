
(ns dbc.core
  (:use clojure.test clojure.template clojure.contrib.condition clojure.walk))


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
  (throw (Exception. (str "Contract failed: " position))))



;;; Mutual recursion, as in ho-warp and wrap, probably ought to
;;; use trampolining:
;;; http://groups.google.com/group/clojure/msg/3addf875319c5c10

(declare fo-wrap ho-wrap)

(defn wrap [contract value p n]
  (if (flat? contract)
    (fo-wrap contract value p n)
    (ho-wrap contract value p n)))

(defn fo-wrap  [contract value p n]
  (if (pred contract value)
      value
      (contract-error p)))

(defn ho-wrap [ct x p n]
  (let [d (dom ct)
	r (rng ct)]
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
    (is (thrown? java.lang.Exception (wrap strict 42 "pos" "neg")))
    (is (= 41 (wrap (partial > 42) 41 "pos" "neg")))
    (is (thrown? java.lang.Exception
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





;;; I want this ..



;; (defn wrap-args
;;   "Return a vector of wrapped arguments.
;; Odd arguments are the vale, even the contract to apply"
;;   [ args p n]
;;   (let [values (take-nth 2 args)
;; 	contracts (take-nth 2 (drop 1 args))]
   
;;     ))

;; (defmacro defcontract [name args body]
;;     (let [values (take-nth 2 args)
;; 	  contracts (take-nth 2 (drop 1 args))
;; 	  wrapped (map wrap values contracts (repeat "p") (repeat "n"))]
;;       `(defn ~name ~args
;; 	 (let (vector (interleaved values wrapped))
;; 	   ~body) )))

;; (defcontract identity [v c]
;;   v)

;; ;;; to turn in to this ...
;; (defn identity [v c]

;;   (let [v (wrap c v "p" "n")]
;;     v))
(defn foo [v c]
  (+ v c))


(defn my-wrap [vs cs]
  (map foo vs cs))


(defmacro defcontract [args]
  (let [values (vector (take-nth 2 args))
	contracts (vector (take-nth 2 (drop 1 args)))]
    (apply-template (quote values) '(+ x x) (quote contracts) )))





;;; What do we want sqrt to expand to?
(defn sqrt-1 [n]
  (Math/sqrt   (wrap gt0 n "p" "n")))

(deftest square-roots
  (is (< 0 (sqrt-1 4)))
  (is (thrown? java.lang.Exception (sqrt-1 -2))))


;;; Looking now at the high order functions in F &F

;; Idea based on section 2.2 of F&F


(def saved (ref (fn [_] 50)))

;;; Contract I'm looking for states that the function takes and
;;; returns gt0

;;; (bigger-than-0 -> bigger-than-0) -> any
(defn save [f] (dosync (ref-set
			saved
			(wrap (make-contract gt0 gt0) f "p" "n")) ))


;;; bigger-than-0 -> bigger-than-0
;;; TODO We're not yet checkig return values
(defn ff-use [n] (saved (wrap gt0 n "p" "n")))


(defn naked-use
  "Enforces no contract of its own"
  [n]
  (saved n))

(defn returns-lt0 [_]
  -1)

(deftest ff-example
  (save (fn [_] 50))
  (is (= 50 (ff-use 42)))
  (is (thrown? java.lang.Exception (ff-use -1)))
  (save (fn [_] -1))
  (is (thrown? java.lang.Exception (ff-use 42))))


(defn returns-gt0 [_]
  1)

(defn requires-ho-returning-gt0 [f]
  ((wrap (make-contract lenient gt0) f "p" "n") 42))


(deftest post-checks
  (is (= 1 (requires-ho-returning-gt0 returns-gt0)))
  (is (thrown? java.lang.Exception
	       (requires-ho-returning-gt0 returns-lt0))))



;;; So how to generate code that looks like
;;; (defn sqrt-1 [n]
;;;   (Math/sqrt   (wrap gt0 n "p" "n")))

;;; I'd like to type:
;;; (defcontract sqrt-c [ {n gt0} ]...)

;; (defmacro defcontract [ n c body]
;;   `(defn foo [~n]
;;      ~(apply-template  '[~n]
;; 		      '~body
;; 		      '[(wrap ~c  ~n "p" "n")])))


(defmacro defcontract [n c body]
  (apply-template '[n] 'body '[c]))

(defmacro expt [n c body]
  `(defn bar [~n ~c]
    ~(apply-template `[~n] '(+ a 5) `[(modify ~n)])))



;; this one looks like its working: but fails with recursive calls to apply-template



(defmacro defcontract [fn-name argv ctxv body]
  (assert (vector? argv))
  (assert (vector? ctxv))
  (assert (list? body))
  `(defn ~fn-name  ~argv ~(apply-template
			  argv
			  body
			  (vec (map wrap-contract argv ctxv)))))
;; need to transform ctxv in line above to `(wrap c n "p" "n")' in
;; line above.


;; so simplify to using clojure.core/replace

;; (defmacro grccontract [fn-name a c body]
;;   `(defn ~fn-name [~a]
;;      ~(replace {a :a} body)))

;From tufflax on #clojure: (clojure.walk/postwalk-replace {'n '(modify n)} '(+ (+ n 1) 5))

(defmacro grccontract [fn-name a c body]
  `(defn ~fn-name [~a]
     ~(clojure.walk/postwalk-replace {a `(wrap ~c ~a "p" "n")} body)))



;; as and cd are vectors of args and contracts.  subst builds up a map
;; of args against replacements for use by postwalk-replace.
(defmacro grccontract2 [fn-name as cs body]
  (assert (vector? as))
  (assert (vector? cs))
  (let [subst (zipmap as (map (fn [a c] `(wrap ~c ~a)) as cs)) ] 
       `(defn ~fn-name ~as
	  ~(clojure.walk/postwalk-replace subst body))))






(defn wrap-contract [n c]
  (list * c n))