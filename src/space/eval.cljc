(ns space.eval
  (:refer-clojure :exclude [eval])
  (:use [space.group]))

(defn atom? [x]
  (instance? #?(:clj clojure.lang.Atom :cljs cljs.core.Atom)
             x))


;; env
(defrecord Proc [env args body])

(defn env-get [env symbol]
  (let [resolved (get env symbol)]
    (cond (atom? resolved) (deref resolved)
          resolved resolved
          :else (str "unresolved symbol " symbol))))

(defn env-set [env symbol value]
  (assoc env symbol (atom value)))

(defn env-set-multiple [env symbols values]
  (if (empty? symbols)
    env
    (recur (env-set env (first symbols) (first values))
           (rest symbols)
           (rest values))))

(defn env-set! [env symbol value]
  (swap! (get env symbol) (constantly value))
  env)

(defn env-set-multiple! [env symbols values]
  (if (empty? symbols) env
      (recur (env-set! env (first symbols) (first values))
             (rest symbols)
             (rest values))))

;; eval
(declare eval)
(declare apply-bindings)

(defn eval-if [exp env]
  (if (eval (exp 1) env)
    (eval (exp 2) env)
    (eval (exp 3) env)))

(defn eval-letrec [exp env]
  (let [bindings (exp 1)
        body (exp 2)
        keys (take-nth 2 bindings)
        exps (take-nth 2 (rest bindings))
        env (env-set-multiple env
                              keys
                              (repeat 'uninitialised))]
    (env-set-multiple! env
                       keys
                       (map #(eval % env)
                            exps))
    (eval body env)))

(defn eval-let [exp env]
  (let [bindings (exp 1)
        body (exp 2)]
    (eval body
          (apply-bindings env bindings))))

(defn eval-fn [exp env]
  (->Proc env (exp 1) (exp 2)))

(defn apply-bindings [env keyvals]
  (if (empty? keyvals)
    env
    (recur (env-set env
                    (first keyvals)
                    (eval  (second keyvals) env))
           (drop 2 keyvals))))

(defn call-proc [proc args]
  (eval (:body proc)
        (merge (:env proc)
               (group-name-positionals (:args proc)
                                       args
                                       0))))

(defn eval-apply* [f group from]
  (cond (fn? f)
        (apply f (group-positionals group from))
        (instance? Proc f)
        (eval (:body f)
              (merge (:env f)
                     (group-name-positionals (:args f)
                                             group
                                             from)))
        (coll? f)
        (get f (group from))))

(defn eval-apply [exp env]
  (let [group (group-map-values #(eval % env) exp)
        f (group-first group)]
    (eval-apply* f group 1)))

(defn eval-unquote1 [exp env]
  (eval (exp 1) env))

(defn eval-unquote [exp env]
  (eval (group-rest exp) env))

(defn eval-quote* [exp env]
  (cond (has-head? exp 'unquote1) (eval-unquote1 exp env)
        (has-head? exp 'unquote) (eval-unquote exp env)
        (coll? exp) (group-map-values #(eval-quote* % env) exp)
        :else exp))

(defn eval-quote [exp env]
  (eval-quote* (group-rest exp) env))

(defn eval-quote1 [exp env]
  (eval-quote* (exp 1) env))

(declare eval-do)


(defn eval-do-defrec [form rest env bindings]
  (if (has-head? form 'defrec)
    (recur (group-first rest)
           (group-rest rest)
           env
           (-> bindings
               (conj (form 1))
               (conj (form 2))))
    (eval-letrec ['letrec bindings (into (conj '[do] form) rest)] env)))

(defn eval-do-def [form rest env]
  (let [key (form 1)
        exp (form 2)
        val (eval exp env)]
    (eval-do rest
             (env-set env key val))))

(defn eval-do [form env]
  (let [exp (group-first form)
        rest (group-rest form)]
    (cond (empty? rest) (eval exp env)
          (= exp 'do) (eval-do rest env)
          (has-head? exp 'defrec) (eval-do-defrec exp rest env [])
          (has-head? exp 'def) (eval-do-def exp rest env)
          :else (do (eval exp env)
                    (eval-do rest env)))))

(def special-forms
  {'if eval-if
   'letrec eval-letrec
   'let eval-let
   'fn eval-fn
   'quote eval-quote
   'quote1 eval-quote1
   'do eval-do})

(defn eval [exp env]
  (cond (number? exp) exp
        (string? exp) exp
        (nil? exp) exp
        (symbol? exp) (env-get env exp)
        (special-forms (group-first exp)) ((special-forms (group-first exp)) exp env)
        (coll? exp) (eval-apply exp env)
        :else "unkown"))

;; test
(def default-env
  {'+ +
   '- -
   '* *
   '/ /
   'dec dec
   'inc inc
   '= =
   'first group-first
   'rest group-rest
   'println println
   'positionals #(group-positionals %1 0)
   'named group-named
   'map-values (fn [m f] (group-map-values #(eval-apply* f [%] 0) m))
   'map (fn [m f] (group-map #(eval-apply* f [%1 %2] 0) m))
   'for-each (fn [m f] (group-for-each #(eval-apply* f [%1 %2] 0) m))
   'append group-append
   'pop group-pop
   'set group-set
   'reduce (fn [m i f] (group-reduce-values #(eval-apply* f [%1 %2] 0) i m))})


   ;; 'map (fn [m f] m)



(eval '[do
        [defrec f [fn [] [g]]]
        [defrec g [fn [] 1]]
        [f 0]]
      default-env)

(eval '[do
        [def x 10]
        [def y [+ x 10]]
        y]
 default-env)

(eval-letrec '[letrec [x 1, y x] x] {})

(eval '[letrec [add [fn [a b]
                     [if [= a 0]
                      b
                      [add [dec a] [inc b]]]]]
        [add 2 3]]
      default-env)

(eval '[+ 2 3] default-env)

;; (remove-method print-method clojure.lang.Atom)
;; (set! *print-level* 4)
