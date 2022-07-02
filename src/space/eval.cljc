(ns space.eval
  [:refer-clojure :exclude [eval]])

(defn atom? [x]
  (instance? #?(:clj clojure.lang.Atom :cljs cljs.core.Atom)
             x))

(defn map-values [f map]
  (let [nmap (transient {})]
    (doseq [key (keys map)]
      (assoc! nmap key (f (map key))))
    (persistent! nmap)))

(defn group-first [g]
  (g 0))

(defn group-positions-size [g]
  (cond (vector? g) (count g)
        (map? g)
        (loop [i 0]
          (if (not (contains? g i))
            i
            (recur (inc i))))))

(defn map-rest [g]
  (let [size (group-positions-size g)]
    (-> (reduce (fn [m i]
                  (assoc m (dec i) (m i)))
                g
                (rest (range size)))
        (dissoc (dec size)))))

(reduce (fn [m i]
          (assoc m (dec i) (m i)))
        {0 0, 1 1, 2 2}
        [1 2])

(defn group-rest [g]
  (cond (vector? g) (subvec g 1)
        (map? g) (map-rest g)))

(defn vec-name-positionals [argnames argvec from]
  (zipmap argnames (subvec argvec from)))

(defn map-name-positionals [argnames argmap from]
  (loop [position 0, named {}]
    (if (contains? argmap (+ position from))
      (recur (inc position)
             (assoc named
                    (argnames position)
                    (argmap (+ position from))))
      (merge argmap named))))

(defn group-name-positionals [argnames arggroup from]
  (cond (vector? arggroup) (vec-name-positionals argnames arggroup from)
        (map? arggroup) (map-name-positionals argnames arggroup from)))


(defn group-positionals [g from]
  (cond (vector? g) (subvec g from)
        (map? g)
        (for [i (iterate inc from)
              :while (contains? g i)]
          (g i))))

(defn group-named [g]
  (cond (vector? g) {}
        (map? g)
        (select-keys g (filter symbol? (keys g)))))

(defn group-map [f g]
  (cond (vector? g) (mapv f g)
        (map? g) (map-values f g)))


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

(defn eval-apply [exp env]
  (let [evald (group-map #(eval % env)
                         exp)
        f (group-first evald)
        args evald]
    (cond (fn? f)
          (apply f (group-positionals args 1))
          (instance? Proc f)
          (eval (:body f)
                (merge (:env f)
                       (group-name-positionals (:args f)
                                               args
                                               1)))
          (coll? f)
          (get f (evald 1)))))

(defn eval-quote [exp env]
  (group-rest exp))

(defn eval-quote1 [exp env]
  (exp 1))

(declare eval-do)

(defn has-head? [exp head]
  (and (coll? exp)
       (= (group-first exp) head)))

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
        (nil? exp) nil
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
   '= =})

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
