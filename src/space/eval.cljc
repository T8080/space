(ns space.eval)

(defrecord Proc [env args body])
;;

(defn atom? [x]
  (instance? #?(:clj clojure.lang.Atom :cljs cljs.core.Atom)
             x))

;; env
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

(defn eval-if [[condition consequent alternative] env]
  (if (eval condition env)
    (eval consequent env)
    (eval alternative env)))

(defn eval-letrec [[bindings body] env]
  (let [keys (take-nth 2 bindings)
        exps (take-nth 2 (rest bindings))
        env (env-set-multiple env
                              keys
                              (repeat 'uninitialised))]
    (env-set-multiple! env
                       keys
                       (map #(eval % env)
                            exps))
    (eval body env)))

(defn eval-let [[bindings body] env]
  (eval body
        (apply-bindings env bindings)))

(defn eval-fn [[args body] env]
  (->Proc env args body))

(defn apply-bindings [env keyvals]
  (if (empty? keyvals)
    env
    (recur (env-set env
                    (first keyvals)
                    (eval  (second keyvals) env))
           (drop 2 keyvals))))

(defn eval-apply-old [[exp & args] env]
  (let [f (eval exp env)
        args (map #(eval % env) args)]
    (if (fn? f)
      (apply f args)
      (eval (:body f)
            (env-set-multiple (:env f)
                              (:args f)
                              args)))))

(defn eval-apply [exp env]
  (let [evald (map-values #(eval % env)
                          (ensure-map exp))
        f (evald 0)
        args (dissoc evald 0)]
    (if (fn? f)
      (apply f (positional-values args 1))
      (eval (:body f)
            (env-set-multiple2 (:env f)
                               (:args f)
                               args)))))

(env-set-multiple2 {} ['x] {'x 3})
(env-set-multiple2 {} ['x] {1 1})

(eval-apply {0 '+, 1 3, 2 5} default-env)
(eval-apply '[+ 3 5] default-env) {}

(eval '{0 [fn [x] [ + 1 x]],
        x 3}
      default-env)

(eval '{0 [fn [x] [ + 1 x]],
        1 3}
      default-env)

(eval '[[fn [x] [ + 1 x]],
        3]
      default-env)

(defn ensure-map [v]
  (if (vector? v)
    (vec->map v)
    v))

(defn vec->map [v]
  (zipmap (range (count v)) v))

(defn env-set-multiple2 [env keys args]
  (loop [i 1, pargs {}]
    (if (args i)
      (recur (inc i)
             (assoc pargs (keys (dec i)) (args i)))
      (merge env pargs args))))



(defn positional-values [m i]
  (for [i (iterate inc i)
        :while (contains? m i)]
    (m i)))

(defn map-values [f map]
  (if (vector? map)
    (mapv f map)
    (map-map-values f map)))

(defn map-map-values [f map]
  (let [nmap (transient {})]
    (doseq [key (keys map)]
      (assoc! nmap key (f (map key))))
    (persistent! nmap)))


(defn eval-quote [[body] env]
  body)

(declare eval-do)

(defn eval-do-defrec [[exp & rest] env bindings]
  (if (and (seq? exp) (= (first exp) 'defrec))
    (recur rest
           env
           (-> bindings
               (conj (nth exp 2))
               (conj (nth exp 1))))
    (eval-letrec (cons bindings (list (cons 'do (cons exp rest)))) env)))

(defn eval-do-def [[[_ key exp] & rest] env]
  (eval-do rest
           (env-set env
                    key
                    (eval exp env))))

(defn eval-do [[exp & rest] env]
  (if (empty? rest)
    (eval exp env)
    (cond (and (seq? exp) (= (first exp) 'defrec)) (eval-do-defrec (cons exp rest) env '())
          (and (seq? exp) (= (first exp) 'def)) (eval-do-def (cons exp rest) env)
          :else (do (eval exp env)
                    (eval-do rest env)))))

(def special-forms
  {'if eval-if
   'letrec eval-letrec
   'let eval-let
   'fn eval-fn
   'quote eval-quote
   'do eval-do})

(defn eval [exp env]
  (cond (number? exp) exp
        (symbol? exp) (env-get env exp)
        (special-forms (first exp)) ((special-forms (first exp))
                                     (rest exp) env)
        (coll? exp) (eval-apply exp env) :else "unkown"))

;; test
(def default-env
  {'+ +})
   ;; '- -
   ;; '* *
   ;; '/ /
   ;; 'dec dec
   ;; 'inc inc
   ;; '= =})

(eval '(do
         (defrec f (fn () (g)))
         (defrec g (fn () 1))
         4
         (f 0))
      default-env)

(eval-do '(do
            (def x 10)
            (def y (+ x 10))
            y)
         default-env)

(eval-letrec '((x 1, y x) x) default-env)

(eval '(letrec (add (fn [a b]
                      (if (= a 0)
                        b
                        (add (dec a) (inc b)))))
        (add 2 3))
      default-env)


(eval [+ 2 3] default-env)

(eval '(letrec (add (fn [a b]
                      (if (= a 0)
                        b
                        (add (dec a) (inc b)))))
               (add 2 3))
      default-env)

(eval
 '(letrec (even
           (fn [x]
             (if (= x 0)
               'even
               (uneven (dec x))))
           uneven
           (fn [x]
             (if (= x 0)
               'uneven
               (even (dec x)))))
          (even 5))
 {'= =, 'dec dec})

;; (remove-method print-method clojure.lang.Atom)
;; (set! *print-level* 4)

(let [a (atom nil)]
  (swap! a (constantly a))
  @a)
