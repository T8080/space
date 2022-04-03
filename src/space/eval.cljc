(ns space.eval)


(defrecord Proc [env args body])


;; env
(defn env-get [env symbol]
  (let [resolved (get env symbol)]
    (cond (instance? clojure.lang.Atom resolved) (deref resolved)
          resolved resolved
          :else (str "unresolved symbol " symbol))))

(defn env-set [env symbol value]
  (assoc env symbol (atom value)))

(defn env-set-multiple [env symbols values]
  (reduce #(fn [env [k v]]
             (env-set env k v))
          env
          [symbols values]))

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

(defn eval-if [[_ condition consequent alternative] env]
  (if (eval condition env)
    (eval consequent env)
    (eval alternative env)))

(defn eval-letrec [[_ bindings body] env]
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

(defn eval-let [[_ bindings body] env]
  (eval body
        (apply-bindings env bindings)))

(defn eval-fn [[_ args body] env]
  (->Proc env args body))

(defn apply-bindings [env keyvals]
  (if (empty? keyvals)
    env
    (recur (env-set env
                    (first keyvals)
                    (eval  (second keyvals) env))
           (drop 2 keyvals))))

(defn eval-apply [[f & args] env]
  (if (fn? f)
    (apply f args)
    (eval (:body f)
          (env-set-multiple (:env f)
                            (:args f)
                            (map #(eval % env)
                                 args)))))

(defn eval-quote [[_ body] env]
  body)

(def special-forms
  {'if eval-if
   'letrec eval-letrec
   'let eval-let
   'fn eval-fn
   'quote eval-quote})

(defn eval [exp env]
  (cond (number? exp) exp
        (symbol? exp) (env-get env exp)
        (special-forms (first exp)) ((special-forms (first exp))
                                     exp env)(list? exp) (call exp env)
        (list? exp) (eval-apply exp env)
        :else "unkown"))

;; test
(eval '(letrec (add (fn [a b]
                      (if (= a 0)
                        b
                        (add (dec a) (inc b)))))
               (add 4 3))
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

(remove-method print-method clojure.lang.Atom)

(let [a (atom nil)]
  (swap! a (constantly a))
  @a)

(def default-env
  {'+ +
   '- -
   '* *
   '/ /
   'dec dec
   'inc inc
   '= =})

