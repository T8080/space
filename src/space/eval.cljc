(ns space.eval)

(defrecord Proc [env args body])

(defrecord Env-rec [env symbol args body])

(defn env-get [env symbol]
  (if (instance? Env-rec env)
    (if (= (:symbol env) symbol)
      (->Proc env (:args env) (:body env))
      (or (get env symbol)
          (env-get (:env env) symbol)))
    (or (get env symbol)
        "error")))

(defn env-set [env symbol value]
  (assoc env symbol value))


(defn env-set-rec [env symbol args body]
  (->Env-rec env symbol args body))

(defn env-set-multiple [env symbols values]
  (if (empty? symbols)
    env
    (recur (env-set env (first symbols) (first values))
           (rest symbols)
           (rest values))))

(declare seval)

(defn apply-bindings [env keyvals]
  (if (empty? keyvals)
    env
    (recur (env-set env
                    (first keyvals)
                    (seval  (second keyvals) env))
           (drop 2 keyvals))))


(defn call [exp env]
  (let [f (seval (first exp) env)
        args (map #(seval %1 env) (rest exp))]
    (if (fn? f)
      (apply f args)
      (seval (:body f)
            (env-set-multiple (:env f)
                              (:args f)
                              args)))))

(defn seval [exp env]
  (cond (number? exp) exp
        (symbol? exp) (env-get env exp)
        (= 'if (first exp)) (if (seval (nth exp 1) env)
                              (seval (nth exp 2) env)
                              (seval (nth exp 3) env))
        (= 'letfn (first exp)) (seval (nth exp 4)
                                     (env-set-rec env
                                                  (nth exp 1)
                                                  (nth exp 2)
                                                  (nth exp 3)))
        (= 'let (first exp)) (seval (nth exp 2)
                                   (apply-bindings env (nth exp 1)))
        (= 'fn (first exp)) (->Proc env
                                    (nth exp 1)
                                    (nth exp 2))
        (list? exp) (call exp env)
        :else "error"))

(def default-env
  {'+ +
   '- -
   'dec dec
   'inc inc
   '= =})

(seval '(letfn add (a b)
               (if (= a 0)
                 b
                 (add (dec a) (inc b)))
               (add 2 3))
      default-env)

(seval '(inc 1) default-env)
