(ns space.eval)

(defrecord Proc [env args body])

(defn env-get [env symbol]
  (let [resolved (get env symbol)]
    (cond (instance? clojure.lang.Atom resolved) (deref resolved)
          resolved resolved
          :else (str "unresolved symbol " symbol))))

(defn env-set [env symbol value]
  (assoc env symbol (atom value)))

(defn env-set! [env symbol value]
  (swap! (get env symbol) (constantly value)))

(defn env-set-multiple [env symbols values]
  (if (empty? symbols)
    env
    (recur (env-set env (first symbols) (first values))
           (rest symbols)
           (rest values))))

(declare eval)
(defn apply-bindings [env keyvals]
  (if (empty? keyvals)
    env
    (recur (env-set env
                    (first keyvals)
                    (eval  (second keyvals) env))
           (drop 2 keyvals))))

(defn call [exp env]
  (let [f (eval (first exp) env)
        args (map #(eval %1 env) (rest exp))]
    (if (fn? f)
      (apply f args)
      (eval (:body f)
            (env-set-multiple (:env f)
                              (:args f)
                              args)))))

(defn eval [exp env]
  (cond (number? exp) exp
        (symbol? exp) (env-get env exp)
        (= 'if (first exp)) (if (eval (nth exp 1) env)
                              (eval (nth exp 2) env)
                              (eval (nth exp 3) env))
        (= 'letrec (first exp)) (eval-letrec exp env)
        (= 'let (first exp)) (eval (nth exp 2)
                                   (apply-bindings env (nth exp 1)))
        (= 'fn (first exp)) (->Proc env
                                    (nth exp 1)
                                    (nth exp 2))
        (list? exp) (call exp env)
        :else "unkown"))

(defn eval-letrec [exp env]
  (let [[_ [key exp] body] exp
        renv (env-set env key 'undefined)
        val (eval exp renv)]
    (env-set! renv key val)
    (eval body renv)))

(eval-letrec '(letrec (f (fn [x]
                           (if (= x 0)
                             1
                             (* x (f (dec x))))))
                      (f 4))
             default-env)

(eval '(let (f (fn (x) 1))
         (f 0))
      {})

(eval '(= 0 1)
      {'= =})

(set! *print-level* 3)
(set! *print-length* 10)
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

(eval '(letfn add (a b)
              (if (= a 0)
                b
                (add (dec a) (inc b)))
              (add 2 3))
      default-env)

(seval '(inc 1) default-env)
