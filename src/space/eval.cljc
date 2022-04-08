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

(defn eval-apply [[exp & args] env]
  (let [f (eval exp env)
        args (map #(eval % env) args)]
    (if (fn? f)
      (apply f args)
      (eval (:body f)
            (env-set-multiple (:env f)
                              (:args f)
                              args)))))


(defn eval-quote [[body] env]
  body)

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
        (list? exp) (eval-apply exp env) :else "unkown"))

;; test

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


(eval '(+ 2 3) default-env)

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

(def default-env
  {'+ +
   '- -
   '* *
   '/ /
   'dec dec
   'inc inc
   '= =})

