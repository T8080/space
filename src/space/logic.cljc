(ns space.logic
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def file "src/space/logictest.cljc")

(def ast (read-string
          (str \[ (slurp "src/space/logictest.cljc") \])))

(defn str->predicates [source]
  (->> (read-string
        (str \[ source \]))
       (filter #(and (= (first %) 'defn)
                     (str/includes? (second %) "?")))
       (map (fn [[_ name args body]]
              {:name name :args args :body body}))))


(defn file->type-hiearchy [file]
  (intern-predicates
   '{(number?) number?}
   (str->predicates (slurp file))))

(def predicate-definitions
  (filter #(and (= (first %) 'defn)
                (str/includes? (second %) "?"))
          ast))

(def predicates-map
  (reduce (fn [m [_ name args body]]
            (assoc m name {:name name :args args :body body}))
          {}
          predicate-definitions))

(def predicates
  (map (fn [[_ name args body]]
         {:name name, :args args, :body body})
       predicate-definitions))

(defn intern-predicates-map [tie predicates]
  (reduce (fn [tie pred]
            (tie-assoc tie))))

(defn intern-predicates [tie predicates]
  (reduce (fn [tie pred]
            (tie-assoc tie (pred->path pred) (pred :name)))
          tie
          predicates))

(def tie (inter-predicates {} predicates))

(defn pred->path [pred]
  (cond (map? pred) (recur (pred :body))
        (= (first pred) 'and) (map pred->path (rest pred))
        :else (conj (drop 2 pred) (first pred))))

(pred->path (second predicates))

(tie-assoc {} (pred->path (first predicates)) 'complex?)

(defn tie-assoc [tie path value]
  (assoc-in tie path value))

(defn apply-tie-pred [pred x]
  (let [[pred-name & pred-args] pred]
    (if (resolve pred-name)
      (apply (var-get (resolve pred-name)) x pred-args)
      (throw "error"))))

(apply-tie-pred '(contains? :x) {:x 1})

(contains? {:x 1} :x)


(var-get (resolve 'contains?))

(defn tie-get [tie x deepest-match depth]
  (let [[pred consequent] (first tie)]
    (cond (empty? tie)
          deepest-match

          (not (apply-tie-pred pred x))
          (recur (dissoc tie pred) x deepest-match depth)

          (map? consequent)
          (recur consequent x pred (inc depth))

          :else consequent)))

(tie-get tie {:real 3, :i 4} nil 0)
(tie-get tie {:x 3, :y 4} nil 0)
(tie-get tie {} nil 0)
(tie-get tie {:x 3} nil 0)

(defn type-of [types x]
  (tie-get types x nil 0))

(type-of (file->type-hiearchy file) {})
