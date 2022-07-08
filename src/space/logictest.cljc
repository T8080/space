(ns space.logictest
  (:use [space.logic]))

(defn complex? [x]
  (and (map? x)
       (contains? x :real)
       (contains? x :i)))

(defn point? [x]
  (and (map? x)
       (contains? x :x)
       (contains? x :y)))

(defn L1 [x y]
  (Math/sqrt (+ (Math/pow x 2)
                (Math/pow y 2))))

(defn abs [x]
  (cond (complex? x) (L1 (x :real) (x :i))
        (point? x) (L1 (x :x) (x :y))
        (number? x) (Math/abs x)))

(def types (file->type-hiearchy *file*))
(type-of types {})              ; map?
(type-of types 14)              ; number?
(type-of types {:real 3, :i 4}) ; complex?
(type-of types {:x 3, :y 4})
