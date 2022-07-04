(ns space.group)


(defn map-values [f map]
  (let [nmap (transient {})]
    (doseq [key (keys map)]
      (assoc! nmap key (f (map key))))
    (persistent! nmap)))

(defn group-first [g]
  (cond (vector? g) (first g)
        (map? g)
        (if (contains? g 0)
          (g 0)
          (second (first g)))))

(defn group-positions-size [g]
  (cond (vector? g) (count g)
        (map? g)
        (loop [i 0]
          (if (not (contains? g i))
            i
            (recur (inc i))))))

(defn map-remove-positional [g from]
  (let [size (group-positions-size g)]
    (-> (reduce (fn [m i]
                  (assoc m i (m (inc i))))
                g
                (range from size))
        (dissoc g (dec size)))))

(defn group-rest [g]
  (cond (vector? g) (subvec g 1)
        (map? g) (map-remove-positional g 0)))

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

(defn group-map-values [f g]
  (cond (vector? g) (mapv f g)
        (map? g) (map-values f g)))

(defn group-map [f g]
  (cond (vector? g) (into {} (map-indexed f g))
        (map g) (into {} (map (fn [[k v]] (f k v)) g))))

(defn group-for-each [f g]
  (cond (vector? g)
        (loop [i 0]
          (if (= i (count g))
            nil
            (do (f i (g i))
                (recur (inc i)))))
        (map? g)
        (doseq [[k v] g] (f k v))))

(defn group-append [g x]
  (cond (vector? g) (conj g x)
        (map? g) (assoc g (group-positions-size g) x)))

(defn group-pop [g]
  (cond
    (vector? g) (pop g)
    (map? g)
    (g (dec (group-positions-size g)))))

(def group-set assoc)

(defn has-head? [exp head]
  (and (coll? exp)
       (= (group-first exp) head)))

(defn group-reduce-values [f init g]
  (cond (vector? g) (reduce f init g)
        (map? g) (reduce-kv (fn [m _ v] (f m v)) init g)))

(group-reduce-values (fn [x v] 1) 0 [0 1 2])

(def group-reduce reduce-kv)
