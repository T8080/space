(ns space.main)

(defn parse [s]
  (with-out-str
    (cljs.pprint/pprint (space.parser/parse s))))
