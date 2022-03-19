(ns space.main
  [:require cljs.pprint space.parser])

(defn parse [s]
  (with-out-str
    (cljs.pprint/pprint (space.parser/parse s))))
