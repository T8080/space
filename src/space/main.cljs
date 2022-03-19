(ns space.main
  [:require
   cljs.pprint
   space.parser])

(defn parse [s]
  (with-out-str
    (cljs.pprint/pprint (space.parser/parse s))))

(defn init []
  (let [input (js/document.querySelector "#input")
        button (js/document.querySelector "#button")
        output (js/document.querySelector "#output")]
    (set! (.-onclick button)
          #(set! (.-innerHTML output)
                 (parse (.-value input))))))

