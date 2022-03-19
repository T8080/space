(ns space.main
  [:require
   [cljs.pprint :as pp]
   space.parser])

(defn process [s]
  (let [tree (space.parser/parse s)
        lisp (space.parser/strip tree)]
    (with-out-str
      (binding [pp/*print-pprint-dispatch* pp/code-dispatch
                pp/*print-miser-width* 20
                pp/*print-right-margin* 30]
        (pp/pprint lisp))
      (newline)
      (pp/pprint tree))))

cljs.pprint/*print-pprint-dispatch*

(process "
def add; fn (a b)
  if a .= 0
    b
    add(a.++, b.++)
")

(defn init []
  (let [input (js/document.querySelector "#input")
        button (js/document.querySelector "#button")
        output (js/document.querySelector "#output")]
    (set! (.-onclick button)
          #(set! (.-innerHTML output)
                 (process (.-value input))))))
