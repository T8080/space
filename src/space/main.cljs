(ns space.main
  (:require
   [cljs.pprint :as pp]
   [space.parser :as parser]
   [clojure.string :as str]
   [space.eval :as eval]))

(defn quote->html [x]
  (cond (string? x) x
        (symbol? x) (str x)
        (coll? x)
        (let [tag (eval/group-first x)
              attributes (eval/group-named x)
              body (eval/group-positionals x 1)]
          (str "<" tag " "
               (str/join (map (fn [[k v]] (str k "='" v "'")) attributes))
               ">"
               (str/join (map quote->html body))
               "</" tag ">"))))

(defn output-html [s]
  (let [out (js/document.querySelector "#htmloutput")]
    (set! (.-innerHTML out) s)))

(defn process [s]
  (let [tree (space.parser/parse s)
        lisp (space.parser/strip tree)
        lisp (vec lisp)
        output (eval/eval-do lisp eval/default-env)]
    (with-out-str
      (when (= (eval/group-first output) 'html)
        (output-html (quote->html output)))
      (pp/pprint output)
      (newline)
      (binding [pp/*print-pprint-dispatch* pp/code-dispatch
                pp/*print-miser-width* 20
                pp/*print-right-margin* 30]
        (pp/pprint lisp))
      (newline)
      (pp/pprint tree))))



(output-html "<p>test</p>")

(output-html (quote->html '[p "test2"]))


(quote->html '
 [body
  [div [p "test1"]]
  {0 a, href "link"}
  [div "test2"]])

(defn init []
  (let [input (js/document.querySelector "#input")
        button (js/document.querySelector "#button")
        output (js/document.querySelector "#output")]
    (set! (.-onclick button)
          #(set! (.-innerHTML output)
                 (process (.-value input))))))
