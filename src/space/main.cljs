(ns space.main
  (:require
   [cljs.pprint :as pp]
   [space.parser :as parser]
   [space.eval :as eval]))



(defn process [s]
  (let [tree (space.parser/parse s)
        lisp (space.parser/strip tree)
        lisp (vec lisp)]
    (with-out-str
      (pp/pprint (eval/eval-do lisp eval/default-env))
      (newline)
      (binding [pp/*print-pprint-dispatch* pp/code-dispatch
                pp/*print-miser-width* 20
                pp/*print-right-margin* 30]
        (pp/pprint lisp))
      (newline)
      (pp/pprint tree))))


;; (process "
;; letfn add (a b)
;;   if a .= 0
;;     b
;;     add(a.dec, b.inc)
;;   add(2, 3)
;; ")

(defn init []
  (let [input (js/document.querySelector "#input")
        button (js/document.querySelector "#button")
        output (js/document.querySelector "#output")]
    (set! (.-onclick button)
          #(set! (.-innerHTML output)
                 (process (.-value input))))))
