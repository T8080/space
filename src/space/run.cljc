(ns space.run
  (:require
   [clojure.pprint :as pp]
   [space.parser :as parser]
   [space.eval :as eval]))

(defn run [s]
  (let [tree (parser/parse s)
        lisp (parser/strip tree)
        lisp (vec lisp)]
    (eval/eval-do lisp eval/default-env)))

(run "

def document; quote
  head
    title \"test\";
    link rel: \"stylesheet\";

document'0'2'rel

")

(run "
def l; quote
  a b c e

l
")


(run "

def post; fn (title content); quote
  div id: post
    p $title;
    p $content;

quote; html
  $(post '

")

(run "

quote
  unquote + 2 3

")

(parser/strip (parser/parse "

quote unquote 1 2 3;

"))

(run "

\"test\"

")

(parser/parse "
'$(1 2 3)
")

(run "
'$(+ 2 3)
")





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
