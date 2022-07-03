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
def l; quote
  a b c e

l
")


(run "

def posts; quote
  (title: title-1
   content: content-1)
  (title: title-2
   content: content-2)

def render-post; fn (p)
  quote post
    strong $p'title;
    p $p'content;

def render-post-page; fn (ps)
  quote div
    h2 blog-posts;
    br;
    unquote map posts render-post;
    br;

render-post-page posts;

")

(run "

def l; quote
  1 2 3 4

l .map (fn (x) x)
")



(parser/strip (parser/parse "

list.map(f)

"))

(parser/parse "
list.map(f)
")

(run "

def map; fn (m f)
  if m .= '()
    m
    f(m.first) .cons m.rest.map(f)

")

(parser/parse "

a: a
  b: b

")

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
