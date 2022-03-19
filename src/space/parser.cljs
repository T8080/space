(ns space.parser
  [:require [instaparse.core :as insta]])


(defn indent-depth [s] (if (= (first s) \space)
                         (+ 1 (indent-depth (rest s)))
                         0))

(defn strip-indent [d s]
  (if (= (first s) \space)
    (recur (+ d 1) (rest s))
    [d s]))

(defn tokenize [indent remaining result]
  (cond (= (first remaining) \newline)
        (let [[depth r] (strip-indent 0 (rest remaining))]
          (recur depth r
                 (conj result
                       (cond (= depth indent) \newline
                             (> depth indent) "#>"
                             (< depth indent) (apply str (concat  (repeat (quot (- indent depth) 2) "#<") "\n"))))))
        (= (first remaining) nil)
        result
        :else
        (recur indent
               (rest remaining)
               (conj result (first remaining)))))

(defn tokenized-string [s]
  (clojure.string/join
   (tokenize 0
             (clojure.string/replace (str "\n" s "\n") #"\n+" "\n") [])))

(def G "
program            = vs (exp vs)*

<exp>              = exp-indent-list
<exp-indent-list>  = indent-list  | exp-nesting-list
<exp-nesting-list> = nesting-list | exp-infix
<exp-infix>        = infix        | exp-postfix
<exp-postfix>      = postfix      | exp-atom
<exp-atom>         = atom         | list
<atom>             = symbol | number

indent-list        = exp-infix (hs exp-infix)* indent indent-list-line (vs indent-list-line)* unindent
<indent-list-line> = exp-infix (hs exp-infix)* | exp-indent-list
nesting-list       = exp-infix (hs exp-infix)* <'; '> exp-indent-list
infix              = exp-infix hs dot exp-atom hs exp-postfix
postfix            = exp-postfix vs? indent? dot exp-atom unindent?
list               = exp? <'('> (exp hs)* exp <')'>

symbol   = #'[^ ,\\n\\(\\);#\\d\\.]+'
number   = #'\\d+'

<dot>      = <'.'>
<vs>       = <#'\\n+'>
<hs>       = <#'[ ,]+'>
<indent>   = <'#>'>
<unindent> = <'#<'>
")

(def s
  "
def plus; fn (a b)
  if a .+ zero
    b
    plus(a.++, b.++)

plus(1, 2)
")

(def parser (insta/parser G))

(defn parse [s]
  (parser (tokenized-string s)))
