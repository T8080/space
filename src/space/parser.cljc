(ns space.parser
  [:require
   [instaparse.core :as insta]
   [clojure.edn :as edn]])


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
<program>            = vs (exp vs)*

<exp>              = exp-indent-list
<exp-indent-list>  = indent-list  | exp-nesting-list
<exp-nesting-list> = nesting-list | exp-infix
<exp-infix>        = infix        | exp-postfix
<exp-postfix>      = postfix      | exp-pair
<exp-par>          = infix-par    / list
<exp-pair>         = pair         | exp-atom
<exp-atom>         = atom         | exp-par | quote | quote-call

<atom>             = symbol | number | string
quote              = <'\\''> exp-atom
quote-call         = (quote-call | symbol) <'\\''> (symbol | number)

pair               = symbol <':'> hs? exp
                   | symbol <':'> indent exp unindent

indent-list        = exp-infix (hs exp-infix)* indent indent-list-line (vs indent-list-line)* unindent
<indent-list-line> = exp-infix (hs exp-infix)* | exp-indent-list
nesting-list       = exp-infix (hs exp-infix)* <';'> hs? exp-indent-list?
infix              = exp-infix hs dot exp-atom hs exp-postfix
infix-par          = <'('> exp-infix hs dot exp-atom hs exp-postfix <')'>
postfix            = exp-postfix vs? indent? dot exp-atom unindent?
list               = exp-infix? <'('> (exp-infix s)* exp-infix? <')'>

symbol   = #'[^ \"\\',\\n\\(\\);#\\d\\.:]+'
number   = #'\\d+'
string   = <'\"'> #'[^\"]*' <'\"'>

<dot>      = <'.'>
<vs>       = <#'\\n+'>
<hs>       = <#'[ ,]+'>
<indent>   = <'#>'>
<unindent> = <'#<'>
<s>        = <#'(\\n|,| |#>|#<)+'>
")

(insta/parse (insta/parser G) (tokenized-string "

def x; fn
  1 2;

"))

(parse "

x
  y

")

(insta/parse (insta/parser G) "
(a: 1, b: 2)
")

(insta/parse (insta/parser G) "
(deffun add (a b)
  (if (= a 0)
    b
    (add (dec a) (dec b))))
")

(def s
  "
def plus; fn (a b)
  if a .= zero
    b
    plus(a.--, b.++)

plus(1, 2)
")

(def parser (insta/parser G))

(defn parse [s]
  (parser (tokenized-string s)))

(defn any [coll pred]
  (if (empty? coll)
    nil
    (or (pred (first coll))
        (any (rest coll) pred))))

(defn vec->table [list]
  (loop [i 0
         list list
         table {}]
    (if (empty? list)
      table
      (let [entry (first list)]
        (if (and (coll? entry) (= (first entry) :pair))
          (recur i (rest list) (assoc table (entry 1) (entry 2)))
          (recur (inc i) (rest list) (assoc table i entry)))))))

(defn ->table? [coll]
  (if (any coll #(and (coll? %) (= (first %) :pair)))
    (vec->table coll)
    coll))


(defn postfix [receiver exp]
  (if (seq? exp)
    (->table?  (vec (cons (first exp) (cons receiver (rest exp)))))
    (->table? (vec (list exp receiver)))))

(defn strip [tree]
  (insta/transform
   {:list (fn [& x] (->table? (vec x)))
    :indent-list (fn [& x] (->table? (vec x)))
    :nesting-list (fn [& x] (->table? (vec x)))
    :string identity
    :symbol symbol
    :number (fn [x] (edn/read-string x))
    :postfix postfix
    :infix (fn [a op b] (->table? (vec (list op a b))))
    :infix-par (fn [a op b] (->table? (vec (list op a b))))
    :quote (fn [x] ['quote1 x])
    :quote-call (fn [x y] [x ['quote1 y]])}
   tree))


(strip (parse "
\"test\"
"))

(parse "
x'y'z
")


(strip (parse "
(a: 1, b: 2)
"))

(strip (parse s))

(strip (parse "
a.b
"))

(def G2 "
<exp0> = sum       | exp1
<exp1> = mul       | exp2
<exp2> = '(' sum ')' / list / exp3
<exp3> = #'[a-z1-9]+'

sum = exp0 ' + ' exp1
mul = exp1 ' * ' exp2
list = '(' (exp0 ' ')* exp0? ')'
")

(insta/parses (insta/parser G2) "(a (b + c) + d e)")
