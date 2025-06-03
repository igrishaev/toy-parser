# Toy Parser

A simple, immutable combinator parser for educational purpose.

The `api` namespace provides a number of functions to build parsers. Each parser
is a function accepting parameters and returning a parsing function. The parsing
function is always called with a sequence of characters. The result is either a
vector `[result, chars-rest]`, where:

- `result` is what the parser has taken from the characters sequence;
- `chars-rest` is the rest of the initial sequence of characters.

Should a parser fail, it turns nil. Not so informative but it's up to you how to
improve it.

Keep in mind, this is **not a library** but rather an example of excercise
students usually do. Surely there is ChatGPT for that, but I hope you'll want to
do it by your own.

The parser ships two demos (demo1 and demo2) with examples how to parse postfix and
infix notations.

# A Small Example Of Infix Notation:

~~~clojure
(ns toy-parser.demo1
  (:require
   [toy-parser.api :as p]))

;;
;; Parse a simple infix notation fragment
;; like '1 2 3 + 3 1 foo -'
;;

(def p-int
  ;; very naive
  (p/+ (p/range \0 \9)))

(def p-var
  (p/+ (p/range \a \z)))

(def p-operator
  (p/nor :minus (p/char \-)
         :plus (p/char \+)
         :times (p/char \*)
         :div (p/char \\)))

(def p-operand
  (p/nor :int p-int
         :var p-var))

(def p-item
  (p/nor :operand p-operand
         :operator p-operator))

(def p-ws
  (p/+ (p/enum \space \r \n \t)))

(def p-expression
  (p/sep p-item p-ws))


(defn parse-postfix [string]
  (-> string
      (str/trim)
      (seq)
      (p-expression)))

(parse-postfix "1 2 3 + 3 1 foo -")

[[:operand [:int [\1]]]
 [:operand [:int [\2]]]
 [:operand [:int [\3]]]
 [:operator [:plus \+]]
 [:operand [:int [\3]]]
 [:operand [:int [\1]]]
 [:operand [:var [\f \o \o]]]
 [:operator [:minus \-]]]
~~~

Now let's wrap it back:

~~~clojure
(defn render-postfix [parsed]
  (str/trim
   (reduce
    (fn [acc [tag inner]]
      (case tag
        :operand
        (let [[tag chars] inner]
          (case tag
            (:int :var)
            (str acc (apply str (-> inner second)) \space)))
        :operator
        (str acc (-> inner second) \space)))
    ""
    parsed)))

(render-postfix
   [[:operand [:int [\1]]]
    [:operand [:int [\2]]]
    [:operand [:int [\3]]]
    [:operator [:plus \+]]
    [:operand [:int [\3]]]
    [:operand [:int [\1]]]
    [:operand [:var [\f \o \o]]]
    [:operator [:minus \-]]])

"1 2 3 + 3 1 foo -"
~~~

## Postfix Notation

~~~clojure
(ns toy-parser.demo2
  (:require
   [clojure.string :as str]
   [toy-parser.api :as p]))

;;
;; Parse a complex infix notation like
;; (a + b) * (c / 3) + (1 * (a - b))
;;
;;
;; expression
;;   el ws? operator ws? expression
;;   el
;;
;; el
;;   group
;;   operand
;;
;; group
;;   ( ws? expression ws? )
;;
;;  operand
;;    var
;;    int
;;
;; operator
;;   -
;;   +
;;   *
;;   /


(def p-int
  (p/+ (p/range \0 \9)))

(def p-var
  (p/+ (p/range \a \z)))


(def p-operand
  (p/nor :int p-int
         :var p-var))

(def p-operator
  (p/nor :minus (p/char \-)
         :plus (p/char \+)
         :times (p/char \*)
         :div (p/char \\)))

(def p-ws?
  (p/* (p/enum \space \r \n \t)))

;; used for recursive declaration
(declare p-expr)

(def p-group
  (p/ncat :lparen (p/char \()
          :ws p-ws?
          :expr (var p-expr)
          :ws p-ws?
          :rparen (p/char \))))

(def p-el
  (p/nor :group p-group
         :operand p-operand))


(def p-expr
  (p/nor :triple (p/ncat :el p-el
                         :ws p-ws?
                         :operator p-operator
                         :ws p-ws?
                         ;; will be an actual parser later
                         :expr (var p-expr))
         :el p-el))


(defn parse-infix [string]
  (p-expr string))

(parse-infix "(a + b) * (b - (a + 42))")

[:triple
 [[:el
   [:group
    [[:lparen \(]
     [:ws []]
     [:expr
      [:triple
       [[:el [:operand [:var [\a]]]]
        [:ws [\space]]
        [:operator [:plus \+]]
        [:ws [\space]]
        [:expr [:el [:operand [:var [\b]]]]]]]]
     [:ws []]
     [:rparen \)]]]]
  [:ws [\space]]
  [:operator [:times \*]]
  [:ws [\space]]
  [:expr
   [:el
    [:group
     [[:lparen \(]
      [:ws []]
      [:expr
       [:triple
        [[:el [:operand [:var [\b]]]]
         [:ws [\space]]
         [:operator [:minus \-]]
         [:ws [\space]]
         [:expr
          [:el
           [:group
            [[:lparen \(]
             [:ws []]
             [:expr
              [:triple
               [[:el [:operand [:var [\a]]]]
                [:ws [\space]]
                [:operator [:plus \+]]
                [:ws [\space]]
                [:expr [:el [:operand [:int [\4 \2]]]]]]]]
             [:ws []]
             [:rparen \)]]]]]]]]
      [:ws []]
      [:rparen \)]]]]]]]
~~~
