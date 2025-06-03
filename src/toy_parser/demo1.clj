(ns toy-parser.demo1
  (:require
   [clojure.string :as str]
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

(comment

  (parse-postfix "1 2 3 + 3 1 foo -")

  [[:operand [:int [\1]]]
   [:operand [:int [\2]]]
   [:operand [:int [\3]]]
   [:operator [:plus \+]]
   [:operand [:int [\3]]]
   [:operand [:int [\1]]]
   [:operand [:var [\f \o \o]]]
   [:operator [:minus \-]]]

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

  )
