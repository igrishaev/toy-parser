(ns toy-parser.core
  (:require
   [clojure.string :as str])
  (:import
   java.util.Stack))

(set! *warn-on-reflection* true)


;; postfix grammar example
;; A B + C *

;; number
;;   0-9+
;;
;; var
;;   a-zA-Z+
;;
;; operand
;;   number
;;   var

;; operator
;;   -
;;   +
;;   *
;;   /
;;
;; item
;;   operand
;;   operator
;;
;; expression
;;   item+

(defn p-char [c]
  (fn [chars]
    (when-let [c1 (first chars)]
      (when (= c c1)
        [c1 (next chars)]))))


(defn p-enum [& cs]
  (let [char-set (set cs)]
    (fn [chars]
      (when-let [c (first chars)]
        (when (contains? char-set c)
          [c (next chars)])))))


(defn p-range [c1 c2]
  (fn [chars]
    (when-let [c (first chars)]
      (when (<= (int c1) (int c) (int c2))
        [c (next chars)]))))


(defn p+ [p]
  (fn [chars]
    (when-let [[i chars] (p chars)]
      (loop [acc [i]
             chars chars]
        (if-let [[i chars] (p chars)]
          (recur (conj acc i)
                 chars)
          [acc chars])))))


(defn p* [p]
  (fn [chars]
    (loop [acc []
           chars chars]
      (if-let [[i chars] (p chars)]
        (recur (conj acc i)
               chars)
        [acc chars]))))


(defn p-or [& tag-parser]
  (let [pairs (partition 2 tag-parser)]
    (fn [chars]
      (some (fn [[tag parser]]
              (when-let [[i chars] (parser chars)]
                [[tag i] chars]))
            pairs))))


(defn p-sep [p p-sep]
  (fn [chars]
    (loop [acc []
           chars chars]
      (when-let [[i chars] (p chars)]
        (let [acc (conj acc i)]
          (if-let [[_ chars] (p-sep chars)]
            (recur acc chars)
            [acc chars]))))))


(def p-int
  (p+ (p-range \0 \9)))

(def p-var
  (p+ (p-range \a \z)))

(def p-operator
  (p-or :minus (p-char \-)
        :plus (p-char \+)
        :times (p-char \*)
        :div (p-char \\)))

(def p-operand
  (p-or :int p-int
        :var p-var))

(def p-item
  (p-or :operand p-operand
        :operator p-operator))

(def p-ws
  (p+ (p-enum \space \r \n \t)))

(def p-expression
  (p-sep p-item p-ws))


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

  (render-postfix [[:operand [:int [\1]]]
                   [:operand [:int [\2]]]
                   [:operand [:int [\3]]]
                   [:operator [:plus \+]]
                   [:operand [:int [\3]]]
                   [:operand [:int [\1]]]
                   [:operand [:var [\f \o \o]]]
                   [:operator [:minus \-]]])

  "1 2 3 + 3 1 foo -"

  )


;;
;; ---------------- infix notation parser
;;


;;  expression
;;    el ws? operator ws? expression
;;    el

;; el
;;   group
;;   operand

;; group
;;   ( ws? expression ws? )

;;  operand
;;    var
;;    int

;; operator
;;   -
;;   +
;;   *
;;   /


(defn p-cat [& tag-parser]
  (let [pairs (partition 2 tag-parser)]
    (fn [chars]
      (loop [chars chars
             pairs pairs
             acc []]
        (if-let [[tag parser] (first pairs)]
          (when-let [[i chars] (parser chars)]
            (recur chars
                   (next pairs)
                   (conj acc [tag i])))
          [acc chars])))))


(def p-ws?
  (p* (p-enum \space \r \n \t)))

(declare p-expr)

(def p-group
  (p-cat :lparen (p-char \()
         :ws p-ws?
         :expr (var p-expr)
         :ws p-ws?
         :rparen (p-char \))))

(def p-el
  (p-or :group p-group
        :operand p-operand))

(def p-expr
  (p-or :triple (p-cat :el p-el
                       :ws p-ws?
                       :operator p-operator
                       :ws p-ws?
                       :expr (var p-expr))
        :el p-el))


(defn parse-infix [string]
  (p-expr string))


(comment

  (parse-infix "(a + b) * (b - (a + 42))")

  [[:triple
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
   nil]

  )
