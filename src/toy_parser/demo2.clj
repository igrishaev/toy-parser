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


(comment

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

  )
