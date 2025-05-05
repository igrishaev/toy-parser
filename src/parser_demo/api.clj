(ns parser-demo.api
  (:refer-clojure
   :exclude [char + range or * cat take
             #_
             take-while]))

(set! *warn-on-reflection* true)

(alias 'cc 'clojure.core)
(alias 'p 'parser-demo.api)

(defn char [c]
  (fn [chars]
    (when-let [c1 (first chars)]
      (when (= c c1)
        [c1 (next chars)]))))

(defn enum [& chars]
  (let [-set (set chars)]
    (fn [chars]
      (when-let [c (first chars)]
        (when (contains? -set c)
          [c (next chars)])))))

(defn range [c1 c2]
  (fn [chars]
    (when-let [c (first chars)]
      (when (<= (int c1) (int c) (int c2))
        [c (next chars)]))))

(defn excact [string]
  (let [len (count string)]
    (fn [chars]
      (loop [i 0
             chars chars]
        (if (= i len)
          [string chars]
          (when-let [c (first chars)]
            (when (= c (get string i))
              (recur (inc i) (next chars)))))))))

(defn skip
  ([]
   (fn [chars]
     (when-let [c (first chars)]
       [c (next chars)])))
  ([n]
   (fn [chars]
     (loop [i 0
            acc []
            chars chars]
       (if (= i n)
         [acc chars]
         (when-let [c (first chars)]
           (recur (inc i)
                  (conj acc c)
                  (next chars))))))))

;; take-while
;; take-until

(defn * [p]
  (fn [chars]
    (loop [acc []
           chars chars]
      (if-let [[i chars] (p chars)]
        (recur (conj acc i)
               chars)
        [acc chars]))))

(defn + [p]
  (fn [chars]
    (when-let [[i chars] (p chars)]
      (loop [acc [i]
             chars chars]
        (if-let [[i chars] (p chars)]
          (recur (conj acc i)
                 chars)
          [acc chars])))))

(defn ? [p]
  (fn [chars]
    (if-let [[i chars] (p chars)]
      [i chars]
      [nil chars])))

(defn or [& tag-parser]
  (assert (-> tag-parser count even?)
          "the 'or' parser must have even number of arguments")
  (let [pairs (partition 2 tag-parser)]
    (fn [chars]
      (some (fn [[tag parser]]
              (when-let [[i chars] (parser chars)]
                [[tag i] chars]))
            pairs))))

;; ncat nor

(defn cat [& tag-parser]
  (assert (-> tag-parser count even?)
          "the 'cat' parser must have even number of arguments")
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

(defn sep [p p-sep]
  (fn [chars]
    (loop [acc []
           chars chars]
      (when-let [[i chars] (p chars)]
        (let [acc (conj acc i)]
          (if-let [[_ chars] (p-sep chars)]
            (recur acc chars)
            [acc chars]))))))

;; -------


(def WS_CHARS
  #{\space \r \n \t \b \f})

(def ws
  (+ (apply enum WS_CHARS)))

(def ws?
  (* (apply enum WS_CHARS)))

;; -------


(def p-int
  (p/+ (p/range \0 \9)))

(def elem-sep
  (p/cat :ws ws? :comma (p/char \,) :ws ws?))

(def array
  (p/or :array-empty
        (p/cat :ws ws?
               :lparen (p/char \[)
               :ws ws?
               :rparen (p/char \]))

        :array-items
        (p/cat :ws ws?
               :lparen (p/char \[)
               :ws ws?
               :elements (p/sep p-int elem-sep)
               :ws ws?
               :rparen (p/char \]))))
