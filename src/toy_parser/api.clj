(ns toy-parser.api
  (:refer-clojure
   :exclude [char + range or * cat take
             #_
             take-while]))

(set! *warn-on-reflection* true)

(alias 'cc 'clojure.core)
;; (alias 'p 'toy-parser.api)


(defn char
  "
  Read a given (expected) character.
  "
  [c]
  (fn [chars]
    (when-let [c1 (first chars)]
      (when (= c c1)
        [c1 (next chars)]))))


(defn enum
  "
  Read any of passed characters.
  "
  [& chars]
  (let [-set (set chars)]
    (fn [chars]
      (when-let [c (first chars)]
        (when (contains? -set c)
          [c (next chars)])))))


(defn range
  "
  Read any character that matches the range
  [c1 ... c2] (both inclusive).
  "
  [c1 c2]
  (fn [chars]
    (when-let [c (first chars)]
      (when (<= (int c1) (int c) (int c2))
        [c (next chars)]))))


(defn excact
  "
  Read a sequence of characters matching a given
  string.
  "
  [string]
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
  "
  With no arguments, skip a single character assuming
  the incoming sequence is not empty. With a single
  argument n, skip exact n characters.
  "
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

(defn *
  "
  Given a parser, read 0 or more matches for it.
  "
  [p]
  (fn [chars]
    (loop [acc []
           chars chars]
      (if-let [[i chars] (p chars)]
        (recur (conj acc i)
               chars)
        [acc chars]))))

(defn +
  "
  Given a parser, read 1 or more matches for it.
  "
  [p]
  (fn [chars]
    (when-let [[i chars] (p chars)]
      (loop [acc [i]
             chars chars]
        (if-let [[i chars] (p chars)]
          (recur (conj acc i)
                 chars)
          [acc chars])))))


(defn ?
  "
  Given a parser, read 0 or 1 matches for it.
  "
  [p]
  (fn [chars]
    (if-let [[i chars] (p chars)]
      [i chars]
      [nil chars])))


(defn or
  "
  Apply the first matching parser. At least one
  should be valid.
  "
  [& parsers]
  (fn [chars]
    (some (fn [p]
            (when-let [[i chars] (p chars)]
              [i chars]))
          parsers)))


(defn nor
  "
  Like `or` but prepends each result node with
  a tag (n - named).
  "
  [& tag-parser]
  (assert (-> tag-parser count even?)
          "the 'nor' parser must have even number of arguments")
  (let [pairs (partition 2 tag-parser)]
    (fn [chars]
      (some (fn [[tag parser]]
              (when-let [[i chars] (parser chars)]
                [[tag i] chars]))
            pairs))))


(defn cat
  "
  Return a tuple of all parsers applied in the
  same order. All parsers should match.
  "
  [& parsers]
  (fn [chars]
    (loop [chars chars
           parsers parsers
           acc []]
      (if-let [p (first parsers)]
        (when-let [[i chars] (p chars)]
          (recur chars
                 (next parsers)
                 (conj acc i)))
        [acc chars]))))


(defn ncat
  "
  Like `cat` but prepends each result node with
  a tag (n - named).
  "
  [& tag-parser]
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


(defn sep
  "
  Given the main parser, and a separator parser,
  return all the main parsers results skipping
  the separator one.
  "
  [p p-sep]
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
