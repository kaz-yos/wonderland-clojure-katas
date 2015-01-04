(ns alphabet-cipher.coder
  (:require [midje.sweet :as mj]))


;;; Creation of the conversion table
;; a-z sequence
(def alphabets (map #(str (char %)) (range 97 (+ 97 26))))
;; infinite lazy seq of a-z
(def alphabets-infinite (cycle alphabets))
;; conversion table as seq of seq
(def conversion-seq-seq (for [n (range 0 26)]
                          (take 26 (drop n alphabets-infinite))))
;; conversion table as map of map
(def conversion-map-map
  (->> conversion-seq-seq
       (map #(zipmap alphabets %), )
       (zipmap alphabets, )))

;;; Test conversion tables
(mj/facts
 (mj/fact
  "Check conversion table"
  (class alphabets)
  => clojure.lang.LazySeq

  (class alphabets-infinite)
  => clojure.lang.LazySeq

  (count conversion-seq-seq)
  => 26
  (set (map count conversion-seq-seq))
  => #{26}

  (count conversion-map-map)
  => 26
  (set (map count (vals conversion-map-map)))
  => #{26}

  (get-in conversion-map-map ["a" "a"])
  => "a"
  (get-in conversion-map-map ["b" "b"])
  => "c"
  (get-in conversion-map-map ["z" "z"])
  => "y"
  ))

;;; Function to convert message to seq of str
(defn seq-of-str
  "Create a seq of strings from a string"
  [word]
  (->> word
       (map char, )
       (map str, )))

;;; Function to create repeating keyword
(defn keyword-repeated
  "keyword repeated as long as message"
  [keyword message]
  (let [cycle-keyword (cycle keyword)]
    (->> cycle-keyword
         (take (count message), )
         (map str, ))))

(mj/facts
 (mj/fact
  "string conversion"
  (seq-of-str "alphabets")
  => '("a" "l" "p" "h" "a" "b" "e" "t" "s")

  (keyword-repeated "abc" "12345678")
  => '("a" "b" "c" "a" "b" "c" "a" "b")
  ))

;;; Encoder
(defn encoder
  "Encode a plain message and return a seq of strings"
  [conversion-map-map keyword message]
  (let [keyword-rep (keyword-repeated keyword message)]
    (loop [key-str keyword-rep
           mess-str (seq-of-str message)
           acc []]
      (if (empty? mess-str)
        acc
        (let [encoded-str (get-in conversion-map-map
                                  [(first key-str) (first mess-str)])]
          (recur (rest key-str) (rest mess-str) (conj acc encoded-str)))))))

;;; Final function constructor
(defn constructor
  "Return a functional closure enclosing the conversion table"
  [fun conversion-map-map]
  (fn [keyword message]
    (apply str (fun conversion-map-map keyword message))))

;;; encoder
(def encode
  "Closure enclosing the conversion-map-map"
  (constructor encoder conversion-map-map))

(mj/facts
 (mj/fact
  "encoder test"
  (encoder conversion-map-map "abc" "abc")
  => '("a" "c" "e")

  (encode "abc" "abc")
  => "ace"

  (encode "scones" "meetmebythetree")
  => "egsgqwtahuiljgs"
  ))


;;; Decoder
(defn decoder
  "Decode an encrypted message and return a seq of strings"
  [conversion-map-map keyword message]
  (let [keyword-rep (keyword-repeated keyword message)]
    (loop [key-str keyword-rep
           mess-str (seq-of-str message)
           acc []]
      (if (empty? mess-str)
        acc
        ;; Select element in conversion-map-map by (first key-str)
        ;; Then return the key of element matching (first mess-str)
        (let [map-of-interest (conversion-map-map (first key-str))
              inverted-map (clojure.set/map-invert map-of-interest)
              decoded-str (inverted-map (first mess-str))]
          (recur (rest key-str) (rest mess-str) (conj acc decoded-str)))))))

;;; decoder
(def decode
  "Closure enclosing the conversion-map-map"
  (constructor decoder conversion-map-map))


(mj/facts
 (mj/fact
  (decode "scones" "egsgqwtahuiljgs")
  => "meetmebythetree"
  ))
