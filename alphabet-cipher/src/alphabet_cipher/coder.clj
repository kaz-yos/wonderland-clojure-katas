(ns alphabet-cipher.coder
  (:require [midje.sweet :as mj]))


;;; Creation of the conversion table
;; a-z sequence
(def alphabets (map #(str (char %)) (range 97 (+ 97 26))))
;; infinite lazy seq of a-z
(def alphabets-infinite (cycle alphabets))
;; 
(def conversion-seq-seq (for [n (range 0 26)]
                          (take 26 (drop n alphabets-infinite))))

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
  => #{26}))


;;; encoder
(defn encode [keyword message]
  "encodeme")

;;; decoder
(defn decode [keyword message]
  "decodeme")
