(ns alphabet-cipher.coder
  (:require [midje.sweet :as mj]))

(mj/facts
 (mj/fact
  "Matrix conversion check"
  (matrix (range 1 10))
  => (partition-all 3 (range 1 10))))

;;; Creation of the conversion table
(def alphabets (map #(str (char %)) (range 97 (+ 97 26))))
(def alphabets-infinite (cycle alphabets))

(def conversion-seq-seq (for [n (range 0 26)]
                          (take 26 (drop n alphabets-infinite))))



;;; encoder
(defn encode [keyword message]
  "encodeme")

;;; decoder
(defn decode [keyword message]
  "decodeme")
