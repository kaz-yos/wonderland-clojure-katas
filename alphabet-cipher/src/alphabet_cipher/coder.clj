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
  "c"
  ))


;;;
(defn )



;;; encoder
(defn encode [keyword message]
  "encodeme")

;;; decoder
(defn decode [keyword message]
  "decodeme")





