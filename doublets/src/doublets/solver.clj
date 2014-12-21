(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [midje.sweet :as mj]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

;;; convert to a set
(def words-set (set words))

;;; Function to find words with the same length
(defn words-with-same-length
  "Function to find words with the same length"
  [word words-set]
  (let [n-char (count word)]
    (->> words-set
         (filter #(= n-char (count %)), )
         set)))

(mj/facts
 (mj/fact
  "Number of characters should match"
  (words-with-same-length "1" #{"1" "2" "3" "11" "22" "33" "111" "222" "333"})
  => #{"1" "2" "3"}
  (words-with-same-length "22" #{"1" "2" "3" "11" "22" "33" "111" "222" "333"})
  => #{"11" "22" "33"}
  (words-with-same-length "333" #{"1" "2" "3" "11" "22" "33" "111" "222" "333"})
  => #{"111" "222" "333"}))


;;;
(defn bool->zero-one
  "Convert true/false to 1/0"
  [bool]
  (if bool 1 0))

(mj/facts
 (mj/fact
  "true to one, false to zero"
  (bool->zero-one true) => 1
  (bool->zero-one false) => 0))


;;; 
(defn one-letter-change?
  "Check for one letter change in two words of same length"
  [word1 word2]
  (->> (map #(= %1 %2) word1 word2)
       (map bool->zero-one, )
       (reduce +, )
       (= 1, )))

(mj/facts
 (mj/fact
  "one letter change"
  (one-letter-change? "abcd" "abcf") => true
  (one-letter-change? "abcd" "abcd") => false
  (one-letter-change? "abcd" "abxy") => false))


;;; Function to find words with one letter change
(defn words-with-one-letter-change
  "Function to find words with one letter change"
  [word words-same-len]
  
  )


;;; Main function for solving
(defn doublets [word1 word2]
  "make me work")
