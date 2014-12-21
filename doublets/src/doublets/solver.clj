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
       ;; Reverse to detect 1 change
       (map not, )
       ;; Numerical transformation
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
  (->> words-same-len
       (filter #(one-letter-change? word %), )
       set))

(mj/facts
 (mj/fact
  "Filter words with only one-letter change"
  (words-with-one-letter-change "abc" #{"abc" "Abc" "aBc" "abC" "ABc" "AbC" "aBC" "ABC"})
  => #{"Abc" "aBc" "abC"}))


;;;
(defn candidate-words
  "Pick candidate words from a set"
  [word words-set]
  (->> words-set
       (words-with-same-length word, )
       (words-with-one-letter-change word, )
       set))

(mj/facts
 (mj/fact
  "Filter words with only one-letter change from various words"
  (candidate-words "abc" #{"abc" "abcd" "Abc" "Abcd" "aBc" "abC" "ABc" "AbC" "AbCd" "aBC" "ABC"})
  => #{"Abc" "aBc" "abC"}))


;;; Function to test if the last word can reach the target word
(defn complete-word-seq?
  "Function to test if the last word can reach the target word"
  [target-word words-seq]
  (one-letter-change? (last words-seq) target-word))

(mj/facts
 (mj/fact
  "Check last word for completion"
  (complete-word-seq? "abc" ["bbb" "bbc"]) => true
  (complete-word-seq? "abc" ["bbb" "cbb"]) => false))


;;;

(defn candidate-seqs
  "All possible sequences given valid words set, word1 and word2"
  ([valid-words-set word1 word2] (candidate-seqs valid-words-set word1 word2 [word1]))
  ;;
  ([valid-words-set word1 word2 acc]
   (cond
     ;; If it can connect to the target word, return the completed seq
     (one-letter-change? word1 word2) [(conj acc word2)]
     ;; Otherwise, recurse for all possible casees
     ;; If next-word [] occurs, '() is returned from the for macro
     ;; No need for explicit handling of incomplete seq
     :else (for [next-word (words-with-one-letter-change word1 valid-words-set)
                 solution (candidate-seqs (disj valid-words-set next-word)
                                          next-word
                                          word2
                                          (conj acc next-word))]
             ;; Each function call only returns one solution
             solution))))

(mj/facts
 (mj/fact
  "Create sequences"
  (candidate-seqs #{"abdd" "abca" "adca" "abba" "fbba" "acba" "azba"} "abcd" "dcba")
  => '(["abcd" "abca" "abba" "azba" "acba" "dcba"] ["abcd" "abca" "abba" "acba" "dcba"])
  
  (candidate-seqs #{"abdd" "abca"} "abcd" "dcba")
  => '()
  
  (candidate-seqs #{"aacd" "aaad" "abbd" "abbb" "abcc" "accc" "abdd" "addd"} "abcd" "dcba")
  => '()

  (candidate-seqs #{"aacd" "aaad" "abbd" "abbb" "abcc" "accc" "abdd" "addd" "dddd" "dcdd" "dcbd" "dcba"} "abcd" "dcba")
  => '(["abcd" "abbd" "abdd" "addd" "dddd" "dcdd" "dcbd" "dcba"] ["abcd" "abdd" "addd" "dddd" "dcdd" "dcbd" "dcba"])))


;;; Create doublets as a closure
(defn doublets-with-words-set
  "Pick the shortest sequence among valid ones"
  [words-set word1 word2]
  (let [valid-words-set (words-with-same-length word1 words-set)]
    (->> (candidate-seqs valid-words-set word1 word2)
         (sort-by count, )
         ;; If no valid sequence is possible, return []
         ;; Otherwise, return the shortest valid sequence
         (#(if (empty? %) [] (first %))))))

(mj/facts
 (mj/fact
  "Create sequences"
  (doublets-with-words-set #{"abcde" "abdd" "abca" "adca" "abba" "fbba" "acba" "azba"} "abcd" "dcba")
  => ["abcd" "abca" "abba" "acba" "dcba"]
  
  (doublets-with-words-set #{"abcde" "abdd" "abca"} "abcd" "dcba")
  => []
  
  (doublets-with-words-set #{"abcde" "aacd" "aaad" "abbd" "abbb" "abcc" "accc" "abdd" "addd"} "abcd" "dcba")
  => []

  (doublets-with-words-set #{"abcde" "aacd" "aaad" "abbd" "abbb" "abcc" "accc" "abdd" "addd" "dddd" "dcdd" "dcbd" "dcba"} "abcd" "dcba")
  => ["abcd" "abdd" "addd" "dddd" "dcdd" "dcbd" "dcba"]))


;;; Main function for solving the puzzle
(def doublets (partial doublets-with-words-set words-set))

