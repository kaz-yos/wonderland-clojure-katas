(ns magic-square.puzzle
  (:require [midje.sweet :as mj]
            [clojure.math.combinatorics :as comb]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

;;; By definition the magic number is total/3
(def total (reduce + values))
(def magic-number (/ total 3))

;;; vector to matrix
(defn matrix
  "Convert length 9 vector to 3x3 matrix"
  [values]
  (partition-all 3 values))

(mj/facts
 (mj/fact
  "Matrix conversion check"
  (matrix (range 1 10))
  => (partition-all 3 (range 1 10))))

;;; row sums checker
(defn equal-row-sums?
  "Check if row sums are the same"
  [board]
  (->> board
       (map #(reduce + %), )
       (apply =, )))

(mj/facts
 (mj/fact
  "row sum check"
  (equal-row-sums? [[1 1 1] [1 1 1] [1 1 1]])
  => true

  (equal-row-sums? [[1 1 1] [1 1 1] [1 1 2]])
  => false))

;;; col sums checker
(defn equal-col-sums?
  "Check if column sums are the same"
  [board]
  (->> board
       ;; transpose
       (apply map vector, )
       (equal-row-sums?, )))

(mj/facts
 (mj/fact
  "col sum check"
  (equal-col-sums? [[1 1 1] [1 1 1] [1 1 1]])
  => true

  (equal-col-sums? [[3 3 2] [2 3 2] [1 0 2]])
  => true

  (equal-col-sums? [[3 3 2] [2 3 2] [1 1 2]])
  => false))

;;; diag sum checker
(defn equal-diag-sums?
  [board]
  (let [[[a _ _] [_ b _] [_ _ c]] board
        [[_ _ A] [_ B _] [C _ _]] board
        diag1 [a b c]
        diag2 [A B C]]
    (->> [diag1 diag2]
         (map #(reduce + %), )
         (apply =, ))))

(mj/facts
 (mj/fact
  "row sum check"
  (equal-diag-sums? [[1 1 1] [1 1 1] [1 1 1]])
  => true

  (equal-diag-sums? [[2 3 2] [3 2 3] [2 3 2]])
  => true

  (equal-diag-sums? [[3 3 2] [2 3 2] [1 1 2]])
  => false))


;;; All sum checker
(defn all-sums-equal?
  [board]
  (and (equal-row-sums? board)
       (equal-col-sums? board)
       (equal-diag-sums? board)))

(mj/facts
 (mj/fact
  "row sum check"
  (all-sums-equal? [[1 1 1] [1 1 1] [1 1 1]])
  => true

  (all-sums-equal? [[2 3 2] [3 2 3] [2 3 2]])
  => false

  (all-sums-equal? [[3 3 2] [2 3 2] [1 1 2]])
  => false))


;;; Main function
(defn magic-square [values]
  (->> (comb/permutations values)
       (map matrix, )
       (filter all-sums-equal?, )
       (first, )
       ;; Need to convert to vector of vectors for get-in
       (map vec, )
       (vec, )))
