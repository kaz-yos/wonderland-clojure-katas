(ns magic-square.puzzle
  (:require [midje.sweet :as mj]
            [clojure.math.combinatorics :as comb]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

;;; 
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

;;;
(defn equal-col-sums?
  "Check if column sums are the same"
  [board]
  (->> board
       ;; transpose
       (apply map vector, )
       (equal-row-sums?)))

(mj/facts
 (mj/fact
  "row sum check"
  (equal-col-sums? [[1 1 1] [1 1 1] [1 1 1]])
  => true

  (equal-col-sums? [[3 3 2] [2 3 2] [1 0 2]])
  => true

  (equal-col-sums? [[3 3 2] [2 3 2] [1 1 2]])
  => false))

;;;
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


;;;
(defn all-sums-equal?
  [board]
  (and (equal-row-sums? board)
       (equal-row-sums? board)
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
  (let [board (values)])
  (filter )
  )
