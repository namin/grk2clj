(ns grk2clj.examples
  (:refer-clojure :exclude [< not=])
  (:import (clojure.lang Seqable PersistentHashSet Symbol)
           (java.io File))
  (:require [clojure.core.typed :refer [ann inst cf fn> pfn> check-ns ann-form]]
            [clojure.repl :refer [pst]]
            [clojure.tools.analyzer :refer [ast]]))

;;; Some examples from the paper
;;; translated from Typed Racket to Typed Clojure
;;; To type-check, load namespace in REPL and execute (check-ns).

;;; Example 2
(ann f [(U String Number) -> Number])
(defn f [x]
  (if (number? x) (inc x) (count x)))

(ann ex4 [Any -> Number])
(defn ex4 [x]
  (if (or (number? x) (string? x))
    (f x)
    0))

(ann ex5 [(U String Number) Any -> Number])
(defn ex5 [x y]
  (if (and (number? x) (string? y))
    (+ x (count y))
    0))

(ann ^:no-check ex6 [(U String Number) Any -> Number])
(defn ex6 [x y]
  (if (and (number? x) (string? y))
    (+ x (count y))
    (count x)))

