(ns git-digger.custering
  (:use clojure.pprint)
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.string :as str])
  (:require [oz.core :as oz]))

(def input-txt
  (slurp "/home/piotr/Workplace/projects/clojure-dominion/LICENSE"))

(defn word-count [s]
  (as-> input-txt x
      (str/lower-case x)
      (str/split x #"\s+")
      (frequencies x)
      (sort-by second > x )))

(take 20
      (word-count input-txt))

(defn euclidan-distance [x y]
  (/
    1
    (+
      1
      (Math/sqrt
        (+
         (Math/pow (- 4.5 4) 2)
         (Math/pow (- 1 2) 2))))))


