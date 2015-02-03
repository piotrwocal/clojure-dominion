(ns clojure-dominion.file
  (:use clojure-dominion.core)
  (:use clojure-dominion.optimize)
  (:use clojure.java.io)
  (:require [clojure.string :as str])
  (:use clojure.pprint))

(defn saving-action*
  [file-name move-prefix action]
  (fn [board hand]
    (let [hand (action board hand)
          out-line (str move-prefix " " hand "\n")]
      (when-not (.exists (as-file file-name))
        (spit file-name "Let's play !\n"))
      (spit file-name out-line :append true)
      hand)))

(defn line->prefix-buy
  [line]
  (let [tokens (str/split line #"\{")]
    [(.trim (first tokens)) (read-string (str "{" (second tokens)))]))

(defn read-last-prefix-buy
  [file-name]
  (with-open [rdr (reader file-name)]
    (let [last-line (last (line-seq rdr))]
      (line->prefix-buy last-line))))

(defn reading-action*
  [file-name move-prefix interval]
  (fn [board hand]
    (loop [[prefix buy] (read-prefix-buy file-name)]
      (if (= prefix move-prefix)
        buy
        (do
          (Thread/sleep interval)
          (recur (read-prefix-buy file-name)))))))

(defn reading-action2*
  [file-name move-prefix interval]
  (fn [board hand]
    (->> (fn [] (Thread/sleep interval) (read-prefix-buy file-name))
         repeatedly
         (filter #(= move-prefix (first %)))
         first
         second)))


;(def verbose-pgds
;  (file-persisted-action "someFileName" "verbose" province-gold-duchy-silver))

;(pprint (play ["1" verbose-pgds]
;              ["2" province-gold-silver]))




