(ns clojure-dominion.file
  (:use clojure-dominion.core)
  (:use clojure-dominion.optimize)
  (:use clojure.java.io)
  (:require [clojure.string :as str])
  (:use clojure.pprint))

(defn persisted-action
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
    [(.trim (first tokens)) (str "{" (second tokens))]))

(defn line->buy
  [line]
  (->> (str/split line #"\{") last (str "{") (read-string)))

; template for read-file-action
(with-open [rdr (reader "someFileName")]
  (let [last-line (last (line-seq rdr))
        prefix-buy (line->prefix-buy last-line)
        prefix (first prefix-buy)
        buy (read-string (second prefix-buy))]
    ))

;(def verbose-pgds
;  (file-persisted-action "someFileName" "verbose" province-gold-duchy-silver))
;
;(pprint (play ["1" verbose-pgds]
;              ["2" province-gold-silver]))
