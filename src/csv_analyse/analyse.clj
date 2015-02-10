(ns clojure-dominion.analyse
  (:use clojure.pprint)
  (:use clojure.java.io)
  (:require [clojure.string :as str]))

(defn file->lines [file-name]
    (with-open [rdr (reader file-name)]
               (reduce conj [] (line-seq rdr))))

(defn line->entry-strings [line]
    (->> line
         (re-seq #"\[(.*?)\]")
         (map second)))

(defn entry-string->entry [entry-string]
    (->> entry-string
         (re-seq #"\"\"(.*?)\"\"")
         (map second)
         (zipmap [:count :avg-processing :channel-id :source-type :payment-type
                  :psp-name :division :merchant :channel])))

(defn calculate-total-time [entry]
    (try (->> entry
              (#(update-in % [:count] read-string))
              (#(update-in % [:avg-processing] read-string))
              (#(assoc % :total-time (* (:count %) (:avg-processing %)))))
         (catch Exception ex {:total-time 0})))

(->> (file->lines "/opt/data/tmp/top25_channels.csv")
     (map line->entry-strings)
     (mapcat (partial map entry-string->entry))
     (map calculate-total-time)
     (group-by :channel-id)
     (mapcat (fn [[k v]] {k (apply + (map :total-time v))}))
     (sort-by val >)
     (take 20)
     pprint)