(ns git-digger.digger
  (:use clojure.pprint)
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.string :as str]))

(defn call-git-log []
  (let [result (sh "git" "log" "--pretty=format:'[%h] %ae %ad'" "--date=short" "--numstat")]
    (when (empty? (:err result))
      (:out result))))

(defn parse-commit-header [header]
  (zipmap [:hash :mail :date]
          (str/split header #" ")))

(defn parse-commit-line [line]
  (-> (zipmap [:added :deleted :file]
              (str/split line #"\t"))
      (update :added read-string)
      (update :deleted read-string)))

(defn parse-commit [entry]
  (let [[x & xs] (str/split entry #"\n")]
    (assoc (parse-commit-header x) :entries (map parse-commit-line xs))))

(defn git-log->commits []
  (as-> (call-git-log) x
        (str/split x #"\n\n")
        (map parse-commit x)))

;---------
(def commits
  (git-log->commits))

(def entries
  (mapcat :entries commits))

(defn update-map-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args)))
          {} m))

(defn most-changed-files [entries n]
  (as-> entries entries
    (group-by :file entries)
    (update-map-values entries count)
    (sort-by second > entries)
    (take n entries)))

(defn remove-entries [entries regex-str]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (remove #(re-find regex (:file %)) entries)))

(defn filter-entries [entries regex-str]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (filter #(re-find regex (:file %)) entries)))

(most-changed-files (remove-entries entries "^.idea/|^test/") 10)




