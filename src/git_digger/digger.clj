(ns git-digger.digger
  (:use clojure.pprint)
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.string :as str])
  (:require [oz.core :as oz]))

(defn call-git-log []
  (let [ result (sh "sh" "-c" "cd /opt/data/payon && git log --pretty=format:'[%h] %ae %ad' --date=short --numstat --after='2018-01-01 00:00:00'")]
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

(defn git-log->commits [x]
  (as-> x x
        (str/split x #"\n\n")
        (map parse-commit x)))

(def commits
  (git-log->commits git-log))

(def git-log
	(slurp "/opt/data/payon/out.txt"))

(def entries
  (mapcat :entries commits))

(defn update-map-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args)))
          {} m))

(defn get-most-changed-files [entries n]
  (as-> entries entries
    (group-by :file entries)
    (update-map-values entries count)
    (sort-by second > entries)
    (take n entries)))


(defn get-most-active-commiter [entries n]
	(as-> entries entries
				(group-by :mail entries)
				(update-map-values entries count)
				(sort-by second > entries)
				(take n entries)))

(def commits-data
	(as-> commits commits
			(group-by :mail commits)
			(update-map-values commits count)
			(sort-by second > commits )
			(take 20 commits)))


(pprint (get-most-changed-files entries 20))

(defn remove-entries [entries regex-str]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (remove #(re-find regex (:file %)) entries)))

(defn filter-entries [entries regex-str]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (filter #(re-find regex (:file %)) entries)))

(def most-changed-files
  (get-most-changed-files (remove-entries entries "^.idea/|^test/|.gradle|.xml|.properties") 50))

(pprint most-changed-files)

;-----------------------
(oz/start-plot-server!)

(def oz-input
  (map (fn[[f s]] {:file f :changes s}) get-most-active-commiter))

(pprint oz-input)

(def bar-plot
  {:data {:values oz-input}
   :encoding {:x {:field "changes" :type "quantitative" }
               :y {:field "file" :type "ordinal" :axis{ :labelLimit 600} :sort { :field "changes"}}}
   :mark "bar"
   :title "Files with most git changes"
   :width 1100
   :height 600
   })

(oz/v! bar-plot)

