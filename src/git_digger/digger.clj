(ns git-digger.digger
  (:use clojure.pprint)
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [oz.core :as oz]))

(defn call-git-log
  "Calls command line 'git log' command and returns logout if call was successful"
  []
  (let [ result (sh "sh" "-c" "cd /home/piotr/Workplace/projects/clojure-dominion && git log --pretty=format:'[%h] %ae %ad' --date=short --numstat --after='2018-01-01 00:00:00'")]
    (when (empty? (:err result))
      (:out result))))

(defn- parse-commit-header [header]
  (zipmap [:hash :mail :date]
          (str/split header #" ")))

(defn- parse-commit-line [line]
  (-> (zipmap [:added :deleted :file]
              (str/split line #"\t"))
      (update :added read-string)
      (update :deleted read-string)))

(defn parse-commit
  "Parse single commit logout to map"
  [entry]
  (let [[x & xs] (str/split entry #"\n")]
    (assoc (parse-commit-header x) :entries (map parse-commit-line xs))))

(defn git-log->commits
  "Parse input string git logout to commit maps"
  [x]
  (as-> x x
        (str/split x #"\n\n")
        (map parse-commit x)))

(defn- commit->entries [commit]
  (map #(assoc % :hash (:hash commit)
                 :mail (:mail commit)
                 :date (:date commit))
       (:entries commit)))

(defn commits->entries [commits]
  (mapcat commit->entries commits))

(defn update-map-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args)))
          {} m))

; -- data to play
;(def git-log
;  (slurp "/opt/data/payon/out.txt"))

(defn entries->file-to-commit-hashes [entries]
  (as-> entries x
      (map #(select-keys % [:file :hash]) x)
      (group-by :file x)
      (update-map-values x (partial map :hash))
      (update-map-values x set)))

(sort-by (comp count second) > file-to-commit-hashes)

(def files-hashes
  (->> (git-log->commits (call-git-log))
       commits->entries
       ;(remove-entries "^.idea/|^test/|.gradle|.xml|.properties")
       entries->file-to-commit-hashes
       (reduce merge {})))

(as-> files-hashes x
      (map first x)
      (pairs x)
      (zipmap x (map (fn[[f s]] (jaccard-similarity
                                  (files-hashes f)
                                  (files-hashes s))) x))
      (sort-by second > x)
      (take 50 x))


(pprint files-hashes)
; -- correlation
(defn jaccard-similarity [set-1 set-2]
  (/
    (count (set/intersection set-1 set-2))
    (count (set/union set-1 set-2))))


(defn pairs [xs]
  (loop [result [] [x & xs] xs]
    (if (empty? xs) result
      (recur (into result (map #(vector x %) xs))
             xs))))

; -- analyze functions

(defn get-most-changed-files [entries n]
  (as-> entries entries
    (group-by :file entries)
    (update-map-values entries count)
    (sort-by second > entries)
    (take n entries)))

(defn get-most-active-commiter [commits n]
	(as-> commits commits
				(group-by :mail commits)
				(update-map-values commits count)
				(sort-by second > commits)
				(take n commits)))

(defn remove-entries [regex-str entries]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (remove #(re-find regex (:file %)) entries)))

(defn filter-entries [regex-str entries]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (filter #(re-find regex (:file %)) entries)))


;-----------------------
(oz/start-plot-server!)

(def oz-input
  (map (fn[[f s]] {:file f :changes s}) filtered-most-changed-files))

(pprint oz-input)

(def bar-plot
  {:data {:values oz-input}
   :encoding {:x {:field "changes" :type "quantitative" }
               :y {:field "file" :type "ordinal" :axis{ :labelLimit 600} :sort { :field "changes"}}}
   :mark "bar"
   :title "Files with most git changes"
   :width 800
   :height 400
   })

(oz/v! bar-plot)

;--- correlations

