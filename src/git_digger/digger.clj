(ns git-digger.digger
  (:use clojure.pprint)
  (:require [clojure.java.shell :as sh])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [oz.core :as oz])
  (:require [clojure.data.json :as json]))

(defn call-git-log
  "Calls command line 'git log' command and returns logout if call was successful"
  []
  (let [ result (sh/sh "sh" "-c" "cd /opt/data/payon && git log --pretty=format:'[%h] %ae %ad' --date=short --numstat --after='2018-01-01 00:00:00'")]
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

(defn entries->file-to-commit-hashes [entries]
  (as-> entries x
        (map #(select-keys % [:file :hash]) x)
        (group-by :file x)
        (update-map-values x (partial map :hash))
        (update-map-values x set)))

(defn remove-entries [regex-str entries]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (remove #(re-find regex (:file %)) entries)))

(defn filter-entries [regex-str entries]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (filter #(re-find regex (:file %)) entries)))

; -- data to play
;(def git-log
;  (slurp "/opt/data/payon/out.txt"))

(def commits
  (git-log->commits (call-git-log)))

(def entries
  (commits->entries commits))

(def files-hashes
  (->> entries
       (remove-entries "^.idea/|^test/|.gradle|.xml|.properties")
       entries->file-to-commit-hashes))

; -- correlation
(defn jaccard-similarity [set-1 set-2]
  (/
    (count (set/intersection set-1 set-2))
    (count (set/union set-1 set-2))))

(defn pairs [xs]
  (loop [result [] [x & xs] xs]
    (if (empty? xs) result
      (recur (into result (map (fn [y] [x y]) xs))
               xs))))

; -- analyze functions
(defn get-most-changed-files [entries]
  (as-> (group-by :file entries) x
    (update-map-values x count)
    (sort-by second > x)))

(defn get-most-active-commiter [commits]
	(as-> (group-by :mail commits) x
				(update-map-values x count)
				(sort-by second > x)))

(defn get-most-similar-files [files-hashes sim-f]
  (as-> (map first files-hashes) x
        (pairs x)
        (zipmap x (map (fn[[k1 k2]] (sim-f (files-hashes k1) (files-hashes k2)))
                       x))
        (sort-by second > x)))

; TODO:
; - find files with high similarity and filter out the same package
; - introduce distance/category based on java package - like higher similarity in the same shared package part
; - usage of  https://vega.github.io/vega/examples/arc-diagram/
;           - https://vega.github.io/vega/examples/force-directed-layout/
; read:
;   - https://en.wikipedia.org/wiki/Graph_drawing
;   - https://en.wikipedia.org/wiki/Force-directed_graph_drawing
;   - https://en.wikipedia.org/wiki/Hierarchical_clustering
;   - https://en.wikipedia.org/wiki/Dimensionality_reduction

(as-> (group-by :date entries) x
      (update-map-values x count)
      (sort-by key x))

;-----------------------
(oz/start-plot-server!)

(def oz-input
  (map (fn[[f s]] {:file f :changes s})
       (get-most-changed-files entries)))

(defn bar-plot [oz-input]
  {:data {:values oz-input}
   :encoding {:x {:field "changes" :type "quantitative" }
               :y {:field "file" :type "ordinal" :axis{ :labelLimit 600} :sort { :field "changes"}}}
   :mark "bar"
   :title "Files with most git changes"
   :width 800
   :height 400
   })

(oz/v!
  (bar-plot oz-input))

(oz/v!
  (json/read-json (slurp "./vega/bar-chart.vg.json"))
  :mode :vega)

;-----
(def miserables-data
  (json/read-json (slurp "./vega/data/miserables.json")))

(def arc-diagram-vg
  (json/read-json (slurp "./vega/arc-diagram.vg.json")))

(def arc-data
  (update-map-values miserables-data (partial take 10)))

(oz/v!
  (-> arc-diagram-vg
      (assoc-in [:data 0 :values] arc-data )
      (update-in [:data 0] dissoc :url )
      (assoc-in [:data 3 :values] arc-data )
      (update-in [:data 3] dissoc :url ))
  :mode :vega)


(defn scale-similar-files-values [sim-files scale]
  (->> sim-files
      (filter (fn[[k v]] (> v 0)))
      (map (fn [[name value]]
             [name (int (* value scale))]))))

(defn files-hashes->files-index[files-hashes]
  (as-> (map first files-hashes) x
        (sort x)
        (zipmap x (iterate inc 0))))

(defn get-vg-nodes[files-hashes]
  (->> (files-hashes->files-index files-hashes)
        (map #(hash-map :name (first %) :group 1 :index (second %)))
        (sort-by :index)))


;------- data playground
(def ps-files-hashes
  (as-> files-hashes x
        (filter #(re-find #".java$" (first %)) x)
        (sort-by (comp count second) > x)
				(take 50 x)
				(flatten x)
				(apply sorted-map-by (comp count second) x)))

(pprint ps-files-hashes)


(get-most-similar-files files-hashes jaccard-similarity)
;
;(let [files-index (files-hashes->files-index files-hashes)
;      file-pairs-sim (-> (get-most-similar-files files-hashes jaccard-similarity)
;                         (scale-similar-files-values 20))]
;  (as-> file-pairs-sim x
;        (map (fn[[[f1 f2] sim]] (hash-map :source f1 :target f2 :value sim))
;             x)
;        (take 10 x)))



(count (get-vg-nodes files-hashes))
(pprint files-hashes)
(pprint arc-data)
(count commits)
(count files-hashes)


(oz/v!
  (json/read-json (slurp "./vega/arc-diagram.vg.json"))
  :mode :vega)

