(ns git-digger.digger
  (:use clojure.pprint)
  (:require [git-digger.git :as parse])
  (:require [clojure.set :as set])
  (:require [oz.core :as oz])
  (:require [clojure.data.json :as json]))

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
  (parse/log->commits (parse/call-git-log)))

(def entries
  (parse/commits->entries commits))

(def files-hashes
  (->> entries
       (remove-entries "^.idea/|^test/|.gradle|.xml|.properties")
       entries->file-to-commit-hashes))



(defn file-similarity-arc-diagram [path cut-date remove-regex scale]
  (let [vg-template (json/read-json (slurp "./vega/arc-diagram.vg.json"))
        log (parse/call-git-log path cut-date)
        commits (parse/log->commits log)
        entries (remove-entries remove-regex (parse/commits->entries commits))
        files-hashes (entries->file-to-commit-hashes entries)
        vg-data (get-vg-data files-hashes scale)]
    (oz/v!
      (-> vg-template
          (assoc-in [:data 0 :values] vg-data)
          (update-in [:data 0] dissoc :url )
          (assoc-in [:data 3 :values] vg-data)
          (update-in [:data 3] dissoc :url ))
      :mode :vega)))


(file-similarity-arc-diagram
  "/home/piotr/Workspace/clojure-dominion"
  "2018-01-01 00:00:00"
  "^.idea/|^test/|.gradle|.xml|.properties"
  20)

(parse/call-git-log "/home/piotr/Workspace/commons-lang" "2018-01-01 00:00:00")

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

(def git-arc-data
  (get-vg-data files-hashes 20))


(oz/v!
  (-> arc-diagram-vg
      (assoc-in [:data 0 :values] git-arc-data )
      (update-in [:data 0] dissoc :url )
      (assoc-in [:data 3 :values] git-arc-data )
      (update-in [:data 3] dissoc :url ))
  :mode :vega)


(defn scale-similar-files-values [sim-files scale]
  (->> sim-files
      (filter (comp pos? second))
      (map (fn [[name value]]
             [name (int (* value scale))]))))


(defn files-hashes->files-index[files-hashes]
  (as-> (map first files-hashes) x
        (sort x)
        (zipmap x (iterate inc 0))))


; ---- vg-transformations

(defn get-vg-nodes[files-hashes]
  (->> (files-hashes->files-index files-hashes)
        (map #(hash-map :name (first %) :group 1 :index (second %)))
        (sort-by :index)))

(defn get-vg-links[files-hashes scale]
  (let [files-index (files-hashes->files-index files-hashes)
        file-pairs-sim (-> (get-most-similar-files files-hashes jaccard-similarity)
                           (scale-similar-files-values scale))]
    (as-> file-pairs-sim x
          (map (fn[[[f1 f2] sim]]
                 (hash-map :source (files-index f1) :target (files-index f2) :value sim)) x ))))

(defn get-vg-data[files-hashes scale]
  {:nodes (get-vg-nodes files-hashes)
   :links (get-vg-links files-hashes scale)})

(get-vg-data files-hashes 20)

;------- data playground
(get-vg-nodes files-hashes)
(get-vg-links files-hashes 20)
(pprint files-hashes)
(pprint arc-data)

(oz/v!
  (json/read-json (slurp "./vega/arc-diagram.vg.json"))
  :mode :vega)

