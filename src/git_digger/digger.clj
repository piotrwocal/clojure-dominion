(ns git-digger.digger
  (:use clojure.pprint)
  (:require [clojure.repl :refer :all])
  (:use git-digger.git)
  (:require [clojure.set :as set])
  (:require [oz.core :as oz])
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

; --- correlations
(defn scale-similar-files-values [sim-files scale]
  (->> sim-files
       (filter (comp pos? second))
       (map (fn [[name value]]
              [name (int (* value scale))]))))

(defn jaccard-similarity [set-1 set-2]
  (/
    (count (set/intersection set-1 set-2))
    (count (set/union set-1 set-2))))

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

; --- data to play
;(def git-log
;  (slurp "/opt/data/payon/out.txt"))

(def commits
  (log->commits (call-git-log)))

(def entries
  (commits->entries commits))

(def files-hashes
  (->> entries
       (remove-entries "^.idea/|^test/|.gradle|.xml|.properties")
       entries->file-to-commit-hashes))


; time scale example
;(as-> (group-by :date entries) x
;      (update-map-values x count)
;      (sort-by key x))


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

;-----------------------
(oz/start-plot-server!)


; --- bar plot
(defn bar-plot [input]
  {:data {:values input}
   :encoding {:x {:field "changes" :type "quantitative" }
               :y {:field "file" :type "ordinal" :axis {:labelLimit 600} :sort {:field "changes"}}}
   :mark "bar"
   :title "Files with most git changes"
   :width 800
   :height 400
   })

(oz/v!
  (bar-plot
    (map (fn[[f s]] {:file f :changes s})
         (->> (filter-entries ".clj$" entries)
              get-most-changed-files
              (take 5)))))

; --- bar plot from file
;(oz/v!
;  (json/read-json (slurp "./vega/bar-chart.vg.json"))
;  :mode :vega)

; --- miserables arch-example
;(def miserables-data
;  (json/read-json (slurp "./vega/data/miserables.json")))
;
;(def arc-diagram-vg
;  (json/read-json (slurp "./vega/arc-diagram.vg.json")))
;
;(def arc-data
;  (update-map-values miserables-data (partial take 10)))
;
;(def git-arc-data
;  (get-vg-data files-hashes 20))
;
;(oz/v!
;  (-> arc-diagram-vg
;      (assoc-in [:data 0 :values] git-arc-data )
;      (update-in [:data 0] dissoc :url )
;      (assoc-in [:data 3 :values] git-arc-data )
;      (update-in [:data 3] dissoc :url ))
;  :mode :vega)



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

(defn file-similarity-arc-diagram [path cut-date remove-regex scale]
  (let [vg-template (json/read-json (slurp "./vega/arc-diagram.vg.json"))
        log (call-git-log path cut-date)
        commits (log->commits log)
        entries (remove-entries remove-regex (commits->entries commits))
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
  "/home/piotr/Workspace/commons-lang"
  "2018-01-15 00:00:00"
  "^.idea/|^src/test/|.gradle|.xml|.yml|.txt|.properties"
  6)


;-------
(let [vg-template (json/read-json (slurp "./vega/arc-diagram.vg.json"))
      log (call-git-log  "/home/piotr/Workspace/commons-lang" "2018-01-15 00:00:00")
      commits (log->commits log)
      entries (filter-entries ".java$" (commits->entries commits))
      most-changed-files (apply hash-set
                                (map first
                                     (take 5 (get-most-changed-files entries))))
      most-changed-entries (filter #(most-changed-files (:file %)) entries)
      files-hashes (entries->file-to-commit-hashes most-changed-entries)
      vg-data (get-vg-data files-hashes 20)]
    (oz/v!
      (-> vg-template
          (assoc-in [:data 0 :values] vg-data)
          (update-in [:data 0] dissoc :url )
          (assoc-in [:data 3 :values] vg-data)
          (update-in [:data 3] dissoc :url ))
            :mode :vega)
    vg-data)