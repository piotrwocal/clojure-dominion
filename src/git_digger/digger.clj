(ns git-digger.digger
  (:use clojure.pprint)
  (:require [clojure.repl :refer :all])
  (:use git-digger.git)
  (:require [clojure.set :as set])
  (:require [oz.core :as oz])
  (:require [clojure.data.json :as json]))


; --- data to play

(def commits
  (log->commits (call-git-log "/home/piotr/Workspace/commons-lang" "2017-01-01 00:00:00")))

(def entries
  (commits->entries commits))

(def files-hashes
  (->> entries
       (remove-entries "^.idea/|^test/|.gradle|.xml|.properties")
       entries->file-to-commit-hashes))


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

; --- most changed files
(oz/v!
  (bar-plot
    (map (fn[[f s]] {:file f :changes s})
         (->> (filter-entries ".java$" entries)
              get-most-changed-files
              (take 20)))))


; --- most active commiter, works with commits and entries for single files
(oz/v!
  (bar-plot
    (map (fn[[f s]] {:file f :changes s})
         (->> (get-most-active-commiter commits)
              (take 20)))))


; ---- vg-transformations

(defn get-vg-nodes [files-hashes]
  (->> (files-hashes->files-index files-hashes)
        (map #(hash-map :name (first %) :group 1 :index (second %)))
        (sort-by :index)))

(defn file-pairs-sim [files-hashes sim-f scale]
  (-> (get-most-similar-files files-hashes sim-f)
      (scale-similar-files-values scale)))

(defn get-vg-links [files-hashes scale]
  (let [files-index (files-hashes->files-index files-hashes)]
    (as-> (file-pairs-sim files-hashes jaccard-similarity scale) x
          (map (fn[[[f1 f2] sim]]
                 (hash-map :source (files-index f1) :target (files-index f2) :value sim)) x)
          (filter #(pos? (:value %)) x)
          )))

(defn get-vg-data [files-hashes scale]
  {:nodes (get-vg-nodes files-hashes)
   :links (get-vg-links files-hashes scale)})

(defn file-similarity-arc-diagram
  [path cut-date & {:keys [filter-regex remove-regex top-files scale]
                    :or { filter-regex ""
                          remove-regex "\\b\\B"
                          top-files 10
                          scale 5 }}]
  (let [vg-template (json/read-json (slurp "./vega/arc-diagram.vg.json"))
        commits (log->commits (call-git-log path cut-date))
        entries (->> (commits->entries commits)
                     (filter-entries filter-regex)
                     (remove-entries remove-regex))
        most-changed-files (->> (get-most-changed-files entries)
                           (take top-files)
                           (map first)
                           (apply hash-set))
        most-changed-entries (filter #(most-changed-files (:file %))
                                     entries)
        files-hashes (entries->file-to-commit-hashes most-changed-entries)
        vg-data (get-vg-data files-hashes scale)]
    (oz/v!
      (-> vg-template
          (assoc-in [:data 0 :values] vg-data)
          (update-in [:data 0] dissoc :url)
          (assoc-in [:data 3 :values] vg-data)
          (update-in [:data 3] dissoc :url))
      :mode :vega)
    (-> vg-data)))

; -- fire me up baby !
(file-similarity-arc-diagram
  "/home/piotr/Workspace/clojure-dominion"
  "2018-01-15 00:00:00"
  :filter-regex ".clj$"
  :remove-regex ".xml$"
  :top-files 30
  :scale 10)


(let [commits (log->commits (call-git-log  "/home/piotr/Workspace/clojure-dominion" "2016-01-01 00:00:00"))
      entries (->> (commits->entries commits)
                   (filter-entries ".clj$")
                   (remove-entries ".*Test.*"))
      most-changed-files (->> (get-most-changed-files entries)
                              (take 20)
                              (map first)
                              (apply hash-set))
      most-changed-entries (filter #(most-changed-files (:file %))
                                   entries)
      files-hashes (entries->file-to-commit-hashes most-changed-entries)
      vg-data (get-vg-data files-hashes 10)]
  (spit "./vega/out.json"
        (json/write-str vg-data))
  (-> vg-data))


(def vg-data
   {:nodes [        {:group 1, :index 0, :name "0"}
                    {:group 1, :index 1, :name "1"}
                    {:group 1, :index 2, :name "2"}
                    {:group 1, :index 3, :name "3"}
                    {:group 1, :index 4, :name "4"}],
            :links [{:value 1, :source 0, :target 1}
                    {:value 1, :source 1, :target 2}
                    {:value 1, :source 2, :target 3}
                    {:value 1, :source 3, :target 4}
                    {:value 5, :source 0, :target 2}
                    ]})

(spit "./vega/out.json" (json/write-str vg-data))
