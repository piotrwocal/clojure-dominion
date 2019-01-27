(ns git-digger.git

  (:require [clojure.string :as str])
  (:require [clojure.java.shell :as sh]))

(defn call-git-log
  "Calls command line 'git log' command and returns logout if call was successful"
  ([]
    (call-git-log "/home/piotr/Workspace/clojure-dominion" "2018-01-01 00:00:00"))
  ([path cut-date]
    (let [ result (sh/sh "sh" "-c" (str "cd " path " && git log --pretty=format:'[%h] %ae %ad' --date=short --numstat --after='" cut-date "'"))]
      (when (empty? (:err result))
        (:out result)))))

(defn- parse-commit-header [header]
  (zipmap [:hash :mail :date]
          (str/split header #" ")))

(defn- parse-commit-line [line]
  (-> (zipmap [:added :deleted :file]
              (str/split line #"\t"))
      (update :added #(Integer/parseInt %))
      (update :deleted #(Integer/parseInt %))))

(defn commit-header? [line]
  (.startsWith line "["))

(defn parse-commit-lines [[x & xs]]
  (if (commit-header? (first xs))
    (parse-commit-lines xs)
    (assoc (parse-commit-header x) :entries (map parse-commit-line xs))))

(defn parse-commit
  "Parse single commit logout to map"
  [entry]
  (parse-commit-lines (str/split entry #"\n")))

(defn log->commits
  "Parse input string git logout to commit maps"
  [x]
  (map parse-commit (str/split x #"\n\n")))

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

(defn pairs [xs]
  (loop [result [] [x & xs] xs]
    (if (empty? xs) result
                    (recur (into result (map (fn [y] [x y]) xs))
                           xs))))

(defn entries->file-to-commit-hashes [entries]
  (as-> entries x
        (map #(select-keys % [:file :hash]) x)
        (group-by :file x)
        (update-map-values x (partial map :hash))
        (update-map-values x set)))

(defn files-hashes->files-index[files-hashes]
  (as-> (map first files-hashes) x
        (sort x)
        (zipmap x (iterate inc 0))))

(defn remove-entries [regex-str entries]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (remove #(re-find regex (:file %)) entries)))

(defn filter-entries [regex-str entries]
  (let [regex (java.util.regex.Pattern/compile regex-str)]
    (filter #(re-find regex (:file %)) entries)))