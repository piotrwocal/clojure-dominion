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
      (update :added read-string)
      (update :deleted read-string)))

(defn parse-commit
  "Parse single commit logout to map"
  [entry]
  (let [[x & xs] (str/split entry #"\n")]
    (assoc (parse-commit-header x) :entries (map parse-commit-line xs))))

(defn log->commits
  "Parse input string git logout to commit maps"
  [x]
  (map parse-commit (str/split x #"\n\n")))

(defn- commit->entries [commit]
  (map #(assoc % :hash (:hash commit)
                 :mail (:mail commit)
                 :date (:date commit))
       (:entries commit)))

(defn commits->entries
  [commits]
  (mapcat commit->entries commits))