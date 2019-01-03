(ns scrap
	(:use clojure.pprint)
	(:use [clojure.java.shell :only [sh]])
	(:require [clojure.java.io :as io]
						[clojure.xml :as xml]
						[clojure.data.zip.xml :as zip-xml]
		  			[clojure.zip :as zip]
						[clojure.string :as str]))

(def hazelcast-cache-names
	(as->	(xml/parse "/opt/data/payon/hazelcast_config/src/main/resources/hazelcast/ps-hazelcast-default.xml") x
			(:content x)
			(map :attrs x)
			(map :name x)
 			(set x)))

(def eh-cache-names
	(->> (xml/parse "/opt/data/payon/ctpc/config/templates/ps-ehcache.xml")
			:content
			(map :attrs)
			(map :name)
			set))

; check diff -> looks ok
(apply disj eh-cache-names hazelcast-cache-names)
(apply disj hazelcast-cache-names eh-cache-names)

;----------
(def merchant-hbm-xml
	(xml/parse "/opt/data/payon/ctpc/src/main/resources/org/pragmatico/ctpc/entities/hbm/Merchant.hbm.xml"))

(defn hbm-file-name->cache-names [file-name]
	(let [xml (xml/parse file-name)
				class-tag (->> xml
											 :content
											 (filter #(= (:tag %) :class))
											 first)
				class-name (get-in class-tag [:attrs :name])
				set-names (->> class-tag
											 :content
											 (filter #(= (:tag %) :set))
											 (map #(get-in % [:attrs :name]))
											 (filter not-empty))]
		(cons class-name
			(map #(str class-name "." %) set-names))))

(defn dir->hbm-file-names [dir]
	(as-> (sh "ls" :dir dir) x
				(:out x)
				(str/split x #"\n")
				(filter #(.contains % ".hbm.xml") x)
				(map #(str dir "/" %) x)))

(def ctpc-hbm-dir
	"/opt/data/payon/ctpc/src/main/resources/org/pragmatico/ctpc/entities/hbm")

(def cache-names-set
	(->> (dir->hbm-file-names ctpc-hbm-dir)
			 (mapcat hbm-file-name->cache-names)
			 set))

;-- to check ?!
(apply disj hazelcast-cache-names cache-names-set)
(apply disj cache-names-set hazelcast-cache-names)

