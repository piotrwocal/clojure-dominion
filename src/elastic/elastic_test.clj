(ns elastic.elastic-test
	(:require [clojurewerkz.elastisch.rest  :as esr]
						[clojurewerkz.elastisch.rest.document :as esd]
						[clojurewerkz.elastisch.query :as q]
						[clj-http.client :as client]
						[clojure.data.json :as json]))

;; Basic Authentication
;(esr/connect "http://127.0.0.1:9200" {:basic-auth ["user" "pass"]})
;
;;; HTTP Timeout
;(esr/connect "http://127.0.0.1:9200" {:conn-timeout 5000})
;
;;; Persistent connections
;;; https://github.com/dakrone/clj-http#using-persistent-connections
(esr/connect "http://127.0.0.1:9200"
             {:connection-manager (clj-http.conn-mgr/make-reusable-conn-manager {:timeout 10})})


(def conn (esr/connect "http://127.0.0.1:9200"))

(-> (esd/get conn "bank" "_all" "1")
		:_source)

(defn get-seq-keys [xs k]
	(map #(get % k) xs))

 (-> (client/get "http://localhost:9200/bank/_search?q=*&sort=account_number:asc&pretty&pretty")
		 :body
		 json/read-str
		 (get-in ["hits" "hits"])
		 (get-seq-keys "_source"))





