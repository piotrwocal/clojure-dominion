(defproject clojure-dominion "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.lispcast/org.apache.commons.lang "2.5.0"]
                 [org.clojure/tools.trace "0.7.8"]
                 [clojurewerkz/elastisch "3.0.0"]
                 [clj-http "3.7.0"]
								 [org.clojure/data.json "0.2.6"]
                 [metasoarous/oz "1.4.1"]]
  :main ^:skip-aot clojure-dominion.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
