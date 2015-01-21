(defproject clojure-dominion "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.lispcast/org.apache.commons.lang "2.5.0"]]
  :main ^:skip-aot clojure-dominion.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
