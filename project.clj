(defproject books "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.6"]
                 [hiccup "1.0.4"]
                 [garden "1.1.4"]
                 [clj-time "0.6.0"]
                 [clj-http "0.7.7"]
                 [cheshire "5.2.0"]
                 [me.raynes/cegdown "0.1.0"]
                 [me.raynes/hickory "0.4.2"]
                 [me.raynes/laser "1.1.1"]]
  :plugins [[lein-ring "0.8.8"]]
  :ring {:handler books.handler/app}
  :main books.handler
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})
