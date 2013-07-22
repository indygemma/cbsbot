(defproject cbsbot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :plugins [[lein-cljsbuild "0.3.2"]]
  :source-paths ["src/clj"]
  :cljs-build
  {:crossovers [cbstest]
   :builds
   [{:source-paths ["src/cljs"],
     :crossover-path "src/cljs",
     :compiler
     {:pretty-print true,
      :output-to "resources/cbsbot.js",
      :optimizations :whitespace
      }}]})

