(ns cbsbot.random
  (:require [clojure.string :refer [upper-case join]]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def ALPHABET (clojure.string/join "" (map clojure.string/upper-case alphabet)))
(def numbers  (clojure.string/join "" (range 0 10)))
(def extras   "-_")
(def pool (str alphabet ALPHABET numbers extras))
(def n 11)

(defn gen-id-n
  [n]
  (clojure.string/join "" (for [i (range n)] (rand-nth pool))))

(defn gen-id [] (gen-id-n 11))

