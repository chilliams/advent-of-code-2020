(ns ^:figwheel-no-load aoc.day6
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr]))

(def example
  "abcx
abcy
abcz")

(defn yes-count [group]
  (->> group
       (filter #(not (re-matches #"\s" %)))
       set
       count))

(yes-count example)

(def example2
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(->> (string/split example2 "\n\n")
     (map yes-count)
     (reduce +))

(go
  (let [groups (-> "input6"
                   xhr/get
                   <p!
                   (string/split "\n\n"))]
    (->> groups
         (map yes-count)
         (reduce +)
         print)))

;; part 2
(defn all-yes-count [group]
  (->> group
       string/split-lines
       (map #(string/split % ""))
       (map set)
       (reduce sets/intersection)
       count))

(->> (string/split example2 "\n\n")
     (map all-yes-count)
     (reduce +))

(go
  (let [groups (-> "input6"
                   xhr/get
                   <p!
                   (string/split "\n\n"))]
    (->> groups
         (map all-yes-count)
         (reduce +)
         print)))
