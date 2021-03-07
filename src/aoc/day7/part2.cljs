(ns ^:figwheel-no-load aoc.day7.part2
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr]))

(def example
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def example2
  "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(defn parse-count [line]
  (let [clean (-> line
                  (string/replace #"\.$" "")
                  (string/replace #"s$" "")
                  (string/replace #"bag" "")
                  string/trim)
        n (js/parseInt clean 10)
        color (-> clean
                  (string/replace (str n) "")
                  string/trim)]
    {color n}))

(parse-count "1 bright white bag")
(parse-count "2 muted yellow bags")

(defn parse-line [line]
  (let [[outside inside] (string/split line " contain ")
        outside (-> outside
                    (string/replace "bags" "")
                    string/trim)
        inside (->> (string/split inside ",")
                    (map parse-count)
                    (filter #(not (contains? % "no other")))
                    (apply merge {}))]
    {outside inside}))

(defn bag-graph [input]
  (->> input
       string/split-lines
       (map parse-line)
       (apply merge)))

example
(-> example bag-graph)

(let [s (js/Set.)]
  (.add s 1)
  (.has s 1)
  )

(defn solve [bag bag-graph]
  (reduce
   +
   (for [[color n] (bag-graph bag)
         :when (number? n)]
     (+ n (* n (solve color bag-graph))))))

(solve "shiny gold" (bag-graph example))

(go
  (->> "input7"
       xhr/get
       <p!
       bag-graph
       (solve "shiny gold")
       pprint))
