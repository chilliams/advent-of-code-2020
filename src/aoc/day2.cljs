(ns ^:figwheel-no-load aoc.day2
  (:require
   [clojure.string :as string]
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [goog.labs.net.xhr :as xhr]))

;; part 1
(defn valid? [line]
  (let [[policy password] (->> (string/split line ":")
                               (map string/trim))
        [range letter] (string/split policy " ")
        [minc maxc] (->> (string/split range "-")
                         (map int))
        c (-> password frequencies (get letter))]
    (if (<= minc c maxc)
      1
      0)))

(go
  (let [input (->> "input2"
                   xhr/get
                   <p!
                   string/split-lines)]
    (->> input
         (map valid?)
         (reduce +)
         print)))

;; part 2
(defn valid2? [line]
  (let [[policy password] (->> (string/split line ":")
                               (map string/trim))
        [positions letter] (string/split policy " ")
        [a b] (->> (string/split positions "-")
                   (map int)
                   (map dec))]
    (if (and
         (or
          (= (get password a) letter)
          (= (get password b) letter))
         (not
          (and
           (= (get password a) letter)
           (= (get password b) letter))))
      1
      0)))

(comment
  (valid2? "1-3 a: abcde")
  (valid2? "1-3 b: cdefg")
  (valid2? "2-9 c: ccccccccc")
  )

(go
  (let [input (->> "input2"
                   xhr/get
                   <p!
                   string/split-lines)]
    (->> input
         (map valid2?)
         (reduce +)
         print)))
