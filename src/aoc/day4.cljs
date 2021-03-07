(ns ^:figwheel-no-load aoc.day4
  (:require
   [cljs.core.async :refer [go]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as sets]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [goog.labs.net.xhr :as xhr]))

(def example
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(defn parse-field [field]
  (let [[f v] (string/split field ":")]
    [(keyword (str "aoc.day4/" f)) v]))

(defn parse [passport]
  (let [fields (->> (string/split passport #"\s+")
                    (map parse-field)
                    (into {}))]
    fields))

(def required-fields
  #{::byr
    ::iyr
    ::eyr
    ::hgt
    ::hcl
    ::ecl
    ::pid})

(s/def ::byr #(<= 1920 (js/parseInt % 10) 2002))
(s/def ::iyr #(<= 2010 (js/parseInt % 10) 2020))
(s/def ::eyr #(<= 2020 (js/parseInt % 10) 2030))

(s/valid? ::byr "2002")
(s/valid? ::byr "2003")

(s/def ::hgt
  #(cond
     (string/ends-with? % "cm") (<= 150 (js/parseInt (string/replace % "cm" "") 10) 193)
     (string/ends-with? % "in") (<= 59 (js/parseInt (string/replace % "in" "") 10) 76)
     :else false))

(s/valid? ::hgt "60in")
(s/valid? ::hgt "190cm")
(s/valid? ::hgt "190in")
(s/valid? ::hgt "190")

(s/def ::hcl #(re-matches #"^#[0-9a-f]{6}$" %))

(s/valid? ::hcl "#123abc")
(s/valid? ::hcl "#123abcd")
(s/valid? ::hcl "#123ab")
(s/valid? ::hcl "#123abz")
(s/valid? ::hcl "123abc")

(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(s/valid? ::ecl "brn")
(s/valid? ::ecl "wat")

(s/def ::pid #(re-matches #"^[0-9]{9}$" %))

(s/valid? ::pid "000000001")
(s/valid? ::pid "0123456789")

(s/def ::passport
  (s/keys :req [::byr
                ::iyr
                ::eyr
                ::hgt
                ::hcl
                ::ecl
                ::pid]))

(s/valid?
 ::passport
 {:aoc.day4/cid "110",
  :aoc.day4/hgt "165cm",
  :aoc.day4/ecl "grn",
  :aoc.day4/iyr "2017",
  :aoc.day4/eyr "2025",
  :aoc.day4/byr "1923"})

(defn valid? [passport]
  (let [fields (-> passport keys set)]
    (= (sets/intersection required-fields fields)
       required-fields)))

(->> (string/split example "\n\n")
     (map parse)
     (map valid?))

(go
  (let [passports (-> "input4"
                      xhr/get
                      <p!
                      (string/split "\n\n"))]
    (->> passports
         (map parse)
         (map valid?)
         (map #(if % 1 0))
         (reduce +)
         print)))

(defn valid2? [passport]
  (s/valid? ::passport passport))

(def example2
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(->> (string/split example2 "\n\n")
     (map parse)
     (map valid2?))

(defn explain [passport]
  (pprint passport)
  (s/explain ::passport passport))

(go
  (let [passports (-> "input4"
                      xhr/get
                      <p!
                      (string/split "\n\n"))]
    (->> passports
         (map parse)
         (filter valid2?)
         (map #(if % 1 0))
         (reduce +)
         print)))
