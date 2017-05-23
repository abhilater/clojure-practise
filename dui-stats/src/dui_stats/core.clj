(ns dui-stats.core
  (:require [cheshire.core :as json]
            [clojure.java.io :as io])
  (:gen-class))


(def fips (->> (json/parse-string (slurp (io/resource "fips.json")) true)
               :table
               :rows
               (into {})))

(defn most-duis
  "Given a JSON filename of UCR crime data for a particular year, finds the
   counties with the most DUIs."
  [file]
  (->> file
       (json/parse-string (slurp (io/resource file)) true)
       (map #(select-keys % [:driving_under_influence :fips_county_code :fips_state_code]))
       (sort-by :driving_under_influence >)
       (take 10)
       (map (fn [item] [(fips (str (:fips_state_code item) (:fips_county_code item)))
                        (:driving_under_influence item)]))
       (into {})
       (sort-by val >)))
;
;(["CA, Los Angeles" 45056]
;  ["AZ, Maricopa" 26235]
;  ["CA, San Diego" 18562]
;  ["CA, Orange" 17572]
;  ["CA, San Bernardino" 13983]
;  ["WA, King" 11439]
;  ["CA, Riverside" 10814]
;  ["NV, Clark" 10443]
;  ["TX, Harris" 10432]
;  ["CA, Sacramento" 8589])


; 1. most-duis tells us about the raw number of reports, but doesn’t account for differences in county population.
; One would naturally expect counties with more people to have more crime! Divide the :driving_under_influence
; of each county by its :county_population to find a prevalence of DUIs, and take the top ten counties based on
; prevalence. How should you handle counties with a population of zero?
(defn preval-key [crime entry]
  (let [pop (:county_population entry)
        dui (crime entry)]
    (if (not (zero? pop))
      (/ dui pop)
      0)))

(defn most-duis-1
  "Given a JSON filename of UCR crime data for a particular year, finds the
   counties with the most DUIs."
  [file]
  (->> file
       (json/parse-string (slurp (io/resource file)) true)
       (map #(select-keys % [:county_population :driving_under_influence :fips_county_code :fips_state_code]))
       (sort-by preval-key >)
       (take 10)
       (map (fn [item] [(fips (str (:fips_state_code item) (:fips_county_code item)))
                        (:driving_under_influence item)]))
       (into {})))

;(most-duis-1 "2008.json")
;=>
;(["NC, Ashe" 655]
;  ["MS, Tunica" 432]
;  ["CA, Inyo" 424]
;  ["CO, Conejos" 210]
;  ["WI, Menominee" 189]
;  ["NC, Hyde" 125]
;  ["VA, Norton" 118]
;  ["CO, Costilla" 85]
;  ["CO, Cheyenne" 45]
;  ["TX, Kenedy" 11])

; 2. How do the prevalence counties compare to the original counties? Expand
; most-duis to return vectors of [county-name, prevalence, report-count,
; population] What are the populations of the high-prevalence counties?
; Why do you suppose the data looks this way? If you were leading a public-health
; campaign to reduce drunk driving, would you target your intervention based
; on report count or prevalence? Why?

(defn most-duis-2
  "Given a JSON filename of UCR crime data for a particular year, finds the
   counties with the most DUIs."
  [file]
  (->> file
       (json/parse-string (slurp (io/resource file)) true)
       (map #(select-keys % [:county_population :driving_under_influence :fips_county_code :fips_state_code]))
       (sort-by (partial preval-key :driving_under_influence) >)
       (take 10)
       (map (fn [item] [(fips (str (:fips_state_code item) (:fips_county_code item)))
                        (float (preval-key :driving_under_influence item))
                        (:driving_under_influence item)
                        (:county_population item)]))))

;(most-duis-2 "2008.json")
;=>
;(["WI, Menominee" 0.040935673 189 4617]
;  ["MS, Tunica" 0.04056338 432 10650]
;  ["VA, Norton" 0.031969655 118 3691]
;  ["TX, Kenedy" 0.028132992 11 391]
;  ["CO, Cheyenne" 0.026254376 45 1714]
;  ["CO, Conejos" 0.026122652 210 8039]
;  ["CO, Costilla" 0.026001835 85 3269]
;  ["NC, Ashe" 0.025449742 655 25737]
;  ["CA, Inyo" 0.024500173 424 17306]
;  ["NC, Hyde" 0.024447488 125 5113])

; 3. We can generalize the most-duis function to handle any type of crime. Write a function most-prevalent
; which takes a file and a field name, like :arson, and finds the counties where that field is most often
; reported, per capita.

(defn most-duis-3
  "Given a JSON filename of UCR crime data for a particular year, finds the
   counties with the most DUIs."
  [file crime]
  (->> file
       (json/parse-string (slurp (io/resource file)) true)
       (map #(select-keys % [crime :county_population :fips_county_code :fips_state_code]))
       (sort-by (partial preval-key crime) >)
       (take 10)
       (map (fn [item] [(fips (str (:fips_state_code item) (:fips_county_code item)))
                        (float (preval-key crime item))
                        (crime item)
                        (:county_population item)]))))

;(most-duis-3 "2008.json" :arson)
;=>
;(["TX, Cochran" 0.0029821075 9 3018]
;  ["GA, Glynn" 0.0025819265 195 75525]
;  ["TX, Kent" 0.0013831259 1 723]
;  ["OR, Gilliam" 0.0012012012 2 1665]
;  ["TX, McMullen" 0.0011389522 1 878]
;  ["NE, Kimball" 0.0011267606 4 3550]
;  ["NE, Jefferson" 0.0010780218 8 7421]
;  ["WI, Outagamie" 0.0010742428 188 175007]
;  ["NM, Dona Ana" 9.980635E-4 201 201390]
;  ["NV, Carson City" 7.732528E-4 42 54316])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
