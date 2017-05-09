(ns clojure-noob.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!!")
  (print "Cleanliness is next to Godliness!")
  )

(defn dec-maker-my
  [dec-val]
  (fn [input]
    (- input dec-val)))

(defn dec-maker-ot
  [dec-val]
  #(- % dec-val)
  )

(defn mapset
  "mapset, that works like map except the return value is a set:"
  [input-func input-set]
  (set
    (map input-func input-set)))

(reduce (fn [new-map [key val]]
          (assoc new-map key (inc val))) {} {:max 30 :min 10})

(reduce (fn [new-map [key val]]
          (if (> val 4)
            (assoc new-map key val)
            new-map))
        {} {:human 4.1 :critter 3.9})
; => {:human 4.1}

; 1. Implement map using reduce (You can do all seq transforms using reduce
; 2. Last two problems of 4th chapter



(defn map-using-red
  "Implements maps using reduce"
  [f, coll]
  (seq (reduce (fn [new-list item]
                      (conj new-list (f item))) [] coll))
  )


;;;;;;;;;;;;;;;;;;
;;Vampire problems
;;;;;;;;;;;;;;;;;;
(def vampire-database
  {0 {:makes-blood-puns? false, :has-pulse? true  :name "McFishwich"}
   1 {:makes-blood-puns? false, :has-pulse? true  :name "McMackson"}
   2 {:makes-blood-puns? true,  :has-pulse? false :name "Damon Salvatore"}
   3 {:makes-blood-puns? true,  :has-pulse? true  :name "Mickey Mouse"}})

(defn vampire?
  [record]
  (and (:makes-blood-puns? record) (not (:has-pulse? record)) record))

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 1000)
  (get vampire-database social-security-number))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire?
                 (map vampire-related-details social-security-numbers))))

;;; Lazy call to map
;(time (def mapped-details (map vampire-related-details (range 0 1000000))))
; => "Elapsed time: 0.049 msecs"
; => #'user/mapped-details
;(time (first mapped-details))
; => "Elapsed time: 32030.767 msecs"
; => {:name "McFishwich", :makes-blood-puns? false, :has-pulse? true}

;;;;;;;;;;;;;;;;;;
;;Hobbit problems
;;;;;;;;;;;;;;;;;;
(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrisize-body-parts
  "Expects a seq of maps that have :name and :size and creates symm"
  [asymm-body-parts]
  (loop [remaining-asymm-parts asymm-body-parts
         final-parts []]
    (if (empty? remaining-asymm-parts)
      final-parts
      (let [[part & remaining] remaining-asymm-parts]
        (recur remaining
               (into final-parts
                     (set [part (matching-part part)])))))))

(defn better-sym-body-parts
  "A better symm body parts routine with reduce"
  [asym-parts]
  (reduce (fn [final_parts part]
            (into final_parts (set [part (matching-part part)])))
          []
          asym-parts))

(defn hit
  [asym-parts]
  (let [sym-parts (better-sym-body-parts asym-parts)
        body-part-sum (reduce + (map :size sym-parts))
        target (rand body-part-sum)]
    (loop [[part & remaining] sym-parts
           acc-size (:size part)]
      (if (> acc-size target)
        part
        (recur remaining (+ acc-size (:size (first remaining))))))))

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])



