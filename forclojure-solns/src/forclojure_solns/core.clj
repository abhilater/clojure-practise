(ns forclojure-solns.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;; Rotate Sequence #44
; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
; (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
(defn rotate-seq
  [n coll]
  (let [times (mod n (count coll))]
    (->> coll
         (split-at times)
         (map reverse)
         flatten
         reverse
         )))

;;; Reverse Interleave #42
;(= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
;(= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))

(defn rev-interleave
  [coll n]
  (loop [rem coll, acc {}, idx 0]
    (if (empty? rem)
      (map seq (vals acc))
      (recur
        (rest rem)
        (update-in acc [(mod idx n)] #(conj (or % []) (first rem)))
        (inc idx)
        ))))

;;; Split by Type #50
; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
; (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
; (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})
(defn split-by-type
  [coll]
  (set
    (map
      #(vec (map second %))
      (vals
        (group-by
          first
          (partition 2 (interleave (map type coll) coll)))))))

#(->> % (group-by type) (vals))

;;; Count Occurrences #55
(defn count-freq
  [coll]
  (->> coll
       (group-by identity)
       (map (fn [ent]
              (vector (first ent) (count (second ent)))))
       (into {})
       ))



