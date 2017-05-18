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

;;; Distinct numbers in list
(defn xdistinct
  [coll]
  (loop [res []
         map (group-by identity coll)
         rem coll
         ]
    (if (empty? rem)
      res
      (recur (if (contains? map (first rem))
               (conj res (first rem))
               res)
             (dissoc map (first rem))
             (rest rem))
      )))

;;; Function Composition #58
; (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
; (= 5 ((__ (partial + 3) second) [1 2 3 4]))
; (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
; (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

(defn xcomp
  [& f]
  (fn [& args]
    (loop [rem (reverse f), res args]
      (if (empty? rem)
        (first res)
        (recur (rest rem) [(apply (first rem) res)])
        )
      )))

;;; Map Defaults #156
; (= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})
; (= (__ "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
; (= (__ [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})
#(into {} (map vec (partition 2 (interleave %2 (repeat %1)))))


;;; Sequence Reductionns #60
; (= (take 5 (__ + (range))) [0 1 3 6 10])
; (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
; (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)
(defn xreductions
  ([f seq] (xreductions f (first seq) (rest seq)))
  ([f firstarg seq]
   (letfn [(reduct [f acc se]
             (lazy-seq (when-not (empty? se)
                         (let [res (f acc (first se))]
                           (cons res (reduct f res (rest se)))))))]
     (lazy-seq (cons firstarg (reduct f firstarg seq)))
     ;; you can also not call lazy-seq but need it
     ;;You would need to wrap it only in case you need to call realized? on it.
     )))
;=> (source iterate)
;In addi­tion, one can gen­er­ate lazy sequences from scratch using the lazy-seq macro.
;This macro takes a form which gen­er­ates a sequences and wraps it in a func­tion which
;is used as the tail of a lazy sequence. You can recur­sively gen­er­ate a lazy
;sequence then, with a recur­sive func­tion that wraps it’s body in a call to lazy-seq. For example:

;(defn iterate
;  "Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects"
;  [f x] (cons x (lazy-seq (iterate f (f x)))))


;;; Partition a Sequence #54
(defn xpartition [n seq]
  (loop [rem seq, res [], part []]
    ;(println rem "," res "," part ",")
    (if (empty? rem)
      ; if
      (if (= (count part) n)
        (conj res part)
        res)
      ; else
      (if (= (count part) n)
        (recur (rest rem) (conj res part) [(first rem)])
        (recur (rest rem) res (conj part (first rem))))
      )))

;;; Juxtaposition #59
(defn xjuxt [& fs]
  (fn [& args]
    (reduce #(conj %1 (apply %2 args)) [] fs)
    ))

(fn myJuxt [& fs]
  (fn [& args]
    (for [f fs]
      (apply f args))))

;;; Word Sorting # 70
(defn word-sort [sentence]
  (sort-by #(.toLowerCase %)
           (clojure.string/split (->>
                                   (seq sentence)
                                   (map int)
                                   (filter #(or (and (>= % 97) (<= % 122)) (and (>= % 65) (<= % 122)) (= % 32)))
                                   (map char)
                                   (apply str)
                                   ) #" ")))

#(->> (re-seq #"\w+" %)
      (sort-by clojure.string/lower-case))

(fn [string]
  (sort
    #(compare (.toLowerCase %1) (.toLowerCase %2))
    (clojure.string/split string #"\W")))