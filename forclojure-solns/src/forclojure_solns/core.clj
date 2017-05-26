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
         reverse)))

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
        (inc idx)))))

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
       (into {})))

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
             (rest rem)))))

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
        ))))

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
     (lazy-seq (cons firstarg (reduct f firstarg seq))))))
;; you can also not call lazy-seq but need it
;;You would need to wrap it only in case you need to call realized? on it.

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
        (recur (rest rem) res (conj part (first rem)))))))

;;; Juxtaposition #59
(defn xjuxt [& fs]
  (fn [& args]
    (reduce #(conj %1 (apply %2 args)) [] fs)))

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

;;; Prime Numbers #67 (have made it lazy)
(defn n-primes
  []
  (filter (fn [num]
            (loop [end (int (Math/sqrt num)), div 2, re (rem num div)]
              (cond
                (< num 2) false
                (= num 2) true
                (= re 0) false
                (> div end) true
                :else (recur end (inc div) (rem num div)))
              )) (range)))

;; Usage (take 5 (n-primes))

;;; Filter Perfect Squares #74
(defn filter-perf-sq
  [input-str]
  (->> (re-seq #"\d+" input-str)
       (map #(Integer/parseInt %))
       (filter #(== (Math/pow (int (Math/sqrt %)) 2) %))
       (clojure.string/join ",")))


;;; Anagram Finder #77
; (= (__ ["meat" "mat" "team" "mate" "eat"])
; #{#{"meat" "team" "mate"}})
; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
; #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

(defn anagram-finder
  [coll]
  (into #{} (->> coll
                 (reduce (fn [acc item]
                           (update-in acc [(apply str (sort item))] conj item)) {})
                 (filter #(> (count (second %)) 1))
                 (map #(second %))
                 (map set))))

#(->> (group-by sort %)
      (vals)
      (map set)
      (filter (comp seq rest))
      (set))

;;; Perfect Numbers #80
; (= (__ 6) true) 1+2+3=6.
; (= (__ 7) false)

(defn perfect-no?
  [num]
  (->> (range 1 num)
       (filter #(zero? (rem num %)))
       (reduce +)
       (= num)))

;;; Merge with a Function #69
; (= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
;{:a 4, :b 6, :c 20})
; (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
;{1 7, 2 10, 3 15})

(defn xmerge-with
  [f & maps]
  (reduce (fn [acc item]
            (reduce (fn [inacc initem]
                      (if (contains? inacc (first initem))
                        (update-in inacc [(first initem)] f (second initem))
                        (assoc inacc (first initem) (second initem)))) acc item))
          {} maps))

;;; intoCamelCase #102
; (= (__ "multi-word-key") "multiWordKey")
; (= (__ "leaveMeAlone") "leaveMeAlone")
(defn to-camel-case
  [string]
  (let [coll (re-seq #"\w+" string)]
    (if (> (count coll) 1)
      (apply str
             (cons (first coll) (map clojure.string/capitalize (rest coll))))
      (apply str coll))))

;The trampoline function takes a function f and a variable number of parameters.
;Trampoline calls f with any parameters that were supplied. If f returns a function,
;trampoline calls that function with no arguments. This is repeated, until the return
;value is not a function, and then trampoline returns that non-function value. This is
;useful for implementing mutually recursive algorithms in a way that won't consume the
;stack. test not run

(= [1 3 5 7 9 11]
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

;;; Happy numbers #86
; (= (__ 7) true)
; (= (__ 2) false)
(defn happy-no [num]
  (letfn [(sum-digit-sqrs [n]
            (->> (str n)
                 seq
                 (map #(int (Math/pow (Integer/parseInt (str %)) 2)))
                 (reduce +)))]
    (loop [xset #{}, sum-digit (sum-digit-sqrs num)]
      ;(println "xset: " xset "sum-digit: " sum-digit)
      (cond
        (= sum-digit 1) true
        (contains? xset sum-digit) false
        :else (recur (conj xset sum-digit) (sum-digit-sqrs sum-digit))))))

;;; The Balance of N #115
; (= true (__ 11))
; (= true (__ 121))
; (= false (__ 123))
; (= true (__ 0))
(defn balanced? [num]
  (let [list (->> (seq (str num))
                  (map #(Integer/parseInt (str %))))
        cnt (count list)
        split (split-at (quot cnt 2) list)]
    (if (odd? cnt)
      (= (reduce + (first split)) (reduce + (rest (second split))))
      (= (reduce + (first split)) (reduce + (second split))))))

;;; Power Set #85
; (= (__ #{}) #{#{}})
; (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
; (= (__ #{1 2 3})
;#{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
(defn powerset [input-set]
  (let [ve (vec input-set)
        cnt (count ve)
        end (int (Math/pow 2 cnt))
        ]
    (if (= cnt 0)
      #{#{}}
      (loop [idx 0
             bitstr (Integer/toBinaryString idx)
             result #{}]
        (if (= idx end)
          result
          (recur (inc idx)
                 (Integer/toBinaryString (inc idx))
                 (conj
                   result
                   ((fn [v bitv]
                      (set
                        (map first
                             (filter #(= (second %) "1")
                                     (map #(vector %1 %2) v bitv))))) ve
                     (map str (format (str "%0" cnt "d") (Integer/parseInt bitstr)))))))))))

;;; Equivalence Classes #98
(defn equivalence-classes [f D]
  (->> D
       (map #(vector (f %) %))
       (group-by first)
       vals
       (map (fn [group] (map second group)))
       (map set)
       set))

;;; Identify keys and values #105
; (= {} (__ []))
; (= {:a [1]} (__ [:a 1]))
; (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))
(defn id-key-val [coll]
  (letfn [(take-next-num-seq [seq]
            (take-while (comp not keyword?) seq))
          (drop-next-num-seq [seq]
            (drop-while (comp not keyword?) seq))]
    (loop [res {}, rem coll]
      (if (empty? rem)
        res
        (recur (assoc res (first rem) (take-next-num-seq (rest rem)))
               (drop-next-num-seq (rest rem)))))))

#(->> (partition-by keyword? %)
      (partition 2)
      (reduce (fn [agg [k v]]
                (-> (zipmap (reverse k) (cons v (repeat ())))
                    (into agg))) {}))

(def acc (atom 0))
;;; Sequs Horribilis #112
; (=  (__ 10 [1 2 [3 [4 5] 6] 7])
;'(1 2 (3 (4))))
; (=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
;'(1 2 (3 (4 (5 (6 (7)))))))
;(=  (__ 1 [[[[[1]]]]])
;'(((((1))))))

(defn seq-horrib [coll]
  (if (coll? coll)
    (map seq-horrib coll)
    (do (swap! acc + coll) coll)))


(fn [max coll]
  (letfn [(seq-horrib [max coll]
            (loop [res [0 []] rem coll]
              (if-let [item (first rem)]
                (if (coll? item)
                  (let [[sum-int coll-int] (seq-horrib (- max (first res)) item)]
                    (recur [(+ sum-int (first res)) (conj (second res) coll-int)]
                           (rest rem)))
                  (if (> (+ item (first res)) max)
                    res
                    (recur [(+ item (first res)) (conj (second res) item)]
                           (rest rem))))
                res)))]
    (second (seq-horrib max coll))))

;; Euler's Totient Function #75
(defn euler-tot [num]
  (let [gcd (fn [a b]
              (if (= a 0) b (recur (mod b a) a)))]
    (loop [res 1, rem (range 2 num)]
      (if (empty? rem)
        res
        (recur (if (= 1 (gcd (first rem) num)) (inc res) res)
               (rest rem))))))

;; Balancing Brackets #177
;(__ "This string has no brackets.")
;(__ "class Test {
;      public static void main(String[] args) {
;        System.out.println(\"Hello world.\");
;      }
;    }")
;(not (__ "(start, end]"))
;(not (__ "())"))

(defn balanced-bracks? [str]
  (let [s (java.util.Stack.)
        inval? (fn [c]
                 (cond
                   (or (= c \()
                       (= c \{)
                       (= c \[)) (not (.push s c))
                   (= c \)) (if (or (.isEmpty s) (not= (.peek s) \())
                              true
                              (not (.pop s)))
                   (= c \]) (if (or (.isEmpty s) (not= (.peek s) \[))
                              true
                              (not (.pop s)))
                   (= c \}) (if (or (.isEmpty s) (not= (.peek s) \{))
                              true
                              (not (.pop s)))
                   :else false))]
    (loop [invalid false, rem (seq str)]
      (if invalid
        false
        (if (empty? rem)
          (if (not (.isEmpty s))
            false
            true)
          (recur (inval? (first rem)) (rest rem)))))))

;;Reimplement Trampoline #78
(defn xtramp [f & args]
  (let [res (apply f args)]
    (if (fn? res)
      (xtramp res)
      res)))

;; Sequence of pronunciations #110
; (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
; (= [3 1 2 4] (first (__ [1 1 1 4 4])))
; (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))

(defn pronoun-seq [coll]
  (let [ps (fn [coll]
             (->> coll
                  (partition-by identity)
                  (mapcat (fn [it] [(count it) (first it)]))))]
    (lazy-seq (cons (ps coll) (pronoun-seq (ps coll))))))

;; Digits and bases #137
; (= [1 2 3 4 5 0 1] (__ 1234501 10))
; (= [0] (__ 0 11))
; (= [1 0 0 1] (__ 9 2))
; (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))

(defn dig-and-bases [num base]
  (if (zero? num)
    [0]
    (loop [acc [], n num]
      (if (zero? n)
        acc
        (recur (cons (rem n base) acc) (quot n base))))))

;; Oscilrate #144
; (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
; (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])


(defn oscilrate [start & fs]
  (letfn [(oscil [start fs]
            (lazy-seq
              (let [curr (drop 1 (reduce #(conj %1 (%2 (last %1))) [start] fs))]
                (concat curr (oscil (last curr) fs)))))]
    (cons start (oscil start fs))))
