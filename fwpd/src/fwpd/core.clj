(ns fwpd.core
  (:gen-class))

(def filename "suspects.csv")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV file into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen \" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows)
  )
(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn namify
  [rows]
  (map #(:name %) rows))

(defn append
  [rows new-suspect]
  (seq (conj (apply vector rows)  new-suspect)))

(def validations {:name #(and (contains? % :name) (<  (count (:name %)) 15)) :glitter-index #(contains? % :glitter-index)})

(defn validate
  [validations record]
  (reduce (fn [final-res validation-func]
            (println (str final-res \space validation-func))
            (and final-res (validation-func record)))
         true
         (map #(% validations) (keys validations)))
  )

(defn convert-csv
  [records]
  (clojure.string/join "\n" (map #(clojure.string/join "," (vals %)) records)))

;;; macro example
(comment (defmacro printandeval [expression]
  (let [result expression]
    (println result)
    (eval result))))

(defmacro my-print
  [expression]
  (list 'let ['result expression]
        (list 'println 'result)
        'result))

(defmacro code-critic
  "Phrases are courtesy Hermes Conrad from Futurama"
  [bad good]
  `(do
     (println "Great squid of Madrid, this is bad code:"  (quote ~bad))
     (println "Sweet gorilla of Manila, this is good code:" (quote ~good))))


;;; Recusrion to evaluate sum
(defn sum
  ([vals]
   (sum vals 0))
  ([vals accumulating-total]
   (if (empty? vals)
     accumulating-total
     (recur (rest vals) (+ (first vals) accumulating-total)))))


;;; Implement comp
(defn two-comp
  [f g]
  (fn [& args]
    (f (apply g args))))

;;; implement comp for any number of functions args
(defn evalfn
  "Helper function for custom comp"
  [flist args]
  (if (empty? flist)
    args
    (recur (rest flist) (list (apply (first flist) args))))
  )

(defn my-comp
  "Custom implementation for comp function"
  [& f]
  (fn
    [& args]
    (evalfn (reverse f) args)))

;; A recursive function is a function which calls itself.
;; This is one of the fundamental techniques used in functional programming.
((fn foo [x]
   (when (> x 0)
     (conj (foo (dec x)) x))) 5)

;;; Last element in set
(fn [x]
  (if-let [r (next x)]
    (recur r)
    (first x)))

(fn get-last [xs]
  (if (= (count xs) 1)
    (first xs)
    (get-last (rest xs))))

(fn dosia [xs]
  (if (empty? (rest xs))
    (first xs)
    (dosia (rest xs))))

#(nth % (dec (count %)))

(comp first reverse)

#(first (reverse %))

;;; Second last element
((fn [l]
   (if (<= (count l) 2)
     (first l)
     (recur (rest l)))) [2])

(comp second reverse)
#(second (reverse %))
(comp first rest reverse)

;;; nth element
(fn neth
  ([l n]
   (neth l n 0))
  ([l n acc-val]
   (if (= acc-val n)
     (first l)
     (recur (rest l) n (inc acc-val))) )
  )
#(loop [x %1 y %2]
   (if (zero? y)
     (first x)
     (recur (rest x) (dec y))))

#(first (drop %2 %1))

;;; Count elements
(fn num-elem
  ([l] (num-elem l 0))
  ([l cnt]
   (if (empty? l)
     cnt
     (recur (rest l) (inc cnt)))))

(fn [l]
  (reduce (fn [c _] (inc c)) 0 l))

(fn [sequence] (reduce (fn [acc v] (inc acc)) 0 sequence))

;;; Sum It All Up
(fn [l]
  (reduce (fn [_ sum] (+ sum _)) 0 l))
#(reduce + 0 %)
#(apply + %)

;;; Filter odd no.s
(fn [l]
  (filter (fn [_] (= (mod _ 2) 1)) l))
#(filter odd? %)

;;; Reverse seq
(fn [l]
  (reduce (fn [res it] (cons it res)) `() l))
reduce #(conj %1 %2) '()
into '()

;;; Palindrome checker
#(= (seq %) (reverse %))
;;; very nice use of -> forms chain
#(
   if(< (count %1)3)
   true
   (if(=(first %1)(last %1))
     (recur (-> %1 rest butlast))
     false)
   )

;;; Fibo numbers
#(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))
#(loop[i %, r '(1 1)]
   (if (= i 2)
     (reverse r)
     (recur (dec i) (conj r (+ (first r) (second r))))))

;;; Max elem
(fn [& params]
  (reduce (fn [res it]
            (if (> it res) it res)) -1 params))

(comp last sort list)

(fn [& params]
  (apply max params) )

(fn [x & y]
  (cond
    (nil? y) x
    (> x (first y)) (recur x (next y))
    :else (recur (first y) (next y))))

;;; Create string from matching characters
#(apply str (re-seq #"[A-Z]" %))

(fn [xs] (reduce str (re-seq #"[A-Z]" xs)))

#(clojure.string/replace % #"[^A-Z]+" "")

;;; Duplicate a sequence
#(reverse (reduce (fn [res it] (conj res it it)) `() %))

#(interleave % %)

(mapcat #(list % %) [1 2 3])
;user=> (mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
;(0 1 2 3 4 5 6 7 8 9)

;;; implement range
(fn [start end]
  (loop [i start, res `()]
    (if (>= i end)
      (reverse res)
      (recur (inc i) (conj res i)))
    ))

#(take (- %2 %1) (iterate inc %1))

(fn [from to]
  (take-while #(< % to)
              (iterate inc from)))

;;; Compress a sequence
#(reduce (fn [res it]
           (if (not (= (last res) it))
             (conj res it)
             res)) [(first %)] %)

#(loop [result [], input %]
   (cond
     (nil? input) result
     (= (last result) (first input)) (recur result (next input))
     :else (recur (conj result (first input)) (next input))))

#(map first (partition-by identity %))
;user=> (partition-by odd? [1 1 1 2 2 3 3])
;((1 1 1) (2 2) (3 3))
;
;user=> (partition-by even? [1 1 1 2 2 3 3])
;((1 1 1) (2 2) (3 3))
;;;

;;; Implement interleave
#(mapcat vector %1 %2)
#(flatten (map vector %1 %2))

;;; Flatten a collection
(fn myFlatten [x]
  (if (coll? x)
    (mapcat myFlatten x)
    [x]))

#(filter (complement sequential?)
         (rest (tree-seq sequential? seq %)))

;;; Factorial
#(apply * (range 1 (+ % 1)))

;;;Replicate a sequence
(fn [l num]
  (mapcat #(repeat num %) l))
(fn [xs n] (reduce concat (map #(repeat n %) xs)))

;;; Half truth
(fn[& args]
  (and (boolean (some true? args))  (not-every? true? args)))

(fn [& input]
  (and
    (<
      (count (filter #(true? %) input))
      (count input)
      )
    (not (=
           (count (filter #(true? %) input))
           0))
    )
  )

#(cond
   (every? identity %&) false
   (some identity %&) true
   :else false)


;user=> (let [grade 85]
;         (cond
;           (>= grade 90) "A"
;           (>= grade 80) "B"
;           (>= grade 70) "C"
;           (>= grade 60) "D"
;           :else "F"))
;"B"

;;; Interpose a sequence
#(drop-last (flatten (map (fn [it] [it %1] ) %2)))

#(drop-last (interleave %2 (repeat %1)))

;;; Pack a sequence
#(partition-by identity %)

(fn [s]
  (loop [res [] left s]
    (if
      (empty? left) res
                    (recur 
                      (concat res [(take-while #(= (first left) %) left)])
                      (drop-while #(= (first left) %) left)))))

;;; Drop nth element
(fn [l n]
  (loop [res [], i 1, rem l]
    (if (empty? rem)
      res
      (recur
        (if (not (= (mod i n) 0))
          (conj res (first rem))
          res) (inc i) (rest rem)))))

(fn [l n]
  (filter (fn [it]
            (not (= (mod (inc (.indexOf l it)) n) 0))) l))

;;; Split at index
(fn [n l]
  (loop [res [(first l)], i 1, rem (rest l)]
    (if (= i n)
      [res rem]
      (recur (conj res (first rem)) (inc i) (rest rem)))))

(fn[pos, col]
  [(subvec col 0 pos)(subvec col pos)])

(fn [n xs] (list (take n xs) (drop n xs)))

;;; Implement zipmap
(fn [v1 v2]
  (loop [res {}, remv1 v1, remv2 v2]
    (if (or (empty? remv1) (empty? remv2))
      res
      (recur (assoc res (first remv1) (first remv2)) (rest remv1) (rest remv2)))))

#(apply hash-map (interleave %1 %2))
;;; use of partial
(comp (partial apply sorted-map) interleave)

(fn [ks vs]
  (reduce merge (map (fn [k v] {k v}) ks vs)))

;;; Set intersection
(fn [set1 set2]
  (reduce (fn [res it]
            (if (contains? set2 it)
              (conj res it)
              res)) #{} set1))

(fn [set1 set2]
  (loop [res #{}, s1 (sort set1), s2 (sort set2)]
    (if (or (empty? s1) (empty? s2))
      res
      (cond
        (= (first s1) (first s2)) (recur (conj res (first s1) (first s2)) (rest s1) (rest s2))
        (< (first s1) (first s2)) (recur res (rest s1) s2)
        :else (recur res s1 (rest s2))
        )
      )))

#(set (for [x %1 :when (%2 x)] x))
;#(clojure.set/difference %1 (clojure.set/difference %1 %2))

;;; GCD
(fn [a b]
  (cond
    (= a 0) b
    :else (recur (mod b a) a)))

;;; Higher order functions example power
;(= [1 8 27 64] (map (__ 3) [1 2 3 4]))
(fn [n] #(int (Math/pow % n)))

#(let [pow %]
   (fn [x]
     (apply * (take pow (repeat x)))))


;;; Implememt iterate
(defn myiterate
  "Re-implementation of iterate"
  [f start-val]
  (cons start-val (lazy-seq
                    (myiterate
                      f (f start-val)))))

;;; #99 Product Digits
(fn [a b]
  (let [prod (* a b)]
    (loop [p prod, res []]
      (if (= p 0)
        res
        (recur (quot p 10) (cons (mod p 10) res))
        )
      )))

(fn [x y]
  (map #(- (int %) (int \0)) (seq (str (* x y)))))

;;; #143 dot product
(fn [l1 l2]
  (loop [rl1 l1, rl2 l2, res 0]
    (if (or (empty? rl1) (empty? rl2))
      res
      (recur
        (rest rl1)
        (rest rl2)
        (+ (* (first rl1) (first rl2)) res)
        ))))

#(reduce + (map * %1 %2))

#(->> (interleave %1 %2)
      (partition 2)
      (reduce (fn [agg x] (+ agg (apply * x))) 0))

;;; #88 Symmetric Difference
(fn [set1 set2]
  (let [intersection (clojure.set/intersection set1 set2)]
    (clojure.set/union
      (clojure.set/difference
        set1
        intersection)
      (clojure.set/difference
        set2
        intersection)
      )))

#(clojure.set/union (clojure.set/difference %1 %2) (clojure.set/difference %2 %1))

#(->> (for [x (into %1 %2)
            :when (not= (%1 x) (%2 x))] x)
      (into #{}))

;;; #90 Cartesian product
(fn [set1 set2]
  (loop [s1 set1, res #{}]
    (if (empty? s1)
      res
      (recur
        (rest s1)
        (apply (partial conj res) (map (fn [it] [(first s1) it]) set2)))
      )))
#(set
   (for[x %1, y %2]
     [x y]))

;;; #122 Read a binary number
(fn [binary-str]
  (loop [l (seq binary-str), order (dec (count l)), res 0]
    (if (< order 0)
      res
      (recur (rest l)
             (dec order)
             (+ res  (if (= (first l) \1)
                       (int (Math/pow 2 order))
                       0))))))
#(reduce
   (fn [x y] (+ (* 2 x) y)) 0
   (map
     (fn [d] (- (int d) (int \0)))
     (seq %)))

(#(Integer/parseInt % 2) "111")


;;; #63 Group a sequence (implement group by)
;(group-by #(> % 5) [1 3 6 8])
;=> {false [1 3], true [6 8]}
;(group-by #(mod % 2) [1 3 6 8])
;=> {1 [1 3], 0 [6 8]}

(fn [f list]
  (reduce (fn [agg it]
            (assoc agg
              (f it)
              (conj (vec (get agg (f it))) it)))  {} list))

(fn [f col]
  (reduce #(update-in
             %1
             [(f %2)]
             (fn [v] (conj (or v []) %2)))         ;; best way to deal with key:list maps manipulation
          {}
          col))

;;; #135 Infix Calculator
;(= 42 (__ 38 + 48 - 2 / 2))
(fn [& col]
  (loop [rem col]
    (if (<= (count rem) 1)
      (first rem)
      (recur (cons ((second rem) (first rem) (nth rem 2)) (drop 3 rem)))
      )))

;;; #157 Indexing a sequence
(fn [col]
  (reduce
    #(conj %1 [%2 (inc (second (last %1)))])
    [[(first col) 0]]
    (rest col)))
#(map vector % (range))

;;; Pascal's triangle
#(letfn[(factorial[n]
          (apply * (range 1 (inc n))))
        (binomial[n k]
          (/ (factorial n) (* (factorial k) (factorial (- n k)))))]
   (map (fn[col] (binomial (dec %) col)) (range %)))

;;; Re-implement map
(fn map- [f coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (cons (f (first s)) (map- f (rest s))))))

(fn myMap [func coll]
  (lazy-seq
    (if (seq coll)
      (cons (func (first coll)) (myMap func (rest coll))))))

(fn ff [f xs]
  (if (empty? xs)
    xs
    (lazy-seq (cons (f (first xs)) (ff f (drop 1 xs))))))

(fn [col]
  (let [sum-dig-sq (fn [n]
                     (reduce +
                             (map (fn [it]
                                    (int (Math/pow it 2)))
                                  (map #(- (int %) (int \0)) (seq (str n))))))]
    (filter #(> (sum-dig-sq %) %) col)
    ))

;;; #120 Sum of square digits
(fn [col]
  (let [sum-dig-sq (fn [n]
                     (reduce +
                             (map (fn [it]
                                    (int (Math/pow it 2)))
                                  (map #(- (int %) (int \0)) (seq (str n))))))]
    (count (filter #(> (sum-dig-sq %) %) col))
    ))

(fn [coll]
  (let [sumOfSquaredComponent (->> coll
                                   (map str)
                                   (map seq)
                                   (map #(map (fn [x] (- (int x) 48)) %))
                                   (map #(map (fn [x] (* x x)) %))
                                   (map #(reduce + %)))]
    (count (filter true? (map > sumOfSquaredComponent coll)))))

;;; #95 To Tree, or not to Tree
(fn tree? [coll]
  (cond
    (or (seq? coll) (vector? coll))
    (and (= 3 (count coll)) (tree? (nth coll 1)) (tree? (nth coll 2)))
    (nil? coll) true
    :else false))

(fn tree? [coll]
  (if (coll? coll)
    (if (= (count coll) 3)
      (and (tree? (second coll)) (tree? (last coll)))
      false)
    (nil? coll)))

;;; #128 Recognie playing cards
(fn [card]
  (let [type {"D" :diamond "H" :heart "C" :club "S" :spade}
        rank {"A" 12 "K" 11 "Q" 10 "J" 9 "T" 8}
        t (first (map str (seq card)))
        r (second (map str (seq card)))]
    {:suit (get type t) :rank (if (contains? rank r) (get rank r) (- (Integer/parseInt r) 2))}
    ))

(fn [[s r]]
  {:suit (condp = s
           \D :diamond
           \H :heart
           \C :club
           \S :spade)
   :rank (condp = r
           \A 12
           \K 11
           \Q 10
           \J 9
           \T 8
           (- (int r) 50))})

;;; #100 LCM
(fn [& args]
  (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
    (/ (reduce * args) (reduce gcd args))))

;;; #147 Pascal's trapezoid
(fn [row]
  (iterate #(map +' `(0 ~@%) `(~@% 0)) row))

(fn pascalSeq [x]
  (letfn[(nextPascal [coll]
           (vec (map +' (cons 0 coll) (conj coll 0))))]
    (lazy-seq
      (let[nextX (nextPascal x)]
        (cons x (pascalSeq nextX))))))

;;; #96Beauty is Symmetry
(fn [[v l r]]
  (letfn [(mirror? [lb rb]
            (cond
              (not= (coll? lb) (coll? rb)) false
              (and (coll? lb) (coll? lb)) (let [[lv ll lr] lb [rv rl rr] rb]
                                            (and (= lv rv) (mirror? ll rr) (mirror? lr rl)))
              :else (= lb rb))
            )]
    (mirror? l r)))

;;; #153 Pairwise Disjoint Sets
(fn [coll]
  (loop [un #{(first coll)}, l (rest coll)]
    (if (empty? (clojure.set/intersection un (first l)))
      (if (not (empty? l))
        (recur (clojure.set/union un (first l)) (rest l))
        true)
      false
      )
    ))

;;; #146 Trees into tables
(fn [coll]
  (reduce (fn [acc it]
            (let [k (first it) v (second it)]
              (if (map? v)
                (reduce #(assoc %1 [k (first %2)] (second %2)) acc v)
                (assoc acc k v))
              )) {} coll))
#(reduce (partial apply assoc) {}
         (for [keyVal %
               value (second keyVal)]
           (list [(first keyVal) (first value)] (second value))))