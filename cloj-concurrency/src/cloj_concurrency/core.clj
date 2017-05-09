(ns cloj-concurrency.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Clojure Concurrency;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;;; Future examples --- futures to define a task and place it on
  ;;; another thread without requiring the result
  ;;; immediately. You can create a future with the future macro
  (let [result (future (Thread/sleep 5000)
                       (+ 1 1))]
    (println "The result is: " @result)
    (println "It will be at least 3 seconds before I print"))

  (realized? (future (Thread/sleep 1000)))
  ; => false

  (let [f (future)]
    @f
    (realized? f))
  ; => true

  ;;; Delay Examples --- Delays allow you to define a task without
  ;;; having to execute it or require the result immediately
  (def jackson-5-delay
    (delay (let [message "Just call my name and I'll be there"]
             (println "First deref:" message)
             message)))

  (force jackson-5-delay)
  ; => First deref: Just call my name and I'll be there
  ; => "Just call my name and I'll be there"

  @jackson-5-delay
  ; => "Just call my name and I'll be there"

  (def gimli-headshots ["serious.jpg" "fun.jpg" "playful.jpg"])
  (defn email-user
    [email-address]
    (println "Sending headshot notification to" email-address))
  (defn upload-document
    "Needs to be implemented"
    [headshot]
    (println (str "Uploading " headshot " ...\n"))
    true)
  (let [notify (delay (email-user "and-my-axe@gmail.com"))]
    (doseq [headshot gimli-headshots]
      (future (upload-document headshot)
              (force notify))))

  ;;; Promises allow you to express that you expect a result without
  ;;; having to define the task that should produce it or when that
  ;;; task should run. You create promises using promise and deliver
  ;;; a result to them using deliver. You obtain the result by dereferencing
  (def my-promise (promise))
  (deliver my-promise (+ 1 2))
  @my-promise
  ; => 3

  (def yak-butter-international
    {:store      "Yak Butter International"
     :price      90
     :smoothness 90})
  (def butter-than-nothing
    {:store      "Butter Than Nothing"
     :price      150
     :smoothness 83})
  ;; This is the butter that meets our requirements
  (def baby-got-yak
    {:store      "Baby Got Yak"
     :price      94
     :smoothness 99})

  (defn mock-api-call
    [result]
    (Thread/sleep 1000)
    result)

  (defn satisfactory?
    "If the butter meets our criteria, return the butter, else return false"
    [butter]
    (and (<= (:price butter) 100)
         (>= (:smoothness butter) 97)
         butter))
  (time (some (comp satisfactory? mock-api-call)
              [yak-butter-international butter-than-nothing baby-got-yak]))
  ; => "Elapsed time: 3002.132 msecs"
  ; => {:store "Baby Got Yak", :smoothness 99, :price 94}

  (time
    (let [butter-promise (promise)]
      (doseq [butter [yak-butter-international butter-than-nothing baby-got-yak]]
        (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
                  (deliver butter-promise satisfactory-butter))))
      (println "And the winner is:" @butter-promise)
      ))
  ;;;You might be wondering what happens if none of the yak butter is satisfactory. If that happens, the dereference would
  ;;; block forever and tie up the thread. To avoid that, you can include a timeout:
  (let [p (promise)]
    (deref p 100 "timed out"))

  ;;; Use promises to register callbacks like Javascript
  (let [ferengi-wisdom-promise (promise)]
    (future (println "Here's some Ferengi wisdom:" @ferengi-wisdom-promise))
    (Thread/sleep 100)
    (deliver ferengi-wisdom-promise "Whisper your way to success."))
  ; => Here's some Ferengi wisdom: Whisper your way to success.

  ;;; Enque code using promise and future
  (defmacro wait
    "Sleep `timeout` seconds before evaluating body"
    [timeout & body]
    `(do (Thread/sleep ~timeout) ~@body))
  (defmacro enqueue
    ([q concurrent-promise-name concurrent serialized]
     `(let [~concurrent-promise-name (promise)]
        (future (deliver ~concurrent-promise-name ~concurrent))
        (deref ~q)
        ~serialized
        ~concurrent-promise-name))
    ([concurrent-promise-name concurrent serialized]
     `(enqueue (future) ~concurrent-promise-name ~concurrent ~serialized)))

  (time @(-> (enqueue saying (wait 200 "'Ello, gov'na!") (println @saying))
             (enqueue saying (wait 400 "Pip pip!") (println @saying))
             (enqueue saying (wait 100 "Cheerio!") (println @saying))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Exercises;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
; 1. Write a function that takes a string as an argument and searches for it on
; Bing and Google using the slurp function. Your function should
; return the HTML of the first page returned by the search.

(defn test-search
  [search-param]
  (let [res-promise (promise)]
    ;;; search on goole
    (future
      (deliver res-promise
               (do
                 (Thread/sleep 200)
                 (slurp (str "http://www.google.com?q=" search-param)))))
    ;;; search on bing
    (future
      (deliver res-promise
               (slurp (str "http://www.bing.com?q=" search-param))))
    @res-promise))

; 2. Update your function so it takes a second argument consisting of
; the search engines to use.
(def default-search-engines
  ["https://google.com/search?q%3D" "https://www.bing.com/search?q%3D"])

(defn test-search-generic
  [search-param engines]
  (let [res-promise (promise)]
    (doseq [engine engines]
      (future
        (deliver res-promise
                 (slurp (str engine search-param))))
      )
    @res-promise
    ))

; 3. Create a new function that takes a search term and search engines as arguments,
; and returns a vector of the URLs from the first page of search results from each
; search engine.
(defn promised-request
  [term search-engine]
  (let [url (str search-engine term)
        request-promise (promise)]
    (future (deliver request-promise (slurp url)))
    request-promise
    )
  )

(defn get-urls
  [source]
  (re-seq #"https?://[^\"]*" source)
  )

(defn search
  "Exercise 3"
  [term search-engines]
  (vec (flatten
         (map #(get-urls (deref %))
                     (map #(promised-request term %) search-engines))))
  )


(comment
  (test-search-generic "abhishek"
                       default-search-engines)

  (search "clojure" default-search-engines)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Clojure Metaphysics;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;;; Atoms
(def fred (atom {
             :cuddle-hunger-level 0
             :percent-deteriorated 0
             }))

(let [zombie-state @fred]
  (if (>= (:percent-deteriorated zombie-state) 50)
    (future (println (:cuddle-hunger-level zombie-state)))))

(swap! fred (fn [current-state]
              (merge-with +
                          current-state
                          {:cuddle-hunger-level 1 :percent-deteriorated 1})))
;;;Sometimes you’ll want to update an atom without checking its current value
(reset! fred {:cuddle-hunger-level 0
              :percent-deteriorated 0})
;;;Compare-set semantics
;1. It reads the current state of the atom.
;2. It then applies the update function to that state.
;3. Next, it checks whether the value it read in step 1 is identical to the atom’s current value.
;4. If it is, then swap! updates the atom to refer to the result of step 2
;5. If it isn’t, then swap! retries, going through the process again with step 1




;Watches allow you to be super creepy and check in on your reference types’
; every move. Validators allow you to be super controlling and restrict what
; states are allowable
(defn shuffle-speed
  [zombie]
  (* (:cuddle-hunger-level zombie)
     (- 100 (:percent-deteriorated zombie))))

(defn shuffle-alert
  [key watched old-state new-state]
  (let [sph (shuffle-speed new-state)]
    (if (> sph 5000)
      (do
        (println "Run, you fool!")
        (println "The zombie's SPH is now " sph)
        (println "This message brought to your courtesy of " key))
      (do
        (println "All's well with " key)
        (println "Cuddle hunger: " (:cuddle-hunger-level new-state))
        (println "Percent deteriorated: " (:percent-deteriorated new-state))
        (println "SPH: " sph)))))

(reset! fred {:cuddle-hunger-level 22 :percent-deteriorated 2})
(add-watch fred :fred-shuffle-alert shuffle-alert)
(swap! fred update-in [:percent-deteriorated] + 1)
; => All's well with  :fred-shuffle-alert
; => Cuddle hunger:  22
; => Percent deteriorated:  3
; => SPH:  2134

(swap! fred update-in [:cuddle-hunger-level] + 30)
; => Run, you fool!
; => The zombie's SPH is now 5044
; => This message brought to your courtesy of :fred-shuffle-alert





;;; Validators --When you add a validator to a reference, the reference is modified
;;; so that, whenever it’s updated, it will call this validator with the value
;;; returned from the update function as its argument. If the validator fails by
;;; returning false or throwing an exception, the reference won’t change to point
;;; to the new value.
(defn percent-deteriorated-validator
  [{:keys [percent-deteriorated]}]
  (or (and (>= percent-deteriorated 0)
       (<= percent-deteriorated 100))
      (throw (IllegalStateException. "That's not mathy!"))))

(def bobby
  (atom
    {:cuddle-hunger-level 0 :percent-deteriorated 0}
    :validator percent-deteriorated-validator))
(swap! bobby update-in [:percent-deteriorated] + 200)




;;; Refs allow you to update the state of multiple identities using transaction
;;; semantics. These transactions have three features:

;1. They are atomic, meaning that all refs are updated or none of them are.
;2. They are consistent, meaning that the refs always appear to have valid states.
;   A sock will always belong to a dryer or a gnome, but never both or neither.
;3. They are isolated, meaning that transactions behave as if they executed serially;
;   if two threads are simultaneously running transactions that alter the same ref,
;   one transaction will retry. This is similar to the compare-and-set semantics of
;   atoms.

;;; Clojure uses software transactional memory (STM)
(def sock-varieties
  #{"darned" "argyle" "wool" "horsehair" "mulleted"
    "passive-aggressive" "striped" "polka-dotted"
    "athletic" "business" "power" "invisible" "gollumed"})

(defn sock-count
  [sock-variety count]
  {:variety sock-variety
   :count count})

(defn generate-sock-gnome
  "Create an initial sock gnome state with no socks"
  [name]
  {:name name
   :socks #{}})

(def sock-gnome (ref (generate-sock-gnome "Barumpharumph")))
(def dryer (ref {:name "LG 1337"
                 :socks (set (map #(sock-count % 2) sock-varieties))}))

(defn steal-sock
  [sock-gnome dryer]
  (dosync
    (when-let [pair (some #(if (= (:count %) 2) %) (:socks @dryer))]
      (let [updated-count (sock-count (:variety pair) 1)]
        (alter sock-gnome update-in [:socks] conj updated-count)
        (alter dryer update-in [:socks] disj pair)
        (alter dryer update-in [:socks] conj updated-count)
        )
      )
    ))
(steal-sock sock-gnome dryer)
(:socks @sock-gnome)
;The transaction will try to commit its changes only when it ends. The commit
; works similarly to the compare-and-set semantics of atoms.
; Each ref is checked to see whether it’s changed since you first tried to alter it.
; If any of the refs have changed, then none of the refs is updated and the
; transaction is retried. For example, if Transaction A and Transaction B are
; both attempted at the same time and events occur in the following order,
; Transaction A will be retried:

;Transaction A: alter gnome
;Transaction B: alter gnome
;Transaction B: alter dryer
;Transaction B: alter dryer
;Transaction B: commit—successfully updates gnome and dryer
;Transaction A: alter dryer
;Transaction A: alter dryer
;Transaction A: commit—fails because dryer and gnome have changed; retries

;;; commute
; just like alter. However, its behavior at commit time is completely different.
; Here’s how alter behaves:

;1. Reach outside the transaction and read the ref’s current state.
;2. Compare the current state to the state the ref started with within the transaction.
;3. If the two differ, make the transaction retry.
;4. Otherwise, commit the altered ref state.

;commute, on the other hand, behaves like this at commit time:
;1. Reach outside the transaction and read the ref’s current state.
;2. Run the commute function again using the current state.
;3. Commit the result.

;As you can see, commute doesn’t ever force a transaction retry. This can
;help improve performance, but it’s important that you only use commute
;when you’re sure that it’s not possible for your refs to end up in an invalid state

;;; vars
;When I first introduced def, I implored you to treat it as if it’s defining a constant.
; It turns out that vars are a bit more flexible than that: you can create
; a dynamic var whose binding can be changed. Dynamic vars can be useful
; for creating a global name that should refer to different values in different contexts.

(def ^:dynamic *notification-address* "dobby@elf.org")
(binding [*notification-address* "test@elf.org"]
  *notification-address*)
; => "test@elf.org"
;;; Stacked bindings
(binding [*notification-address* "tester-1@elf.org"]
  (println *notification-address*)
  (binding [*notification-address* "tester-2@elf.org"]
    (println *notification-address*))
  (println *notification-address*))
;;; Practical use case
;;; This is much less burdensome than passing an output destination to every
;;; invocation of println. Dynamic vars are a great way to specify a common resource
;;; while retaining the flexibility to change it on an ad hoc basis.
(binding [*out* (clojure.java.io/writer "print-output")]
  (println "A man who carries a cat by the tail learns
something he can learn in no other way.
-- Mark Twain"))
(slurp "print-output")
; => A man who carries a cat by the tail learns
;something he can learn in no other way.
;-- Mark Twain
(def ^:dynamic *troll-thought* nil)
(defn troll-riddle
  [your-answer]
  (let [number "man meat"]
    (when (thread-bound? #'*troll-thought*)
      (set! *troll-thought* number))
    (if (= number your-answer)
      "TROLL: You can cross the bridge!"
      "TROLL: Time to eat you, succulent human!")))

(binding [*troll-thought* nil]
  (println (troll-riddle 2))
  (println "SUCCULENT HUMAN: Oooooh! The answer was" *troll-thought*))

; => TROLL: Time to eat you, succulent human!
; => SUCCULENT HUMAN: Oooooh! The answer was man meat

;;; Thread bindings
(let [out *out*]
  (.start
    (Thread. #(binding [*out* out]
                (.write *out* "prints to repl from thread")))))
(.start (Thread. (bound-fn [] (.write *out* "prints to repl from thread"))))

;The point is that bindings don’t get passed on to manually created threads.
;They do, however, get passed on to futures. This is called binding conveyance

;with-redefs can be used with any var, not just dynamic ones. Because it has has
; such far-reaching effects, you should only use it during testing. For example,
; you could use it to redefine a function that returns data from a network call,
; so that the function returns mock data without having to actually make a network
; request.
(with-redefs [*out* *out*]
  (doto (Thread. #(println "with redefs allows me to show up in the REPL"))
    .start
    .join))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Stateless Concurrency and Parallelism with pmap;;;;;;;;;;;;;;;;;;;
(comment
(def alphabet-length 26)

;; Vector of chars, A-Z
(def letters (mapv (comp str char (partial + 65)) (range alphabet-length)))

(defn random-string
  "Returns a random string of specified length"
  [length]
  (apply str (take length (repeatedly #(rand-nth letters)))))

(defn random-string-list
  [list-length string-length]
  (doall (take list-length (repeatedly (partial random-string string-length)))))

(def orc-names (random-string-list 3000 7000))

(time (dorun (map clojure.string/lower-case orc-names)))
"Elapsed time: 62.146644 msecs"

(time (dorun (pmap clojure.string/lower-case orc-names)))
"Elapsed time: 30.498653 msecs"

; Add grain size sublist concept using partition-all
(def numbers [1 2 3 4 5 6 7 8 9 10])
(partition-all 3 numbers)
; => ((1 2 3) (4 5 6) (7 8 9) (10))
(pmap (fn [number-group] (doall (map inc number-group)))
      (partition-all 3 numbers))

;doall within the mapping function. This forces the lazy sequence returned by
; (map inc number-group) to be realized within the thread. Third, we need to
; ungroup the result. Here’s how we can do that
(apply concat
       (pmap (fn [number-group] (doall (map inc number-group)))
             (partition-all 3 numbers)))
(time
  (dorun
    (apply concat
           (pmap (fn [name] (doall (map clojure.string/lower-case name)))
                 (partition-all 1000 orc-names)))))
;"Elapsed time: 24.383568 msecs"

(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel
  overhead worthwhile"
  [grain-size f & colls]
  (apply concat
         (apply pmap
                (fn [& pgroups] (doall (apply map f pgroups)))
                (map (partial partition-all grain-size) colls))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Exercises;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
;1. Create an atom with the initial value 0, use swap! to increment it a couple
; of times, and then dereference it.
(def an-atom (atom 0))
(swap! an-atom inc)
(swap! an-atom inc)
@an-atom

;2. Create a function that uses futures to parallelize the task of downloading
; andom quotes fromhttp://www.braveclojure.com/random-quote using
; (slurp "http://www.braveclojure.com/random-quote"). The futures should update
; an atom that refers to a total word count for all quotes. The function will
; take the number of quotes to download as an argument and return the atom’s
; final value. Keep in mind that you’ll need to ensure that all futures have
; finished before returning the atom’s final value. Here’s how you would call
; it and an example result:
;(quote-word-count 5)
; => {"ochre" 8, "smoothie" 2}
(def random-quote-res "http://www.braveclojure.com/random-quote")

(defn fetch-rand-quote-fut
  []
  (future (slurp random-quote-res)))

(defn to-word-list
  [quote-str]
  (clojure.string/split quote-str #"[ ,]")
  )

(defn fetch-fut-list
  "Fetch list of specified futures"
  [limit]
  (println "fetch-fut-list limit" limit)
  (loop [cnt limit, futs []]
    (if (= cnt 0)
      futs
      (recur
        (dec cnt)
        (conj futs (fetch-rand-quote-fut))
        )))
  )

(defn generate-word-list
  [limit]
  (println "generate-word-list limit" limit)
  (reduce (fn [res it]
            (concat res (to-word-list (deref it)))) [] (fetch-fut-list limit))
  )

(defn quote-word-count
  "Exercise 2"
  [limit]
  (reduce (fn [res it]
            (if (contains? res it)
              (assoc res it (inc (get res it)))
              (assoc res it 1)
              )
            ) {} (generate-word-list limit))
  )

;;; Super awesome solution
(defn get-raw-quote []
  (slurp "http://www.braveclojure.com/random-quote"))

(defn parse-quote [raw-quote]
  (-> raw-quote
      (clojure.string/split #"--")
      first                                                 ; discard author
      clojure.string/trim))

(defn get-quote []
  (-> (get-raw-quote) parse-quote))

(defn get-words [quote]
  (-> quote
      clojure.string/lower-case
      (clojure.string/replace #"[^ A-Za-z]*" "")               ; removes punctuation from quote
      (clojure.string/split #" ")))

(defn histogramize [histogram words]
  (reduce
    (fn [histogram word]
      (update histogram word (fnil inc 0)))                 ; when val is nil, replace it with 0, then increment
    histogram
    words))


(defn quote-word-count-better [number-of-quotes]
  "Returns a histogram of the word counts in `number-of-quotes`
   quotes retrieved from http://www.braveclojure.com/random-quote"
  (let [word-count (atom {})]
    (doall
      (pmap
        (fn [_]
          (swap! word-count
                 (fn [histogram]
                   (histogramize histogram (-> (get-quote)
                                               get-words)))))
        (range number-of-quotes)))
    @word-count))

; Exercise 2:
; Create a function that uses futures to parallelize the task of
; downloading random qutes from  http://www.braveclojure.com/random-quote
; using (slurp "http://www.braveclojure.com/random-quote"). The futures
; should update an atom that refers to a total word count for all quotes.
; The function will take the number of quotes to download as an argument
; and return the atom's final value

(def rq-url "http://www.braveclojure.com/random-quote")

(defn word-count
  "Given a string, return a mapping of words to word count"
  [wctext]
  (->> (clojure.string/split wctext #"[ .;?!\-\n\r]")
       (filter #(not (empty? %)))
       (map #(clojure.string/lower-case %))
       (frequencies)))

(defmacro n-futures
  "Create n-futures that all execute the same task, then block and wait
  for each future to finish"
  [n body]
  `(let [promises# (take ~n (repeatedly promise))]
     (doseq [p# promises#]
       (do ~body (deliver p# true)))
     (every? ~deref promises#)))

(defn random-quote-word-count
  "Given a number of random-quotes to download, return a map of words
  to word count for the downloaded quotes"
  [num-quotes]
  (let [wc (atom {})]
    (do
      (n-futures num-quotes
                 (swap! wc (fn [current-wc]
                             (merge-with + current-wc (word-count (slurp rq-url))))))
      @wc)))

; Exercise 3:
; Create a representation of two characters in a game. The first character has
; 15 health points out of a total of 40.  The second character has a healing
; potion in his inventory.  Use refs and transactions to model the consumption
; of the healing potion and the first character healing.

(defn str-character-state
  "Given a character map, return a string of its state"
  [character]
  (clojure.string/join " " (map #(str % " => " (get character %)) (keys character))))

(defn ex3
  "Model the consumption of character 2's health potion by character 1"
  []
  (let [guy1 (ref {:hp 15 :max-hp 40 :health-potions 0})
        guy2 (ref {:hp 40 :max-hp 40 :health-potions 1})]
    (do
      (println (str "Character 1: " (str-character-state @guy1)))
      (println (str "Character 2: " (str-character-state @guy2)))
      (println "Drinking potion....")
      (dosync
        (alter guy2 update-in [:health-potions] dec)
        (alter guy1 update-in [:hp] + (- (:max-hp @guy1) (:hp @guy1))))
      (println (str "Character 1: " (str-character-state @guy1)))
      (println (str "Character 2: " (str-character-state @guy2))))))

)