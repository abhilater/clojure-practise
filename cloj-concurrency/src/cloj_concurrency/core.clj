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
