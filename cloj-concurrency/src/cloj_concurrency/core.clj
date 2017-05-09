(ns cloj-concurrency.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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
  )
