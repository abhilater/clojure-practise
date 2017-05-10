(ns playsync.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Core.async;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;;; Create a process

;Channels communicate messages. You can put messages on a channel and take messages off a channel.
; Processes wait for the completion of put and take—these are the events that processes respond to.
; You can think of processes as having two rules: 1) when trying to put a message on a channel or
; take a message off of it, wait and do nothing until the put or take succeeds, and 2) when the put
; or take succeeds, continue executing.
(def echo-chan (chan))                                      ; Creates a channel named echo-chan
(go (println (<! echo-chan)))                                ; go block runs concurrently on a sep thread
(>!! echo-chan "ketchup")

;;; Buffered channel
(def echo-buffer (chan 2))
(>!! echo-buffer "ketchup")
; => true
(>!! echo-buffer "ketchup")
; => true
(>!! echo-buffer "ketchup")
; This blocks because the channel buffer is full

(def hi-chan (chan))
(doseq [n (range 1000)]
  (go (>! hi-chan (str "hi " n))))


;;; thread
(thread (println (<!! echo-chan)))
(>!! echo-chan "mustard")

(let [t (thread "chili")]
  (<!! t))
; => "chili"

;go, thread, chan, <!, <!!, >!, and >!! are the core tools you’ll use for creating and
; communicating with processes. Both put and take will cause a process to wait until its
; complement is performed on the given channel. go allows you to use the parking variants
; of put and take, which could improve performance. You should use the blocking variants,
; along with thread, if you’re performing long-running tasks before the put or take.

;;; Hot dog vending machine problem

(defn hot-dog-machine-v2
  [hot-dog-count]
  (let [in (chan)
        out (chan)]
    (go (loop [hc hot-dog-count]
          (if (> hc 0)
            (let [input (<! in)]
              (if (= 3 input)
                 (do (>! out "hot dog")
                     (recur (dec hc)))
                 (do (>! out "wilted lettuce")
                     (recur hc))))
             (do (close! in)
                 (close! out)))))
    [in out]))

(let [[in out] (hot-dog-machine-v2 2)]
  (>!! in "pocket lint")
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (<!! out))
; => wilted lettuce
; => hotdog
; => hotdog
; => nil

;;; Chain of processes simulation
(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (go (>! c2 (clojure.string/upper-case (<! c1))))
  (go (>! c3 (clojure.string/reverse (<! c2))))
  (go (println (<! c3)))
  (>!! c1 "redrum"))
; => MURDER

;alts!! lets you use the result of the first successful channel
; operation among a collection of operations.
(defn upload
  [headshot c]
  (go (Thread/sleep (rand 100))
      (>! c headshot)))

(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (upload "serious.jpg" c1)
  (upload "fun.jpg" c2)
  (upload "sassy.jpg" c3)
    (let [[headshot channel] (alts!! [c1 c2 c3])]
      (println "Sending headshot notification for" headshot)))

(let [c1 (chan)]
  (upload "serious.jpg" c1)
  (let [[headshot channel] (alts!! [c1 (timeout 20)])]
    (if headshot
      (println "Sending headshot notification for" headshot)
      (println "Timed out!"))))
; => Timed out!

(let [c1 (chan)
      c2 (chan)]
  (go (<! c2))
    (let [[value channel] (alts!! [c1 [c2 "put!"]])]
        (println value)
        (= channel c2)))
; => true
; => true

(defn append-to-file
  "Write string to the end of a file"
  [filename s]
  (spit filename s :append true))

(defn format-quote
  [quote]
  (str "=== BEGIN QUOTE ===\n" quote "=== END QUOTE ===\n\n"))

(defn random-quote
  "Retrieve a random quote and format it"
  []
  (format-quote (slurp "http://www.braveclojure.com/random-quote")))

(defn snag-quotes
  [filename num-quotes]
  (let [c (chan)]
    (go (while true (append-to-file filename (<! c))))
    (dotimes [n num-quotes] (go (>! c (random-quote))))))

;;; Process pipelines as pure functions
(defn upper-caser
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/upper-case (<! in)))))
    out))

(defn reverser
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/reverse (<! in)))))
    out))

(defn printer
  [in]
  (go (while true (println (<! in)))))

(def in-chan (chan))
(def upper-caser-out (upper-caser in-chan))
(def reverser-out (reverser upper-caser-out))
(printer reverser-out)

(>!! in-chan "redrum")
; => MURDER

(>!! in-chan "repaid")
; => DIAPER
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;JVM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1. The Java compiler reads source code.
;2. The compiler outputs bytecode, often to a JAR file.
;3. JVM executes the bytecode. (A running JVM executes bytecode by translating it on the fly into
;   machine code that its host will understand, a process called just-in-time compilation.)
;4. The JVM sends machine instructions to the CPU.

;; Leiningen, a Clojure build tool, has the concept of profiles. One thing profiles are useful for
;; is allowing you to have development tools available to a project without having them as dependencies
;; when you release your project. An example of when you might want to do this is when you are using a
;; testing library like expectations.