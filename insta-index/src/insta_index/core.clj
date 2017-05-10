(ns insta-index.core
  (:require [clojure.string :as string])
  (:gen-class))

(def space-split-regex #" +")
(def to-lowercase-conversion #(.toLowerCase %))

;; Space tokenizer
(def space-tokenizer (create-tokenizer space-split-regex))

;; Lowercase normalizer
(def lowercase-normalizer (create-normalizer to-lowercase-conversion))

;; Tokenizer chain
(def tokenizer-chain (list space-tokenizer))

;; Normalizer chain
(def normalizer-chain (list lowercase-normalizer))

;;; Creates new tokenizer
(defn create-tokenizer
  "Returns a tokenizer based on specified split regex. eg space chars"
  [split-regex]
  (fn [text]
    (string/split text split-regex)))

;;; Creates new normalizer
(defn create-normalizer
  "Returns a normalizer based on specified conversion eg toLowerCase"
  [conversion]
  (fn [token]
    (conversion token)))

(defprotocol IdGenerator
  "Abstraction unique id generator, default impl in-memory"
  (next-id [_] "Get next id"))

;;; Generates unique auto increment ids in memory
(deftype InMemoryIdGenerator [uid]
  IdGenerator
  (next-id [_] (swap! uid inc)))

;;; Instance of InMemoryIdGenerator
(def in-mem-id-generator (InMemoryIdGenerator. (atom 0)))


(comment
  ;;; tests the id-generator
  (do
    (future (println (next-id in-mem-id-generator)))
    (future (println (next-id in-mem-id-generator)))
    (future (println (next-id in-mem-id-generator)))
    (future (println (next-id in-mem-id-generator))))

  )



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
