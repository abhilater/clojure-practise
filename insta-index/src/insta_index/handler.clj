(ns insta-index.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [clojure.core]
            [insta-index.core :as service]))

(defn- str-to [num]
  (apply str (interpose ", " (range 1 (inc num)))))

(defn- str-from [num]
  (apply str (interpose ", " (reverse (range 1 (inc num))))))

(defroutes app
           (GET "/search/:query" [query] (service/test-server))
           (GET "/count-down/:from" [from] (str-from (Integer. from))))