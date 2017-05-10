(ns insta-index.handler
  (:require [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [insta-index.routes :refer [routes]]))

(def app
  (-> routes
      (wrap-json-body)
      (wrap-json-response)
      (wrap-defaults api-defaults)))