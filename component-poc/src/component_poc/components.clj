(ns component-poc.components
  (:require [com.stuartsierra.component :as component]
            [clojure.core.async :as async]))

(defn connect-to-database [host port]
  {:conn {:host host :port port}})

(defrecord DB [host port connection]
  ;; Implement the Lifecycle protocol
  component/Lifecycle

  (start [component]
    (println ";; Starting database")
    ;; In the 'start' method, initialize this component
    ;; and start it running. For example, connect to a
    ;; database, create thread pools, or initialize shared
    ;; state.
    (let [conn (connect-to-database host port)]
      ;; Return an updated version of the component with
      ;; the run-time state assoc'd in.
      (assoc component :connection conn)))

  (stop [component]
    (println ";; Stopping database")
    ;; In the 'stop' method, shut down the running
    ;; component and release any external resources it has
    ;; acquired.
    ;; In the 'stop' method, shut down the running
    ;; component and release any external resources it has
    ;; acquired.
    ;(.close connection)
    ;; Return the component, optionally modified. Remember that if you
    ;; dissoc one of a record's base fields, you get a plain map.
    (assoc component :connection nil)))

(defrecord Email [end-point api-key]
  component/Lifecycle

  (start [this]
    (println ";; Starting email")
    (assoc this :to-str (str end-point api-key)))

  (stop [this]
    (println ";; Stopping email")
    this))

(defrecord Customers [db email]
  component/Lifecycle

  (start [this]
    (println ";; Starting Customers")
    (assoc this :to-str (str db email)))

  (stop [this]
    (println ";; Stopping Customers")
    this))


(defrecord ExampleComponent [options cache database scheduler]
  component/Lifecycle

  (start [this]
    (println ";; Starting ExampleComponent")
    ;; In the 'start' method, a component may assume that its
    ;; dependencies are available and have already been started.
    (assoc this :admin (get-user database "admin")))

  (stop [this]
    (println ";; Stopping ExampleComponent")
    ;; Likewise, in the 'stop' method, a component may assume that its
    ;; dependencies will not be stopped until AFTER it is stopped.
    this))

;;;;;; APIs ;;;;;;;

(defn example-component [config-options]
  (map->ExampleComponent {:options config-options
                          :cache (atom {})}))

(defn scheduler []
  (do (println "Starting scheduler") "scheduler"))

(defprotocol Send
  (send-email [email-service address body]))

(defrecord StubEmail [channel]
  Send
  (send-email [_ address body]
    (async/>!! channel {:address address :body body})))

(defn stub-email []
  (->StubEmail (async/chan 32)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-user [database username]
  (str (:connection database)
                 "SELECT * FROM users WHERE username = ?"
                 username))

(defn add-user [database username favorite-color]
  (str (:connection database)
                  "INSERT INTO users (username, favorite_color)"
                  username favorite-color))

(defn get-email [database username]
  "test@test.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defn send [email address body]
;  (str email address body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn notify [customers name message]
  (let [{:keys [db email]} customers
        address (get-email db name)]
    (send-email email address message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn example-system [config-options]
  (let [{:keys [host port]} config-options]
    (component/system-map
      :db (map->DB {:host host :port port})
      :scheduler (scheduler)
      :app (component/using
             (example-component config-options)
             {:database :db
              :scheduler :scheduler}))))

(defn create-system [config-options]
  (let [{:keys [host port end-point api-key]} config-options]
    (component/system-map
      :db (map->DB {:host host :port port})
      :email (map->Email {:end-point end-point :api-key api-key})
      :customers (component/using
                   (map->Customers {})
                   [:db :email]))))

;The system map provides its own implementation of the Lifecycle protocol
; which uses this dependency information (stored as metadata on each component)
; to start the components in the correct order.

; In Production
;(defn main [] (component/start (example-system {:host "localhost" :port 3306})))

; In Testing
;(defn test-system []
;  (let [system (system {:host "localhost" :port 3306 :end-point "email://abc" :api-key "abcXYZ"})
;        test-system (assoc system :email (stub-email))])
;  test-system)
;(alter-var-root #'system component/start)


(defn test-notify-customers []
  (let [system (create-system {:host "localhost" :port 3306 :end-point "email://abc" :api-key "abcXYZ"})
        test-system (assoc system :email (stub-email))
        sys* (component/start test-system)
        {:keys [customers email]} sys*
        {:keys [channel]} email]
    (try
      (notify customers "bob" "Hi, Bob!")
      (println (async/<!! channel))
      (finally (component/stop sys*)))))

; Associative Injection -- before starting it
(comment
(defn test-system [...]
  (assoc (system ...)
    :email (stub-email)
    :db (test-db)))
)
