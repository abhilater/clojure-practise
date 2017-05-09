(ns if-valid-macro.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def ^:dynamic order-details
  {:name  "Abhishek Gupta"
   :email "abhishekgmail.com"})

(def order-details-validations
  {
   :name  ["Please enter a name" not-empty]

   :email ["Please enter an email id" not-empty

           "Your email id is invalid" #(or (empty? %) (re-seq #"@" %))]
   })

(defn error-messages-for
  "Returns a seq of error message for a field"
  [to-validate message-validation-pair]
  (map
    first
    (filter
      #(not
         (
           (second %)
           to-validate)
         )
      (partition 2 message-validation-pair)
      )))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce
    (fn [errors validation]
      (let [[fieldname validation-check-groups] validation
            value (get to-validate fieldname)
            error-messages (error-messages-for value validation-check-groups)
            ]
        (if (empty? error-messages)
          errors
          (assoc errors fieldname error-messages))
        ))
    {}
    validations))

;;; 1.
(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

;;; 2.
(defmacro when-valid
  "Similar to if-valid except works like when"
  [to-validate validations & then-else]
  `(let [errors# (validate ~to-validate ~validations)]
     (when (empty? errors#)
       ~@then-else)))

(defmacro when-valid-1
  "Exercise 1: Like 'if-valid' but with 'when'"
  [to-validate validations & body]
  `(let [errors# (validate ~to-validate ~validations)]
     (if (empty? errors#)
       (do
         ~@body)))
  )

;;; 3.
(defmacro my-or
  "My implementation of or macro"
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (or ~@next)))))

;;; 4. defattrs
(def character
  {:name       "Smooches McCutes"
   :attributes {:intelligence 10
                :strength     4
                :dexterity    5}})

(defmacro defattrs
  "Takes an arbitrary number of attrb function inputs and defines them"
  ([] nil)
  ([fn-name attr]
   `(def ~fn-name (comp ~attr :attributes))
    )
  ([fn-name attr & rest]
   `(do
      (defattrs ~fn-name ~attr)
      (defattrs ~@rest))
    )
  )

;;; Tests

(comment


  ;;;1. Test when-valid with false
  (when-valid order-details order-details-validations
              (println "It's a success!")
              (inc 1))

  ;;;2. Test when-valid with true
  (binding [order-details {:name  "Abhishek Gupta"
                           :email "abhishek@gmail.com"}]
    (when-valid order-details order-details-validations
                (println "It's a success!")
                (inc 1)))

  ;;;3. Test my-or
  (my-or (= 0 1) (println "Hey") "Ho")

  ;;;4. Test defattrs
  (macroexpand `(defattrs c-int :intelligence
                          c-str :strength
                          c-dex :dexterity))
  (defattrs c-int :intelligence
            c-str :strength
            c-dex :dexterity)
  (c-str character)

  )

