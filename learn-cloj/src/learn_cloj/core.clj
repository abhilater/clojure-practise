(ns learn-cloj.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def *min-bal* 500)

(defstruct account
  :name
  :balance)

(defn make-account
  "Create a new account"
  [name op-balance]
  ;; op-balance should be stored in a ref so that we can modify it in
  ;; a concurrent manner
  (struct account name (ref op-balance)))

(defn account-balance
  "Get the balance from an account"
  [acc]
  @(:balance acc))

(defn account-holder
  "Get the name of the account holder"
  [acc]
  (:name acc))

(defn deposit
  "Deposits amount into acc"
  [acc amount]
  (dosync
    (commute (:balance acc) + amount)))

(defn withdraw
  "Withdraw amount from account. Min balance is 500"
  [acc amount]
  (dosync
    (let [curr (:balance acc)]
      (if (> (- @curr amount) *min-bal*)
        (alter curr - amount)
        :insufficient-funds))))

(comment

  (def *acc (make-account "John Doe" 5000))
  (deposit *acc 500)
  (account-balance *acc)
  (withdraw *acc 20)
  (account-balance *acc)

  (binding [*min-bal* 0]
    (withdraw *acc 5000))
  )