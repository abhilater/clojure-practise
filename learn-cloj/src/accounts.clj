(ns accounts)

(def *min-bal* 500)

(defstruct account
  :name
  :balance)

(defn make-accunt
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




