(ns were-creatures
  (:require [clojure.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Multi-Methods;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (defmulti full-moon-behavior (fn [were-creature] (:were-type were-creature)))

(defmethod full-moon-behavior :wolf
  [were-creature]
  (str (:name were-creature) " will howl and murder"))

(defmethod full-moon-behavior :simmons
  [were-creature]
  (str (:name were-creature) " will encourage people and sweat to the oldies"))

(defmethod full-moon-behavior nil
  [were-creature]
  (str (:name were-creature) " will stay at home and eat ice cream"))

(defmethod full-moon-behavior :default
  [were-creature]
  (str (:name were-creature) " will stay up all night fantasy footballing"))

(full-moon-behavior {:were-type :wolf
                     :name      "Rachel from next door"})
(full-moon-behavior {:name      "Andy the baker"
                     :were-type :simmons})

(full-moon-behavior {:were-type nil
                     :name "Martin the nurse"})

(full-moon-behavior {:were-type :office-worker
                     :name "Jimmy from sales"})


(defmulti types (fn [x y] [(class x) (class y)]))
(defmethod types [java.lang.String java.lang.String]
  [x y]
  "Two strings!")

(types "String 1" "String 2")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Protocols;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (defprotocol Psychodynamics
  "Plumb the inner depths of your data types"
  (thoughts [x] "The data type's innermost thoughts")
  (feelings-about [x] [x y] "Feelings about self or other"))

(extend-type java.lang.String
  Psychodynamics
  (thoughts [x] (str x " thinks, 'Truly, the character defines the data type'"))
  (feelings-about
    ([x] (str x " is longing for a simpler way of life"))
    ([x y] (str x " is envious of " y "'s simpler way of life"))))

(extend-type java.lang.Object
  Psychodynamics
  (thoughts [x] "Maybe the Internet is just a vector for toxoplasmosis")
  (feelings-about
    ([x] "meh")
    ([x y] (str "meh about " y))))
;;or
;(extend-protocol Psychodynamics
;  java.lang.String
;  (thoughts [x] "Truly, the character defines the data type")
;  (feelings-about
;    ([x] "longing for a simpler way of life")
;    ([x y] (str "envious of " y "'s simpler way of life")))
;
;  java.lang.Object
;  (thoughts [x] "Maybe the Internet is just a vector for toxoplasmosis")
;  (feelings-about
;    ([x] "meh")
;    ([x y] (str "meh about " y))))

(thoughts "blorb")
; => "blorb thinks, 'Truly, the character defines the data type'"

(feelings-about "schmorb")
; => "schmorb is longing for a simpler way of life"

(feelings-about "schmorb" 2)
; => "schmorb is envious of 2's simpler way of life"

(thoughts 3)
; => "Maybe the Internet is just a vector for toxoplasmosis"

(feelings-about 3)
; => "meh"

(feelings-about 3 "blorb")
; => "meh about blorb")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Records;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment (defrecord WereWolf [name title])

(WereWolf. "David" "London Tourist")
; => #were_creatures.WereWolf{:name "David", :title "London Tourist"}

(->WereWolf "Jacob" "Lead Shirt Designer")
;=> #were_creatures.WereWolf{:name "Jacob", :title "Lead Shirt Designer"}

(map->WereWolf {:name "Lucian" :title "CEO of Melodrama"})
;=> #were_creatures.WereWolf{:name "Lucian", :title "CEO of Melodrama"}

(def jacob (->WereWolf "Jacob" "Lead Shirt Designer"))
(.name jacob)
(:name jacob)
(get jacob :name)

 (= jacob (->WereWolf "Jacob" "Lead Shirt Designer"))
; => true

 (= jacob (WereWolf. "David" "London Tourist"))
; => false

 (= jacob {:name "Jacob" :title "Lead Shirt Designer"})
; => false

(assoc jacob :title "Lead Third Wheel")
;=> #were_creatures.WereWolf{:name "Jacob", :title "Lead Third Wheel"}



;;; protocols extended with records
(defprotocol WereCreature
  (full-moon-behavior [x]))

(defrecord WereWolf [name title]
  WereCreature
  (full-moon-behavior [x]
    (str name " will howl and murder")))

(full-moon-behavior (map->WereWolf {:name "Lucian" :title "CEO of Melodrama"}))
;=> "Lucian will howl and murder"
         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Exercise;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Extend the full-moon-behavior multimethod to add behavior for your own kind
; of were-creature.

(comment (defmulti full-moon-behavior
          (fn [were-creature]
            (:were-type were-creature)))

(defmethod full-moon-behavior :wolf
  [were-creature]
  (str (:name were-creature) " will howl and murder"))

(defmethod full-moon-behavior :simmons
  [were-creature]
  (str (:name were-creature) " will encourage people and sweat to the oldies"))

(defmethod full-moon-behavior :bear
  [were-creature]
  (str (:name were-creature) " has the right to bear arms"))

  )

; Exercise 2:
; Create a WereSimmons record type, then extend the WereCreature
; protocol
(defprotocol WereCreature
  (full-moon-behavior [x]))

(defrecord WereWolf [name title]
  WereCreature
  (full-moon-behavior [x]
    (str name " will howl and murder")))

(defrecord WereSimmons [name title]
  WereCreature
  (full-moon-behavior [x]
    (str name " will encourage people and sweat to the oldies")))

(full-moon-behavior (WereSimmons. "Abhishek" "Just a good guy"))

; Exercise 3:
; Create your own protocol, and then extend it using extend-type
; and extend-protocol
(defprotocol JavaSequence
  (add [x el])
  (size [x]))

(extend-protocol JavaSequence
  java.util.ArrayList
  (add [x el] (.add x el))
  (size [x] (.size x))

  java.util.Stack
  (add [x el] (.push x el))
  (size [x] (.size x)))

(let [arrlist (java.util.ArrayList. )]
  (add arrlist 1)
  (add arrlist 2)
  {:coll arrlist :size (size arrlist)})

(let [stack (java.util.Stack. )]
  (add stack 1)
  (add stack 2)
  (add stack 3)
  {:coll stack :size (size stack)})

(defn test-protocol
  [java-seq]
  (dotimes [n 5] (add java-seq n))
  (println java-seq)
  (size java-seq))