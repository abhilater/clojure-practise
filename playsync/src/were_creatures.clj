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
(comment
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

; Exercise 4:
; Create a role-playing game that implements behavior using multiple
; dispatch
(defmulti attack (fn [x] (:class x)))
(defmethod attack :fighter
  [character]
  (str "Character " (:name character) " swings his " (:weapon character)))
(defmethod attack :mage
  [character]
  (str "Character " (:name character) " casts " (:spell character)))

(println (attack {:name "Tucker", :class :fighter, :weapon "sword"}))
(println (attack {:name "Morphumax", :class :mage, :spell "magic missile"}))

)


;; Artifact ecosystem isn’t an official programming term; I use it to refer to the suite
;; of tools, resources, and conventions used to identify and distribute artifacts.
;; Java’s ecosystem grew up around the Maven build tool, and because Clojure uses this
;; ecosystem
;;Maven specifies a pattern for identifying artifacts that Clojure projects adhere to,
;; and it also specifies how to host these artifacts in Maven repositories, which are
;; just servers that store artifacts for distribution.

;;  group ID, an artifact ID, and a version

;(defproject clojure-noob "0.1.0-SNAPSHOT"

; clojure-noob is both the group ID and the artifact ID of your project, and
; "0.1.0-SNAPSHOT" is its version

;; In general, versions are permanent; if you deploy an artifact with version 0.1.0
;; to a repository, you can’t make changes to the artifact and deploy it using the
;; same version number. If you want to indicate that the version is a work in progress
;; and you plan to keep updating it, you can append -SNAPSHOT to your version number

;; TO separate groupId from artifactId
;(defproject group-id/artifact-id "0.1.0-SNAPSHOT"

;:dependencies [[org.clojure/clojure "1.7.0"]
;               [clj-time "0.9.0"]]

;; Add Java library dependency to leiningen dependency list
;<dependency>
;<groupId>org.apache.commons</groupId>
;<artifactId>commons-email</artifactId>
;<version>1.3.3</version>
;</dependency>

;:dependencies [[org.clojure/clojure "1.7.0"]
;               [clj-time "0.9.0"]
;               [org.apache.commons/commons-email "1.3.3"]]

;The main Clojure repository is Clojars (https://clojars.org/), and the main Java
; repository is The Central Repository (http://search.maven.org/),

;To deploy your own projects to Clojars, all you have to do is create an account
; there and run lein deploy clojars
