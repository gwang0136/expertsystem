(defrule rule-1 "description"
   (coordinate (x ?a) (y ?b))
 =>
   (printout t ?a " " ?b crlf)
)
(defrule enumLetters "enumerate pattern Letter ?"
   (Letter (p ?p1) (c ?L1)) (Letter (p ?p2) (c ?L2)) (Letter (p ?p3) (c ?L3)) (Letter (p ?p4) (c ?L4))
=>
   (printout t ?L1 ?L2 ?L3 ?L4 crlf)
)

(deftemplate Letter (slot p (type INTEGER)) (slot c))
(assert (Letter (p 1) (c E)))
(assert (Letter (p 2) (c R)))
(assert (Letter (p 3) (c I)))
(assert (Letter (p 4) (c C)))

(deffunction slice$ (?word)


(generateRule mammal is-mammal)
(generateRule reptile is-reptile)
(generateRule fish is-fish)

(generateRule fly main-fly)
(generateRule slither main-slither)
(generateRule walk main-walk)
(generateRule swim main-swim)
(generateRule jump main-jump)

(generateRule zeroLegs has-0-legs)
(generateRule twoLegs has-2-legs)
(generateRule fourLegs has-4-legs)
(generateRule eightLegs has-8-legs)
(generateRule tenLegs has-10-legs)


(generateRule water main-water)
(generateRule biggerHuman is-bigger-than-human)
(generateRule domesticated is-domesticated)
(generateRule africa is-African)

(generateRule striped is-striped)
(generateRule blackwhite is-black-and-white)
(generateRule orange is-orange)

(generateRule crustacean is-a-crustacean)
(generateRule rodent is-a-rodent)

(generateRule pinchers has-pinchers)
(generateRule tusks has-tusks)

(generateRule birdOfPrey is-bird-of-prey)
(generateRule growl does-growl)
(generateRule predator is-a-predator)

(generateRule smallerHand is-smaller-than-a-human-hand)
(generateRule longTongue has-a-long-tongue)
(generateRule blowhole has-a-blowhole)
(generateRule intelligent is-very-intelligent)

(generateRule quills has-quills)