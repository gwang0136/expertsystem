/**
* @author Gene Wang
* @version 19 October 2018
*
* Defines rules and functions that allows user to play the 20 questions think of an animal game.
* This program uses a backwards-chaining implementation that utilizes pattern matching to determine guesses
* for the user's animal given the known facts in the system. The system will generate backwards chaining rules
* for each trait using the generateRule function, which utilizes the ask and askYN functions
* to ask the user questions regarding the traits of their animal. The truncate and validatedAsk functions are then
* used to validate the user's answers to the questions asked by the system, and will repeatedly prompt the user until
* they enter a response that corresponds to either yes, no, or don't know. Once a validated answer is received, an assertion
* will be made with the data received from the user. As the facts accumulate in the knowledge base, rules will begin to fire
* making guesses at the user's animal. If a correct guess is made in under the maximum allowed questions, then the game will end
* and the system will declare victory. If the question limit is exceeded or if all the rules are fired without a correct guess being
* made, then the system will declare failure and the game will end. The go function must be called to start the game, and will clear
* and reset the system upon the conclusion of the game to prepare for a new game.
*/


/****************************************************************************
* The system is cleared and reset each time the game is loaded in.          *
* The user is asked to call the go function to initiate the game.           *
* The startup rule is then fired, signalling the beginning of the game and  *
* providing the user with a brief description of the game and its function. *
*****************************************************************************
*/
(clear)
(reset)

(printout t "Enter (go) to start the game!" crlf)           ; tells the user how to start the game.

(defrule startup "Starts the Game, printing a startup message and defining the guidelines for the game"

   (declare (salience 100))                                 ; this rule should fire immediately as the game begins

   =>

   (printout t "Welcome to the 20 Questions game. Please think of an animal." crlf)
   (printout t "I will ask you questions to try and guess your animal." crlf)
   (printout t "Please respond to the questions with either 'y', 'n', or '?' if you are unsure." crlf)

) ; (defrule startup)

(defglobal ?*MAX_QUESTIONS* = 20)                           ; the maximum number of questions that the system will ask before declaring failure
(defglobal ?*numAsked* = 0)                                 ; the current number of questions asked (incrememted after every question)

/*********************************************************
* This section contains all the backwards-chaining calls *
* for all the traits that the system will ask about.     *
* They are grouped in terms of association (i.e. all the *
* traits that describe modes of transportation are       *
* grouped together.                                      *
**********************************************************
*/

(do-backward-chaining mammal)
(do-backward-chaining reptile)
(do-backward-chaining amphibian)
(do-backward-chaining fish)
(do-backward-chaining bird)

(do-backward-chaining cephalopod)
(do-backward-chaining rodent)

(do-backward-chaining fly)
(do-backward-chaining walk)
(do-backward-chaining swim)
(do-backward-chaining jump)
(do-backward-chaining slither)

(do-backward-chaining zeroLegs)
(do-backward-chaining twoLegs)
(do-backward-chaining fourLegs)
(do-backward-chaining eightLegs)

(do-backward-chaining water)
(do-backward-chaining urban)
(do-backward-chaining jungle)
(do-backward-chaining china)

(do-backward-chaining domesticated)
(do-backward-chaining predator)

(do-backward-chaining biggerHuman)
(do-backward-chaining smallerHand)

(do-backward-chaining striped)
(do-backward-chaining blackWhite)
(do-backward-chaining orange)
(do-backward-chaining gold)
(do-backward-chaining grey)

(do-backward-chaining tusks)
(do-backward-chaining longTongue)
(do-backward-chaining blowhole)
(do-backward-chaining quills)
(do-backward-chaining bill)

(do-backward-chaining intelligent)

(do-backward-chaining growl)
(do-backward-chaining bark)
(do-backward-chaining meow)
(do-backward-chaining rattle)
(do-backward-chaining quack)

(do-backward-chaining bananas)
(do-backward-chaining bamboo)
(do-backward-chaining nuts)
(do-backward-chaining eggs)

/*************************************************************
* This section contains function definitions for all the     * 
* functions that are used in prompting the user for          *
* responses regarding certain traits. In addition, the       *
* generateRule function is able to create backwards-chaining * 
* rules for each trait.                                      *
**************************************************************
*/

/**
* This function accepts a string prompt and prints the prompt for the user,
* appending the given suffix to the prompt and accepts a user input value.
* It then returns the user inputted value.
* @precondition the inputed prompt is a string
* @param ?prompt the string prompt that is to be transformed into a question
* @param ?suffix the given suffix that is to be appended to the end of the question
* @return the user's response to the question
*/
(deffunction ask (?prompt ?suffix)

   (printout t (str-cat ?prompt ?suffix))

   (bind ?response (read))

   (return ?response)
   
) ; (deffunction ask (?prompt))

/**
* This function accepts a string prompt and asks the user to respond to
* the prompt with either "Y", "N", or "?". It accomplishes this task by
* calling upon the ask function with a suffix value of "? Y/N/?: ".
* @param ?prompt the string prompt that is to be asked as a yes/no question.
* @return the response of the user to the prompt with the appended suffix
*/
(deffunction askYN (?prompt)

   (return (ask ?prompt "? Y/N/?: "))

)

/**
* This function accepts a string and takes the first letter of the string,
* checking to see if it corresponds with either "y", "n", or "?", the three values
* that are accepted by the program as valid responses to each question. If the first letter
* is one of these three values, it then returns that letter, else, it will return a string "invalid".
* @param ?response the string that is to be truncated and then checked.
* @return the first letter of ?response if this letter is either "y", "n", or "?", otherwise, 
*         "invalid"
*/
(deffunction truncate (?response)
   
   (bind ?first (sub-string 1 1 ?response))

   (if (or (= ?first "y") (= ?first "n") (= ?first "?")) then
      (bind ?value ?first)

      else
      (bind ?value "invalid")
   )

   (return ?value)

)

/**
* This function creates the prompt asking the user if the animal is
* a certain trait by appending "Is your animal " to the trait in question.
* @param the trait that is in question.
* @return the trait with "Is your animal " appended to the beginning
*/
(deffunction isAsk (?trait)

   (return (str-cat "Is your animal " ?trait))

)

/**
* This function creates the prompt asking the user if the animal does
* a certain trait by appending "Does your animal " to the trait in question.
* @param the trait that is in question.
* @return the trait with "Does your animal " appended to the beginning
*/
(deffunction doesAsk (?trait)

   (return (str-cat "Does your animal " ?trait))

)

/**
* This function creates the prompt asking the user if the animal mainly does
* a certain trait by appending "Does your animal mainly " to the trait in question.
* @param the trait that is in question.
* @return the trait with "Does your animal mainly " appended to the beginning
*/
(deffunction mainAsk (?trait)

   (return (str-cat "Does your animal mainly " ?trait))

)

/**
* This function creates the prompt asking the user if the animal 
* passed as a parameter is their correct animal.
* @param the animal that is the system's guess
* @return "Is your animal a " + the animal's name
*/
(deffunction animalGuess (?animal)

   (return (str-cat "Is your animal a " ?animal))

)

/**
* This function first increments the ?*numAsked* variable to check if the system will exceed ?*MAX_QUESTIONS* 
* asked with this question. If it does exceed the maximum number of questions alotted, the system will assert failure.
* If the question can be asked, then the system will print the question number and call upon the askYN function to 
* prompt the user to respond to the yes/no question. The function will then call upon the truncate function to check 
* if the user's response is a response corresponding to yes/no/?. If not, the system will remind the user to respond
* with one of these responses, and will continually ask the user the same yes/no question until a proper response is 
* received. Once a correct response is received, it is returned.
* @param ?prompt the string prompt that is to be asked as a yes/no question.
* @return a valid response of the form yes/no/?, or if the question limit is exceeded, an empty string.
*/
(deffunction validatedAsk (?prompt)

   (bind ?isValid FALSE)
   (bind ?response "")

   (++ ?*numAsked*)

   (if (> ?*numAsked* ?*MAX_QUESTIONS*) then

      (bind ?isValid TRUE)                    ; prevents the question from being asked once the question limit is exceeded.

      (assert (failure))

   )

   (while (not ?isValid)

      (printout t  ?*numAsked* ": " )

      (bind ?response (truncate (askYN ?prompt)))

      (if (= ?response "invalid") then
         (printout t "Please input a response corresponding with 'y', 'n', or '?'." crlf)

      else
         (bind ?isValid TRUE)
      )

   )

   (return ?response)

) ; (deffunction validatedAsk (?prompt))

/**
* Calls upon the validatedAsk function to prompt the user if the animal passed
* in the prompt is the correct animal. It then will assert a fact containing this information.
* For example, if the user's animal is a zebra, then the following assertion will be made.
*    (animal zebra y)
* @param ?animal the name of the animal in question
* @param ?prompt the prompt containing the animal guess produced by the system 
* @return null
*/
(deffunction askAnimal (?animal ?prompt)

   (assert (animal ?animal (validatedAsk ?prompt)))

   (return)

)

/**
* This function generates a backwards-chaining rule for the given trait. For example, if the rule is 
* for the trait "mammal", it would look something like this (indentation and line breaks used for clarity):
*
* (defrule backward-mammal 
*    (need-mammal ?)
*    =>
*    (bind ?answer (validatedAsk ?prompt))  
*    (assert (mammal ?answer))
* )
* 
* The function will then build the rule. When fired, the rule will prompt the user with the given prompt, then 
* it will assert the trait with the response given by the user. For example, if the rule above is fired and the 
* user responds with "yes", then the system will make the following assertion:
*    (mammal "y')
* @param ?trait the trait that the function is building a rule for
* @param ?prompt the prompt that will be used by the validatedAsk function to ask the user about the given trait
* @return null
*/
(deffunction generateRule (?trait ?prompt)

   (bind ?genRule (str-cat "(defrule backward-" ?trait " (need-" ?trait " ?) => (bind ?answer (validatedAsk \"" ?prompt "\" )) (assert ("?trait " ?answer)) "))

   (bind ?genRule (str-cat ?genRule ")"))

   (build ?genRule)

   (return)

)

/*********************************************************
* This section calls the generateRule function           *
* to generate all the backward-chaining rules for each   *
* trait. These function calls also call upon the isAsk,  *
* mainAsk, and doesAsk functions to produce correct      *
* prompts for when each backwards-chaining rule prompts  *
* the user about each trait.                             *
**********************************************************
*/

(generateRule mammal (isAsk "a mammal"))
(generateRule reptile (isAsk "a reptile"))
(generateRule amphibian (isAsk "an amphibian"))
(generateRule fish (isAsk "a fish"))
(generateRule bird (isAsk "a bird"))

(generateRule cephalopod (isAsk "a cephalopod"))
(generateRule rodent (isAsk "a rodent"))

(generateRule fly (mainAsk "fly"))
(generateRule walk (mainAsk "walk"))
(generateRule swim (mainAsk "swim"))
(generateRule jump (mainAsk "jump"))
(generateRule slither (mainAsk "slither"))

(generateRule zeroLegs (doesAsk "have no legs"))
(generateRule twoLegs (doesAsk "have two legs"))
(generateRule fourLegs (doesAsk "have four legs"))
(generateRule eightLegs (doesAsk "have eight legs"))

(generateRule water (mainAsk "live in water"))
(generateRule urban (mainAsk "reside in urban areas"))
(generateRule jungle (mainAsk "live in jungle areas"))
(generateRule china (mainAsk "known to live in China"))

(generateRule domesticated (isAsk "domesticated"))
(generateRule predator (isAsk "a predator"))

(generateRule biggerHuman (isAsk "bigger than a human"))
(generateRule smallerHand (isAsk "smaller than a human hand"))

(generateRule striped (isAsk "striped"))
(generateRule blackWhite (isAsk "black and white in color"))
(generateRule orange (isAsk "orange in color"))
(generateRule gold (isAsk "gold in color"))
(generateRule grey (isAsk "grey in color"))

(generateRule tusks (doesAsk "have tusks"))
(generateRule longTongue (doesAsk "have an abnormaly long tongue"))
(generateRule blowhole (doesAsk "have a blowhole"))
(generateRule quills (doesAsk "have quills"))
(generateRule bill (doesAsk "have a bill"))

(generateRule intelligent (isAsk "exceptionally intelligent"))

(generateRule growl (doesAsk "growl"))
(generateRule bark (doesAsk "bark"))
(generateRule meow (doesAsk "meow"))
(generateRule rattle (doesAsk "rattle"))
(generateRule quack (doesAsk "quack"))

(generateRule bananas (isAsk "known for eating bananas"))
(generateRule bamboo (isAsk "known for eating bamboo"))
(generateRule nuts (isAsk "known for storing nuts"))
(generateRule eggs (isAsk "known for laying eggs that are popularly consumed by humans"))

/**********************************************************
* This section contains rules that assert facts that      *
* must also be true based on the current facts            *
* within the knowledge base. For example, if an animal    *
* mainly flies, then it will not mainly swim walk or jump,*
* so these facts are asserted to be "n", or not true.     *
***********************************************************
*/

(defrule flies "asserts corresponding facts if the animal flies"

   (fly "y")

   =>

   (assert (walk "n"))
   (assert (swim "n"))
   (assert (jump "n"))
   (assert (zeroLegs "n"))
   (assert (water "n"))
   (assert (biggerHuman "n"))
   (assert (fish "n"))

)


(defrule walks "asserts corresponding facts if the animal walks"

   (walk "y")

   =>

   (assert (fly "n"))
   (assert (swim "n"))
   (assert (jump "n"))
   (assert (zeroLegs "n"))
   (assert (fish "n"))

)

(defrule swims "asserts corresponding facts if the animal swims"

   (swim "y")

   =>

   (assert (fly "n"))
   (assert (walk "n"))
   (assert (jump "n"))
   (assert (water "y"))

)

(defrule jumps "asserts corresponding facts if the animal jumps"

   (jump "y")

   =>

   (assert (fly "n"))
   (assert (swim "n"))
   (assert (walk "n"))
   (assert (zeroLegs "n"))
   (assert (water "n"))
   (assert (fish "n"))
)

(defrule zeroLegs "asserts corresponding facts if the animal has no legs"

   (zeroLegs "y")

   =>

   (assert (walk "n"))
   (assert (jump "n"))
   (assert (twoLegs "n"))
   (assert (fourLegs "n"))
   (assert (eightLegs "n"))

)

(defrule twoLegs "if the animal has two legs, asserts corresponding facts that must be true"

   (twoLegs "y")

   =>

   (assert (zeroLegs "n"))
   (assert (fourLegs "n"))
   (assert (eightLegs "n"))

)

(defrule fourLegs "asserts corresponding facts if the animal has four legs"

   (fourLegs "y")

   =>

   (assert (zeroLegs "n"))
   (assert (twoLegs "n"))
   (assert (eightLegs "n"))

)

(defrule eightLegs "asserts corresponding facts if the animal has eight legs"

   (eightLegs "y")

   =>

   (assert (zeroLegs "n"))
   (assert (twoLegs "n"))
   (assert (fourLegs "n"))

)

(defrule isMammal "asserts corresponding facts if the animal is a mammal"

   (mammal "y")

   =>

   (assert (reptile "n"))
   (assert (fish "n"))
   (assert (cephalopod "n"))
   (assert (amphibian "n"))
   (assert (bird "n"))

)

(defrule isReptile "asserts corresponding facts if the animal is a reptile"

   (reptile "y")

   =>

   (assert (mammal "n"))
   (assert (fish "n"))
   (assert (cephalopod "n"))
   (assert (amphibian "n"))

)

(defrule isAmphibian "asserts corresponding facts if the animal is an amphibian"

   (amphibian "y")

   =>

   (assert (mammal "n"))
   (assert (fish "n"))
   (assert (cephalopod "n"))
   (assert (bird "n"))
   (assert (reptile "n"))

)

(defrule isFish "asserts corresponding facts if the animal is a fish"

   (fish "y")

   =>

   (assert (mammal "n"))
   (assert (reptile "n"))
   (assert (amphibian "n"))
   (assert (cephalopod "n"))
   (assert (bird "n"))
   (assert (swim "y"))
   (assert (walk "n"))
   (assert (water "y"))
   
)

(defrule isBird "asserts corresponding facts if the animal is a bird"

   (bird "y")

   =>

   (assert (mammal "n"))
   (assert (reptile "n"))
   (assert (fish "n"))
   (assert (cephalopod "n"))
   (assert (cephalopod "n"))

)

(defrule isCephalopod "asserts corresponding facts if the animal is a cephalopod"

   (cephalopod "y")

   =>

   (assert (mammal "n"))
   (assert (reptile "n"))
   (assert (fish "n"))
   (assert (swim "y"))
   (assert (fly "n"))
   (assert (walk "n"))
   (assert (jump "n"))
   (assert (water "y"))
   (assert (zeroLegs "y"))

)

(defrule liveInWater "asserts corresponding facts if the animal primarily lives in water"

   (water "y")

   =>

   (assert (fly "n"))
   (assert (jump "n"))
   (assert (swim "y"))

)

(defrule notInWater "asserts corresponding facts if the animal does not primarily live in water"

   (water "n")

   =>

   (assert (swim "n"))
   (assert (fish "n"))
   (assert (cephalopod "n"))

)

/***********************************************************
* This section contains all the rules that guess animals   *
* based on the current facts within the knowledge base     *
* using pattern matching that allows for uncertainty in    *
* the user's answer. For example, a response from the      *
* user can be "?" and it will not prevent a rule from      *
* being fired. There also are some repetitive uses of the  *
* mammal trait to ensure that the mammal question is       *
* always the first question asked, as the majority of the  *
* animals in the system are mammals. If a certain animal's *
* rule is fired, the askAnimal and animalGuess functions   *
* are called to ask the user if the animal determined by   *
* the system is indeed the correct animal.                 *
************************************************************
*/

(defrule isZebra "determines if the animal is a Zebra"

   (mammal ?zebra1 &~"n")
   (fourLegs ?zebra2 &~"n")
   (walk ?zebra3 &~"n")
   (domesticated ?zebra4 &~"y")
   (biggerHuman ?zebra5 &~"n")
   (striped ?zebra8 &~"n")
   (blackwhite ?zebra9 &~"n")

   =>

   (askAnimal "zebra" (animalGuess "zebra"))

)

(defrule isDog "determines if the animal is a Dog (that one would keep as a pet)"

   (mammal ?dog1 &~"n")
   (fourLegs ?dog2 &~"n")
   (walk ?dog3 &~ "n")
   (domesticated ?dog4 &~"n")
   (bark ?dog5 &~"n")

   =>

   (askAnimal "dog" (animalGuess "dog"))

)

(defrule isCat "determines if the animal is a Cat (that one would keep as a pet)"

   (mammal ?cat1 &~"n")
   (fourLegs ?cat2 &~"n")
   (walk ?cat3 &~ "n")
   (domesticated ?cat4 &~"n")
   (meow ?cat5 &~"n")

   =>

   (askAnimal "cat" (animalGuess "cat"))

)

(defrule isElephant "determines if the animal is an Elephant"

   (mammal ?elephant1 &~"n")
   (fourLegs ?elephant2 &~"n")
   (walk ?elephant3 &~"n")
   (domesticated ?elephant4 &~"n")
   (biggerHuman ?elephant5 &~"n")
   (striped ?elephant8 &~"y")
   (tusks ?elephant9 &~"n")
   (grey ?elephant10 &~"n")

   =>
   
   (askAnimal "elephant" (animalGuess "elephant"))

)

(defrule isBear "determines if the animal is a Bear"

   (mammal ?bear1 &~"n")
   (fourLegs ?bear2 &~"n")
   (walk ?bear3 &~"n")
   (predator ?bear4 &~"n")
   (biggerHuman ?bear5 &~"n")
   (furry ?bear6 &~"n")
   (growl ?bear8 &~"n")

   =>

   (askAnimal "bear" (animalGuess "bear"))

)

(defrule isTiger "determines if the animal is a Tiger"

   (mammal ?tiger1 &~"n")
   (fourLegs ?tiger2 &~"n")
   (walk ?tiger3 &~"n")
   (predator ?tiger4 &~"n")
   (biggerHuman ?tiger5 &~"n")
   (striped ?tiger8 &~"n")
   (predator ?tiger9 &~"n")
   (orange ?tiger10 &~"n")

   =>

   (askAnimal "tiger" (animalGuess "tiger"))

)

(defrule isFrog "determines if the animal is a Frog"

   (mammal ?frog1 &~"y")
   (amphibian ?frogt2 &~"n")
   (fourLegs ?frog3 &~"n")
   (jump ?frog4 &~"n")
   (water ?frog7 &~"y")
   (longTongue ?frog9 &~"n")

   =>

   (askAnimal "frog" (animalGuess "frog"))

)

(defrule isShark "determines if the animal is a Shark"

   (mammal ?shark8 &~"y")
   (fish ?shark1 &~"n")
   (zeroLegs ?shark2 &~"n")
   (swim ?shark3 &~"n")
   (predator ?shark4 &~"n")
   (biggerHuman ?shark5 &~"n")
   (water ?shark6 &~"n")
   (grey ?shark8 &~"n")
   (blowhole ?shark7 &~"y")

   =>

   (askAnimal "shark" (animalGuess "shark"))

)

(defrule isOctopus "determines if the animal is an Octopus"

   (mammal ?octopus6 &~"y")
   (cephalopod ?octopus1 &~"n")
   (eightLegs ?octopus2 &~"n")
   (swim ?octopus3 &~"n")
   (domesticated ?octopus4 &~"y")
   (water ?octopus5 &~"n")

   =>

   (askAnimal "octopus" (animalGuess "octopus"))

)

(defrule isDolphin "determines if the animal is a Dolphin"

   (mammal ?dolphin1 &~"n")
   (zeroLegs ?dolphin2 &~"n")
   (swim ?dolphin3 &~"n")
   (predator ?dolphin4 &~"y")
   (biggerHuman ?dolphin5 &~"n")
   (water ?dolphin6 &~"n")
   (blowhole ?dolphin7 &~"n")
   (intelligent ?dolphin8 &~"n")

   =>

   (askAnimal "dolphin" (animalGuess "dolphin"))

)

(defrule isPorcupine "determines if the animal is a porcupine"

   (mammal ?porcupine1 &~"n")
   (fourLegs ?porcupine2&~"n")
   (walk ?porcupine3 &~"n")
   (rodent ?porcupine7 &~"n")
   (quills ?porcupine8 &~"n")

   =>

   (askAnimal "porcupine" (animalGuess "porcupine"))

)

(defrule isHedgehog "determines if the animal is a hedgehog"

   (mammal ?hedgehog1 &~"n")
   (fourLegs ?hedgehog2 &~"n")
   (walk ?hedgehog3 &~"n")
   (rodent ?hedgehog7 &~"y")
   (quills ?hedgehog8 &~"n")

   =>

   (askAnimal "hedgehog" (animalGuess "hedgehog"))

)

(defrule isRattlesnake "determines if the animal is a rattlesnake"

   (mammal ?rattlesnake1 &~"y")
   (reptile ?rattlesnake2 &~"n")
   (zeroLegs ?rattlesnake3 &~"n")
   (slither ?rattlesnake4 &~"n")
   (rattle ?rattlesnake5 &~"n")

   =>

   (askAnimal "rattlesnake" (animalGuess "rattlesnake"))

)

(defrule isMonkey "determines if the animal is a monkey"

   (mammal ?monkey1 &~"n")
   (twoLegs ?monkey2 &~"n")
   (jungle ?monkey3 &~"n")
   (intelligent ?monkey4 &~"n")
   (bananas ?monkey5 &~"n")

   =>

   (askAnimal "monkey" (animalGuess "monkey"))

)

(defrule isGoldfish "determines if the animal is a goldfish"

   (mammal ?goldfish1 &~"y")
   (fish ?goldfish2 &~"n")
   (domesticated ?goldfish3 &~"n")
   (gold ?goldfish &~"n")
   
   =>

   (askAnimal "goldfish" (animalGuess "goldfish"))

)

(defrule isChicken "determines if the animal is a chicken"

   (mammal ?chicken1 &~"y")
   (bird ?chicken2 &~"n")
   (walk ?chicken3 &~"n")
   (eggs ?chicken4 &~"n")

   =>

   (askAnimal "chicken" (animalGuess "chicken"))

)

(defrule isDuck "determines if the animal is a duck"

   (mammal ?duck1 &~"y")
   (bird ?duck2 &~"n")
   (bill ?duck3 &~"n")
   (quack ?duck4 &~"n")

   =>

   (askAnimal "duck" (animalGuess "duck"))

)

(defrule isPanda "determines if the animal is a panda"

   (mammal ?panda1 &~"n")
   (biggerHuman ?panda2 &~"n")
   (blackWhite ?panda3 &~"n")
   (china ?panda4 &~"n")
   (bamboo ?panda5 &~"n")

   =>

   (askAnimal "panda" (animalGuess "panda"))

)

(defrule isSquirrel "determines if the animal is a squirrel"

   (mammal ?squirrel1 &~"n")
   (biggerHuman ?squirrel2 &~"y")
   (fourLegs ?squirrel3 &~"n")
   (rodent ?squirrel4 &~"n")
   (nuts ?squirrel5 &~"n")

   =>

   (askAnimal "squirrel" (animalGuess "squirrel"))

)

(defrule isPigeon "determines if the animal is a pigeon"

   (mammal ?pigeon1 &~"y")
   (bird ?pigeon2 &~"n")
   (grey ?pigeon3 &~"n")
   (urban ?pigeon4 &~"n")

   =>

   (askAnimal "pigeon" (animalGuess "pigeon"))

)


/*********************************************************
* This section contains all the rules that trigger once  *
* conditions signalling the end of the game are met.     *
* The system loses if the question limit is exceeded or  *
* if all the rules in the system are fired and no animal *
* is found. The system wins if it is able to correctly   *
* guess the user's animal within the question limit.     *
**********************************************************
*/

(defrule animalFound "Ends the game once an animal is found to be correct"

   (animal ?animal "y")

   =>

   (printout t "I WIN!" crlf)
   (halt)

)

(defrule failure "The user wins when the system exceeds 20 questions asked"

   (declare (salience 50))         ; if failure is asserted, this rule should fire immediately.

   (failure)

   =>

   (printout t "You win, I have run out of questions" crlf)
   (halt)

)

(defrule unknown "Tells the player that their animal is not in the current knowledge base"
   
   (declare (salience -1000))       ; this rule should only fire once all other rules have been fired.

   =>

   (printout t "Sorry, I do not currently have your animal in my knowledge base" crlf)
   (halt)

)

