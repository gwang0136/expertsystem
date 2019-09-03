/**
* @author Gene Wang
* @version 13 September 2018
* This module defines the functions necessary to generate all the unique anagrams of a user-inputted 4 letter word.
* Calling the runAnagram function will prompt the user to input a word as a string through the askAnagram fucntion. 
* The user inputted value will be validated by the validate function, checking to make sure that the value is both a string 
* and is less than or equal to ?*MAX_LENGTH* (predefined constant) letters long. The runAnagram function then takes the length of the 
* validated word and passes that to the genRule function, which constructs a string that contains the code to define the correct rule to 
* find anagrams of the given number of letters (the length of the word). The runAnagram function take the returned string contianing the 
* rule and builds it. The runAnagram function then passes the validated string to the slice$ function, which 
* splits the word into a list with each index containing a single letter in order. The sliced string in a list is
* then passed to the assertLetters function, which asserts each letter in the string with a unique corresponding
* position integer value. If the string is not validated, the user is prompted again and reminded to input a string 
* that is less than or equal to ?*MAX_LENGTH*. Using the go function will call the runAnagram function to acquire the word and create all 
* the assertions and correct rule, running the generated enumLetters rule which prints out all the anagrams of the given word when fired, 
* then resets to remove all the assertions to prepare for a clean next run.
*/

(reset)                                                     ; resets and clears the knowledge base 
(clear)

(defglobal ?*MAX_LENGTH* = 9)                               ; defines the maxiumum number of letters that the computer 
                                                            ; is able to find anagrams for

(deftemplate Letter (slot p (type INTEGER)) (slot c))       ; defines the template for an assertion for a unique letter with 
                                                            ; an integer position slot p and character slot 

/**
* The slice function takes in a string value ?word, creates a list and separates the characters
* in the word into unique indicies in order in the list. The list is then returned.
* @param ?word the word that is to be split into characters
* @return a list with the characters of the word ?word
*/
(deffunction slice$ (?word)

   (bind ?charlist (create$))

   (for (bind ?i 1) (<= ?i (str-length ?word)) (++ ?i)
      (bind ?charlist (insert$ ?charlist ?i (sub-string ?i ?i ?word)))
   )

   (return ?charlist)

) ; (deffunction slice$ (?word))


/**
* The validate function takes in a string value ?word, returning true if the word is both 
* a string and less than or equal to ?*MAX_LENGTH* letters long, returning false otherwise.
* @param ?word the word that is to be validated
* @return true if the word is both a string and <= ?*MAX_LENGTH* letters long, otherwise
*         false
*/
(deffunction validate (?word)

   (return (<= (str-length ?word) ?*MAX_LENGTH*))             

) ; (deffunction validate (?word))

/**
* This function calls upon the askColon function and prompts the user to input a word.
* The user inputted value is then passed on to the validate function, and if the value is validated,
* the value is returned. If the value did not meet the requirements, the user is repeatedly prompted
* to input a string that is less than or equal to ?*MAX_LENGTH* letters.
* @return a validated user input value
*/
(deffunction askAnagram ()

   (bind ?isValid FALSE)

   (while (not ?isValid)

      (bind ?value (askColon "Enter a word"))

      (if (not (validate ?value)) then

         (bind ?reprompt (str-cat "Please ensure that your word is less than or equal to " ?*MAX_LENGTH* " letters long"))
         (printout t ?reprompt crlf)

      else
         (bind ?isValid TRUE)
      )

   ) ; (while (not ?isValid))

   (return ?value)

) ; (deffunction askAnagram())

/**
* This function takes in a list of characters and goes through the values, 
* asserting each individually with the Letter template, using unique position
* values along with each letter. 
*/
(deffunction assertLetters (?listvals)

   (bind ?posindex 0)

   (foreach ?letter ?listvals
      (assert (Letter (p (++ ?posindex)) (c ?letter)))
   )

   (return)

) ; (deffunction assertLetters (?listvals))

/**
* This function calls upon the askAnagram function to acquire a validated user input value. 
* The length validated value is then passed to the genRule function, where the corresponding 
* enumLetters rule will be generated. The word is then passed to the slice$ function to acquire 
* a list of the characters in the word. The list is then passed to the assertLetters function, 
* where each letter will be asserted.
*/
(deffunction runAnagram ()

   (bind ?word (askAnagram))

   (build (genRule (str-length ?word)))

   (bind ?listvals (slice$ ?word))

   (assertLetters ?listvals)

   (return)

) ; (deffunction runAnagram())

/**
* This function takes in an integer value corresponding to the number of letters of the word
* that the user wants to find the anagrams of. It first creates a string that creates the defrule
* portion of the rule. Then, for every letter in the word, a spot is added with slots for position 
* and character values. Each sequential position slot will look for position values that are different
* from the position values that came before it. After the left hand side of the rule is added to the string,
* the right hand side is created, defining the process of printing out all the letters with unique positions
* that are found. Finally, the completed string containing the rule is returned.
* @param ?length the length of the word that the user wants anagrams of
* @return the completed string that contains the entire enumLetters rule defined to find the anagrams of 
*         a word with ?length letters.
*/
(deffunction genRule (?length)

   (bind ?rule "(defrule enumLetters ")

   (for (bind ?i 1) (< ?i (+ ?length 1)) (++ ?i)                               ; The loop is executed for each separate letter           
      (bind ?rule (str-cat ?rule "(Letter (p "))                               ; The Letter template and p slot is added for each letter

      (for (bind ?j ?i) (> ?j 1) (-- ?j)                                       ; Depending on the location of the current letter in regards
         (bind ?rule (str-cat ?rule "?p" ?j " &~ "))                           ; to the full length of the word, the p slot takes only positions
      )                                                                        ; that have not been selected already
      (bind ?rule (str-cat ?rule "?p" ?j ")"))

      (bind ?rule (str-cat ?rule " (c ?L" ?i ")) "))                           ; The letter slot takes the letter corresponding to the position

   ) ; (for (bind ?i 1) (< ?i (+ ?length 1)) (++ ?i))

   (bind ?rule (str-cat ?rule "=> (printout t "))                              ; The "=>" and "printout" are added to the rule string

   (for (bind ?i 1) (< ?i (+ ?length 1)) (++ ?i)                               ; Each Letter is added to the string and will be printed
      (bind ?rule (str-cat ?rule "?L" ?i " "))
   )

   (bind ?rule (str-cat ?rule "crlf ))"))                                      ; "crlf" and final parentheses are added to the string

   (return ?rule)

) ; (deffunction genRule (?length))

/**
* This function accepts a string prompt and prints the prompt for the user,
* appending the given suffix to the prompt and accepts a user input value.
* It then returns the user inputted value.
* @precondition the inputed prompt is a string
* @param ?prompt the string prompt that is to be transformed into a question
* @return the inputted prompt as a question
*/
(deffunction ask (?prompt ?suffix)

   (printout t (str-cat ?prompt ?suffix))

   (bind ?response (read))

   (return ?response)
   
) ; (deffunction ask (?prompt))


/**
* This function accepts a string prompt and calls the ask function with the prompt and a colon,
* prompting the user to enter a value after the prompt and a colon.
* @param ?prompt the prompt that the user receives
* @return the users response to the ask function
*/
(deffunction askColon (?prompt)

   (return (ask ?prompt ": "))
   
) ; (deffunction askColon (?prompt))

/**
* This function runs the runAnagram function, calls the Jess run function to fire the enumLetters rule,
* then resets the assertions to prepare for a clean next run. 
* @return the number of times the rules are fired
*/
(deffunction go ()

   (runAnagram)

   (bind ?runvals (run))

   (reset)

   (return ?runvals)

) ; (deffunction go ())