/**
* @author Gene Wang 
*    with assistance from expert Mr. Alfredo Alves
* @version 01 December 2018
* 
* This expert system will use forward chaining in order to make a guess about the winner of a basketball
* game between two basketball teams. The system will prompt the user to input various advanced statistics
* and other information using specific functions for each data type required. The system will then assert
* this information using the statistic template and then will use forward chaining comparisons to determine
* advantages that each team has over the other, altering the strength values for each team accordingly with 
* the defined fuzzy logic constants as it goes. After all the comparisons are made, the system will compare the 
* final strength values for each team and will come to a conclusion about the winner of the game.
*
* Note: It is recommended that this system be used primarily for predicting results of professional basketball matches, as
* the statistics for these teams are readily accesible. This system can be used for other levels of competition, but the user
* will have to acquire or calculate the advanced statistics necessary before using the system.
*/

/****************************************************************************
* The system is cleared and reset each time it is loaded in.                *
* The user is asked to call the go function to initiate the system.         *
* The startup rule is then fired, signalling the beginning of the program   *
* and providing the user with a brief description of the system             *
* and its function, also providing some resources to the user regarding     *
* advanced basketball statistics that the system will prompt the user for.  *
*****************************************************************************
*/

(clear)
(reset)

(printout t "Enter (go) to start the system." crlf)

(defrule startup "Starts the System, printing a startup message and defining the guidelines on how to use the system"

   (declare (salience 100))                                 ; this rule should fire immediately as the system is loaded up

   =>

   (printout t "Welcome to the Basketball Match Result Prediction System." crlf)
   (printout t "This system will ask you a series of questions regarding the statistics of the NBA teams in question." crlf)
   (printout t "Please respond appropriately to the prompts (i.e. if you are asked for a number, then please input an integer or float)" crlf)
   (printout t "All of the advanced statistics requested by the system can be found at https://stats.nba.com/" crlf)
   (printout t "(note: this system can be used for any two competing basketball teams, as long as you are able to acquire or calculate the necessary statistics)" crlf)
   (printout t "" crlf)

) ; (defrule startup)

(defglobal ?*teamOneStrength* = 0)                     ; the strength of each team as determined by the comparisons in the system (will be altered)                     
(defglobal ?*teamTwoStrength* = 0)

(defglobal ?*teamOneName* = "")                        ; the name of each team as inputted by the user
(defglobal ?*teamTwoName* = "")

/**
* Defines a template for a certain statistic with slots for
* the name of the statistic, team one's value of that statistic, 
* and team two's value of that statistic.
*/
(deftemplate statistic (slot name) (slot team1) (slot team2))

/**
* Fuzzy logic values determined by the expert.
* These constants will be added to the strength values of each team
* as the system determines through its comparisons.
*/
(defglobal ?*NO_IMPACT* = 0)
(defglobal ?*LOW_IMPACT* = 1)
(defglobal ?*MEDIUM_IMPACT* = 3)
(defglobal ?*HIGH_IMPACT* = 5)

/*************************************************************
* This section contains function definitions for all the     * 
* functions that are used in prompting the user for          *
* responses regarding certain facts.                         *
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
* This function accepts a user inputted value and checks to see if it is a string. 
* If not, then it will return FALSE, otherwise, TRUE. 
* @return FALSE if not a string, otherwise, 
*         TRUE.
*/
(deffunction validateString (?response)

   (if (or (integerp ?response) (floatp ?response)) then
      (bind ?value FALSE)

   else 
      (bind ?value TRUE))

   (return ?value)
   
)

/**
* This function accepts a value and checks to see if the value is of integer or float type.
* @param ?response the value that is to be checked.
* @return true if the number is of integer or float type, otherwise,
*         false.
*/
(deffunction validateNumber (?response)

   (if (and (not (integerp ?response)) (not (floatp ?response))) then
      (bind ?response FALSE)

   else
      (bind ?response TRUE))

   (return ?response)
)


/**
* This function calls upon the ask function to prompt the user to respond to the question with a number. 
* The function will then call upon the validateNumber function to check if the user's response is a integer or float. 
* If not, the system will remind the user to respond with one of these responses, and will continually ask the user the 
* same  question until a proper response is received. Once a correct response is received, it is returned.
* @param ?prompt the string prompt that is a question that should be responded to with a number.
* @return a valid response in a integer or float form
*/
(deffunction askNumber (?prompt ?suffix)

   (bind ?isValid FALSE)

   (while (not ?isValid)

      (bind ?response (ask ?prompt ?suffix))

      (if (not (validateNumber ?response)) then

         (printout t "Please input a valid number in either integer or float form" crlf)

      else
         (bind ?isValid TRUE)
      )
   )

   (return ?response)

) ;(deffunction askNumber (?prompt ?suffix))

/**
* This function calls upon the ask function to prompt the user to respond to the question given in ?prompt. 
* The function will then call upon the validateString function to check if the user's response is a string value. 
* If not, the system will remind the user to respond with a string, and will continually ask the user the 
* same  question until a proper response is received. Once a correct response is received, it is returned.
* @param ?prompt the string prompt that is to be asked as a yes/no question.
* @return a valid response of the string form
*/
(deffunction askString (?prompt ?suffix)

   (bind ?isValid FALSE)

   (while (not ?isValid)

      (bind ?response (ask ?prompt ?suffix))

      (if (not (validateString ?response)) then
         (printout t "Please input a valid string" crlf)

      else
         (bind ?isValid TRUE))
   )

   (return ?response)

) ; (deffunction askString (?prompt))

/**
* This function calls upon the askNumber function with the prompt asking the user to 
* input an integer or a float value.
* @param ?prompt the question that is to be asked to the user
* @return the user's response to the question as asked by the askNumber function
*/
(deffunction askGreaterOne (?prompt)

   (return (askNumber ?prompt "? Enter an integer or a float "))

)

/**
* This function calls upon the askNumber function with the prompt asking the user to 
* input a decimal value less than one.
* @param ?prompt the question that is to be asked to the user
* @return the user's response to the question as asked by the askNumber function
*/
(deffunction askLessOne (?prompt)

   (return (askNumber ?prompt "? Enter as a decimal value less than 1 (i.e. 0.613) "))
   
)

/****************************************************************************
* This section contains the bulk of the rules that the system will use to 
* determine a winner of the matchup through comparisons between statistics
* between the teams. The acquireStats rule is fired and prompts the user 
* for information about each team, including team names adn advanced 
* statistics, asserting this information using the statistic template. After
* all the information is acquired, the system then uses a forward chaining 
* system to add strength values to  *
*****************************************************************************
*/

(defrule acquireStats "This rule prompts the user for information regarding each team and asserts this information using the statistic template"

   (declare (salience 10))          ; this rule should be fired at the beginning of the game to acquire the required stats from the user                
   
   =>

   (bind ?*teamOneName* (askString "What is the name of the first team in the matchup that you are analyzing" "? "))
   (bind ?*teamTwoName* (askString "What is the name of the second team in the matchup that you are analyzing" "? "))

   (assert (home (askNumber "In which team's arena will this matchup take place" "? (please input 1 or 2) ")))

   (assert (statistic (name homeWL) 
      (team1 (askLessOne (str-cat "What is the current HOME win percentage of the " ?*teamOneName*)))
      (team2 (askLessOne (str-cat "What is the current HOME win percentage of the " ?*teamTwoName*)))))

   (assert (statistic (name awayWL) 
      (team1 (askLessOne (str-cat "What is the current AWAY win percentage of the " ?*teamOneName*)))
      (team2 (askLessOne (str-cat "What is the current AWAY win percentage of the " ?*teamTwoName*)))))

   (assert (statistic (name ppg) 
      (team1 (askGreaterOne (str-cat "How many points are the " ?*teamOneName* " currently averaging per game")))
      (team2 (askGreaterOne (str-cat "How many points are the " ?*teamTwoName* " currently averaging per game")))))

   (assert (statistic (name OREB) 
      (team1 (askGreaterOne (str-cat "How many OFFENSIVE rebounds are the " ?*teamOneName* " currently averaging per game")))
      (team2 (askGreaterOne (str-cat "How many OFFENSIVE rebounds are the " ?*teamTwoName* " currently averaging per game")))))

   (assert (statistic (name DREB) 
      (team1 (askGreaterOne (str-cat "How many DEFENSIVE rebounds are the " ?*teamOneName* " currently averaging per game")))
      (team2 (askGreaterOne (str-cat "How many DEFENSIVE rebounds are the " ?*teamTwoName* " currently averaging per game")))))

   (assert (statistic (name EFG) 
      (team1 (askLessOne (str-cat "What is the current overall effective field goal percentage of the " ?*teamOneName*)))
      (team2 (askLessOne (str-cat "What is the current overall effective field goal percentage of the " ?*teamTwoName*)))))

   (assert (statistic (name TS) 
      (team1 (askLessOne (str-cat "What is the current overall true shooting percentage of the " ?*teamOneName*)))
      (team2 (askLessOne (str-cat "What is the current overall true shooting percentage of the " ?*teamTwoName*)))))

   (assert (statistic (name ATOR) 
      (team1 (askGreaterOne (str-cat "What is the current overall assist to turnover ratio of the " ?*teamOneName*)))
      (team2 (askGreaterOne (str-cat "What is the current overall assist to turnover ratio of the " ?*teamTwoName*)))))

   (assert (statistic (name pace) 
      (team1 (askGreaterOne (str-cat "What is the current overall pace value of the " ?*teamOneName*)))
      (team2 (askGreaterOne (str-cat "What is the current overall pace value of the " ?*teamTwoName*)))))

   (assert (statistic (name streak) 
      (team1 (askNumber (str-cat "What is the current streak that the " ?*teamOneName* " are on") "? (input a losing streak as a negative value) "))
      (team2 (askNumber (str-cat "What is the current streak that the " ?*teamTwoName* " are on") "? (input a losing streak as a negative value) "))))

) ; (defrule acquireStats)

(defrule noImpactStreaksTeam1 "This rule checks to see if the win/loss streaks should have no impact on team one's strength"

   (or 
      (statistic (name streak) {(team1 > 0) && (team1 <= 3)})
      (statistic (name streak) {(team2 < 0) && (team2 >= -3)}))
   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*NO_IMPACT*))

)

(defrule noImpactStreaksTeam2 "This rule checks to see if the win/loss streaks should have no impact on team two's strength"

   (or 
      (statistic (name streak) {(team2 > 0) && (team2 <= 3)})
      (statistic (name streak) {(team1 < 0) && (team1 >= -3)})
   )

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*NO_IMPACT*))

)

(defrule lowImpactStreaksTeam1 "This rule checks to see if the win/loss streaks should have low impact on team one's strength"

   (or
      (and
         (statistic (name streak) {(team1 > 0) && (team1 <= 3)})
         (statistic (name streak) {(team2 < 0) && (team2 >= -3)})
      )
         
         (statistic (name streak) {(team1 > 3) && (team1 <= 5)})

         (statistic (name streak) {(team2 < -3) && (team2 >= -5)})
   )

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*LOW_IMPACT*))

) ; (defrule lowImpactStreaksTeam1)

(defrule lowImpactStreaksTeam2 "This rule checks to see if the win/loss streaks should have low impact on team two's strength"

   (or
      (and
         (statistic (name streak) {(team2 > 0) && (team2 <= 3)})
         (statistic (name streak) {(team1 < 0) && (team1 >= -3)})
      )
         
         (statistic (name streak) {(team2 > 3) && (team2 <= 5)})

         (statistic (name streak) {(team1 < -3) && (team1 >= -5)})
   )

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*LOW_IMPACT*))

) ; (defrule lowImpactStreaksTeam2)

(defrule medImpactStreaksTeam1 "This rule checks to see if the win/loss streaks should have medium impact on team one's strength"

   (or
      (and
         (statistic (name streak) {(team1 > 3) && (team1 <= 5)})
         (statistic (name streak) {(team2 < -3) && (team2 >= -5)})
      )
         
         (statistic (name streak) {(team1 > 5) && (team2 >= -5)})
         
         (statistic (name streak) {(team2 < -5) && (team1 <= 5)})
   )

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*MEDIUM_IMPACT*))

) ; (defrule medImpactStreaksTeam1)

(defrule medImpactStreaksTeam2 "This rule checks to see if the win/loss streaks should have medium impact on team two's strength"

   (or
      (and
         (statistic (name streak) {(team2 > 3) && (team2 <= 5)})
         (statistic (name streak) {(team1 < -3) && (team1 >= -5)})
      )
         
         (statistic (name streak) {(team2 > 5) && (team1 >= -5)})
         
         (statistic (name streak) {(team1 < -5) && (team2 <= 5)})
   )

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*MEDIUM_IMPACT*))

) ; (defrule medImpactStreaksTeam2)

(defrule highImpactStreaksTeam1 "This rule checks to see if the win/loss streaks should have high impact on team one's strength"

   (statistic (name streak) {(team1 > 5) && (team2 < -5)})
      
   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*HIGH_IMPACT*))
)

(defrule highImpactStreaksTeam2 "This rule checks to see if the win/loss streaks should have high impact on team two's strength"

   (statistic (name streak) {(team2 > 5) && (team1 < -5)})
      
   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*HIGH_IMPACT*))

)

(defrule offensiveRebTeam1 "This rule checks to see if team one has an advantage in offensive rebounding, which corresponds to a medium impact in strength"

   (statistic (name OREB) {team1 > team2})

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*MEDIUM_IMPACT*))

)

(defrule offensiveRebTeam2 "This rule checks to see if team two has an advantage in offensive rebounding, which corresponds to a medium impact in strength"

   (statistic (name OREB) {team2 > team1})

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*MEDIUM_IMPACT*))
   
)

(defrule defensiveRebTeam1 "This rule checks to see if team one has an advantage in defensive rebounding, which corresponds to a low impact in strength"

   (statistic (name DREB) {team1 > team2})

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*LOW_IMPACT*))
   
)

(defrule defensiveRebTeam2 "This rule checks to see if team two has an advantage in defensive rebounding, which corresponds to a low impact in strength"

   (statistic (name DREB) {team2 > team1})

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*LOW_IMPACT*))
   
)

(defrule combinedRebTeam1 

   "This rule checks to see if team one has an advantage in both offensive and defensive rebounding, which corresponds to a high impact in strength"

   (and
      (statistic (name OREB) {team1 > team2})
      (statistic (name DREB) {team1 > team2})
   )

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*HIGH_IMPACT*))

) ; (defrule combinedRebTeam1)

(defrule combinedRebTeam2

   "This rule checks to see if team two has an advantage in both offensive and defensive rebounding, which corresponds to a high impact in strength"

   (and
      (statistic (name OREB) {team2 > team1})
      (statistic (name DREB) {team2 > team1})
   )

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*HIGH_IMPACT*))

) ; (defrule combinedRebTeam2)

(defrule locationTeam1Good "This rule checks to see if team 1's home record forecasts an increase in strength for their home game"
   
   (home 1)
   (statistic (name HWL) (team1 ?team1HWL))
   (statistic (name AWL) (team2 ?team2AWL))
   (test (> ?team1HWL ?team2AWL))
   
   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*MEDIUM_IMPACT*))
   
)

(defrule locationTeam1Bad "This rule checks to see if team 1's home record forecasts an decrease in strength for their home game"
   
   (home 1)
   (statistic (name HWL) (team1 ?team1HWL))
   (statistic (name AWL) (team2 ?team2AWL))
   (test (< ?team1HWL ?team2AWL))
   
   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*MEDIUM_IMPACT*))
   
)

(defrule locationTeam2Good "This rule checks to see if team 2's home record forecasts an increase in strength for their home game"
   
   (home 2)
   (statistic (name HWL) (team2 ?team2HWL))
   (statistic (name AWL) (team1 ?team1AWL))
   (test (> ?team2HWL ?team1AWL))
   
   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*MEDIUM_IMPACT*))
   
)

(defrule locationTeam2Bad "This rule checks to see if team 2's home record forecasts an decrease in strength for their home game"
   
   (home 2)
   (statistic (name HWL) (team2 ?team2HWL))
   (statistic (name AWL) (team1 ?team1AWL))
   (test (< ?team2HWL ?team1AWL))
   
   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*MEDIUM_IMPACT*))
   
)

(defrule pointsTeam1 "This rule checks to see if team one has an advantage in points per game, which corresponds to a medium impact in strength"
   
   (statistic (name ppg) {team1 > team2})

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*MEDIUM_IMPACT*))

)

(defrule pointsTeam2 "This rule checks to see if team two has an advantage in points per game, which corresponds to a medium impact in strength"
   
   (statistic (name ppg) {team2 > team1})

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*MEDIUM_IMPACT*))

)

(defrule EFGTeam1 "This rule checks to see if team one has an advantage in effective field goal percentage, which corresponds to a low impact in strength"

   (statistic (name EFG) {team1 > team2})

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*LOW_IMPACT*))

)

(defrule EFGTeam2 "This rule checks to see if team two has an advantage in effective field goal percentage, which corresponds to a low impact in strength"

   (statistic (name EFG) {team2 > team1})

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*LOW_IMPACT*))

)

(defrule trueShootingTeam1 "This rule checks to see if team one has an advantage in true shooting percentage, which corresponds to a low impact in strength"

   (statistic (name TS) {team1 > team2})

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*LOW_IMPACT*))

)

(defrule trueShootingTeam2 "This rule checks to see if team two has an advantage in true shooting percentage, which corresponds to a low impact in strength"

   (statistic (name TS) {team2 > team1})

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*LOW_IMPACT*))
   
)

(defrule assistTurnoverTeam1 "This rule checks to see if team one has an advantage in assist to turnover ratio, which corresponds to a medium impact in strength"

   (statistic (name ATOR) {team1 > team2})

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*MEDIUM_IMPACT*))
   
)


(defrule assistTurnoverTeam2 "This rule checks to see if team two has an advantage in assist to turnover ratio, which corresponds to a medium impact in strength"

   (statistic (name ATOR) {team2 > team1})

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*MEDIUM_IMPACT*))
   
)


(defrule paceTeam1 "This rule checks to see if team one has an advantage in pace, which corresponds to a medium impact in strength"
   
   (statistic (name pace) {team1 > team2})

   =>

   (bind ?*teamOneStrength* (+ ?*teamOneStrength* ?*MEDIUM_IMPACT*))
   
)

(defrule paceTeam2 "This rule checks to see if team two has an advantage in pace, which corresponds to a medium impact in strength"
   
   (statistic (name pace) {team2 > team1})

   =>

   (bind ?*teamTwoStrength* (+ ?*teamTwoStrength* ?*MEDIUM_IMPACT*))
   
)


/*****************************************************************************
* After all the appropriate changes are made to the strengths of each team   *
* based on the facts provided by the user, the system will then make a       *
* decision on the winner of the game using the following rules. If the       *
* strength disparity is above 5 points, then the system can confidently make *
* a prediction about the winner of the game. If the strength disparity is    *
* between 0 and 5 points, then the system can take a guess at the winner of  *
* the game. If the strength values are equal, the system is unable to make a *
* guess and therefore lets the user know about how                           *
* evenly matched the teams are.                                              *
******************************************************************************
*/

(defrule findResult "Compares the strength values and asserts the winner along with the certainty level"

   (declare (salience -50))

   =>
   
   (if (> (- ?*teamOneStrength* ?*teamTwoStrength*) 5) then
      (assert (team1 certain))

   else (if (> (- ?*teamTwoStrength* ?*teamOneStrength*) 5) then
      (assert (team2 certain))

   else (if (and (> (- ?*teamOneStrength* ?*teamTwoStrength*) 0) (<= (- ?*teamOneStrength* ?*teamTwoStrength*) 5)) then
      (assert (team1 uncertain))

   else (if (and (> (- ?*teamTwoStrength* ?*teamOneStrength*) 0) (<= (- ?*teamTwoStrength* ?*teamOneStrength*) 5)) then
      (assert (team2 uncertain)

   else (if (= ?*teamOneStrength* ?*teamTwoStrength*) then
      (assert (even))))))))

) ; (defrule findResult)

(defrule team1Certain "Ends the program, confidently declaring team 1 the winner"

   (declare (salience -100))
   (team1 certain)

   =>

   (printout t (str-cat "I can confidently predict that the " ?*teamOneName* " will emerge victorious from this matchup!") crlf)
   (halt)

)

(defrule team2Certain "Ends the program, confidently declaring team 2 the winner"

   (declare (salience -100))
   (team2 certain)

   =>

   (printout t (str-cat "I can confidently predict that the " ?*teamTwoName* " will emerge victorious from this matchup!") crlf)
   (halt)

)

(defrule team1Uncertain "Ends the program, making a guess that team 1 will win"

   (declare (salience -100))
   (team1 uncertain)

   =>

   (printout t (str-cat "It's close, but I think that " ?*teamOneName* " will emerge victorious from this matchup!") crlf)
   (halt)

)

(defrule team2Uncertain "Ends the program, making a guess that team 2 will win"

   (declare (salience -100))
   (team2 uncertain)

   =>

   (printout t (str-cat "It's close, but I think that " ?*teamTwoName* " will emerge victorious from this matchup!") crlf)
   (halt)

)

(defrule evenlyMatched "Ends the program, declaring that the teams are evenly matched."

   (declare (salience -150))
   (even)

   =>

   (printout t "These teams are too evenly matched, I cannot make a solid prediction!")
   (halt)

)

/**
* Clears the engine, then calls upon the Jess run function 
* to begin firing rules.
* @return null
*/
(deffunction go ()

   (reset)

   (run)

   (return)

)