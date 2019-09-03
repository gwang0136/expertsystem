;
; @author Gene Wang
; @version 5 September 2018
; This module defines functions that prompts and accepts a user inputted positive integer, generates 
; a list that contains that number of values from the fibonacci sequence, and prints the list to the terminal.
; The fib function accepts a positive integer and generates a list of that number of values from the fibonacci sequence.
; The askfib function asks the user to input a requested number of values from the fibonacci sequence, generates a list 
; containing all the requested values, then prints the complete list to the terminal.
; The ask function accepts a string prompt and turns the prompt into a question by appending a question mark then returns 
; the question in string form.
; 


; This function accepts a positive integer and generates a list of that number of values from the fibonacci sequence.
; The function generates the values iteratively.
; @precondition the value of ?val is a positive integer
; @param ?val the number of values requested from the fibonacci sequence.
; @return a list containing ?val amount of numbers from the fibonacci sequence.
;
(deffunction fib(?val)

   (bind ?twoBefore 0)                                             ; The value before the first number in the fibonnaci sequence.

   (bind ?oneBefore 1)                                             ; The first value in the fibonnaci sequence.

   (bind ?listVals (create$ ?oneBefore))                           ; Intializes the list to be the first value in the sequence,
                                                                   ; assuming that the number of values requested will be greater 
                                                                   ; than or equal to one.
   (for (bind ?i 1) (< ?i ?val) (++ ?i)
      
      (bind ?current (+ ?twoBefore ?oneBefore))                   
      (bind ?listVals (insert$ ?listVals (+ ?i 1) ?current))
      (bind ?twoBefore ?oneBefore)
      (bind ?oneBefore ?current)
      
   )
   
   (return ?listVals)
   
)

; This function asks the user to input a requested number of values from the fibonacci sequence.
; The function calls upon the validation function isPositiveInteger to check if the user inputted value is 
; a positive integer value, and if it is, then the function
; calls upon the fib function to generate a list containing all the requested values. 
; If the value is not a positive integer, the user is told to try again and the function is terminated.
; The function then prints the complete list to the terminal.
;
(deffunction askfib ()

   (bind ?value (ask "How many digits of the fibonacci sequence do you want")) ; prompts the user for a value
   
   (if (not (isPositiveInteger ?value)) then                       ; validates the user inputted value

      (printout t "Please input a positive integer value" crlf)    ; reminds the user to input a positive integer value

   else
      
      (printout t (fib ?value) crlf)                               ; calls the fib function to generate the list of 
                                                                   ; values and prints them to the terminal
   )
)



; This function accepts a string prompt and turns the prompt into a question by appending a question mark.
; It then returns the question in string form.
; @precondition the inputed prompt is a string
; @param ?prompt the string prompt that is to be transformed into a question
; @return the inputted prompt as a question
;
(deffunction ask (?prompt)

   (printout t ?prompt "? ")

   (bind ?response (read))

   (return ?response)
   
)

; This function validates the user inputted value, checking to ensure that it is a positive integer value.
; The function first checks if the value is an integer, if it is not, the function returns false. 
; The function then checks to see if the value is positive, if it is not, the function returns false.
; Finally, if the value is both an integer and positive, the function returns true.
; @param ?value the value that is to be validated
; @return true, if the value is a positive integer value, otherwise,
;         false
;
(deffunction isPositiveInteger(?value)

   (if (not (integerp ?value)) then                                ; checks to see if the value is an integer
      (return FALSE)

   elif (<= ?value 0) then                                         ; checks to see if the value is positive
      (return FALSE)

   else                                                            ; the value is a positive integer, return true
      (return TRUE)

   )
)