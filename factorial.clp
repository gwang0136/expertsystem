;
; @author Gene Wang
; @version 5 September 2018
;
; This module defines functions that calculate the factorial of a given number and is 
; able to accept user input and return the factorial of the user inputted value.
;


; This function takes in a positive integer value and recursively calculates the
; factorial of that number and returns it.
;
(deffunction factorial (?value)
   (if (<= ?value 1) then
      (return 1)
   else (return (* ?value (factorial (-- ?value))))
   )
)

;
; This function uses the previously written 'ask' function to prompt the user for a number
; then returns the factorial of that number.
;
(deffunction askfactorial ()
   (bind ?result (factorial(ask "What is the value that you would like to calculate the factorial of")))
   (printout t ?result crlf)
)