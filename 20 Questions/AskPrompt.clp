
/*
* Created by Naveen Mirapuri
* Created 9/1/18
* This program defines ask functions that prompt the user given questions and then returns their answers.
*/

/*
* The function prints the String prompt with a question mark at the end, and then binds the
* response to a variable and returns it
*/
(deffunction askQ (?prompt) 	
   (printout t ?prompt "?" crlf)  ; prints the prompt with a question mark at the end and then creates a new line
   (bind ?response (read))        ; reads the response and binds it to a variable
   (return ?response)             ; returns the response
)

/*
* The function prints the String prompt and then binds the response to a variable and returns it
*/
(deffunction ask (?prompt) 	
   (printout t ?prompt crlf)      ; prints the prompt and then creates a new line
   (bind ?response (read))        ; reads the response and binds it to a variable
   (return ?response)             ; returns the response
)


