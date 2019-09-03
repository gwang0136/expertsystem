(deffunction ask (?prompt) ; prompt user and get token
 (printout t ?prompt "? ")
 (bind ?response (read))
 (return ?response)
)
(deffunction prefixask (?prefix ?prompt)
 (bind ?newPrompt(str-cat ?prefix ?prompt))
 (ask ?newPrompt)
)