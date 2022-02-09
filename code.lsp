(defun length (list)
	(if (eq list nil)
		0
		(+ 1 (length (rest list)))
	)
)
#inputs are two lists
#returns a single list that is the two lists concatenated together
(defun append (list1 list2)
	(if (eq list1 nil)
		list2
		(cons (first list1) (append (rest list1) list2))
	)
)
#input is a list 
#output is the list reversed
#in this code, compute the solution on the way back using append
(defun reverseA (list)
	(reverseH list nil)
)
# helper function for reverseA
(defun reverseH (list reverseList)
    (if
        (eq list nil)
        reverseList
        (reverseH (rest list)(cons (first list) reverseList ) )
    )
)
#input is a list
#return an integer that counts the number of items in the list
(defun length (list)
	(if (eq list nil)
		0
		(+ 1 (length (rest list)))
	)
)
#input a list of lists
#returns a list of atoms in a preorder traversal of the expression tree
(defun flatten (expr)
	(if (eq expr nil)
		nil
		(if (atom expr)
			(cons expr nil)
			(append (flatten (first expr)) (flatten (rest expr)))	
		)
	)
)
#input is two expressions
#return True if the two expressions are the same, False otherwise.
#To be the same they must have the same nested structure and each atom must be eq and in the same position
#Do NOT use flatten to solve this problem
(defun equal (expr0 expr1)
	(if (and (atom expr0) (atom expr1))
		(eq expr0 expr1)
		(and (equal (first expr0) (first expr1))
			 (equal (rest expr0) (rest expr1))
		)
	)
)
#input is an atom (item) and an expression
#returns True if item is contained anywhere in expression
(defun find (item expr)
	(if (eq expr nil)
		False
		(if (atom expr)
			(eq item expr)
			(or (find item (first expr)) (find item (rest expr)))
		)	
	)
	
)



#### TESTS
#(length (quote (1 2 3 4)))
#(length nil)
#(length (cons True (cons False nil)))
#(append (quote (1 3 4)) (quote (2 2 2)))
#(append (cons 1 (cons 2 nil ))nil )
#(reverseA  (quote (1 3 7 10 15 3 2 7 )))
#(reverseA (cons 8 (cons 5 (cons 2 nil ))))
#(length nil)
#(length (cons True (cons False (cons False (cons False nil )))))
#(length (quote (1 8 6 4 4 3 2 1 )))
#(flatten 1 )
#(flatten (quote (1 2 3)))
#(flatten (quote ((1 )(2 )(3 ))))
#(flatten (quote (1 (7 8 (3 )(((4 ))(5 6 ))(9 10 0 (11 ))))))
#(equal 1 nil)
#(equal (quote (1 ))(quote (2 3 4 5 )))
#(equal (quote (1 (7 8 (3 )(((4 ))(5 6 ))(9 10 0 (11 )))))(quote (1 (7 8 (3 )(((4 ))(5 6 ))(9 10 0 (11 ))))))
(find 1 nil )
(find True (cons False (cons False nil )))
(find 111111 (quote (1 2 3 4 6 )))
(find 555 (quote (6 555 67 545 )))