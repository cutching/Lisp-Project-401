;Assignment6.lisp - CMSC 403
;Basic functions of lisp while getting to know lisp
;Gavin Cutchin
;4/5/2022

;1. function myList() that generates the specified list from (4 (7 22) "art" ("math" (8) 99) 100) 
(defun myList()
  ;simply uses nested lists to generate appropriate result
  (list 4 (list 7 22) "art" (list "math" (list 8) 99) 100))

;2. HELPER FUNC that checks if a year is a leap year
(defun checkLeapYear(inputYear)
  (cond
    ((and (zerop (mod inputYear 400))(zerop (mod inputYear 100))) T) ;equates to year % 400 AND year % 100 = 0 = True
    ((and (not (zerop (mod inputYear 100)))(zerop (mod inputYear 4))) T))) ;equates to year % 100 != 0 AND year % 4 = 0 = True

;2. grabs every leap year from 1800 to 2021 
(defun leapYear(&key (start 1800) (end 2021))
  (loop for year from start to end ;Loops through every value between 1800 and 2021
    if (checkLeapYear year) collect year));Checks the year with our helper function and collects it into a list if true


;3. union- returns a list of unique elements in both lists
(defun union- (l1 l2)
    (cond
        ((null l2) l1) ;if list 2 is null, return list 1
        ((null l1) l2) ;if list 1 is null, return list 2
        ((member (first l2) l1) (union- l1 (rest l2))) ;if first element of list 2 is a member of list 1, move onto the next list 2 item
        (T (append l1 (first l2)) (union- (append l1 (list (first l2))) (rest l2))))) ;append an item to list 1 when it is not found in list 1, recursively continue

;4. Tail end recursively finds the average of a list
(defun avg(aList)
  (cond
    ((null aList) NIL) ;return NIL if list is null
    ((= (length aList) 1) (first aList)) ;if list size is 1, return only that element
    (T (/ (+ (first aList) (* (- (length aList) 1) (avg(rest aList)))) (length aList))))) ;works like this ((sum)(size - 1)!) / (size)! this is equivalent to sum/size

;5 Returns anonymous function which takes a parameter and checks it against the dataType
(defun isType(dataType)
  ;checks the type of input and the dataType against each other in both orders to cover all cases
  ;uses lambda for the anonymous function
  (lambda (input) (or (subtypep (type-of input) dataType) (subtypep dataType (type-of input)))))



;6 Returns a list of values where every value larger than the limit is multiplied by the rate
(defun taxCalculator(limit rate values)
  ;using mapcar and lambda to map a function to each value and check if it has to be multiplied by the rate
  (mapcar #'(lambda (value)
    (cond
    ((< limit value)(* rate value))
    (T value))
    ) values))

;7 Returns an edited list based on the boolean function each member needs to be tested by
(defun clean(aFunc aList)
  (cond
     ((null aList) aList) ;return aList if null, equivalent to ()
     ((typep (first aList) 'list) (cons (clean aFunc (first aList))(clean aFunc (rest aList)))) ;checks for sublists and recurs appropriately
     ((funcall aFunc(first aList)) (cons (first aList) (clean aFunc (rest aList)))) ;if no sublist is present then checks boolean function and places value
     (t (clean aFunc (rest aList)))));reach this when a value is not valid





;8 macro that 3 list parameters where the first element of each list is a conditional that decides which list to then execute
(defmacro threeWayBranch(x y z)
`(if ,(first x) ;check the conditional of branch x
    (progn ,@(rest x)) ;if true then the body is every other statement of branch x
    (if ,(first y) ;if not x then check the conditional of branch y
      (progn ,@(rest y)) ;if true then the body is every other statement of branch y
      (if ,(first z) ;if not y then check the conditional of branch z
        (progn ,@(rest z)) ;if true then the body is every other statement of branch z
        ()))))  ;do nothing otherwise -> NIL





     


