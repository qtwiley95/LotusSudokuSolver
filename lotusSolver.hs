--Authors Quinton Wiley & Michael Neises
--Precode
import Data.List.Split
import Data.List

--rows of like shape, outside to inside
row0 = [0,1,2,3,4,5,6]
row1 = [7,8,9,10,11,12,13]
row2 = [14,15,16,17,18,19,20]
row3 = [21,22,23,24,25,26,27]
row4 = [28,29,30,31,32,33,34]
row5 = [35,36,37,38,39,40,41]
row6 = [42,43,44,45,46,47,48]

--counter-clockwise out from the center
ccw0 = [42,41,34,26,19,11,4]
ccw1 = [43,35,28,27,20,12,5]
ccw2 = [44,36,29,21,14,13,6]
ccw3 = [45,37,30,22,15,7,0]
ccw4 = [46,38,31,23,16,8,1]
ccw5 = [47,39,32,24,17,9,2]
ccw6 = [48,40,33,25,18,10,3]

--clockwise out from the center
cw0 = [42,35,29,22,16,9,3]
cw1 = [43,36,30,23,17,10,4]
cw2 = [44,37,31,24,18,11,5]
cw3 = [45,38,32,25,19,12,6]
cw4 = [46,39,33,26,20,13,0]
cw5 = [47,40,34,27,14,7,1]
cw6 = [48,41,28,21,15,8,2]

-----------------------------------------------------------------------------------
--Begin Test Cases
-----------------------------------------------------------------------------------
allZero = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
myList0 = [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]
myList1 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]
myList2 = [4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0]
myList3 = [0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0]
myList4 = [4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0]
myList5 = [0,1,2,0,6,0,0,0,0,7,1,0,0,0,0,0,6,0,0,0,0,1,0,0,0,0,0,0,6,0,0,0,0,2,0,2,3,0,0,6,0,0,1,4,0,0,0,0,0]
myList6 = [0,0,0,7,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]
unSolve = [0,0,3,7,6,1,4,4,0,5,1,7,0,0,0,2,6,0,4,5,3,1,0,0,2,0,0,5,6,0,7,0,0,2,3,2,3,0,7,6,0,0,0,4,5,6,1,7,0]
-----------------------------------------------------------------------------------
--End Test Cases
-----------------------------------------------------------------------------------
{- commenting out our main since it was our testing conditions and thought it would conflict with you testing our program.
main = do
      print  $ lotusSolver allZero
      print  $ lotusSolver myList0
      print  $ lotusSolver myList1
      print  $ lotusSolver myList2
      print  $ lotusSolver myList3
      print  $ lotusSolver myList4
      print  $ lotusSolver myList5
      print  $ lotusSolver myList6
      print  $ lotusSolver unSolve
-}
-----------------------------------------------------------------------------------
--Name: fiftyList
--Inputs: list of ints
--Outputs: list of ints of size one greater than the input list
--Explanation: Takes in a list and adds a zero element to the end.
--This is valuable because checkAndRecurse validates entry N when it is called on entry N+1
-----------------------------------------------------------------------------------
fiftyList::[Int] -> [Int]
fiftyList a = a ++ [0]

-----------------------------------------------------------------------------------
--Name: fortyNineList
--Inputs: list of ints
--Outputs: list of ints of size one less than the input list
--Explanation: The final element is always 1, and is meaningless to the lotus.
--We drop it before we return the lotus.
-----------------------------------------------------------------------------------
fortyNineList::[Int] -> [Int]
fortyNineList a = take 49 a

-----------------------------------------------------------------------------------
--Name: lotusSolver
--Inputs: a list of ints, representing an unsolved lotus sudoku puzzle
--Outputs: a list of ints, representing a now solved lotus sudoku puzzle
--Explanation: We append a trivial element before solving, and
--			   drop that same element before returning
-----------------------------------------------------------------------------------
lotusSolver::[Int] -> [Int]
lotusSolver bigList = fortyNineList $ checkAndRecurse (fiftyList bigList) True 0

-----------------------------------------------------------------------------------
--Name: addToList
--Inputs: a list of ints; an index int; a value int
--Outputs: a list of ints
--Explanation: The output list is identical to the input list,
--             except that "value" is placed at "index."
-----------------------------------------------------------------------------------
addToList::[Int] -> Int -> Int -> [Int]
addToList bigList index value
 |(bigList!!index)/=0 = bigList
 |otherwise = ((take index bigList) ++ [value] ++ (drop (index+1) bigList))

-----------------------------------------------------------------------------------
--Name: checkAndRecurse
--Inputs: a list of ints; a bool called "valid"; an int called "index
--Outputs: the output is either a solved lotus, or an empty list, meaning the lotus given was unsolvable.
--Explanation: This is the Top Level function in our code. This function will attempt to add a value to the list. then calls itself
--             onto the remainder of the list keeping track by incrementing an index counter for its next call. By using where statement
--             each recursive call to check its valid option of number to add will only be evaluated to work if it needs to be evaluated
--             Either index will hit 50 and it will return the completed solved list. or every option will be exhausted and its initial call
--             will reach the otherwise condition and return an empty list thus noting that the puzzle is unsolveable.
-----------------------------------------------------------------------------------
checkAndRecurse::[Int] -> Bool -> Int -> [Int]
checkAndRecurse bigList valid index
 | index== 50 = bigList --The base recursive case is when index hits 50
 | bigList!!index /= 0 = checkAndRecurse bigList valid (index + 1) --if the value at index is not = 0 skip over it
 | valid == False = [] --if it failed check on call return its fail condition (empty list)
 | a1 /= [] = a1
 | a2 /= [] = a2
 | a3 /= [] = a3
 | a4 /= [] = a4
 | a5 /= [] = a5
 | a6 /= [] = a6
 | a7 /= [] = a7
 | otherwise = []--will return to previous call if unable to add any value into the List
 where listToCheck = (addToList bigList index)--setting these conditions to where GREEAAAATLLLY improves evaluation time via lazy evaluation
       a1 = (checkAndRecurse (listToCheck 1) (check(listToCheck 1)) (index+1))
       a2 = (checkAndRecurse (listToCheck 2) (check(listToCheck 2)) (index+1))
       a3 = (checkAndRecurse (listToCheck 3) (check(listToCheck 3)) (index+1))
       a4 = (checkAndRecurse (listToCheck 4) (check(listToCheck 4)) (index+1))
       a5 = (checkAndRecurse (listToCheck 5) (check(listToCheck 5)) (index+1))
       a6 = (checkAndRecurse (listToCheck 6) (check(listToCheck 6)) (index+1))
       a7 = (checkAndRecurse (listToCheck 7) (check(listToCheck 7)) (index+1))

-----------------------------------------------------------------------------------
--Name:check
--Inputs: list of Ints
--Outputs: boolean
--Explanation: using list comprehension to check every value that corresponds to the index that occurs in any of
--             the statically determined rows, ccws, and cws. then calls hasDuplicate on each of the returned lists made by
--             the list comprehension methods. since we're using boolean logic, if any of the hasDuplicates return True then
--             the whole line is true and the function returns False. otherwise they all pass and return True
-----------------------------------------------------------------------------------
check::[Int] -> Bool
check a
 | (hasDuplicate [a!!x | x <-row0] || hasDuplicate [a!!x | x <-row1] || hasDuplicate [a!!x | x <-row2] || hasDuplicate [a!!x | x <-row3] || hasDuplicate [a!!x | x <-row4] || hasDuplicate [a!!x | x <-row5] || hasDuplicate [a!!x | x <-row6]) == True = False
 | (hasDuplicate [a!!x | x <-cw0] || hasDuplicate [a!!x | x <-cw1] || hasDuplicate [a!!x | x <-cw2] || hasDuplicate [a!!x | x <-cw3] || hasDuplicate [a!!x | x <-cw4] || hasDuplicate [a!!x | x <-cw5] || hasDuplicate [a!!x | x <-cw6]) == True = False
 | (hasDuplicate [a!!x | x <-ccw0] || hasDuplicate [a!!x | x <-ccw1] || hasDuplicate [a!!x | x <-ccw2] || hasDuplicate [a!!x | x <-ccw3] || hasDuplicate [a!!x | x <-ccw4] || hasDuplicate [a!!x | x <-ccw5] || hasDuplicate [a!!x | x <-ccw6]) == True = False
 | otherwise = True

-----------------------------------------------------------------------------------
--Name: hasDuplicate
--Inputs: A list of Ints
--Outputs: boolean
--Explanation: until the passed in list is empty take the head of the list and if it is not equivalent to 0 then using
--             elem function to check if the head of list matches any other element inside of the list or if the next
--             head of list matches any subsequent recursively calls until list is empty, if none matched then returns
--             false. if any matched then return true.
-----------------------------------------------------------------------------------
hasDuplicate::[Int] -> Bool
hasDuplicate [] = False
hasDuplicate (x:xs)
 |x == 0 = hasDuplicate xs
 |otherwise = (elem x xs ) || hasDuplicate xs
