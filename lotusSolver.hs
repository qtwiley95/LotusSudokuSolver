--Precode
import Data.List.Split
import Data.List
import Data.Char
import Data.String

-----------------------------------------------------------------------------------------------------------
--Begin static declarations
-----------------------------------------------------------------------------------------------------------

--Rings of like shape, outside to inside
ring0 = [0,1,2,3,4,5,6]
ring1 = [7,8,9,10,11,12,13]
ring2 = [14,15,16,17,18,19,20]
ring3 = [21,22,23,24,25,26,27]
ring4 = [28,29,30,31,32,33,34]
ring5 = [35,36,37,38,39,40,41]
ring6 = [42,43,44,45,46,47,48]

--counter-clockwise out from the center
ccw_spiral0 = [42,41,34,26,19,11,4]
ccw_spiral1 = [43,35,28,27,20,12,5]
ccw_spiral2 = [44,36,29,21,14,13,6]
ccw_spiral3 = [45,37,30,22,15,7,0]
ccw_spiral4 = [46,38,31,23,16,8,1]
ccw_spiral5 = [47,39,32,24,17,9,2]
ccw_spiral6 = [48,40,33,25,18,10,3]

--clockwise out from the center
cw_spiral0 = [42,35,29,22,16,9,3]
cw_spiral1 = [43,36,30,23,17,10,4]
cw_spiral2 = [44,37,31,24,18,11,5]
cw_spiral3 = [45,38,32,25,19,12,6]
cw_spiral4 = [46,39,33,26,20,13,0]
cw_spiral5 = [47,40,34,27,14,7,1]
cw_spiral6 = [48,41,28,21,15,8,2]

--goodList is a sample given by Dain Vermaak
goodList::[Int]
goodList = [5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,6]

--badList is the same as goodList, but the first index is 1 instead of 5, rendering the entire lotus invalid
badList::[Int]
badList = [1,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,6] 

-----------------------------------------------------------------------------------------------------------
--End static declarations
-----------------------------------------------------------------------------------------------------------

main = putStrLn $ show goodList

lotusSolver::[Int] -> [Int]
lotusSolver bigList = bigList

--addToList places a new value "value" at the index "index" of the list "bigList", and returns the new list
addToList::[Int] -> Int -> Int -> [Int]
addToList bigList index value
 |(bigList!!index)/=0 = bigList
 |otherwise = buildList bigList (-1) index value
 
--buildList is used by addToList to construct the new list.
--It copies all values of the original list until it reaches the desired index, at which it adds the new value.
--It then copies all remaining values of the original list
buildList::[Int] -> Int -> Int -> Int -> [Int]
buildList bigList loopIndex index value
 | loopIndex==49 = []
 | loopIndex==(-1) = buildList bigList 0 index value
 | loopIndex==index = value:(buildList bigList (loopIndex+1) index value)
 | otherwise = (bigList!!loopIndex):(buildList bigList (loopIndex+1) index value)

checkAndRecurse::[Int] -> Bool -> Int -> [Int]
checkAndRecurse bigList valid index
 | index==49 = bigList
 | valid == False = [0]
 | (checkAndRecurse (listToCheck 1) (check(listToCheck 1)) (index+1)) /= [0] = checkAndRecurse (listToCheck 1) (check(listToCheck 1)) (index+1)
 | (checkAndRecurse (listToCheck 2) (check(listToCheck 2)) (index+1)) /= [0] = checkAndRecurse (listToCheck 2) (check(listToCheck 2)) (index+1)
 | (checkAndRecurse (listToCheck 3) (check(listToCheck 3)) (index+1)) /= [0] = checkAndRecurse (listToCheck 3) (check(listToCheck 3)) (index+1)
 | (checkAndRecurse (listToCheck 4) (check(listToCheck 4)) (index+1)) /= [0] = checkAndRecurse (listToCheck 4) (check(listToCheck 4)) (index+1)
 | (checkAndRecurse (listToCheck 5) (check(listToCheck 5)) (index+1)) /= [0] = checkAndRecurse (listToCheck 5) (check(listToCheck 5)) (index+1)
 | (checkAndRecurse (listToCheck 6) (check(listToCheck 6)) (index+1)) /= [0] = checkAndRecurse (listToCheck 6) (check(listToCheck 6)) (index+1)
 | (checkAndRecurse (listToCheck 7) (check(listToCheck 7)) (index+1)) /= [0] = checkAndRecurse (listToCheck 7) (check(listToCheck 7)) (index+1)
 | otherwise = [0]
 where listToCheck = (addToList bigList index)

--check is used to verify the ENTIRE lotus
check::[Int] -> Bool
check list = (checkRing list) && (checkCCW list) && (checkCW list)

--checkRing verifies that all rings are legal
checkRing::[Int] -> Bool
checkRing list = (compareValues list ring0) && (compareValues list ring1) && (compareValues list ring2) && (compareValues list ring3) && (compareValues list ring4) && (compareValues list ring5) && (compareValues list ring6)

--checkCCW verifies that all CCW spirals are legal
checkCCW::[Int] -> Bool
checkCCW list = (compareValues list ccw_spiral0) && (compareValues list ccw_spiral1) && (compareValues list ccw_spiral2) && (compareValues list ccw_spiral3) && (compareValues list ccw_spiral4) && (compareValues list ccw_spiral5) && (compareValues list ccw_spiral6)

--checkCW verifies that all CW sprials are legal
checkCW::[Int] -> Bool
checkCW list = (compareValues list cw_spiral0) && (compareValues list cw_spiral1) && (compareValues list cw_spiral2) && (compareValues list cw_spiral3) && (compareValues list cw_spiral4) && (compareValues list cw_spiral5) && (compareValues list cw_spiral6)

--compareValues checks if the values of "list" at the indices contained in "indices" span 1 through 7.
--compareValues does NOT check if two values are equal.
--Hence, a lotus with multiple zeros will return as false.
compareValues::[Int] -> [Int] -> Bool
compareValues list indices = sort partialList == [1,2,3,4,5,6,7]
 where partialList = (list!!(indices!!0)):(list!!(indices!!1)):(list!!(indices!!2)):(list!!(indices!!3)):(list!!(indices!!4)):(list!!(indices!!5)):(list!!(indices!!6)):[]





