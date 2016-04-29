--Precode
import Data.List.Split
import Data.List
import Data.Char
import Data.String

row0 = [[0,5],[1,0],[2,0],[3,0],[4,1],[5,6],[6,0]]
row1 = [[7,0],[8,0],[9,0],[10,3],[11,0],[12,0],[13,0]]
row2 = [[14,0],[15,6],[16,2],[17,1],[18,0],[19,0],[20,7]]
row3 = [[21,1],[22,7],[23,0],[24,0],[25,6],[26,0],[27,0]]
row4 = [[28,0],[29,3],[30,6],[31,7],[32,2],[33,0],[34,5]]
row5 = [[35,2],[36,1],[37,0],[38,0],[39,4],[40,0],[41,0]]
row6 = [[42,0],[43,0],[44,1],[45,0],[46,0],[47,0],[48,4]]

cc_spiral0 = [[0,0],[13,0],[19,0],[25,0],[31,0],[37,0],[43,0]]
cc_spiral1 = [[1,0],[7,0],[20,0],[26,0],[32,0],[38,0],[44,0]]
cc_spiral2 = [[2,0],[8,0],[14,0],[27,0],[33,0],[39,0],[45,0]]
cc_spiral3 = [[3,0],[9,0],[15,0],[21,0],[34,0],[40,0],[46,0]]
cc_spiral4 = [[4,0],[10,0],[16,0],[22,0],[28,0],[41,0],[47,0]]
cc_spiral5 = [[5,0],[11,0],[17,0],[23,0],[29,0],[35,0],[48,0]]
cc_spiral6 = [[6,0],[12,0],[18,0],[24,0],[30,0],[36,0],[42,0]]

bigList = row0++row1++row2++row3++row4++row5++row6

--checkLotus::


--addValue::

main = putStrLn $  show  bigList

addToList::[Int] -> Int -> Int -> [Int]
addToList bigList index value
 |(bigList!!index)/=0 = bigList
 |otherwise = buildList bigList (-1) index value
 
buildList::[Int] -> Int -> Int -> Int -> [Int]
buildList bigList loopIndex index value
 | loopIndex==49 = []
 | loopIndex==(-1) = buildList bigList 0 index value
 | loopIndex==index = value:(buildList bigList (loopIndex+1) index value)
 | otherwise = (bigList!!loopIndex):(buildList bigList (loopIndex+1) index value)
