
module Agari(isAgariForm, isAgari, isTempai, getWaitCount) where
import qualified RemoveHai

isAgariForm :: Int -> Int -> [Int] -> Bool
isAgariForm atama mentsu hai =
    let
        hais = RemoveHai.repeatRemoveForm mentsu RemoveHai.removeMentsuPossibleFromList $ RemoveHai.repeatRemoveForm atama RemoveHai.removeAtamaPossibleFromList [hai]
    in
    hais /= []

isAgari :: [Int] -> Bool
isAgari hai =
    isAgariForm atama mentsu hai
    where
        atama = if mod count 3 == 2 then 1 else 0
        mentsu = div count 3
        count = sum hai

countTrue :: [Bool] -> Int
countTrue x = length $ filter id x

getAgariHai :: [Int] -> [Bool]
getAgariHai x = map isAgari (getOnePlusList [] x)

getOnePlusList :: [Int] -> [Int] -> [[Int]]
getOnePlusList _ [] = []
getOnePlusList xs (y:ys) = [(xs++((y+1):ys))] ++ getOnePlusList (xs ++ [y]) ys

isTempai :: [Int] -> Bool
isTempai hai = getWaitCount hai /= 0

getWaitCount :: [Int] -> Int
getWaitCount = countTrue . getAgariHai
