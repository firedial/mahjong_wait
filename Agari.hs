
module Agari(getAgariHai, isAgariForm, isTempaiForm, getWaitCount) where
import qualified RemoveHai

isAgariFormWithArgs :: Int -> Int -> [Int] -> Bool
isAgariFormWithArgs atama mentsu hai =
    let
        hais = RemoveHai.repeatRemoveForm mentsu RemoveHai.removeMentsuPossibleFromList $ RemoveHai.repeatRemoveForm atama RemoveHai.removeAtamaPossibleFromList [hai]
    in
    hais /= []

-- 和了形であるか判定する
isAgariForm :: [Int] -> Bool
isAgariForm hai =
    isAgariFormWithArgs atama mentsu hai
    where
        atama = if mod count 3 == 2 then 1 else 0
        mentsu = div count 3
        count = sum hai

countTrue :: [Bool] -> Int
countTrue x = length $ filter id x

-- 和了牌を取得する
getAgariHai :: [Int] -> [Bool]
getAgariHai x = map isAgariForm (getOnePlusList [] x)

getOnePlusList :: [Int] -> [Int] -> [[Int]]
getOnePlusList _ [] = []
getOnePlusList xs (y:ys) = [(xs++((y+1):ys))] ++ getOnePlusList (xs ++ [y]) ys

-- 聴牌形であるか判定する
isTempaiForm :: [Int] -> Bool
isTempaiForm hai = getWaitCount hai /= 0

-- 和了牌の種類数を取得する
getWaitCount :: [Int] -> Int
getWaitCount = countTrue . getAgariHai
