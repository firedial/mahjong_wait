
module NormalForm(isNormalForm) where

isNormalForm :: [Int] -> Bool
isNormalForm hai = if not (isFormerGravity middleHai reverseMiddleHai) then False else if len >= 8 then True else isStartTwo
    where
        len = length middleHai
        middleHai = getMiddleHai hai
        reverseMiddleHai = reverse middleHai
        isStartTwo = (length . removeHeadZero $ hai) == 8

-- 重心の位置が真ん中含めて前にあるか
isFormerGravity :: [Int] -> [Int] -> Bool
isFormerGravity [] [] = True
isFormerGravity [] _ = True
isFormerGravity _ [] = True
isFormerGravity (x:xs) (y:ys) = if x > y then True else if x < y then False else isFormerGravity xs ys


-- 両端の 0 続きを除いたリスト
getMiddleHai :: [Int] -> [Int]
getMiddleHai = reverse . removeHeadZero . reverse . removeHeadZero

-- 先頭から初めて 0 ではないものが出てくるまで除去する
removeHeadZero :: [Int] -> [Int]
removeHeadZero [] = []
removeHeadZero (x:xs) = if x == 0 then removeHeadZero xs else (x:xs)






