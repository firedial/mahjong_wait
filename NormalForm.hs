
module NormalForm(isNormalForm) where

-- 標準形かどうかをみる
isNormalForm :: [Int] -> Bool
isNormalForm hai = (isFormerGravity middleHai reverseMiddleHai) && (len >= 8  || isStartTwo)
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
isFormerGravity (x:xs) (y:ys) = if x /= y then x > y else isFormerGravity xs ys


-- 両端の 0 続きを除いたリスト
getMiddleHai :: [Int] -> [Int]
getMiddleHai = reverse . removeHeadZero . reverse . removeHeadZero

-- 先頭から初めて 0 ではないものが出てくるまで除去する
removeHeadZero :: [Int] -> [Int]
removeHeadZero [] = []
removeHeadZero (x:xs) = if x == 0 then removeHeadZero xs else (x:xs)






