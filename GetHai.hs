
module GetHai(nextHai, isNotExistOverHaiCount) where

-- 5 枚以上使っている牌がないかみる
isNotExistOverHaiCount :: [Int] -> Bool
isNotExistOverHaiCount x = null $ filter (\x -> x >= 5) x

-- 次に考える牌形を取得する
nextHai :: [Int] -> [Int]
nextHai (x:xs)
    | x == 0 = slide (x:xs)
    | otherwise = revert (x:xs)

slide :: [Int] -> [Int]
slide [] = []
slide (x:[]) = [x]
slide (x:y:xs) = if x == 0 && y /= 0 then 1:(y-1):xs else x:(slide (y:xs))

revert :: [Int] -> [Int]
revert [] = []
revert (x:[]) = [x]
revert (x:y:xs) = if y /= 0 then (x+1):(y-1):xs else 0:(revert (x:xs))

