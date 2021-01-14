import Debug.Trace
import qualified GetHai
import qualified Agari

isIrreducible :: [Int] -> Bool
isIrreducible hai = isNonRelatedAtama hai && isNonRelatedAtamaConnectedMentsu hai

-- 待ちに関係ない雀頭接続面子がないかどうか
-- ない: true / ある: false
isNonRelatedAtamaConnectedMentsu :: [Int] -> Bool
isNonRelatedAtamaConnectedMentsu hai =
    null hasUnchangedCount
    where
        hasUnchangedCount = filter (\x -> fst x == count || snd x) (map getHaiAgariCondition list)
        -- 可能なかぎり雀頭を取り除いた牌形の配列
        list = Agari.removeAtamaConnectedMentsuPossibleFromList [hai]
        -- 与えられた牌形の待ちの種類数
        count = (countTrue . getAgariHai) hai

-- 待ちに関係ない雀頭がないかどうか
-- ない: true / ある: false
isNonRelatedAtama :: [Int] -> Bool
isNonRelatedAtama hai =
    null hasUnchangedCount
    where
        hasUnchangedCount = filter (\x -> fst x == count || snd x) (map getHaiAgariCondition list)
        -- 可能なかぎり雀頭を取り除いた牌形の配列
        list = Agari.removeAtamaPossibleFromList [hai]
        -- 与えられた牌形の待ちの種類数
        count = (countTrue . getAgariHai) hai

-- 与えられた牌形に対して和了に関することを取得
-- (和了牌の種類数, 与えられた牌形が和了形なら true / それ以外 false)
getHaiAgariCondition :: [Int] -> (Int, Bool)
getHaiAgariCondition hai = ((countTrue . getAgariHai) hai, Agari.isAgari hai)

-- 待ちに関係ない面子がないかどうか
-- ない: true / ある: false
isSemiIrreducible :: [Int] -> Bool
isSemiIrreducible hai =
    null hasUnchangedCount
    where
        hasUnchangedCount = filter (\x -> x == count) (map (countTrue . getAgariHai) list)
        list = Agari.removeMentsuPossibleFromList [hai]
        count = (countTrue . getAgariHai) hai

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (x:xs) = if x then 1 + countTrue xs else countTrue xs

getAgariHai :: [Int] -> [Bool]
getAgariHai x = map Agari.isAgari (getOnePlusList [] x)

getOnePlusList :: [Int] -> [Int] -> [[Int]]
getOnePlusList _ [] = []
getOnePlusList xs (y:ys) = [(xs++((y+1):ys))] ++ getOnePlusList (xs ++ [y]) ys

isTenpai :: [Int] -> Bool
isTenpai hai = (countTrue . getAgariHai) hai /= 0

isTargetHai :: [Int] -> Bool
isTargetHai hai = (not . GetHai.isExistOverHaiCount) hai && isTenpai hai && isSemiIrreducible hai && isIrreducible hai 

-- main = print $ GetHai.nextHai [0, 0, 3]
-- main = print $ Agari.isAgariForm 0 1 [1, 0, 2] 
-- main = print $ isNonRelatedAtama [0, 2, 0, 0, 0, 0, 1, 1, 3]
-- main = print $ (map getAgariHai (Agari.removeMentsuPossibleFromList [[0, 2, 2, 0, 3, 0, 0, 0, 0]]) )

main = printResult [0, 0, 0, 0, 0, 0, 0, 0, 7]

printResult :: [Int] -> IO ()
printResult hai = do
    if isTargetHai hai then print hai else return ()
    if hai == [7, 0, 0, 0, 0, 0, 0, 0, 0] then print "end" else printResult $ GetHai.nextHai hai




