
module Irreducible(isIrreducible) where
import qualified Agari
import qualified RemoveHai

isIrreducible :: [Int] -> Bool
isIrreducible hai = isMentsuIrreducible hai && if isRegularForm then isAtamaIrreducible hai && isAtamaConnectedShuntsuIrreducible hai else True
    where
        isRegularForm = mod (sum hai) 3 == 1

-- 雀頭接続順子既約であるか
isAtamaConnectedShuntsuIrreducible :: [Int] -> Bool
isAtamaConnectedShuntsuIrreducible hai =
    null hasUnchangedCount
    where
        hasUnchangedCount = filter (\x -> x == count) (map (getEssentialWaitCount hai) list)
        -- 可能なかぎり雀頭接続順子を取り除いた牌形の配列
        list = RemoveHai.removeAtamaConnectedMentsuPossibleFromList [hai]
        -- 与えられた牌形の待ちの種類数
        count = Agari.getWaitCount hai

-- 雀頭既約であるか
isAtamaIrreducible :: [Int] -> Bool
isAtamaIrreducible hai =
    null hasUnchangedCount
    where
        hasUnchangedCount = filter (\x -> x == count) (map (getEssentialWaitCount hai) list)
        -- 可能なかぎり雀頭を取り除いた牌形の配列
        list = RemoveHai.removeAtamaPossibleFromList [hai]
        -- 与えられた牌形の待ちの種類数
        count = Agari.getWaitCount hai

getEssentialWaitCount :: [Int] -> [Int] -> Int
getEssentialWaitCount originalHai hai =
    length $ filter id essentialWaitHai
    where
        waitHai = Agari.getAgariHai hai
        diff = RemoveHai.removeHai originalHai hai
        addWaitHai = Agari.getAgariHai diff
        isAgariForm = Agari.isAgariForm hai
        essentialWaitHai = if isAgariForm then orList waitHai addWaitHai else waitHai

orList :: [Bool] -> [Bool] -> [Bool]
orList [] [] = []
orList x [] = x
orList [] y = y
orList (x:xs) (y:ys) = (x || y) : orList xs ys

-- 与えられた牌形に対して和了に関することを取得
-- (和了牌の種類数, 与えられた牌形が和了形なら true / それ以外 false)
getHaiAgariCondition :: [Int] -> (Int, Bool)
getHaiAgariCondition hai = (Agari.getWaitCount hai, Agari.isAgariForm hai)

-- 待ちに関係ない面子がないかどうか
-- ない: true / ある: false
isMentsuIrreducible :: [Int] -> Bool
isMentsuIrreducible hai =
    null hasUnchangedCountIrregular
    where
        hasUnchangedCountIrregular = filter (\x -> not isAgariForm || snd x) hasUnchangedCount
        hasUnchangedCount = filter (\x -> fst x == count) (map getHaiAgariCondition list)
        list = RemoveHai.removeMentsuPossibleFromList [hai]
        count = Agari.getWaitCount hai
        isAgariForm = Agari.isAgariForm hai


