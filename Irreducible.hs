
module Irreducible(isIrreducible, isSemiIrreducible) where
import qualified Agari
import qualified RemoveHai

isIrreducible :: [Int] -> Bool
isIrreducible hai = isNonRelatedAtama hai -- && isNonRelatedAtamaConnectedMentsu hai

-- 待ちに関係ない雀頭接続面子がないかどうか
-- ない: true / ある: false
isNonRelatedAtamaConnectedMentsu :: [Int] -> Bool
isNonRelatedAtamaConnectedMentsu hai =
    null hasUnchangedCount
    where
        hasUnchangedCount = filter (\x -> fst x == count || snd x) (map getHaiAgariCondition list)
        -- 可能なかぎり雀頭を取り除いた牌形の配列
        list = RemoveHai.removeAtamaConnectedMentsuPossibleFromList [hai]
        -- 与えられた牌形の待ちの種類数
        count = Agari.getWaitCount hai

-- 待ちに関係ない雀頭がないかどうか
-- ない: true / ある: false
isNonRelatedAtama :: [Int] -> Bool
isNonRelatedAtama hai =
    null hasUnchangedCount
    where
        hasUnchangedCount = filter (\x -> fst x == count || (fst x == count - 1 && snd x)) (map getHaiAgariCondition list)
        -- 可能なかぎり雀頭を取り除いた牌形の配列
        list = RemoveHai.removeAtamaPossibleFromList [hai]
        -- 与えられた牌形の待ちの種類数
        count = Agari.getWaitCount hai

-- 与えられた牌形に対して和了に関することを取得
-- (和了牌の種類数, 与えられた牌形が和了形なら true / それ以外 false)
getHaiAgariCondition :: [Int] -> (Int, Bool)
getHaiAgariCondition hai = (Agari.getWaitCount hai, Agari.isAgariForm hai)

-- 待ちに関係ない面子がないかどうか
-- ない: true / ある: false
isSemiIrreducible :: [Int] -> Bool
isSemiIrreducible hai =
    null hasUnchangedCountIrregular
    where
        hasUnchangedCountIrregular = filter (\x -> not isAgariForm || snd x) hasUnchangedCount
        hasUnchangedCount = filter (\x -> fst x == count) (map getHaiAgariCondition list)
        list = RemoveHai.removeMentsuPossibleFromList [hai]
        count = Agari.getWaitCount hai
        isAgariForm = Agari.isAgariForm hai

