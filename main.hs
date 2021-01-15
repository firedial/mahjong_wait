import Debug.Trace
import qualified GetHai
import qualified Agari
import qualified RemoveHai
import qualified NormalForm

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


isTargetHai :: [Int] -> Bool
isTargetHai hai = (not . GetHai.isExistOverHaiCount) hai && NormalForm.isNormalForm hai && Agari.isTempaiForm hai && isSemiIrreducible hai && isIrreducible hai

isTargetHaiRemoveAtama :: [Int] -> Bool
isTargetHaiRemoveAtama hai = (not . GetHai.isExistOverHaiCount) hai && NormalForm.isNormalForm hai && Agari.isTempaiForm hai && isSemiIrreducible hai

-- main = print $ GetHai.nextHai [0, 0, 3]
-- main = print $ Agari.isAgariForm 0 1 [1, 0, 2] 
-- main = print $ isNonRelatedAtama [0, 2, 0, 0, 0, 0, 1, 1, 3]
-- main = print $ (map getAgariHai (Agari.removeMentsuPossibleFromList [[0, 2, 2, 0, 3, 0, 0, 0, 0]]) )

-- main = print $ isIrreducible [0, 3, 2, 2, 0, 0, 0, 0, 0]
main = printResult [0, 0, 0, 0, 0, 0, 0, 0, 7]

printResult :: [Int] -> IO ()
printResult hai = do
    -- if isTargetHaiRemoveAtama hai then print hai else return ()
    if isTargetHai hai then print hai else return ()
    if hai == [7, 0, 0, 0, 0, 0, 0, 0, 0] then return () else printResult $ GetHai.nextHai hai




