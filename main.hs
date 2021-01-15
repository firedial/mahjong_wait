import Debug.Trace
import qualified GetHai
import qualified Agari
import qualified NormalForm
import qualified Irreducible

isTargetHai :: [Int] -> Bool
isTargetHai hai = (not . GetHai.isExistOverHaiCount) hai && NormalForm.isNormalForm hai && Agari.isTempaiForm hai && Irreducible.isSemiIrreducible hai && Irreducible.isIrreducible hai

isTargetHaiRemoveAtama :: [Int] -> Bool
isTargetHaiRemoveAtama hai = (not . GetHai.isExistOverHaiCount) hai && NormalForm.isNormalForm hai && Agari.isTempaiForm hai && Irreducible.isSemiIrreducible hai

-- main = print $ GetHai.nextHai [0, 0, 3]
-- main = print $ Agari.isAgariForm 0 1 [1, 0, 2] 
-- main = print $ isNonRelatedAtama [0, 2, 0, 0, 0, 0, 1, 1, 3]
-- main = print $ (map getAgariHai (Agari.removeMentsuPossibleFromList [[0, 2, 2, 0, 3, 0, 0, 0, 0]]) )

-- main = print $ isIrreducible [0, 3, 2, 2, 0, 0, 0, 0, 0]
main = printResult [0, 0, 0, 0, 0, 0, 0, 0, 10]

printResult :: [Int] -> IO ()
printResult hai = do
    -- if isTargetHaiRemoveAtama hai then print hai else return ()
    if isTargetHai hai then print hai else return ()
    if hai == [10, 0, 0, 0, 0, 0, 0, 0, 0] then return () else printResult $ GetHai.nextHai hai




