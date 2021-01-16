import Debug.Trace
import qualified GetHai
import qualified Agari
import qualified NormalForm
import qualified Irreducible

isTargetForm :: [Int] -> Bool
isTargetForm hai = and $ map (\f -> f hai) checkFunctions
    where
        checkFunctions = if isRegularForm then baseCheckFunctions ++ [Irreducible.isIrreducible] else baseCheckFunctions
        isRegularForm = mod (sum hai) 3 == 1
        baseCheckFunctions = [(not . GetHai.isExistOverHaiCount), NormalForm.isNormalForm, Agari.isTempaiForm, Irreducible.isSemiIrreducible]

main = do
    n <- readLn
    getList n 

getList :: Int -> IO()
getList n =
    printResult [0, 0, 0, 0, 0, 0, 0, 0, n]

printResult :: [Int] -> IO ()
printResult hai = do
    if isTargetForm hai then print hai else return ()
    if head hai == sum hai then return () else printResult $ GetHai.nextHai hai

