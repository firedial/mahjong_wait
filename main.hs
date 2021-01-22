import Debug.Trace
import qualified GetHai
import qualified Agari
import qualified NormalForm
import qualified Irreducible

isTargetForm :: [Int] -> Bool
isTargetForm hai = and $ map (\f -> f hai) [GetHai.isNotExistOverHaiCount, NormalForm.isNormalForm, Agari.isTempaiForm, Irreducible.isIrreducible]

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

