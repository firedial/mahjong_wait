import Data.List
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
    if isTargetForm hai then printDetail hai else return ()
    if head hai == sum hai then return () else printResult $ GetHai.nextHai hai


printDetail :: [Int] -> IO ()
printDetail hai = 
    putStrLn $ intercalate " " 
        [ show regularCount
        , show count
        , "|"
        , stringHai
        , "|"
        , stringAgariHaiCount
        , "|"
        , show len
        , if isRightClosed hai agariHaiCount then "o" else "x"
        , if isLeftClosed hai agariHaiCount then "o" else "x"
        , stringSendableForm
        , if len == 8 then "?" else "-"
        , "|"
        , show $ agariKindCount + if isSendableForm then 1 else 0
        , show $ agariHaiAllCount + if isSendableForm then 2 else 0
        ]
    where
        stringHai = intercalate " " (map show hai)
        count = sum hai
        len = length $ NormalForm.getMiddleHai hai
        isRegular = mod (sum hai) 3 == 1
        regularCount = if not isRegular then count + 2 else count
        agariHai = Agari.getAgariHai hai
        agariHaiCount = getAgariHaiCount hai agariHai
        agariHaiAllCount = sum agariHaiCount
        agariKindCount = length $ filter (\x -> x /= 0) agariHaiCount
        stringAgariHaiCount = intercalate " " $ getAgariHaiCountWithDetail hai agariHai
        isSendableForm = Agari.isAgariForm hai
        stringSendableForm = if isRegular then "-" else if isSendableForm then "o" else "x"

getAgariHaiCount :: [Int] -> [Bool] -> [Int]
getAgariHaiCount [] [] = []
getAgariHaiCount _ [] = []
getAgariHaiCount [] _ = []
getAgariHaiCount (x:xs) (y:ys) = (if not y then 0 else 4 - x) : getAgariHaiCount xs ys

getAgariHaiCountWithDetail :: [Int] -> [Bool] -> [String]
getAgariHaiCountWithDetail [] [] = []
getAgariHaiCountWithDetail _ [] = []
getAgariHaiCountWithDetail [] _ = []
getAgariHaiCountWithDetail (x:xs) (y:ys) = (if not y then "-" else show (4 - x)) : getAgariHaiCountWithDetail xs ys

isRightClosed :: [Int] -> [Int] -> Bool
isRightClosed [] [] = False
isRightClosed _ [] = False
isRightClosed [] _ = False
isRightClosed (x:xs) (y:ys) = if x > 0 then True else if y > 0 then False else isRightClosed xs ys

isLeftClosed :: [Int] -> [Int] -> Bool
isLeftClosed hai wait = isRightClosed (reverse hai) (reverse wait)


