
module RemoveHai(removeHai, repeatRemoveForm, removeMentsuPossibleFromList, removeAtamaPossibleFromList, removeAtamaConnectedShuntsuPossibleFromList) where

type Hai = [Int]
type Atama = Int
type Mentsu = Int

shuntsuList :: [Hai]
shuntsuList =
    [
        [1, 1, 1, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 1, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 1, 1, 1, 0, 0, 0],
        [0, 0, 0, 0, 1, 1, 1, 0, 0],
        [0, 0, 0, 0, 0, 1, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 1, 1, 1]
    ]

kotsuList :: [Hai]
kotsuList =
    [
        [3, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 3, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 3, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 3, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 3, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 3, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 3, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 3, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 3]
    ]

atamaList :: [Hai]
atamaList =
    [
        [2, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 2, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 2, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 2, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 2, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 2, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 2, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 2, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 2]
    ]

atamaConnectedShuntsuList :: [Hai]
atamaConnectedShuntsuList =
    [
        [3, 1, 1, 0, 0, 0, 0, 0, 0],
        [0, 3, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 3, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 3, 1, 1, 0, 0, 0],
        [0, 0, 0, 0, 3, 1, 1, 0, 0],
        [0, 0, 0, 0, 0, 3, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 3, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 3],
        [0, 0, 0, 0, 0, 1, 1, 3, 0],
        [0, 0, 0, 0, 1, 1, 3, 0, 0],
        [0, 0, 0, 1, 1, 3, 0, 0, 0],
        [0, 0, 1, 1, 3, 0, 0, 0, 0],
        [0, 1, 1, 3, 0, 0, 0, 0, 0],
        [1, 1, 3, 0, 0, 0, 0, 0, 0]
    ]

removeHai :: Hai -> Hai -> Hai
removeHai [] [] = []
removeHai (x:_) [] = [x]
removeHai [] (y:_) = [-y]
removeHai (x:xs) (y:ys) = (x - y) : removeHai xs ys

isCorrectHai :: Hai -> Bool
isCorrectHai [] = True
isCorrectHai (x:xs) = if x >= 0 then isCorrectHai xs else False

removePossible :: [Hai] -> Hai -> [Hai]
removePossible list hai = filter isCorrectHai $ map (removeHai hai) list

removePossibleFromList :: [Hai] -> [Hai] -> [Hai]
removePossibleFromList list hais = foldl (++) [] $ map (removePossible list) hais

removeAtamaConnectedShuntsuPossibleFromList :: [Hai] -> [Hai]
removeAtamaConnectedShuntsuPossibleFromList hais = removePossibleFromList atamaConnectedShuntsuList hais

removeAtamaPossibleFromList :: [Hai] -> [Hai]
removeAtamaPossibleFromList hais = removePossibleFromList atamaList hais

removeMentsuPossibleFromList :: [Hai] -> [Hai]
removeMentsuPossibleFromList hais = removePossibleFromList (kotsuList ++ shuntsuList)  hais

repeatRemoveForm :: Int -> ([Hai] -> [Hai]) -> [Hai] -> [Hai]
repeatRemoveForm 0 _ hais = hais
repeatRemoveForm x f hais = repeatRemoveForm (x - 1) f $ f hais

