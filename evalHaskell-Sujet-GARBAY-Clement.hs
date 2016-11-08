
-- NOM : GARBAY
-- Prenom : Clément
-- TP NOTE HASKEL FIL A2
-- detection de n-gram commun a deux documents 

-- fonctions autorisees : (.), (++), (==), (/=), (<), (>=), concat, drop, dropWhile, elem, filter, foldr, head, init, isSpace, last, length, map, not, null, tail, take, takeWhile, toUpper

import Data.Char (toUpper,isSpace)

-- Q1 les prefixes de longueur croissante
testQ1 = prefixesRec "abc" == ["","a","ab","abc"]
prefixesRec :: [a] -> [[a]]
prefixesRec (x:xs) = [] : map (x:) (prefixesRec xs)
prefixesRec []     = [[]]

-- Q2 les suffixes de longueur croissante
testQ2 = suffixesRec "abc" == ["abc","bc","c",""]
suffixesRec :: [a] -> [[a]]
suffixesRec (x:xs) = (x:xs) : suffixesRec xs
suffixesRec []     = [[]]

-- Q3 les segments
testQ3 = segmentsNonRec "abc" == ["","a","ab","b","abc","bc","c"]
segmentsNonRec :: [a] -> [[a]]
segmentsNonRec xs = [] : foldr (\x acc -> filter (not . null) (suffixesRec x) ++ acc) [] (prefixesRec xs)

-- Q4 supprime les doublons
testQ4 = listToSetRec ["","a","ab","b","abc","bc","c","abcb","bcb","cb","b","abcbc","bcbc","cbc","bc","c"] == ["","a","ab","b","abc","bc","c","abcb","bcb","cb","abcbc","bcbc","cbc"]
listToSetRec :: Eq a => [a] -> [a]
listToSetRec (x:xs) = x : listToSetRec (filter (/=x) xs)
listToSetRec []     = []

-- Q5 intersection ensembliste
testQ5 = interNonRec "abc" "bccad" == "bcca"
interNonRec :: Eq a => [a] -> [a] -> [a]
interNonRec e = filter (`elem` e)

-- Q6 tri rapide decroissant selon la metrique passee en parametre
testQ6 = qSortByRec length  ["","a","ab","b","abc","bc","c","abcb","bcb","cb","abcbc","bcbc","cbc"] == ["abcbc","abcb","bcbc","abc","bcb","cbc","ab","bc","cb","a","b","c",""]
qSortByRec :: (a -> Int) -> [a] -> [a]
qSortByRec f (x:xs) = qSortByRec f [e | e <- xs, f e > f x] ++ [x] ++ qSortByRec f [e | e <- xs, f e <= f x]
qSortByRec f []     = []

-- Q7 test is le premier argument est le prefixe du second
testQ7 = isPrefixNonRec ["abcbc","abcb","bcbc"] ["abcbc","abcb","bcbc","abc","bcb","cbc","ab","bc","cb","a","b","c",""] == True
testQ7' = isPrefixNonRec ["abcb","bcbc"] ["abcbc","abcb","bcbc","abc","bcb","cbc","ab","bc","cb","a","b","c",""] == False
isPrefixNonRec :: Eq a => [a] -> [a] -> Bool
isPrefixNonRec e1 e2 = e1 == take (length e1) e2

-- Q8 remplace dans le deuxieme arg les occurrences du premier en les passant en majuscule
testQ8 = replaceRec [" ","texte"," ","a"," ","analyser"] ["voici"," ","un"," ","texte"," ","a"," ","analyser"] == ["voici"," ","un"," ","TEXTE"," ","A"," ","ANALYSER"]
replaceRec :: [String] -> [String] -> [String]
replaceRec e (x:xs) | x `elem` e = map toUpper x : replaceRec e xs
                    | otherwise  = x : replaceRec e xs
replaceRec e []     = []

--Q9 coupe une chaine a chaque espace (les espaces multiples sont ramenes a un seul)
testQ9 = tokenizeRec "voici un  texte a   analyser" == ["voici"," ","un"," ","texte"," ","a"," ","analyser"]
tokenizeRec :: String -> [String]
tokenizeRec (x:xs) | isSpace x = " " : tokenizeRec (dropWhile isSpace xs)
                   | otherwise = (x:takeWhile (not . isSpace) xs) : tokenizeRec (dropWhile (not . isSpace) xs)
tokenizeRec []     = []

--Q10 colle les chaines
testQ10 = unTokenizeNonRec ["voici"," ","un"," ","TEXTE"," ","A"," ","ANALYSER"] == "voici un TEXTE A ANALYSER"
unTokenizeNonRec :: [String] -> String
unTokenizeNonRec = foldr (++) ""
-- unTokenizeNonRec = concat

-- met en majuscule la sequence la plus longue commune (sans tenir compte du nombre d'espaces)
mainTest =
    let ref = tokenizeRec "voici un petit texte a analyser"
        plagiat = tokenizeRec "voici un  texte a   analyser"
        longestNGram = head (qSortByRec length (listToSetRec (interNonRec (segmentsNonRec ref) (segmentsNonRec plagiat))))
    in unTokenizeNonRec (replaceRec longestNGram plagiat) == "voici un TEXTE A ANALYSER"
       