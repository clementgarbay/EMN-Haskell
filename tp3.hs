import Data.List
import Data.Ord

-- ZF expressions / liste en comprehension

rectangle :: [(Int,Int)]
rectangle = [ (i,j) | i <- [1..n], j <- [1..m]]
            where n = 4
                  m = 5

triangle :: [(Int,Int)]
triangle = [ (i,j) | i <- [1..n], j <- [1..m], j<=i]
            where n = 4
                  m = 5


triangle' :: [(Int,Int)]
triangle' = [ (i,j) | i <- [1..n], j <- [1..i]]
            where n = 4
                  m = 5

myQSort :: Ord a => [a] -> [a]
myQSort (x:xs) = myQSort [ e | e <- xs, e<=x] ++ [x] ++ myQSort [ e | e <- xs, e>x]
myQSort []     = []


-- but du TP : trouver les solutions pour le compte est bon

-- sous listes et permutations pour considerer les nombres utilises dans le calcul

-- sous liste

-- deja vu au bloc 2
sousListes :: [a] -> [[a]]
sousListes []     = [[]]
sousListes (x:xs) = ys ++ map (x:) ys
    where ys = sousListes xs

injections :: a -> [a] -> [[a]]
injections x (y:ys) = (x:y:ys) : map (y:) (injections x ys)
injections x []     = [[x]]

permuts :: [a] -> [[a]]
permuts (x:xs) = concat (map (injections x) (permuts xs))
permuts []     = [[]]

permSousListes :: [a] -> [[a]]
permSousListes xs = [zs | ys <- sousListes xs, not (null ys), zs <- permuts ys]

partitionStricte :: [a] -> [([a],[a])]
partitionStricte [x1,x2] = [([x1],[x2])]
partitionStricte (x:xs)  = ([x],xs): [ (x:ls,rs) | (ls,rs) <- partitionStricte xs ]

-- I) generate and test (brute force)

data Op = Add | Sub | Mul | Div deriving (Eq, Enum) -- Show

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

validOp :: Op -> Int -> Int -> Bool
validOp Sub x y = x>y
validOp Div x y = x `mod` y == 0
validOp _   _ _ = True

evalOp :: Op -> Int -> Int -> Int
evalOp Add i1 i2 = i1+i2
evalOp Sub i1 i2 = i1-i2
evalOp Mul i1 i2 = i1*i2
evalOp Div i1 i2 = div i1 i2

data Exp = Val Int | App Op Exp Exp
            deriving Show

-- step1: enumerate expressions
exps :: [Int] -> [Exp]
exps [n] = [Val n]
exps ns  =
    [ App o g d
      | (gs,ds) <- partitionStricte ns
    , g <- exps gs
    , d <- exps ds
    , o <- [Add .. Div]
    ]

-- step2: filter out invalid expressions

evalExp :: Exp -> Int
evalExp (App o g d) = evalOp o (evalExp g) (evalExp d)
evalExp (Val n) = n

validExp :: Exp -> Bool
validExp (Val n) = n>0
validExp (App o g d) = validExp g && validExp d && validOp o (evalExp g) (evalExp d)

solutions :: [Int] -> Int -> [Exp]
solutions nombres cible =
    let ns = permSousListes nombres
        es = concat (map exps ns)
        es' = filter validExp es
        es'' = filter (\e -> evalExp e == cible) es'
    in es''

test1 = solutions [1,3,7,10,25,50] 765
-- 780 solutions (207.32 secs)


-- II) fusionner la generation et le filtrage des expressions invalides

exps2 :: [Int] -> [Exp]
exps2 [n] = [Val n]
exps2 ns  =
    [ App o g d
      | (gs,ds) <- partitionStricte ns
    , g <- exps2 gs
    , d <- exps2 ds
    , o <- [Add .. Div]
    , validOp o (evalExp g) (evalExp d)
    ]

solutions2 :: [Int] -> Int -> [Exp]
solutions2 nombres cible =
    let ns = permSousListes nombres
        es = concat (map exps2 ns)
        es' = filter (\e -> evalExp e == cible) es
    in es'

test2 = solutions2 [1,3,7,10,25,50] 765
-- 780 solutions (31.56 secs)


-- III) memoiser l'evaluation

data Exp' = Val' Int | App' Op Exp' Exp' Int
            -- deriving Show

evalExp' :: Exp' -> Int
evalExp' (Val' n) = n
evalExp' (App' _ _ _ n) = n

validExp' :: Exp' -> Bool
validExp' (Val' n) = n>0
validExp' (App' o g d n) = validExp' g && validExp' d && validOp o (evalExp' g) (evalExp' d)

exps3 :: [Int] -> [Exp']
exps3 [n] = [Val' n]
exps3 ns  =
    [ App' o g d (evalOp o (evalExp' g) (evalExp' d))
      | (gs,ds) <- partitionStricte ns
    , g <- exps3 gs
    , d <- exps3 ds
    , o <- [Add .. Div]
    , validOp o (evalExp' g) (evalExp' d)
    ]

solutions3 :: [Int] -> Int -> [Exp']
solutions3 nombres cible =
    let ns = permSousListes nombres
        es = concat (map exps3 ns)
        es' = filter (\e -> evalExp' e == cible) es
    in es'

test3 = solutions3 [1,3,7,10,25,50] 765
-- 780 solutions (14.69 secs)


-- IV) exploiter des proprietes arithmetiques

-- pour reduire l'espace de recherche on ajoute les regles :
-- - pas de multiplication par 1
-- - pas de division par 1
-- - addition et multiplication commutatives (ne considerer qu'un sens (quand les deux operandes sont differents))
validOp' :: Op -> Int -> Int -> Bool
validOp' Add x y = x >= y
validOp' Sub x y = x > y
validOp' Mul x 1 = False
validOp' Mul 1 y = False
validOp' Mul x y = x >= y
validOp' Div x 1 = False
validOp' Div x y = x `mod` y == 0

exps4 :: [Int] -> [Exp']
exps4 [n] = [Val' n]
exps4 ns  =
    [ App' o g d (evalOp o (evalExp' g) (evalExp' d))
      | (gs,ds) <- partitionStricte ns
    , g <- exps4 gs
    , d <- exps4 ds
    , o <- [Add .. Div]
    , validOp' o (evalExp' g) (evalExp' d)
    ]

solutions4 :: [Int] -> Int -> [Exp']
solutions4 nombres cible =
    let ns = permSousListes nombres
        es = concat (map exps4 ns)
        es' = filter (\e -> evalExp' e == cible) es
    in es'

test4 = solutions4 [1,3,7,10,25,50] 765
-- 49 solutions (2.06 secs)


-- nombre de solutions

nombreDeSolutions3 = length test3
nombreDeSolutions4 = length test4

-- V) ne retourner qu'une solution exacte ou bien la plus proche

solutions5 :: [Int] -> Int -> [(Exp', Int)]
solutions5 nombres cible =
    let ns  = permSousListes nombres
        es  = concat (map exps4 ns)
        es' = sortBy (\x y -> compare (abs (evalExp' x - cible)) (abs (evalExp' y - cible))) es
        -- es' = sortOn (\e -> abs (cible - evalExp' e)) es
        res = map (\e -> (e, evalExp' e)) es'
    in res

test5 = solutions5 [1,3,7,10,25,50] 765
test6 = solutions5 [1,3,7,10,25,50] 831

-- VI) affichez les expressions sous forme infixe en evitant des parentheses inutiles
instance Show Exp' where
  -- show :: Exp' -> String
  show (Val' n) = show n
  show (App' o g d n) = "(" ++ (show g) ++ " " ++ (show o) ++ " " ++ (show d) ++ ")"

-- VII) generalisez certaines fonctions avec de l'ordre superieur afin de reduire la duplication de code dans ce programme

-- misc : cherchez les solutions avec le moins d'operations en priorite

testt1 = listToBinTree "abcde"
testt2 = fst (numberLeaves 0 testt1)

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

listToBinTree :: [a] -> BinTree a
listToBinTree [a] = Leaf a
listToBinTree as  = let (ls,rs) = splitAt(length as `div` 2) as in Node (listToBinTree ls) (listToBinTree rs)

numberLeaves :: Int -> BinTree a -> (BinTree (a,Int), Int)
numberLeaves n (Leaf i) = (Leaf (i,n), n+1)
numberLeaves n (Node l r) =
  let (l', n1) = (numberLeaves n l)
      (r', n2) = (numberLeaves n1 r)
  in (Node l' r', n2)
