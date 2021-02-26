s1 1 = True
s1 2 = True
s1 _ = False

s2 x
	| x `mod` 2 == 0 = True
	| otherwise = False

s3 _ = False

mem :: (Integer -> Bool) -> Integer -> Bool
mem set x = set x

--48. Define the set {2n∣n∈N} . 
pow 1 = True
pow 0 = False
pow x = pow (x / 2)

--49. Define the set of natural numbers. 
nat x = (x > 0) && (isInt x)

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

--50. Implement the intersection of two sets. Use lambdas. 
intersection = (\s1 s2 x -> (s1 x) && (s2 x))

--51. Write intersection in another way, (without using lambdas). 
intersection' s1 s2 x = (s1 x) && (s2 x)

--52. Write a function which takes a list of integers, and returns the set which contains them. 
toSet :: [Integer] -> (Integer -> Bool)
toSet l x = elem x l

--53. Implement a function which takes a list of sets and computes their intersection. 
capList (x:y:[]) n = intersection x y n
capList (x:xs) n = (x n) && (capList xs n)

filter1 :: (Integer -> Bool) -> [Integer] -> [Integer]
filter1 _ [] = []
filter1 p (x : xs) = if((p x) == True)
                        then x : (filter1 p xs)
                        else filter1 p xs 


-- 56. Test the function map::(a→b) → [a] → [b] from Haskell. Implement it. 
map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 func (x : xs) = (func x) : (map1 func xs)

-- 57. Solve exercise 17. from Lab 1 using map. 

boolToInteger :: Bool -> Integer
boolToInteger any 
    | any == False = 0
    | otherwise = 1

-- 58. Solve exercise 27. from Lab 2 using map and filter. (Hint. Pattern
-- matching can be used in lambdas. Use fst and snd to introspect pairs.) 

f58 str l = map fst_letM (filter (\(x,y) -> x == str) l) where
    fst_letM (s, []) = []
    fst_letM (s, x:xs) = if (head x) == 'M' then x:fst_letM(s, xs) else fst_letM(s, xs)

-- 59. Solve exercise 31. from Lab 2 using map and filter. 
--  f [("Dan Matei Popovici",9),("Mihai",4),("Andrei Alex",6)] =
     --   [(["Dan", "Matei", "Popovici"],10),(["Andrei,Alex"],6)]

f59 list = map g (filter (\(x,y) -> y >= 5) list) where
                g (st, dr)
                    | (length (words st)) >= 3 = ((words st), dr + 1)
                    | otherwise = ((words st), dr)

--60. Write a function which appends a list of lists of integers: 
app :: [[Integer]] -> [Integer] 
app [] = []
app (x:xs) = x++(app xs)

--62. Implement foldr (see lecture). 
myFoldr op acc [] = acc
myFoldr op acc l = myFoldr op (op acc (last l)) (init l)

--63. Implement foldl (see lecture). 
myFoldl op acc [] = acc
myFoldl op acc l = myFoldl op (op acc (head l)) (tail l)

--64. Implement list concatenation using a fold. 
concatenation l1 l2 = foldr (:) l2 l1

--65. Implement list reversal using a fold.
rev l = foldr (\x acc -> acc++[x]) [] l

--66. Implement the function from exercise 52 using folds. (Hint: thing about what the accumulator should hold) 
toSet_fold :: [Integer] -> (Integer -> Bool)
toSet_fold list elem = foldr (\x acc -> (x == elem) || acc) False list

--67. Implement exercise 53 using a fold. 
list_intersect_fold :: [Integer->Bool] -> Integer -> Bool
list_intersect_fold sets elem = foldr (\set acc -> (set elem) && acc) True sets