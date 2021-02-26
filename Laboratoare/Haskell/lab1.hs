import Data.List

-- ex1
f1 :: Integer
f1 = 5

--ex2
f2 :: a -> b -> a
f2 x y = x

-- ex3
myand :: Bool -> Bool -> Bool
myand x y = x && y

-- ex5
f5 :: Bool -> a -> a -> a
f5 expr x y 
	| expr == True = x
	| otherwise = y

-- ex6
f6 :: Integer -> Integer -> Integer -> Integer
f6 x y z 
	| (x > y) && (x > z) = x
	| (y > z) = y
	| otherwise = z

--ex7
f7 :: Bool -> a -> a -> a
f7 x y z = if x then y else z

-- ex11
f11 l = head (reverse (sort l))

-- ex12
f12 [] = []
f12 l = (last l):(f12 (init l))

-- ex13
f13 n l
	| n == 1 = odd (last l)
	| otherwise = f13 (n - 1) (init l)

-- ex14
f14 l = foldl (+) 0 l

-- ex15
f15 list
	| length (filtrare list) == 0 = True
	| otherwise = False
	where
		filtrare list = filter (\x -> x == False) list

-- ex16
f16 l = filter odd l

-- ex17
f17 l = map g l
	where
		g x
			| x == True = 1
			| otherwise = 0

-- ex19
f19 l = foldl (+) 0 (f17 l)
f19' l = length (filter (\x -> x == True) l)

-- ex20
insertionSort :: [Integer] -> [Integer]
insertSorted x [] = [x]
insertSorted x list = if x <= (head list) then x:list
                       else (head list):(insertSorted x (tail list))

insertionSort [x] = [x]
insertionSort l = insertSorted (head l) (insertionSort (tail l))