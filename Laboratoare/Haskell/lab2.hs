import Data.List
--21. Extract the fourth element from a list of integers 
elems _ [] = 0
elems n (x:xs)
	| n == 1 = x
	| otherwise = elems (n - 1) xs

--24. Write a function which filters out all negative numbers from a list. Use patterns. 
filters1 [] = []
filters1 (x:xs)
	| x >= 0 = x:(filters1 xs)
	| otherwise = filters1 xs

filters2 [] = []
filters2 (x:xs) = filter (>0) (x:xs)

--26. What is the type of the function: 
f26 :: Char -> [Char] -> [Char]
f26 'a' _ = []
f26 x y = x:y

--27. Let f “321CB” [(“321CB”, [“Matei”, “Andrei”, “Mihai”]), (“322CB”,[“George, Matei”])] = [“Matei”, “Mihai”]
f27 grp ((s, d):xs) 
	| s == grp = extract d
	| otherwise = f27 grp xs
	where
		extract [] = []
		extract (y:ys)
			| (head y) == 'M' = y:(extract ys)
			| otherwise = extract ys

--28. Write a function which returns True if the third largest element from a list is positive 
f28 l 
	| g > 0 = True
	| otherwise = False
	where
		g = elems 3 (reverse (sort l))

--30. Rewrite exercise 28 via function composition: 
f30 l = (h.g) l
	where
		h x
			| x > 0 = True
			| otherwise = False
		g l = elems 3 (reverse (sort l))

--31. Write a function which takes a list of pairs - student-name and grade, 
--removes those with grades less than 5, splits the name in substrings, and adds one bonus point to people with three names. 
f31 [] = []
f31 ((s,d):xs)
	| d >= 5 = (g (s,d)):(f31 xs)
	| otherwise = f31 xs 
	where
		g (s,d)
			| length (words s) >= 3 = (words s, d+1)
			| otherwise = (words s,d)

-- f ["Matei", "Mihai"] ["Popovici","Dumitru"] = [("Matei","Popovici"), ("Mihai","Dumitru")]
f32 _ [] = []
f32 [] _ = []
f32 (x:xs) (y:ys) = (x,y):(f32 xs ys)

-- f ["Matei", "Mihai"] ["Popovici","Dumitru"] = ["MPopovici", "MDumitru"]
f33 _ [] = []
f33 [] _ = []
f33 (x:xs) (y:ys) = ((head x):y):(f33 xs ys)

--Implement a function for transforming lists into pairs, in exercise 32. and use it in the infix form. 
f34 :: ([String] -> [String] -> [(String, String)]) -> [String] -> [String] -> [(String, String)]
f34 list1 list2 = list1 `f32` list2

{-
(+) :: Num a => a -> a -> a
(&&) :: Bool -> Bool -> Bool
(++) :: [a] -> [a] -> [a]
-}

f37 l = (((++) "?").((:) '>')) l --postfixata

f37' = (('>':).(++"?")) "ceva" --infixata

-- f "Matei" = "{Matei}"
f38 :: String -> String
f38 list = "{" ++ list ++ "}"

-- 39. Use only (:), reverse and functional composition
-- . to solve exercise 38 
f39 :: String -> String
f39 list =(((:) '{') . reverse . ((:) '}').reverse) list

-- ($) :: (a -> b) -> a -> b

-- 41. f "||" "Matei" = "||Matei||"
f41 :: String -> String -> String
f41 pattern string = (++pattern) $ reverse $ (++pattern) $ reverse string

-- Write a function which extracts the third to last
-- number from a list and returns True, if that number
-- is odd (hint: the function mod may be useful)

-- Var Lab 1
f42 :: Integer -> [Integer] -> Bool 
f42 n list = if(elems n (reverse list) `mod` 2 == 0)
                    then False
                    else True

-- Var Lab 2
f42' [] = False
f42' [x] = False
f42' [x,y] = False
f42' l = penultim `mod` 2 == 1
        where penultim = last $ init $ init l -- Echivalent cu penultim = last ( init ( init l))