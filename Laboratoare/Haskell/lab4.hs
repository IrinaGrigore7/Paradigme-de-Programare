
type Matrix = [[Integer]] 

-- 1. Write a function parsem which takes a string and parses it to a Matrix. In the string, the columns are separated by whitespace and lines - by '\n'.
--parsem :: String -> Matrix

parsem :: String -> Matrix
parsem str = map g (map f (lines str))
			where
				f x = words x
				g [] = []
				g (x:xs) = (read x :: Integer) : g xs

--2. Write a function that converts a matrix to a string encoded as illustrated in the previous exercise. 

toString :: Matrix -> String
toString l = foldl (\acc x -> acc++x) "" (map f l)
			where
				f [] = []++"\n"
				f (x:xs) = (show x)++ " " ++ (f xs) 

--3. Add the following to your code. Test the function displaymat. 
displaymat = putStrLn . toString

--4. Write a function that computes the scalar product with an integer: 
vprod :: Integer -> Matrix -> Matrix
vprod v l = map f l
		where
			f [] = []
			f (x:xs) = (x*v) : f xs

--5. Write a function which adjoins two matrices by extending rows: 
hjoin :: Matrix -> Matrix -> Matrix
hjoin [] [] = []
hjoin l1 l2 = [(head l1)++(head l2)]++(hjoin (tail l1) (tail l2)) 

-- 6. Write a function which adjoins two matrices by adding new rows:
vjoin :: Matrix -> Matrix -> Matrix
vjoin l1 l2 = l1++l2 

-- 7. Write a function which adds two matrices. 
msum :: Matrix -> Matrix -> Matrix
msum [] [] = []
msum (x:xs) (y:ys) = [zipWith (+) x y]++(msum xs ys)

--8. Write a function which computes the transposition of a matrix: 
--tr :: [[a]] -> [[a]]
tr ([]:_) = []
tr m = (map head m):(tr (map tail m))

--9. Write a function which computes the vectorial product of two matrices. 
--mprod :: Matrix -> Matrix -> Matrix
mprod m1 m2 = map makeline m1
				where
					makeline li = map makecolumn (tr m2)
					where
						makecolumn cj = foldr (+) 0 (zipWith (*) li cj)