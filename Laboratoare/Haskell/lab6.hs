type Dict = [(String,Integer)]

--1.Implement the function valueOf which takes a key and a dictionary, and returns the associated value. It is guaranteed that the value exists. 
valueOf :: String -> Dict -> Integer
valueOf str ((s, d):xs)
	| str == s = d
	| otherwise = valueOf str xs

--2. Implement the function ins which takes a key s, a value i, a dictionary d and updates the value of s in the dictionary,
ins :: String -> Integer -> Dict -> Dict
ins str val [] = [(str, val)]
ins str val ((s, d):xs)
	| str == s = (s, val):xs
	| otherwise = (s, d):(ins str val xs)

--3. Consider the type PExpr of program expressions defined in the lecture. Implement a function for showing PExpr values: 
data PExpr = Val Integer |
             Var String  |
             PExpr :+: PExpr 

show_pexpr :: PExpr -> String  
show_pexpr (Val x) = show x 
show_pexpr (Var x) = show x
show_pexpr (e :+: e') = (show_pexpr e) ++ " + " ++ (show_pexpr e')

instance Show PExpr where
    show = show_pexpr

--For instance: eval_pexpr [(“x”,2),(“y”,1)] ( (Var “x”) :+: (Var “y”) ) returns 3. 
eval_pexpr :: Dict -> PExpr -> Integer
eval_pexpr ((s,d):xs) (Var a) 
	| s == a = d
	| otherwise = eval_pexpr xs (Var a)
eval_pexpr dict (Val a) = a
eval_pexpr dict (e1 :+: e2) = (eval_pexpr dict e1) + (eval_pexpr dict e2)

--5. Consider the type BExpr of boolean expressions defined in the lecture. Implement a function for displaying values of BExpr: 
