--1. Consider the following datatype which encodes λ-expressions: 
data LExpr = Var Char | Lambda Char LExpr | App LExpr LExpr 
instance Show LExpr where
	show (Var a) = [a]
	show (Lambda a expr) = "\0955"++[a]++"."++show(expr)
	show (App e e') = show(e)++" "++ show(e')

--2. Write a function vars which returns a list of variables used in a λ-expression: 
vars :: LExpr -> [Char]
vars (Var a) = [a]
vars (Lambda a expr) = [a]++","++(vars expr)
vars (App e e') = (vars e)++","++(vars e')

--3. Write a function reducible which tests if an expression can be reduced to another.
--Write tests first! What are the cases when an expression is reducible? 
reductible :: LExpr -> Bool
reductible (Var c) = False
reductible (Lambda c expr) = reductible expr
reductible (App e1 e2) = case e1 of 
	        Lambda _ _ -> case e2 of
	       				Var _ -> True
	       				Lambda _ _ -> True
	       				App _ _ -> reductible e2
	      	Var _ -> case e2 of
	      				Var _ -> False
	      				Lambda _ _ -> False
	      				App _ _ -> reductible e2
	      	App _ _ -> reductible e1

--4. Write a function which renames all occurrences of a variable with another, in a λ-expression:
rename :: Char -> Char -> LExpr -> LExpr
rename x ch (Var y)
	| x == y = Var ch
	| otherwise = Var y
rename x ch (Lambda c expr) = Lambda c (rename x ch expr)
rename x ch (App e1 e2) = App (rename x ch e1) (rename x ch e2)

--5. Write a function which replaces all occurrences of a variable with a λ-expression, in a λ-expression: 
replace :: Char -> LExpr -> LExpr -> LExpr
replace ch 