--1. Enroll the type Extended shown below, in class Eq:
data Extended = Infinity | Value Integer 
instance Eq Extended where
	Infinity == Infinity = True
	Value v == Value v' = v == v'
	_ == Infinity = False
	Infinity == _ = False

--2. Enroll the type Formula a in class Eq: 
data Formula a = Atom a |
                Or (Formula a) (Formula a) |
                And (Formula a) (Formula a) |
                Not (Formula a)

instance Eq a => Eq (Formula a) where
	Atom a == Atom b = a == b
	Or a b == Or c d = (a == c) && (b == d)
	And a b == And c d = (a == c) && (b == d)
	Not a == Not b = a == b
	_ == _ = False

--3. Enroll the type Formula a in class Num: 
data ExtendedPlus = PInfinity | MInfinity | ValueP Integer deriving Show
instance Num ExtendedPlus where
	--(+) (ValueP a) (ValueP b) = a + b
	(PInfinity) + (PInfinity) = PInfinity
	(MInfinity) + (PInfinity) = PInfinity
	(MInfinity) + (MInfinity) = MInfinity
	PInfinity + _ = PInfinity
	_ + PInfinity = PInfinity
	MInfinity + _ = MInfinity
	_ + MInfinity = MInfinity
	ValueP a + ValueP b = ValueP (a + b)
	

	ValueP a - ValueP b = ValueP (a - b)




-- Design a class called Eval which contains a unique evalmethod. How should the class parameters be?
--Enrol types PExpr, BExpr and Prog in this class, thus eliminating the need for multiple evaluation function names.
{-
data PExpr = Val Integer |
             Var String  |
             PExpr :+: PExpr 

data BExpr = PExpr :==: PExpr | 
            PExpr :<: PExpr |
            Not BExpr |
            BExpr :&&: BExpr 

data Prog = PlusPlus Var |       
            Var :=: PExpr |     
            DeclareInt Var |     
            Begin Prog Prog |     
            While BExpr Prog |     
            If BExpr Prog Prog 
 
type Var = String


class Eval (a b) where
	eval :: Dict -> a -> b

instance Eval PExpr Integer where
	eval_pexpr dict (Val x) = x
    eval_pexpr ((l, r):xs) (Var x)
	| l == x = r
	| otherwise = eval_pexpr xs (Var x)
    eval_pexpr dict (e :+: e') = (eval_pexpr dict e) + (eval_pexpr dict e')

instance Eval Bexpr Bool where
	eval_bexpr :: Dict -> BExpr -> Bool
	eval_bexpr dict (e :==: e')
	| (eval_pexpr dict e) == (eval_pexpr dict e') = True
	|otherwise = False
	eval_bexpr dict (e :<: e')
	| (eval_pexpr dict e) < (eval_pexpr dict e') = True
	| otherwise = False
	eval_bexpr dict (Not e) 
	| (eval_bexpr dict e) == True = False
	| otherwise = True
	eval_bexpr dict (v :&&: v') = (eval_bexpr dict v) && (eval_bexpr dict v') 

instance Eval Prog Dict where
	eval_prog :: Dict -> Prog -> Dict
	eval_prog ((s, d):xs) (PlusPlus x)
	| x == s = (s, d + 1):xs
	| otherwise = (s, d):(eval_prog xs (PlusPlus x)) 
	eval_prog ((s, d):xs) (x :=: e)
	| x == s = (s, (eval_pexpr ((s, d):xs)) e):xs
	| otherwise = (s, d):(eval_prog ((s, d):xs) (x :=: e))
	eval_prog dict (DeclareInt x) = (x, 0):dict
-}