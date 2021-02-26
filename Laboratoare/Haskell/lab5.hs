--1. Define the type list with elements of type Integer: 
data IntList = Void | Cons Integer IntList

-- 2. Define a function which computes the sum of elements of such a list:
isum :: IntList -> Integer
isum Void = 0
isum (Cons elem list) = elem + (isum list)  

--3. Define type polymorfic type List a encoding lists with elements of type a: 
data List a = VoidA | ConsA a (List a) deriving Show

--4. Define a function which converts IntList lists to List Integer lists: 
to_poly_list :: IntList -> List Integer
to_poly_list Void = VoidA
to_poly_list (Cons elem list) = ConsA elem (to_poly_list list)

--5. Define a function which displays lists. What type will this function have? 
show_list :: Show a => List a -> String 
show_list VoidA = "[]"
show_list (ConsA x l) = (show x)++":"++(show_list l)

data Tree a = VoidT | Node (Tree a) a (Tree a) deriving Show

--6. Implement the function flatten: 
flatten :: Tree a -> List a
flatten VoidT = VoidA
flatten (Node s k d) = ConsA k (app (flatten d) (flatten s))

--7. Define list concatenation over type List a: 
app :: (List a) -> (List a) -> (List a)
app l1 VoidA = l1
app l1 (ConsA e2 l2) = ConsA e2 (app l1 l2)

--8. Define the function tmap which is the Tree a correspondent to map::(a→b) → [a] → [b]. 
tmap :: (a -> b) -> Tree a -> Tree b
tmap f VoidT = VoidT
tmap f (Node s k d) = Node (tmap f s) (f k) (tmap f d)

--9. Define the function tzipWith: 
tzipWith :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
tzipWith f _ VoidT = VoidT
tzipWith f VoidT _ = VoidT
tzipWith f (Node s1 k1 d1) (Node s2 k2 d2) = Node (tzipWith f s1 s2) (f k1 k2) (tzipWith f d1 d2)

-- 10. Define the function tfoldr: 
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc VoidT = acc
tfoldr op acc (Node l k r) = tfoldr op (op k (tfoldr op acc l)) r

--11. Implement the flattening function using tfoldr:
--tflatten :: Tree a -> List a
tflatten VoidT = VoidA
tflatten tree = tfoldr (\x acc -> app acc (ConsA x VoidA)) VoidA tree