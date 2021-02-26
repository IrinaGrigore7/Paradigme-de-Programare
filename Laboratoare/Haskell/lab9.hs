--1. Consider the following recurrence scheme described informally below. 
--Use it to build the sequence 1,2,…,n!,…
factorial n = n:(map (*n) (factorial (n + 1)))

--2. Define the sequence 1,1/2,…,1/k!,…
my_seq = map (\x -> 1/x) (factorial 1)

--3. Write a function which takes a sequence (an)n≥0 and computes the sequence 
--(sn)n≥0 where sk=∑k≥0ak . Use a strategy similar to that from exercise 1. 
p_sum (h:t) = h:(map (+h) (p_sum t))

--4. Write the stream of approximations of e
val_e = map (+1) (p_sum my_seq)

--5. Write a function which takes a value d , a sequence of approximations 
--(an)n≥0 and returns that value ak from the sequence which satisfies the condition ∣ak−ak+1∣≤d
take_tol e (x:y:xs)
	|abs (x-y) < e = x
	|otherwise = take_tol e (y:xs)

--6. Write a function which takes an f , a value a0 and computes the sequence a0,f(a0),f(f(a0)),…
multi_call f a = a:(map f (multi_call f a))

--7. The sequence (an)n≥0 defined as ak+1=(ak+nak)/2 , will converge to √n as 
--k approaches infinity. Use it to write a function which approximates √n within 3 decimals. 
sqrt_aprox n = multi_call (\x -> (x + n/x) / 2) 10