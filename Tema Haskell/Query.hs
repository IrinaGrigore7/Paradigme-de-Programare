module Query where
import Data.List
import UserInfo
import Rating
import Movie

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

--desparte un string dupa un anumit delimitator si imi intoarce o lista 
splitBy :: Char -> String -> [String]
splitBy delim [] = [[]]
splitBy delim (x:xs)
    | x == delim = "": g
    | otherwise = (x : (head g)):(tail g)
    where
       g = splitBy delim xs

--separa un string dupa terminatorul de linie
lines_trans :: Char -> String -> [String]
lines_trans lineSep str = splitBy lineSep str

--deoarece la finalul stringului este un '\n', atunci lines_trans va intoarce la final 
--o lista goala, de aceea folosesc init
str_line lineSep str = init (lines_trans lineSep str)

--separ fiecare linie dupa separatorul de coloana
list_lines_trans :: Char -> Char -> [String] -> [[String]]
list_lines_trans colSep lineSep [] = []
list_lines_trans colSep lineSep (x:xs) = (splitBy colSep x):(list_lines_trans colSep lineSep xs)

--imi formez o lista cu head-ul si toate intrarile din table
aux_table :: Char -> Char -> String -> [[String]]
aux_table colSep lineSep str = list_lines_trans colSep lineSep (str_line lineSep str)

--creez tabelul
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table colSep lineSep string =  Table (head (aux_table colSep lineSep string)) 
											(tail (aux_table colSep lineSep string))

user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

--formeaza un tabel ce contine maximul pe fiecare coloana
max_col :: Table -> [Int]
max_col (Table header []) = []
max_col (Table header entries) = maximum(get_size (map head entries)):
									max_col (Table header (map tail entries))

--formeaza un vector cu lungimea fiecarui element din header
get_size :: [String] -> [Int]	
get_size [] = []
get_size (x:xs) = length(x):(get_size xs)

--calculeaza maximul dintre lungimile elementelor din header si maximele pe fiecare coloana
maxim :: Table -> [Int]
maxim (Table header entries) = max_header (get_size header) (max_col (Table header entries))
	where
		max_header [] _ = []
		max_header _ [] = []
		max_header (x:xs) (y:ys)
			| x > y = x:(max_header xs ys)
			| otherwise = y:(max_header xs ys)

--determina cu cate caractere '-' trebuie bordate liniile din tabel
lines_number :: Table -> Int		
lines_number table = foldl (+) 0 (maxim table) + length(maxim table) + 1

--adauga padding 
white_spaces :: Int -> [Char]
white_spaces n
	| n > 0 = " " ++ (white_spaces(n -1)) 
	| otherwise = "|" ++ []

--string ce reprezinta bordura pentru liniile din tabel
show_lines :: Int -> [Char]
show_lines n
	| n > 0 = "-" ++ (show_lines (n -1)) 
	| otherwise = []

--formeaza string-ul corespunzator header-ului
show_header :: Table -> [Int] -> [Char]
show_header (Table [] entries) _ = []
show_header (Table _ entries) [] = []
show_header (Table (y:ys) entries) (x:xs) = y ++ (white_spaces (x - length (y))) ++
												(show_header (Table ys entries) xs)

--formeza stringul corespunzator intrarilor din tabel
show_entries :: Table -> [Int] -> [Char]
show_entries (Table head []) l = []
show_entries  (Table header (y:ys)) l = "|" ++ (show_entries_aux y l) ++ "\n" ++ 
											(show_entries (Table header ys) l)

--formeaza stringul corespunzator unei linii din tabel
show_entries_aux :: [[Char]] -> [Int] -> [Char]
show_entries_aux [] _ = []
show_entries_aux _ [] = []
show_entries_aux (x:xs) (y:ys) = x ++ (white_spaces (y - length (x))) ++ (show_entries_aux xs ys)

instance Show Table where
	show (Table header entries) = (show_lines (lines_number (Table header entries))) ++ "\n" ++ "|" ++ 
								(show_header (Table header entries) (maxim (Table header entries))) ++ 
								"\n" ++ (show_lines (lines_number (Table header entries))) ++ "\n" ++ 
								(show_entries (Table header entries) (maxim (Table header entries))) ++
								show_lines (lines_number (Table header entries)) ++ "\n"

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom table) = table

eval (Select [elem] query) = Table [elem] (get_column elem (eval query))
eval (Select fiels_list@(x:xs) query) = concat_table (Table [x] (get_column x (eval query))) 
											(eval (Select xs query))
	
eval (SelectLimit [elem] n (Atom table)) = Table [elem] (take (fromIntegral n) (get_column elem table))
eval (SelectLimit fiels_list@(x:xs) n (Atom table)) = concat_n_table n (Table [x] (get_column x table)) 
														(eval (SelectLimit xs n (Atom table)))

eval (Filter (Lt field number) query) = get_table (Lt field number) (eval query)
	where
		get_table (Lt field number) (Table header entries) = 
												Table header (f field number (Table header entries))
			where 
				f field number (Table header []) = []
				f field number (Table header entries@(x:xs))
					| (verify number entries (get_index field (Table header entries))) == True = 
																x:(f field number (Table header xs))
					| otherwise = f field number (Table header xs)
					where
						verify number (x:xs) index
							| (string_to_integer (x !! index)) < number = True
							| otherwise = False

eval (Filter (Eq field str) query) =  get_table (Eq field str) (eval query)
	where
		get_table (Eq field str) (Table header entries) = 
													Table header (f field str (Table header entries))
			where 
				f field str (Table header []) = []
				f field str (Table header entries@(x:xs))
					| (verify str entries (get_index field (Table header entries))) == True = 
																x:(f field str (Table header xs))
					| otherwise = f field str (Table header xs)
					where
						verify str (x:xs) index
							|  (x !! index) == str = True
							| otherwise = False

eval (Filter (In field list) query) =  get_table (In field list) (eval query)
	where
		get_table (In field list) (Table header entries) = 
													Table header (f field list (Table header entries))
			where 
				f field _ (Table header []) = []
				f field [] (Table header _) = []
				f field list (Table header entries@(x:xs))
					| (verify list entries (get_index field (Table header entries))) == True = 
																x:(f field list (Table header xs))
					| otherwise = f field list (Table header xs)
					where
						verify [] _ index = False
						verify (y:ys) (x:xs) index
							|  (x !! index) == y = True
							| otherwise = verify ys (x:xs) index


eval (Filter (Not filter_cond) query) = case filter_cond of
	(Lt field number) -> get_table (Lt field number) (eval query)
		where
			get_table (Lt field number) (Table header entries) = 
												Table header (f field number (Table header entries))
				where 
					f field number (Table header []) = []
					f field number (Table header entries@(x:xs))
						| (verify number entries (get_index field (Table header entries))) == True =
																 x:(f field number (Table header xs))
						| otherwise = f field number (Table header xs)
							where
								verify number (x:xs) index
									| (string_to_integer (x !! index)) >= number = True
									| otherwise = False
	(Eq field str) -> get_table (Eq field str) (eval query)
		where
			get_table (Eq field str) (Table header entries) = 
													Table header (f field str (Table header entries))
				where 
					f field str (Table header []) = []
					f field str (Table header entries@(x:xs))
						| (verify str entries (get_index field (Table header entries))) == True = 
																	x:(f field str (Table header xs))
						| otherwise = f field str (Table header xs)
							where
								verify str (x:xs) index
									|  (x !! index) /= str = True
									| otherwise = False
	(In field list) ->	get_table (In field list) (eval query)
		where
			get_table (In field list) (Table header entries) = Table header (f field list (Table header entries))
				where 
					f field _ (Table header []) = []
					f field [] (Table header _) = []
					f field list (Table header entries@(x:xs))
						| (verify list entries (get_index field (Table header entries))) == False = 
																x:(f field list (Table header xs))
						| otherwise = f field list (Table header xs)
							where
								verify [] _ index = False
								verify (y:ys) (x:xs) index
									|  (x !! index) == y = True
									| otherwise = verify ys (x:xs) index

eval (query1 :|| query2) = get_table (eval query1) (eval query2)
	where 
		get_table (Table header1 entries1) (Table header2 entries2) = Table header1 (entries1++entries2)

--concatenez cele doua headere, apoi cele n intrari 
concat_n_table :: Integer -> Table -> Table -> Table
concat_n_table n (Table header1 entries1) (Table header2 entries2) = Table (header1++header2) 
															(concat_n_entries n entries1 entries2)

--concatenez un numar n de intrari
concat_n_entries :: Integer -> [Entry] -> [Entry] -> [Entry]
concat_n_entries 0 _ _ = []
concat_n_entries n [] _ = []
concat_n_entries n _ [] = []
concat_n_entries n (x:xs) (y:ys) = [x++y]++(concat_n_entries (n-1) xs ys)

--avand doua tabele, concatenez headerele, iar pentru intrari apelez functia de concatenare a acestora
concat_table :: Table -> Table -> Table
concat_table (Table header1 entries1) (Table header2 entries2) = 
										Table (header1++header2) (concat_entries entries1 entries2)

--concatenez intrarile din doua tabele diferite
concat_entries :: [Entry] -> [Entry] -> [Entry]
concat_entries [] _ = []
concat_entries _ [] = []
concat_entries (x:xs) (y:ys) = [x++y]++(concat_entries xs ys)

--obtin o anumita coloana din tabel
get_column :: Column -> Table -> [Entry]
get_column col (Table header entries@(x:xs)) =	
									map (\x -> [x !! (get_index col (Table header entries))]) entries

--obtin indexul unei anumite coloane
get_index :: Column -> Table -> Int
get_index l (Table header entries) = get_value (elemIndex l header)

get_value :: Maybe Int -> Int
gat_value Nothing = -1
get_value (Just x) = x

--converstesc un string in Integer
string_to_integer :: String -> Integer
string_to_integer x = read x :: Integer

--cu get_zone obitin zona id-ului dat ca parametru 
same_zone :: String -> Query
same_zone id = Select ["user_id", "occupation"] 
				$ Filter (Not (In "user_id" [id])) 
				$ Filter (Eq "zone" (get_zone id)) $ Atom user_info
				where
					get_zone id =  get_entrie (eval $ Select ["zone"] 
								$ Filter (Eq "user_id" id) $ Atom user_info)
						where
							get_entrie (Table header (x:xs)) = head x

male_within_age :: Integer -> Integer -> Query
male_within_age x y = Select ["occupation", "zone"] 
						$ Filter (Eq "sex" "M") 
						$ Filter (Lt "age" y) 
						$ Filter (Not (Lt "age" (x+1))) $ Atom user_info

mixed :: [String] -> [String] -> Int -> Query
mixed zone_list occup_list age = Select ["user_id"] 
									$ Filter (Lt "age" (toInteger age)) 
									$ Filter (In "occupation" occup_list) 
									$ Filter (In "zone" zone_list) $ Atom user_info