-- This is a haskell file. 
-- We can compile it by running ghc sample.hs
-- Below we define insertSort

insertSort[] = [] 
insertSort(x:xs) = insert x (insertSort xs)

insert a [] = [a]
insert a(b:xs)
	| a <= b = a:b:xs
	| otherwise = b:(insert a xs)

runInsertionSort :: IO()
runInsertionSort = do 
	putStrLn "Running insertion sort"
	-- If we had passed [200..1] it wouldn't know that we want decrement 
	-- because it defaults to incrementing, so we have to pass the 198 too.
	print (insertSort [200,198..0])

{-
List operations: 
----------------

[1,2] ++ [3,4] = [1,2,3,4]
1 : [2,3] = [1,2,3]

head [1..5]
tail [1..5]
init [1..5]
last [1..5]

-}

-- Fibonacci 
fib :: Integer -> Integer
fib 0 = 1 
fib 1 = 1 
fib n = fib (n-1) + fib(n-2)

runFibonacci:: IO() 
runFibonacci = do 
	putStrLn "Running Fibonacci"
	let n = 10
	print(fib n)

-- Square 
square :: Integer -> Integer 
square x = x * x

runSquare:: IO()
runSquare = do 
	putStrLn "Running square"
	let n = 10 
	let r = square n 
	print("Square of " ++ show n ++ " is " ++ show r)

-- map function signature:
-- map :: (a -> b) -> [a] -> [b]
-- map takes as an argument
--		(a -> b) which is a function that takes an a and returns a b
--		and a list of [a] 
-- 		and returns a list of [b]

-- Define the function quad as lambda function 
quad :: Integer -> Integer
quad = \x -> square x * square x 

runQuad :: IO() 
runQuad = do 
	putStrLn "Running quad "
	let n = 4 
	let r = quad n 
	print("Quad of " ++ show n ++ " is " ++ show r)

{- Note -}
{- Reached 1.3 in the slides -}

main :: IO()
main = do 
	-- runInsertionSort
	-- runFibonacci
	-- runSquare
	runQuad
