import Debug.Trace

{-lines:: String -> [String]-}
{-lines "" = []-}
{-lines (xs:x) -}
  {-| xs == "\n" = -} {-lines x = klklgt-}

prod:: [Int] -> Int
prod [] = 1
prod (xs:x) = xs * prod x

-- return true if all entries are true
allTrue:: [Bool] -> Bool
allTrue [] = True
allTrue (xs:x) = xs && allTrue(x)

allFalse:: [Bool] -> Bool
allFalse [] = True
allFalse (xs:x) = (not xs) && allFalse(x)

decAll:: [Int] -> [Int]
decAll x = map (\x -> x - 1) x

decAll1:: [Int] -> [Int]
decAll1 [] = []
decAll1 (xs:x) = [xs - 1] ++ decAll1(x)

convertIntBool:: [Int] -> [Bool]
convertIntBool [] = []
convertIntBool (xs:x)
  | xs == 0 = [False] ++ convertIntBool(x)
  | otherwise = [True] ++ convertIntBool(x)

-- Note how I started to omit parentheses from the function arguments 
-- as they were causing all sorts of problems here. I think I got away with it
-- above because all functions took one argument.
pairUp:: [Int] -> [Char] -> [(Int, Char)]
pairUp [] [_] = []
pairUp [_] [] = []
pairUp (as:a) (bs:b) = [(as,bs)] ++ (pairUp a b)

-- Note. Took me a bit to figure out my error. 
-- My second equation had 0 [x] as patterns which would only match when 
-- the first arg is zero and the second is a list of size 1. 
-- What I meant was 0 (x), or actually, I should have just used 0 _
-- Now I know.
takePrefix:: Int -> [a] -> [a]
takePrefix 0 (x) = trace("matched with 0!") $ []
takePrefix _ [] = trace("matched with empty list!") []
takePrefix n (xs:x) = trace ("n: " ++ show n) $ [xs] ++ (takePrefix (n - 1) x)

dropPrefix:: Int -> [a] -> [a]
dropPrefix 0 x = x
dropPrefix _ [] = []
dropPrefix n (xs:x) = dropPrefix (n-1) x

-- Difficulty with this one was understanding what is meant by => 
-- Apparently this is a type class constraint that means that this function
-- will work with any type 'a' that is an instance of Eq. 
member:: Eq a => [a] -> a -> Bool 
member [] x = False
member (as:a) x = (as == x) || (member a x)

equals:: Eq a => [a] -> [a] -> Bool
equals [] [] = True
equals [] _ = False 
equals _ [] = False 
equals (xs:x) (ys:y) = (xs == ys) && (equals x y)


select:: [a] -> Int -> a
select (xs:x) 0 = xs
select [] _ = error "Index out of bounds"
select (xs:x) n = select x (n-1)

-- Won't bother with -infinity, just put low enough number
largest:: [Int] -> Int
largest [] = -10000
largest (xs:x) = max xs (largest x)

-- Won't bother with -infinity, just put low enough number
smallest:: [Int] -> Int
smallest [] = 10000
smallest (xs:x) = min xs (smallest x)

main:: IO()
main =  do 
  print("Hello lists!")
  print(prod [1,2,3])

  print("--------------")
  print("AllTrue test")
  print("--------------")
  print(allTrue([True, True, True]))
  print(allTrue([True, False, True]))
  print(allTrue([False, False, True]))
  print(allTrue([False, False, False]))

  print("--------------")
  print("AllFalse test")
  print("--------------")
  print(allFalse([True, True, True]))
  print(allFalse([True, False, True]))
  print(allFalse([False, False, True]))
  print(allFalse([False, False, False]))

  print("--------------")
  print("decAll test")
  print("--------------")
  print(decAll [1,2,3,4,55])
  print(decAll1 [1,2,3,4,55])

  print("--------------")
  print("convertIntBool test")
  print("--------------")
  print(convertIntBool([1,2,3,0,2,10,3,0,0,12]))

  print("--------------")
  print("pairUp")
  print("--------------")
  print(pairUp [1,2,3,4] ['S', 'A', 'M'])

  print("--------------")
  print("takePrefix")
  print("--------------")
  print(takePrefix 3 [1..10])

  print("--------------")
  print("dropPrefix")
  print("--------------")
  print(dropPrefix 3 [1..10])

  print("--------------")
  print("member")
  print("--------------")
  print(member [1..50] 20)
  print(member [1..50] (-20))

  print("--------------")
  print("equals")
  print("--------------")
  print(equals [1..20] [1..20])
  print(equals [1..20] [1..21])
  print(equals [2..20] [1..20])

  print("--------------")
  print("select")
  print("--------------")
  print(select [2,4..20] 1)
  {-print(select [2,4..20] 11)-}

  print("--------------")
  print("largest")
  print("--------------")
  print(largest [2,4..20])

  print("--------------")
  print("smallest")
  print("--------------")
  print(smallest [2,4..20])
