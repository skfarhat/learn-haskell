import Data.Char

(££):: Bool -> Bool -> Bool 
False ££ x  = x
True ££ _ = True 


-- (&&):: Bool -> Bool -> Bool
-- False && _ = False
-- True && x = x

-- Implementing OR
($$):: Bool -> Bool -> Bool
($$) x y =  if x then True else y

-- Function that defines a digit character to its numeric equivalent
charToNum:: Char -> Integer
charToNum '0' = 0
charToNum '1' = 1
charToNum '2' = 2
charToNum '3' = 3
charToNum '4' = 4
charToNum '5' = 5
charToNum '6' = 6
charToNum '7' = 7
charToNum '8' = 8
charToNum '9' = 9
charToNum x = -1

charToNum2:: Char -> Int
charToNum2 x = ord x - ord '0'

dayDate:: Integer -> String
dayDate 1 = "1st"
dayDate 2 = "2nd"
dayDate 3 = "3rd"
dayDate x = (show x) ++ "th"

month:: Integer -> String
month 1 =  "January"
month 2 =  "February"
month 3 =  "March"
month 4 =  "April"
month 5 =  "May"
month 6 =  "June"
month 7 =  "July"
month 8 =  "August"
month 9 =  "September"
month 10 =  "October"
month 11 =  "November"
month 12 =  "December"
month x = "N/A"

showDate:: Integer -> Integer -> Integer -> String
showDate x y z = (dayDate x) ++ " " ++ (month y) ++ " " ++ (show z)

main:: IO()
main = do 
  print(True ££ False)
  print("Testing $$")
  print("-------------------")
  print(True $$ False)
  print(False $$ False)
  print(False $$ True)

  print("Testing charToNum")
  print("-------------------")
  print( charToNum '-' )
  print( charToNum '5' )
  print( charToNum '8' )
  print(  (charToNum '5') + (charToNum '9'))

  print(  (charToNum2 '5') + (charToNum2 '9'))
 -- print(True && True)
  putStrLn(showDate 05 01 1994)
