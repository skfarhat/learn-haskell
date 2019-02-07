

square:: Integer -> Integer
square x = x * x

squareDouble:: Double -> Double 
squareDouble x = x * x

quad:: Integer -> Integer
quad x = square(x) * square(x)

larger:: (Integer, Integer) -> Integer 
larger (x, y)
  | x >= y = x
  | otherwise = y 

circleArea:: Double -> Double 
circleArea r = pi * squareDouble r 

add:: Integer -> Integer -> Integer 
add x y = x + y 

twice :: (Integer -> Integer) -> Integer -> Integer
twice f x = f(f x)

infinity:: Integer 
infinity = infinity + 1   

main:: IO() 
main = do 
  let x = add 1 0
  print(x)
  print(twice (add 1) 0) 