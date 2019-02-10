
until1:: (a -> Bool) -> a -> (a -> a) -> a
until1 p x f = if p x then x else until1 p (f x) f

main:: IO()
main = do 
  print(until1 (>100) (1) (*7))
