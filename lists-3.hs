

prod:: [Int] -> Int
prod [] = 1
prod (xs:x) = xs * prod x

main:: IO()
main =  do 
  print("Hello lists!")
  print(prod [1,2,3])
  

