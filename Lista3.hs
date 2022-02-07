--Questão 1)
membro :: Int -> [Int] -> Bool
membro _ [] = False
membro n (x:xs)
    | n == x = True
    | otherwise = membro n xs
--Questão 2)
membroNum :: Int -> [Int] -> Int
membroNum _ [] = 0
membroNum n (x:xs) 
    | n==x = 1+(membroNum n xs)
    | otherwise = membroNum n xs
--Questão 3)
membro2 :: Int -> [Int] -> Bool
membro2 n x
    | membroNum n x > 0 = True
    | otherwise = False

--Questão 4)
unico :: [Int] -> [Int]
unico [] = []
unico (x:xs) 
    | membroNum x xs == 0 = x : unico xs
    | otherwise = unico (removeItem x xs)

removeItem :: Int -> [Int] -> [Int]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

--Questão 5)

quikSort :: [Int] -> [Int]
quikSort [] = []
quikSort (x:xs) = quikSort (menores x xs)
                  ++ [x] ++
                  quikSort (maiores x xs)

maiores :: Int -> [Int] -> [Int]  
maiores _ [] = []
maiores n (x:xs) 
    | n < x = x : maiores n xs
    | otherwise = maiores n xs

menores :: Int -> [Int] -> [Int]     
menores _ [] = []
menores n (x:xs) 
    | n > x = x : menores n xs
    | otherwise = menores n xs




