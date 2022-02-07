-- Questão 1)
aplicaDuasVezes :: (Int->Int) -> Int -> Int
aplicaDuasVezes f a = f (f a)

dobra :: Int -> Int
dobra x = x*x

incrementa :: Int -> Int
incrementa x = x+1

-- Questão 2)
vendas :: Int -> Int 
vendas 0 = 0
vendas 1 = 3
vendas 2 = 2
vendas 3 = 4
vendas 4 = 2

vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal v 0 = 0
vendaTotal v n = v n + vendaTotal v (n-1)

-- Questão 3)
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "erro"
foldInt f (x:xs) 
    | null xs = x
    | otherwise = f x (foldInt f xs)

soma :: Int -> Int -> Int
soma x y = x + y
mult :: Int -> Int -> Int
mult x y = x * y

-- Questão 4)
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs) 
    | f x = x : filterString f xs
    | otherwise = filterString f xs

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

-- Questão 5)
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs

somaQuadrado :: [Int] -> Int
somaQuadrado n  = foldInt soma (mapInt quadrado n)

quadrado :: Int -> Int
quadrado n = n*n

-- Questão 6)
iter :: Int -> (Int->Int) -> Int -> Int
iter n f x 
    | n == 1 = f x
    | otherwise = f (iter (n-1) f x)
