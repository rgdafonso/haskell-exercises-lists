--Questão 1)
multDoisLista :: [Int] -> [Int]
multDoisLista x = map mult x

mult :: Int  -> Int
mult a  = a * 2

--Questão 2)
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:xs) = 1 + tamanho xs

--Questão 3)
produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs

--Questão 4)
andLista :: [Bool] -> Bool
andLista [] = True
andLista (x:xs) = x && andLista xs

--Questão 5)
concatLista :: [[Int]] ->[Int]
concatLista a = concat a

--Questão 6)
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista l = last l : inverteLista(init l)