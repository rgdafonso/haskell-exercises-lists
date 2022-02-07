--Questao 1)
concatena :: [[a]] -> [a]
concatena a =  foldr (++) [] a
--Questao 2)
andLista :: [Bool] -> Bool
andLista a = foldr (&&) True a
--Questao 3)
somaQuadPos :: [Int] -> Int
somaQuadPos a = foldr (+) 0 (map (quadrado) (filter (>0) a))

quadrado :: Int->Int
quadrado num = num*num
--Questao 4)
somaListas :: [[Int]]-> Int
somaListas x =  foldr (+) 0 (map (foldr (+) 0) x) 

--Questao 5)
tamanhoListas :: [[Int]] -> Int
tamanhoListas x =  foldr (+) 0 (map (length) x) 

--Questao 6)

-- / X ACC -> ACC ++[X]
inverte :: [a] -> [a]
inverte x = foldr (invereteElemento) [] x

invereteElemento :: a -> [a] -> [a]
invereteElemento x acc = acc ++ [x]

--Questao 7) 
separaPalavras :: String -> [String]
separaPalavras [] = []
separaPalavras (' ':xs) = separaPalavras xs
separaPalavras (x:xs) = (takeWhile (/= ' ') (x:xs)) : (separaPalavras (dropWhile (/= ' ') (x:xs)))



