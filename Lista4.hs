--Questão 1)
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao _ [] = error "Lista Vazia"
pegaPosicao n (x:xs)
    | n == 1 = x
    | otherwise = pegaPosicao (n-1) xs

--Questão 2)
pega :: Int -> [Int] -> [Int]
pega _ [] = []
pega n (x:xs)
    | n > 0 = x: pega (n-1) xs
    | otherwise = pega(n-1) xs

--Questão 3)
retira :: Int -> [Int] -> [Int]
retira _ [] = []
retira n (x:xs)
    | n > 0 =  retira (n-1) xs
    | otherwise = x:retira(n-1) xs

--Questão 4)
mediaLista :: [Int] -> Int
mediaLista xs =  div (somaLista xs)  (length xs)

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

--Questão 5)
pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores _ [] = []
pegaMaiores n (x:xs)
    | n < x = x : pegaMaiores n xs
    | otherwise = pegaMaiores n xs

--Questão 6)
contaMaiores :: Int -> [Int] -> Int
contaMaiores _ [] = 0
contaMaiores n (x:xs)
    | n < x = 1 + contaMaiores n xs
    | otherwise = contaMaiores n xs

--Questão 7)
intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala (x:xs) [] = x : intercala xs []
intercala [] (x:xs) = x : intercala xs []
intercala (x:xs) (y:ys) = x : y : intercala xs ys

--Questão 8)
dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

--Questão 9)
repli ::  Int -> String -> String
repli _ [] = []
repli n (x:xs)
    | null xs = repliAux n x
    | otherwise = repliAux n x ++ repli n xs

repliAux :: Int -> Char -> String
repliAux 0 _ = []
repliAux n x= x : repliAux (n-1) x

--Questão 10)
dropEvery :: Int -> String -> String
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

--Questão 11)
split :: Int -> [Char] -> ([Char], [Char])
split n x = (splitAuxAntes n x, splitAuxDepois n x)
    
splitAuxAntes ::Int -> [Char] -> [Char]
splitAuxAntes _ [] = []
splitAuxAntes n (x:xs) 
    | n > 0 = x : splitAuxAntes (n-1) xs
    | otherwise = splitAuxAntes n xs

splitAuxDepois ::Int -> [Char] -> [Char]
splitAuxDepois _ [] = []
splitAuxDepois n (x:xs) 
    | n > 0 = splitAuxDepois (n-1) xs
    | otherwise = x: splitAuxDepois n xs
