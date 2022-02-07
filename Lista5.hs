-- Questão 1)
somaQuadrupla :: [(Int,Int,Int,Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla ((a,b,c,d):xs) = a + b + c + d + somaQuadrupla xs

-- Questão 2)
somaTuplas :: [((Int,Int),(Int,Int))] -> Int
somaTuplas [] = 0
somaTuplas (((a,b),(c,d)):xs) =   a + b+c +d + somaTuplas xs

-- Questão 3)
zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp [] [] = []
zipp _ [] = []
zipp [] _ = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

-- Questão 4)
zipTres:: [Int] -> [Int] -> [Int]-> [(Int,Int,Int)]
zipTres [] [] [] = []
zipTres _ _ [] = []
zipTres _ [] _ = []
zipTres [] _ _ = []
zipTres (x:xs) (y:ys) (z:zs)= (x,y,z) : zipTres xs ys zs


-- Questão 5)
unZipp :: [(Int,Int)] -> ([Int], [Int])
unZipp x = (unzipEsq(x), unzipDir (x))

unzipEsq, unzipDir :: [(Int,Int)] -> [Int]
unzipEsq [] = []
unzipEsq ((a,b):xs) = a : unzipEsq xs
unzipDir [] = []
unzipDir ((a,b):xs) = b : unzipDir xs
