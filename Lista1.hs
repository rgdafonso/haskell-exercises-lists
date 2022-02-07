vendas :: Int -> Int
vendas 0 = 22
vendas 1 = 33
vendas 2 = 0
vendas 3 = 44
vendas _ = 12

--Questão 1)
somaTuplas :: ((Int,Int),(Int,Int)) -> Int
somaTuplas ((a,b),(c,d)) = a + b + c + d

--Questão 2)
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) =  (a, (b, c))

--Questão 3)
maxi :: Int -> Int -> Int -> Int
maxi x y z
  | (x>=y) && (x>=z)  = x
  | (z>=y) && (z>=x)  = z
  | otherwise   = y

mini ::  Int -> Int -> Int -> Int
mini x y z
  | (x<=y) && (x<=z)  = x
  | (z<=y) && (z<=x)  = z
  | otherwise   = y

minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = (maxi a b c , mini a b c)

--Questão 4)
zeroVendas :: Int -> (Int, Bool)
zeroVendas 0
  | vendas 0 == 0  = (0, True)
  | otherwise      = (-1, False)
zeroVendas n 
  | vendas n == 0  = (0, True)
  | otherwise      = zeroVendas (n-1)

  --Questão 5)
type Livro = (String, String, Int)
livro :: Livro
livro = ("titulo qualquer", "fulano", 84842)

titulo :: Livro -> String
titulo (a,_,_) = a

autor :: Livro -> String
autor (_,b,_) = b

isbn :: Livro -> Int
isbn (_,_,c) = c