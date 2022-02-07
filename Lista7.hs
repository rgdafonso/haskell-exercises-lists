-- Questão 1)
head2 :: [t] -> t
head2 (a:x) = a

tail2 :: [t] -> [t]
tail2 (a:x) = x

fst2 :: (t,t) -> t
fst2 (t,u) = t

shift2 :: ((t, t), t) -> (t, (t, t))
shift2 ((a,b),c) = (a,(b,c))

-- Questão 2)
concatena :: [[t]] -> [t]
concatena []  = []
concatena (x:xs) = x ++ concatena xs

-- Questão 3)
inverte :: [t] -> [t]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

-- Questão 4)
zipp3 :: [t] -> [t] -> [t]-> [(t,t,t)]
zipp3 [] x y = []
zipp3 x [] y = []
zipp3 x y [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs

-- Questão 5)
mapMaisUm :: Num a=> (a->b) -> [a] -> [b]
mapMaisUm f [] = []
mapMaisUm f (x:xs) = f (x+1) : mapMaisUm f xs

-- Questão 6)

{-  A função foldr faz uso desse tipo pois assim temos a possiblidade de utiliza-la para outros tipos como por exemplo Strings, 
com uso dessa funçao pode-se aplicar facilmente uma operação entre cada um dos chars dessa lista,
um exemplo simples e similar ao apresentado no enunciado seria a reconstrução do vetor adicionando "a" no final, por exemplo:
foldr (:) "a" "String" = 'S' : 't' : 'r' : 'i' : 'n' 'g' : 'a' = "Stringa"
foldr (:) "a" "" = "" : 'a' = "a" 
Exemplo implementado abaixo
A função (:) poderia ser substituida por outra função desejada por exemplo substituir todos caracteres por 'a'
-}

exemploFoldr :: [Char] -> [Char]
exemploFoldr a = foldr (:) "a" a


