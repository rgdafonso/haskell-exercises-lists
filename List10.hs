data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
    deriving(Eq,Show)

--Questão 1)
multDois :: Arvore Int -> Arvore Int
multDois (Folha f) = Folha (f * 2)
multDois (Nodo f a1 a2) = Nodo (f*2) (multDois a1) (multDois a2)

--Questão 2)
contaElementos :: Arvore a -> Int
contaElementos (Folha f) = 1
contaElementos (Nodo f a1 a2) = 1 + (contaElementos a1) + (contaElementos a2)

--Questão 3)
altura :: Arvore a -> Int
altura (Folha f) = 1
altura (Nodo f a1 a2) = 1 + max (altura a1)  (altura a2)

--Questão 4)
maiorElemento  :: Arvore Int -> Int
maiorElemento (Folha f) = f 
maiorElemento (Nodo f a1 a2) = max f (max (maiorElemento a1) (maiorElemento a2))

--Questão 5)
procuraInt :: Int -> Arvore Int -> Bool
procuraInt n (Folha f) = n == f
procuraInt n (Nodo f a1 a2) 
    | n == f = True
    | otherwise = (procuraInt n a1) || (procuraInt n a2 )

--Questão 6)
quantasVezes :: Int -> Arvore Int -> Int
quantasVezes n (Folha f) 
    | f == n = 1
    | otherwise = 0
quantasVezes n (Nodo f a1 a2) 
    | n == f = 1+ (quantasVezes n a1) + (quantasVezes n a2 )
    | otherwise = (quantasVezes n a1) + (quantasVezes n a2 )

--Questão 7)
refleteArvore :: Arvore a -> Arvore a
refleteArvore (Folha f) = Folha (f)
refleteArvore (Nodo f a1 a2) = Nodo f (refleteArvore a2) (refleteArvore a1)

--Questão 8)
arvoreToLista :: Arvore a -> [a]
arvoreToLista (Folha f) = f : []
arvoreToLista (Nodo f a1 a2) = f : (arvoreToLista a1) ++ (arvoreToLista a2)

--Questão 9)
mapTree :: (a-> b) -> Arvore a -> Arvore b
mapTree f (Folha l) = Folha (f l) 
mapTree f (Nodo l a1 a2) = Nodo (f l) (mapTree f a1) (mapTree f a2)


{- incrementa :: Int -> Int
incrementa a = a+1-}
--(Nodo 10 (Nodo 11 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Nodo 14 (Nodo 5 (Folha 6) (Folha 7)) (Folha 8)))
--(Nodo 10 (Nodo 14 (Folha 4) (Folha 2)) (Nodo 1 (Folha 6) (Folha 9)))



