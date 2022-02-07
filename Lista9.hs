data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
    deriving(Eq,Show)

finalDeSemana :: Dia -> Bool
finalDeSemana Segunda = False
finalDeSemana Terca = False
finalDeSemana Quarta = False
finalDeSemana Quinta = False
finalDeSemana Sexta = False
finalDeSemana Sabado = True
finalDeSemana Domingo = True

data TalvezFloat = Valor Float | Erro String
    deriving(Eq,Show)


divisao :: Float -> Float -> TalvezFloat
divisao n1 n2 
    | n2 /= 0 = Valor( n1 /  n2)
    | otherwise = Erro ("Divisão por zero!")



data Nat = Zero | Suc Nat
    deriving(Eq,Show)

sete :: Nat
sete = Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero))))))

natToint :: Nat -> Int
natToint Zero= 0
natToint (Suc z) = 1 + natToint z

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n-1))