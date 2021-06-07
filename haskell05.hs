-- Prática 05 de Haskell
-- Nome: Andriza Campanhol

--01
bmi :: Float -> Float -> String
bmi peso h = 
  let imc = peso / h^2
  in if imc >= 30 then "ACIMA" else if imc <= 18.5 then "ABAIXO" else "NORMAL"

--02
bmi' :: Float -> Float -> String
bmi' peso h = if imc >= 30 then "ACIMA" else if imc <= 18.5 then "ABAIXO" else "NORMAL"
  where imc = peso / h^2

--03
--cpfValid :: [Int] -> Bool
--cpfValid cpf =
-- let digits = take 9 cpf
--     dv1 = cpfDV digits [10,9..]
--     dv2 = cpfDV (digits ++ [dv1]) [11,10..]
--  in dv1 == cpf !! 9 && dv2 == cpf !! 10

--cpfDV :: [Int] -> [Int] -> Int
--cpfDV digits mults = if expr < 2 then 0 else 11-expr
-- where expr = (sum $ zipWith (*) digits mults) `mod` 11

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
  let expr = (sum $ zipWith (*) digits mults) `mod` 11
  in if expr < 2 then 0 else 11-expr

--04
--faz todas as possibilidades como um for aninhado (um dentro do outro)
andTable :: [(Bool, Bool, Bool)]
andTable = 
  let bole = [True, False]
  in [(b1, b2, b1 && b2) | b1 <- bole, b2 <- bole]
