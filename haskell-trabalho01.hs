-- ECOM11 - Haskell – Lista de Exercícios
-- UNIFEI – Universidade Federal de Itajubá
-- Prof. João Paulo R. R. Leite (joaopaulo@unifei.edu.br)

-- Aluno: Marcos Vinícius Barbosa
-- Matrícula: 2020016324

-- Usado para Repl.it
-- main :: IO ()    -- This says that main is an IO action.
-- main = return () -- This tells main to do nothing.

-- 1
areaRetangulo :: Float -> Float -> Float
areaRetangulo w h = w*h

areaTriangulo :: Float -> Float -> Float
areaTriangulo b h = (b*h)/2

-- 2
-- velocidade em km/h, s em km e saida em minutos
tempoGasto :: Float -> Float -> Float
tempoGasto s v = (s/v)*60

diferencaDoisCarros :: Float -> Float -> Float -> Float
diferencaDoisCarros s v1 v2 = abs (tempoGasto s v1 - tempoGasto s v2)

-- 3
guardaFuncao :: Int -> Int
guardaFuncao n 
    | n == 0           = 1
    | n == 1 || n == 2 = 3
    | otherwise        = guardaFuncao (n-1) + guardaFuncao (n-2)

-- 4 Função  de Ackerman
ackerman :: Int -> Int -> Int 
ackerman m n
    | m == 0          = (n + 1)
    | m > 0 && n == 0 = ackerman (m-1) 1
    | m > 0 && n > 0  = ackerman (m-1) $ ackerman m (n-1)

--  5 - valor  das  arestas  de  um  triângulo  e indique se o triângulo é escaleno, isóscelesou equilátero
trianguloClassificacao :: Int -> Int -> Int -> Int
trianguloClassificacao a b c
    | a /= b && b /= c && a /= c = 1
    | a == b && b == c && a == c = 3
    | otherwise                  = 2

-- 06
reais :: Float -> Float -> Float
reais a b
    | (a < b) = -1
    | (a == b) = 0
    | (a > b) = a * b

-- 07
somaPar :: Int -> Int
somaPar n = sum [x | x <- [0..n], mod x 2 == 0] 

-- 08 
isPrimo :: Int -> Bool
isPrimo a
    | (a == 1) = False
    | otherwise = auxiliar a 2
    where
        auxiliar a b = if (a == b)
                       then True
                       else if (mod a b) == 0
                            then False
                            else auxiliar a (b + 1)

-- 09        
fibonacci :: Int -> Int -> Int -> Int
fibonacci n ac1 ac2
    | (n == 0) = ac1
    | (n == 1) = ac2
    | (n > 1) = fibonacci(n - 1) ac2 (ac1 + ac2)

-- 10 MUV with Where
muv :: Float -> Float -> Float -> Float
muv v0 v1 t = s
  where 
    a = (v1 - v0) / t
    s = v0*t + (a/2) * (t^2)

-- 10 MUV with Let
muvLet :: Float -> Float -> Float -> Float
muvLet v0 v1 t = let a = (v1 - v0) / t
                 in  v0*t + (a/2) * (t^2)

-- 11 
somaRec :: Int -> Int -> Int
somaRec a b
  | a == 0 = b
  | otherwise  = somaRec (a-1) (b+1)

-- 12 
quicksort::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x|x <- xs,x < s] ++ [s] ++ quicksort [x|x <- xs,x >= s]

concList :: [Int] -> [Int] -> [Int] 
concList a b = quicksort ([x | x <- lista, elem x a /= elem x b] ++ rep) --concatena valores que estão apenas em suas listas + as que estão nas duas
    where 
      lista = a ++ b --lista com todos os valores de a e b
      rep = [x | x <- a, elem x b] --valores que estão em a e b

-- 13
findElem :: Int -> [Int] -> Int
findElem  a b = b !! a

-- 14 - Posição começa em 0
addN :: Int -> Int -> [Int] -> [Int]
addN e pos lista = let (lista1, lista2) = splitAt pos lista in lista1 ++ [e] ++ lista2
  
-- 15
subLetra :: String -> Char -> Char -> String
subLetra [] a b = []
subLetra (c:d) a b 
      |c /= a = [c] ++ subLetra d a b
      |c == a = [b] ++ subLetra d a b
      |otherwise = [] 

-- 16
isPolindromo :: [Char] -> Bool
isPolindromo poli 
        | poli == reverse poli = True
        | otherwise            = False 

-- 17
iniciais :: [Int] -> Int -> [Int]
iniciais lista n = let (begin, end) = splitAt n lista in begin

finais :: [Int] -> Int -> [Int]
finais lista n = let (begin, end) = splitAt ((length lista) -n ) lista in end

-- 18
prefix :: [Char] -> [Char] -> Bool
prefix pre y = let (begin, end) = splitAt (length pre) y in if begin == pre 
                                                            then True
                                                            else False

-- 19 - Lista de Lista de Inteiros
sozinhos :: [[Int]] -> [Int] 
sozinhos lista = concat [x | x<-lista, length x == 1]

-- 20 
positivos :: [Int] -> [Int]
positivos lista = filter (>=0) lista