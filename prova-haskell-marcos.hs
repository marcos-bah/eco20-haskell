-- ECOM11 - Haskell – Lista de Exercícios
-- UNIFEI – Universidade Federal de Itajubá
-- Prof. João Paulo R. R. Leite (joaopaulo@unifei.edu.br)

-- Aluno: Marcos Vinícius Barbosa
-- Matrícula: 2020016324

{- Usado para Repl.it
main :: IO ()    -- This says that main is an IO action.
main = return () -- This tells main to do nothing. -}

--1
average :: [Float] -> Float
average a 
  | div > 0.0 = soma / div
  | otherwise = 0.0
  where 
    div = fromIntegral (length a) -- transforma para float o tamanho da lista
    soma = sum a -- soma da lista

{-
Exemplo sem funções
media :: [Float] -> Float
media a = m
    where
      m = soma a / tam a

soma :: [Float] -> Float
soma[] = 0
soma(a:b) = a + soma b

tam :: [Float] -> Float
tam[] = 0
tam(a:b) = 1 + tam b-}

--2
corrente :: Float -> Float -> Float
corrente v r 
    | r == 0 = 0.0
    | v == 0 = 0.0
    | otherwise = i
    where 
      i = v / r

--3
descartar :: Int -> [Char] -> [Char]
descartar a b = drop a b

{-
Outra forma de fazer sem usar o drop
tirar :: Int -> [Char] -> [Char]
tirar n lista = let (begin, end) = splitAt n lista in end-}

--4
type Pessoa = (String, Int)

autorizados :: [Pessoa] -> [Pessoa]
autorizados p = [x | x <- p, snd x  >= 18]

{-
Exemplo sem funções do haskell
auth :: [Pessoa] -> [Pessoa]
auth p = [(x,y) | (x,y) <- p, y  >= 18]-}
