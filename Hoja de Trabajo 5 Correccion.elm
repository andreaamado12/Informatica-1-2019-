module Main exposing (esPrimo, fibonacci, primos, nPrimos)



--Ejercicio #2

esPrimo: Int -> Bool

esPrimo n =  primo 2 n 

primo  cont n = if n == 1 then False else if n == 2 then True else if modBy   cont  n  == 0 then False else if cont == n-1 then True else primo (cont + 1) n





--Ejercicio #3

fibonacci n =case n of

    0 -> 0

    1 -> 1

    o -> fibonacci (n-1) + fibonacci (n-2) 



--Ejercicio #4

primos: Int -> List Int

primos n = if n == 0 then [] else if n == 1 then [] else if esPrimo n == True then n:: primos (n-1) else primos (n-1)

   

--Ejercicio #5

nPrimos: Int -> List Int

nPrimos n  = parametro (n,2) 

parametro (n,x) = if n == 0 then [] else if esPrimo x == False then parametro (n, x + 1) else x:: parametro (n - 1, x + 1)

 