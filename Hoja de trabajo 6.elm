module Main exposing (..)

ifilterdivision z xs = case xs of
     [] -> []
     b::bs -> if modBy z b == 0  then b::ifilterdivision z bs else ifilterdivision z bs 

filter2 f xs = case xs of    
     [] -> []
     b::bs -> if f b == True  then b::filter2 f bs else filter2 f bs

izipwith xs ys = case (xs,ys) of    
     ([],_) -> []
     (_,[]) -> []
     (b::bs,c::cs) -> (b+c)::izipwith bs cs

zipwith f xs ys = case (xs,ys) of    
     ([],_) -> []
     (_,[]) -> []
     (b::bs,c::cs) -> (f b c)::zipwith f bs cs


{-
miVariable : String 
miVariable = "hola"

miVariable2 = 0 * 100

add5 x = 5 + x


altura : number
altura = 5


base : number
base = 4

area m = base^2

res = area 6
-}
range : Int -> List Int
range x = case x of
      0 -> []
      a -> a :: range (a-1)

esPar num = case num of
      0 -> True
      1 -> False 
      q -> esPar (q-2)
     {-modBy 2 i == 0-}


irange inicio fin =
       if inicio == fin
       then []
       else inicio :: irange (inicio + 1) fin

{-}
arange inicio fin = case inicio fin of 
       inicio == fin -> []
       h -> inicio :: irange (inicio + 1) fin
-}
{-
item i xs = case (i,xs) of
     (0 , b::bs) -> b
     (a , b::bs) -> item (a-1) bs

-}
count : List Int -> Int 
count xs = case xs of
     [] -> 0
     b::bs -> if b == 0 then count bs else 1 + count bs 

existe x xs = case xs of
     [] -> False
     b::bs -> if b == x then True else existe x bs
     {-
exist x xs = case (x,xs) of
    (a, b :: bs) -> if x == b then True else False 
    _-> False -}

sumatoria xs = case xs of
     [] -> 0
     b::bs -> b + sumatoria bs 

multiplicar xs = case xs of
     [] -> 1
     b::bs -> b * multiplicar bs 

operar f inicial xs = case xs of
     [] -> inicial
     b::bs -> f b (operar f inicial bs) 
     {-Main.operar (*) 1 [1,2,3] = 6
       Main.operar (+) 0 [3,7,5] = 15-}

buscar f xs = case xs of
     [] -> False
     b::bs -> if  f b == True then True else buscar f bs

ifilter f xs = case xs of
     [] -> []
     b::bs -> if f b == True then b::ifilter f bs else ifilter f bs 
{-
esPrimox : Int -> Bool
esPrimox x = case x of
     a -> if Basics.modBy 2 x == 0 then False else if Basics.modBy 3 x == 0 then False else if Basics.modBy 5 x == 0 then False else if Basics.modBy 7 x == 0 then False else True
     -}

{-
esPrimo x = 
     Main.count bs == 2 -> True
     b::bs -> if Basics.modBy q x == 0 then b::esPrimo x else esPrimo x 
     -}


verificar : Int -> Int -> Bool
verificar x n =
     if x <= 1 then False 
     else if x == 2 then True
     else if x == n then True
     else if modBy n x == 0 then False else verificar x (n+1)

esPrimo : Int -> Bool
esPrimo x = verificar x 2

primos : Int -> List Int
primos x = ifilter (esPrimo) (range x)
{-
nprimo : Int -> List Int
nprimo x  = if x==0 then [] else if 
-}
{-
nprimos' x = if count(primos n) == x then primos x else 1 + nprimos x
-}
{-
nprimo : Int -> List Int
nprimo x = if x==0 then [] else if count (primos i) == x then primos x else nprimo (x+1)
-}
{-
numero x = count -}

sumar1 x = x + 1

map f xs = case xs of 
     [] -> []
     b::bs -> f b :: map f bs

longitud xs = case xs of 
     [] -> 0
     _::bs -> 1 + longitud bs

promedio xs = sumatoria xs / longitud xs

fold elementos vacio xs = case xs of 
     [] -> vacio
     b::bs -> elementos b (fold elementos vacio bs)

elemento _ previo = 1 + previo
{-
longitud2 xs = fold elemento 0 xs
-}
longitud3 xs = fold (\_ previo -> 1 + previo) 0 xs
{-}
sumatoriaFold xs = fold (\_ valor previo -> valor + previo) 0 xs
-}
map2 f xs = fold (\valor previo -> f valor :: previo) [] xs
{-
filterWilly f valor resto = if f valor then valor :: resto else resto 
-}
filterElemento f valor resto = if f valor == True then (valor :: resto) else resto
filter f xs = fold (filterElemento f ) [] xs

cons2 bs cs = case cs of
     [] -> bs 
     d::ds -> d::cons2 bs ds

cons bs cs = List.reverse (cons2 (List.reverse bs) (List.reverse cs))









type Lista = Nil | Cons Int Lista

crearlista l = case l of  
     [] -> Nil
     i::is -> Cons i (crearlista is)

listaAlist l = case l of  
     Nil -> []
     Cons i is -> i:: listaAlist is

countlista l = case l of 
     Nil -> 0
     Cons _ is -> 1 + countlista is

add1 l = case l of 
     Nil -> Nil 
     Cons b bs -> Cons (b+1) (add1 bs)

maplista : (Int -> Int) -> Lista -> Lista
maplista f l = case l of 
     Nil -> Nil
     Cons b bs ->  Cons (f b) (maplista f bs)

arbol x (b,c) = case (b,c) of 
     ([],[]) -> x::[] , x::[]
     ([],_) -> x::[] , x::c::[]
     (_,[]) -> x::c::[] , x::[]
     ([],[]) -> x::[] , x::[]

     
{-
foldlista :(Int -> Int -> Int) -> Int -> Lista -> Int
foldlista f inicial lista =
-}
type Natural = Cero |  Succ Natural

uno = Succ Cero
dos = Succ uno
tres = Succ dos
cuatro = Succ tres
cinco = Succ cuatro

intNat : Int -> Natural
intNat i = case i of    
     0 -> Cero
     o -> Succ (intNat(o-1))

natInt : Natural -> Int
natInt i = case i of    
     Cero -> 0
     Succ o ->  1 + natInt o

suma nat1 nat2 = case nat1 of    
     Cero -> nat2
     Succ x -> Succ(suma x nat2)

resta nat1 nat2 = case (nat1 , nat2) of 
     (Cero , _) -> Cero
     (_ , Cero) -> nat1
     (Succ i , Succ j) -> resta i j


mult nat1 nat2 = case nat1 of    
     Cero -> Cero
     Succ(Cero) -> uno
     Succ y -> suma nat2 (mult y nat2)

multErnesto nat1 nat2 = case (nat1,nat2) of    
     (Cero , _) -> Cero
     (_ , Cero) -> Cero
     (Succ Cero , x ) -> x
     (Succ i , _) -> suma nat2 (mult i nat2)


igual : Natural -> Natural -> Bool
igual nat1 nat2 = case (nat1 , nat2) of    
     (Cero,Cero) -> True
     (Succ a , Succ b) -> igual a b
     _ -> False    
{-
mayorque : Natural -> Natural -> Bool
mayorque nat1 nat2 =
     if igual nat1 nat2 
     then False   
     else 
     case (nat1,nat2) of    
     (Succ a, Succ b)
-}

type Booleanos = NTrue | NFalse

bId : Booleanos -> Booleanos
bId x = x
{- 
Main.bId Main.NTrue
Main.bId Main.NFalse
-}

not : Booleanos -> Booleanos
not x = case x of
     NTrue -> NFalse
     NFalse -> NTrue
{- 
Main.not Main.NTrue
Main.not Main.NFalse
-}

bIF cond si no = case cond of
     NTrue -> si
     NFalse -> no 
{-
Main.bIF (Main.NFalse) 5 6
-}

and b1 b2 = case (b1,b2) of
     (NTrue,NTrue) -> NTrue
     _ -> NFalse 

or b1 b2 = case (b1,b2) of
     (NFalse,NFalse) -> NFalse
     _ -> NTrue 

