module Main exposing (..)
--Andrea Amado
--Byron terre
--Fernando de Tezanos

                        --LABORATORIO 8



--Ejercicio #1

type Arbol t = Nil | Cons t (Arbol t) (Arbol t)





--Ejercicio #2

map: (t->u)->Arbol t->Arbol u

map f arb = case arb of

    Nil->Nil

    Cons x arb1 arb2 -> Cons (f x) (map f arb1) (map f arb2)





--Ejercicio #3

filtrar: (t->Bool)->(Arbol t)->(List t)

filtrar f arb = case arb of

    Nil->[]

    Cons x arb1 arb2 ->if f x then [x]++(filtrar f arb1)++(filtrar f arb2) else (filtrar f arb1)++(filtrar f arb2)



--Ejercicio #4

foldTree: (t->s->s->s)->s->(Arbol t)->s

foldTree f inicial arb = case arb of

    Nil->inicial

    Cons x arb1 arb2 -> f x (foldTree f inicial arb1) (foldTree f inicial arb2)



--Ejercicio #5

filtrarFold f (Cons x arb1 arb2) =

    let

        arb = Cons x arb1 arb2

        filt= if f x then [x]++(filt f arb1)++(filt f arb2) else filt f arb1++filt f arb2

    in

        foldTree filt [] arb
--Funciones a utilizar

xg = Cons 3 (Cons 1 Nil Nil) Nil

mg = Cons 9 (Cons 7 (Cons 5 Nil Nil) Nil) (Cons 2 Nil Nil)

esPar x = if modBy 2 x == 0 then True else False

mas2 x = x+2

mas x y z = x+y+z

por x y z =x*y*z

gg=Cons "a" (Cons "b" Nil Nil) Nil

aa x = "amigo"

bb x y z = "amigo, "++"barco, "++"casa"