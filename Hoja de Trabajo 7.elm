module Main exposing(..)
	
	--Andrea Amado
	--Byron Terre 
	--Fernando de Tezanos
	
	
	--Ejercicio #1
	type Arbol = Nil | Cons Int Arbol Arbol 
	
	
	--Ejercicio #2
	masUno: Arbol->Arbol
	masUno arb = case arb of
	Nil->Nil
	Cons x arb1 arb2 -> Cons (x+1) (masUno arb1) (masUno arb2)
	
	
	--Ejercicio #3
	map: (Int->Int)->Arbol->Arbol
	map f arb = case arb of
	Nil->Nil
	Cons x arb1 arb2 -> Cons (f x) (map f arb1) (map f arb2)
	
	
	--Ejercicio #4
	sum: Arbol->Int
	sum arb = case arb of
	Nil->0 
	Cons x arb1 arb2 -> x + (sum arb1) + (sum arb2)
	
	
	--Ejercicio #5
	foldTree: (Int->Int->Int->Int)->Int->Arbol->Int
	foldTree f inicial arb = case arb of
	Nil->inicial
	Cons x arb1 arb2 -> f x (foldTree f inicial arb1) (foldTree f inicial arb2) 
	
	
	--Complementos utilizados para pruebas
	xg = Cons 3 (Cons 1 Nil Nil) Nil
	mg = Cons 9 (Cons 7 (Cons 5 Nil Nil) Nil) (Cons 2 Nil Nil)
	mas2 x = x+2
	mas x y z = x+y+z
	por x y z =x*y*z

