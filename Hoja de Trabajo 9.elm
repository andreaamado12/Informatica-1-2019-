module Main exposing (..)
	
	--Andrea Amado
	--Byron Terre
	--Fernando de Tezanos
	--LABORATORIO 9
	
	
	--Ejercicio #1
	type Grupo t = 
	Valor t | Suma (Grupo t) (Grupo t) | Inverso (Grupo t)
	
	--Ejercicio #2
	type Algebra t s = Algebra (t->s) (s->s->s) (s->s)
	
	--Ejercicio #3
	evaluar: (Algebra t s)->(Grupo t)->s
	evaluar (Algebra iValor iSuma iInverso) expr = case expr of
	Valor b->iValor b
	Suma b1 b2->iSuma (evaluar (Algebra iValor iSuma iInverso) b1) (evaluar (Algebra iValor iSuma iInverso) b2)
	Inverso b3->iInverso (evaluar (Algebra iValor iSuma iInverso) b3)
	
	--Ejercicio #4
	zAlgebra: Int->(Algebra Int Int)
	zAlgebra x =
	let
	iValor n = n
	iSuma n1 n2 = modBy x (n1+n2)
	iInverso n3 = auxiliar x (x-1) n3 (x-1)
	in
	Algebra iValor iSuma iInverso
	
	auxiliar x y n3 b = if modBy x (n3+y+b) == y then b else auxiliar x y n3 (b-1)
	
	--Función de prueba
	prueba = Suma (Valor 3) (Suma (Valor 5) (Inverso (Valor 3)))
	

