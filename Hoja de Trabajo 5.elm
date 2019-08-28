módulo  Exposición principal  (..)
esPrimo :  Int  ->  Bool
esPrimo n =   esPrimoN 2 n
esPrimoN   cont n =
    si n ==  2 
    entonces cierto 
    más si modBy cont n   ==  0 
    entonces falso 
    más si cont == n - 1
    entonces cierto
    de lo contrario esPrimoN ( cont +  1 ) n

Fibonacci :   número  ->  número1
fibonacci n = 
    si n ==  0 
    entonces 0
    más si n ==  1
    entonces 1 
    más si n >  1
    luego fibonacci ( n - 1 )  + fibonacci ( n - 2 )
    más 5

primos :  Int  ->  Lista  Int
primos n = 
    si n <  2
    entonces []
    más si esPrimo n ==  Falso 
    entonces primos ( n -  1 )
    sino n :: primos ( n -  1 )

nPrimos :  Int  ->  Lista  Int
nPrimos s = conta ( s ,  2 ) 
conta ( s , y )  = 
    si s ==  0 
    entonces []
    más si esPrimo y ==  Falso
    entonces conta ( s , y +  1 )
    de lo contrario y :: conta ( s - 1 , y + 1 )
    