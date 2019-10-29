module Main exposing (..)

import Canvas
import CanvasColor as Color exposing (Color)
import Html exposing (Html,div,button,text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Browser

type Fractales = Snowflake | Sierpinsky | Fractal 

type alias Modelo = {fract : Fractales, repte : Int}

modeloInicial : Modelo
modeloInicial = {fract = Fractal , repte = 0}

type Mensaje = Fract Fractales | Repte Int 

actualizador : Mensaje -> Modelo -> Modelo
actualizador mensaje modelo = case mensaje of 
    Fract l -> {fract = l, repte = 0}
    Repte l -> {fract = modelo.fract, repte = 
            if l == 1 then (modelo.repte) + 1 else 
            if modelo.repte == 0 then modelo.repte 
            else (modelo.repte) - 1}


fractal3 = [
    (300, 45),
    (500, 425),
    (100, 425),(300, 45),(300, 45)]


--Snowflake 
punt1 x y = case (x , y) of 
   ((x1, y1), (x2, y2)) -> (((x1 + (1/2) * x2) / (1 + (1/2))), ((y1 + (1/2) * y2) / (1 + (1/2)))) 

punt2 x y = case (x , y) of 
    ((x1, y1), (x2, y2)) -> (((x1 + (2 * x2)) / (3)), ((y1 + (2 * y2)) / (3))) 

puntomaximo : (Float , Float) -> (Float , Float)
puntomaximo x = case x of
    (x1 , y1) -> (((x1 * (cos(degrees 60))) - y1 * (sin(degrees 60))), ((x1 * sin(degrees 60) + y1 * cos(degrees 60))))

rotacion x y = case (x, y) of
    ((x1, y1), (x2, y2)) -> ((x1 + x2), (y1 + y2))

rotarpuntos : (Float, Float) -> (Float, Float) -> (Float, Float)
rotarpuntos x y = case (x, y) of
    ((x1, y1), (x2, y2)) -> rotacion (puntomaximo ((x1 - x2), (y1 - y2))) (x2, y2)

triangulitos listaspuntos = case listaspuntos of 
    x1::x2::xs -> x1::(punt1 x1 x2)::(rotarpuntos (punt1 x1 x2) (punt2 x1 x2))::(punt2 x1 x2)::(triangulitos (x2::xs))
    _ -> []

snowflake2 n listas = if n == 0 then listas else snowflake2 (n - 1) (triangulitos listas)

snowflake n = snowflake2 n fractal3


--Sierpinsky 
fractal : List (Float , Float)
fractal = [
    (300, 200),
    (400, 400),
    (200, 400)]

fractals : List (List (Float , Float))
fractals = [[
    (300, 200),
    (400, 400),
    (200, 400)]]

puntoMedio : (Float , Float) -> (Float, Float) -> (Float , Float)
puntoMedio x y = case (x,y) of 
    ((x1 , y1),(x2 , y2)) -> ((x2+x1)/2,(y2+y1)/2)

trianInver : List (Float, Float) -> List (Float, Float)
trianInver inv = case inv of 
    x1::x2::x3::xs -> (puntoMedio x1 x2)::(puntoMedio x1 x3)::(puntoMedio x3 x2)::[]
    _ -> []

st1 : List (Float , Float) -> List (Float , Float)
st1 lm = case lm of 
    x1::x2::x3::xs -> x1::(puntoMedio x1 x2)::(puntoMedio x3 x1)::[]
    _ -> []

st2 : List (Float , Float) -> List (Float , Float)
st2 lm = case lm of 
    x1::x2::x3::xs -> x2::(puntoMedio x1 x2)::(puntoMedio x2 x3)::[]
    _ -> []

st3 : List (Float , Float) -> List (Float , Float)
st3 lm = case lm of 
    x1::x2::x3::xs -> x3::(puntoMedio x3 x1)::(puntoMedio x2 x3)::[]
    _ -> []

fase : Int -> List (Float, Float) -> List(List (Float , Float))
fase h n = case (h , n) of 
    (0 , []) -> []
    (0 , x::xs) -> []
    (hs , x::xs) -> (x::xs)::(trianInver (x::xs))::(fase (hs - 1)(st1(x::xs)))++(fase (hs - 1)(st2(x::xs)))++(fase (hs - 1)(st3(x::xs)))
    (_ , []) -> []

sierpinsky x = if x == 0 then fractals else fase x fractal




dibujar triangulo context =
    let
        acc (x,y) s = s |> Canvas.lineTo x y
    in
        case triangulo of
            (x0,y0)::xs ->///
                List.foldl acc (context |> Canvas.moveTo x0 y0) ((x0,y0)::xs)
                |> Canvas.lineTo x0 y0
            _ -> context

dibujarTriangulo : List (List (Float, Float)) -> Canvas.Commands -> Canvas.Commands
dibujarTriangulo triangulo context = case triangulo of 
    [] -> context
    x::xs -> dibujar x (dibujarTriangulo xs context)

-- Funcion definida para crear el poligono en Html

vista : Modelo -> Html Mensaje
vista modelo = div[style "background" "HSL(181, 100%, 44%)"]
    [div[
        style "display" "flex", style "justify-content" "left", style "align-items" "left"
        ]
    [button [onClick (Fract Snowflake), style "height" "70px" , style"width" "100px",style "background" "HSL(181, 4%, 32%)",style "color" "HSL(138, 43%, 226%)"] [text "SNOWFLAKE"],
     button [onClick (Fract Sierpinsky), style "height" "70px" , style"width" "100px",style "background" "HSL(181, 4%, 32%)",style "color" "HSL(255, 222%, 177%)"] [text "SIERPINSKI"]],
    ////////////////////8//////////////
    div[style "display" "flex", style "justify-content" "left", style "align-items" "left"]
    [button [onClick (Repte 0), style "height" "70px" , style"width" "100px",style "background" "HSL(181, 4%, 32%)",style "color" "HSL(138, 43%, 226%)"] [Html.text "Disminuir"],
     button [onClick (Repte (1)), style "height" "70px" , style"width" "100px",style "background" "HSL(181, 4%, 32%)",style "color" "HSL(138, 43%, 226%)"] [Html.text "Aumentar"]],
   
   div[style "display" "flex", 
         style "justify-content" "center", 
         style "align-items" "center"]
    [
    let
        width = 600
        height = 550
        poligono = case (modelo.fract) of 
            Snowflake -> dibujar (snowflake (modelo.repte))
            Sierpinsky -> dibujarTriangulo (sierpinsky (modelo.repte))
            Fractal -> dibujar fractal
    in
        Canvas.element****************///////////////////////////////////////////////////////*//////////////////////////////9999999999999999999999999999999999**///////988988888//////////////////////////////////////9//9//8//9////////////////////////////////////9//////9//////////////////////////////////////
            width
            height
            [ style "border" "9px solid white",
              style "background""HSL(199, 77%, 54%)"
            ]
            (
                Canvas.empty
                |> Canvas.beginPath
                |> Canvas.clearRect 0 0 width height
                |> Canvas.lineWidth 3
                |> Canvas.fillStyle (Color.hsla 0 0 0 0.7)
                |> Canvas.fillRect 0 0 width height
                |> Canvas.strokeStyle (Color.hsl 280 100 62)
                |> poligono
                |> Canvas.stroke 
            )
    ] 
    
    ]

main =
    Browser.sandbox
        { init = modeloInicial
        , view = vista
        , update = actualizador
        }