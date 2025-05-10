module Library where
import PdePreludat


data Tesoro = Tesoro {
    antiguedad :: Number
    precioEnPesos :: Number
}deriving (Eq,Show)

--Parte 1
data Tipo = DeLujo | TelaSucia | Estandar deriving (Eq,Show)


evaluarTipo:: Tesoro -> Tipo
evaluarTipo tesoro 
    | antiguedad tesoro > 200 || precioEnPesos tesoro > 1000 = DeLujo
    | precioEnPesos tesoro < 50 = TelaSucia
    | otherwise = Estandar

tipoDeTesoro :: Tesoro -> Tipo
tipoDeTesoro tesoro = evaluarTipo tesoro

valorTesoro :: Tesoro -> Number
valorTesoro tesoro = precioEnPesos tesoro + (2 * antiguedad tesoro)

--Parte 2


data Cerradura = Cerradura{
    clave :: [Char]
}deriving (Eq,Show)


quitarPrimerosTresDigitos :: Cerradura -> Cerradura
quitarPrimerosTresDigitos (Cerradura (_:_:_:resto)) = Cerradura resto

martillo :: Cerradura -> Cerradura
martillo cerradura = quitarPrimerosTresDigitos cerradura

llaveMaestra :: Cerradura -> Cerradura
llaveMaestra (Cerradura lista) = Cerradura []

eliminarValorPasado :: [Char] -> [Char]
eliminarPasado lista = []

ganzuaGancho :: Cerradura -> Cerradura
ganzuaGancho (Cerradura lista) = eliminarValorPasado . filter isUpper lista  



ganzuaRastrillo :: Cerradura -> Cerradura
ganzuaRastrillo (Cerradura lista) = eliminarValorPasado . filter isDigit lista