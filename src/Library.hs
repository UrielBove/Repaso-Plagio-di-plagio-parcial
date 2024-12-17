{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Obra = UnaObra{
    texto :: String,
    anioPublicacion :: Number
} deriving (Show, Eq)

data Autor = UnAutor{
    nombreAutor :: String,
    obras :: [Obra]
}deriving (Show, Eq)

obraA :: Obra
obraA = UnaObra "Había una vez un pato." 1997

obraB :: Obra
obraB = UnaObra "¡Habia una vez un pato!" 1996

obraC :: Obra
obraC = UnaObra "Mirtha, Susana y Moria." 2010

obraD :: Obra
obraD = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020

obraE :: Obra
obraE = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022

--2

sinAcento :: Char -> Char
sinAcento 'á' = 'a'
sinAcento 'é' = 'e'
sinAcento 'í' = 'i'
sinAcento 'ó' = 'o'
sinAcento 'ú' = 'u'
sinAcento 'Á' = 'A'
sinAcento 'É' = 'E'
sinAcento 'Í' = 'I'
sinAcento 'Ó' = 'O'
sinAcento 'Ú' = 'U'

todasLasLetrasYNum :: [Char]
todasLasLetrasYNum = ['a'..'z']++['A'..'z']++"0123456789"

tieneTodasLetrasYNum :: Char -> Bool
tieneTodasLetrasYNum letra =  letra `elem` todasLasLetrasYNum

sacarAcentos :: [Char] -> [Char]
sacarAcentos = map sinAcento

versionCruda :: String -> String
versionCruda = filter tieneTodasLetrasYNum . sacarAcentos

--3
type FormaDeDeteccion = Obra -> Obra -> Bool

copiaLiteral :: FormaDeDeteccion
copiaLiteral obra1 obra2 = versionCruda (texto obra1) == versionCruda (texto obra2)

empiezaIgual :: Number -> FormaDeDeteccion
empiezaIgual cant obra1 obra2 = take cant (texto obra1) == take cant (texto obra2)

leAgregaronIntro :: FormaDeDeteccion
leAgregaronIntro obraOriginal obra2 = ultimosElementos (texto obraOriginal) (texto obra2) == texto obraOriginal

ultimosElementos :: String -> String ->[Char]
ultimosElementos textoOrig texto2= drop (length texto2 - length textoOrig) texto2

cantIgualDeElementos = \obra1 obra2 -> length (texto obra1) == length (texto obra2)

--4

data Bot = UnBot{
    formasDePlagio :: [FormaDeDeteccion],
    fabricante :: String
} deriving (Show, Eq)

bot1 :: Bot
bot1 = UnBot [empiezaIgual 10, leAgregaronIntro] "uri"

bot2 :: Bot
bot2 = UnBot [copiaLiteral, cantIgualDeElementos] "botazo"

--5

detectarPlagio :: Bot -> Obra -> Obra -> Bool
detectarPlagio bot obra1 obra2 = any (verificaFormaDeDeteccion obra1 obra2)  (formasDePlagio bot)

verificaFormaDeDeteccion :: Obra -> Obra -> FormaDeDeteccion -> Bool
verificaFormaDeDeteccion obraOriginal obra2 formaDeDeteccion = anioPublicacion obraOriginal < anioPublicacion obra2 && formaDeDeteccion obraOriginal obra2

