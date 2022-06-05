module Lib where
import Text.Show.Functions()

-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = UnaHabilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (UnaHabilidad 25 60)
todd = UnJugador "Todd" "Ned" (UnaHabilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (UnaHabilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)


type Puntos = Int

-- Funciones útiles

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

----------------------------------------------
---- Resolución del ejercicio
----------------------------------------------

mapVelocidad :: (Int -> Int) -> Tiro -> Tiro
mapVelocidad unaFuncion unTiro = unTiro { velocidad = unaFuncion . velocidad $ unTiro }

mapPrecision :: (Int -> Int) -> Tiro -> Tiro
mapPrecision unaFuncion unTiro = unTiro { precision = unaFuncion . precision $ unTiro }

mapAltura :: (Int -> Int) -> Tiro -> Tiro
mapAltura unaFuncion unTiro = unTiro { altura = unaFuncion . altura $ unTiro }

setVelocidad :: Int -> Tiro -> Tiro
setVelocidad = mapVelocidad . const

setPrecision :: Int -> Tiro -> Tiro
setPrecision = mapPrecision . const

setAltura :: Int -> Tiro -> Tiro
setAltura = mapAltura . const



-------------------- PUNTO 1 --------------------------

type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = UnTiro 10 (precisionJugador unaHabilidad * 2) 0

madera :: Palo
madera unaHabilidad = UnTiro 100 (precisionJugador unaHabilidad `div` 2) 5 

hierro :: Int -> Palo
hierro unNumero unaHabilidad = UnTiro (fuerzaJugador unaHabilidad * unNumero) (precisionJugador unaHabilidad `div` unNumero) ((unNumero-3) `max` 0) 

palosDeHierro :: [Palo]
palosDeHierro = map hierro [1..10]

palos :: [Palo]
palos = [putter,madera] ++ palosDeHierro




-------------------- PUNTO 2 --------------------------

golpe :: Jugador -> Palo -> Tiro
golpe unJugador unPalo = unPalo.habilidad $ unJugador






-------------------- PUNTO 3 --------------------------

data Obstaculo = UnObstaculo {
  puedeSuperar :: Condicion,
  efecto       :: Efecto
} deriving (Show)

type Condicion = Tiro -> Bool
type Efecto    = Tiro -> Tiro


tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo condicionTunelConRampita efectoTunelConRampita

condicionTunelConRampita :: Condicion
condicionTunelConRampita unTiro = ((>90).precision) unTiro && alRasDelSuelo unTiro

efectoTunelConRampita :: Efecto
efectoTunelConRampita = mapVelocidad (*2) . setPrecision 100 . setAltura 0



laguna :: Int -> Obstaculo
laguna unLargo = UnObstaculo condicionLaguna (efectoLaguna unLargo)

condicionLaguna ::  Condicion
condicionLaguna unTiro = ((>80).velocidad) unTiro && (between 1 5.altura) unTiro

efectoLaguna :: Int -> Efecto
efectoLaguna unLargo = mapAltura (`div` unLargo)




hoyo :: Obstaculo
hoyo = UnObstaculo condicionHoyo efectoHoyo

condicionHoyo :: Condicion
condicionHoyo unTiro = (between 5 20.velocidad) unTiro && alRasDelSuelo unTiro && ((>95).precision) unTiro

efectoHoyo :: Efecto
efectoHoyo _ = tiroDetenido




intentarSuperarObstaculo ::  Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo unObstaculo unTiro
    | puedeSuperar unObstaculo unTiro       = efecto unObstaculo unTiro
    | otherwise                             = tiroDetenido

alRasDelSuelo :: Tiro -> Bool
alRasDelSuelo = (==0).altura

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0


-------------------- PUNTO 4A --------------------------


palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (paloLeSirveParaSuperar unJugador unObstaculo) palos

paloLeSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
paloLeSirveParaSuperar unJugador unObstaculo = puedeSuperar unObstaculo . efecto unObstaculo . golpe unJugador




-------------------- PUNTO 4B --------------------------


cuantosObstaculosConsecutivosSupera ::[Obstaculo] -> Tiro ->  Int
cuantosObstaculosConsecutivosSupera [] _                 = 0
cuantosObstaculosConsecutivosSupera (cabeza:cola) unTiro 
  | puedeSuperar cabeza unTiro   = 1 + cuantosObstaculosConsecutivosSupera cola (efecto cabeza unTiro)
  | otherwise                    = 0



-------------------- PUNTO 4C --------------------------


paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador unosObstaculos =  maximoSegun (cuantosObstaculosConsecutivosSupera unosObstaculos . golpe unJugador) palos



-------------------- PUNTO 5 --------------------------

type Resultado = (Jugador,Puntos)

padresQuePierdenApuesta :: [Resultado] -> [String]
padresQuePierdenApuesta unosResultados = map (padre.fst) (niñosQueNoGanaron unosResultados)

niñosQueNoGanaron :: [Resultado] -> [Resultado]
niñosQueNoGanaron unosResultados = filter (not.jugadorGano unosResultados) unosResultados

jugadorGano :: [Resultado] -> Resultado -> Bool
jugadorGano unosResultados resultadoDeJugador = ( all ((< snd resultadoDeJugador).snd) . filter (/= resultadoDeJugador) )  unosResultados

--Otra forma de pensarlo que anda pero estaria mal en caso de empate porque segun la consigna SOLO se gana si tener mas puntos que los demas, si hay empate nadie gana
jugadorGano' :: [Resultado] -> Resultado -> Bool
jugadorGano' unosResultados resultadoDeJugador = snd resultadoDeJugador == maximum (map snd unosResultados)