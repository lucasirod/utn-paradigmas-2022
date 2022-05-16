import Text.Show.Functions()

data Participante = UnParticipante {
    nombre          :: Nombre,
    dinero          :: Dinero,
    tactica         :: Tactica,
    propiedades     :: [Propiedad],
    acciones        :: [Accion]
} deriving (Show)

type Accion = Participante -> Participante
type Nombre = String
type Tactica = String
type Dinero = Int
type Precio = Int
type Propiedad = (Nombre,Precio)


------MODELO PARTICIPANTES ----------

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista" [] [pasarPorElBanco,pagarAAccionistas]

manuel :: Participante
manuel = UnParticipante "Manuel" 500 "Oferente singular" [] [pasarPorElBanco,enojarse]

lucas :: Participante
lucas = UnParticipante "Manuel" 500 "Oferente singular" [bombonera,monumental,nuevoGasometro] [pasarPorElBanco,enojarse]

--MODELO TAMBIEN ALGUNAS PROPIEDADES

monumental :: Propiedad
monumental = ("Estadio Monumental",560)

bombonera :: Propiedad
bombonera = ("Bombonera",200)

cilindro :: Propiedad
cilindro = ("Cilindro de Avellaneda",160)

nuevoGasometro :: Propiedad
nuevoGasometro = ("Nuevo Gasometro",100)



-- MAPPERS y SETTERS
mapNombre :: (String -> String) -> Participante -> Participante
mapNombre unaFuncion unParticipante = unParticipante {nombre = unaFuncion . nombre $ unParticipante}

mapDinero :: (Dinero -> Dinero) -> Participante -> Participante
mapDinero unaFuncion unParticipante = unParticipante {dinero = unaFuncion . dinero $ unParticipante}

mapPropiedades :: ([Propiedad] -> [Propiedad]) -> Participante -> Participante
mapPropiedades unaFuncion unParticipante = unParticipante {propiedades = unaFuncion . propiedades $ unParticipante}

mapTactica :: (String -> String) -> Participante -> Participante
mapTactica unaFuncion unParticipante = unParticipante {tactica = unaFuncion . tactica $ unParticipante}

mapAcciones :: ([Accion] -> [Accion]) -> Participante -> Participante
mapAcciones unaFuncion unParticipante = unParticipante {acciones = unaFuncion . acciones $ unParticipante}

--OTRAS FUNCIONES UTILES

setTactica :: Tactica -> Participante -> Participante
setTactica = mapTactica.const

agregarAccion :: Accion -> Participante -> Participante
agregarAccion unaAccion unParticipante = mapAcciones (unaAccion:) unParticipante

agregarPropiedad :: Propiedad -> Participante -> Participante
agregarPropiedad unaPropiedad unParticipante = mapPropiedades (unaPropiedad:) unParticipante

--ACCESORS

nombrePropiedad :: Propiedad -> Nombre
nombrePropiedad (unNombre,_) = unNombre

precio :: Propiedad -> Precio
precio (_,unPrecio) = unPrecio





pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = setTactica "Comprador Compulsivo".mapDinero (+40) $ unParticipante

enojarse :: Accion
enojarse unParticipante = agregarAccion gritar. mapDinero (+50) $ unParticipante

gritar :: Accion
gritar unParticipante = mapNombre ("AHHHH"++) unParticipante



subastar :: Propiedad -> Accion
subastar unaPropiedad unParticipante
    | puedeGanarPropiedad unParticipante    = comprarUnaPropiedad unaPropiedad unParticipante
    | otherwise                             = unParticipante

puedeGanarPropiedad :: Participante -> Bool
puedeGanarPropiedad unParticipante = ((=="Oferente Singular").tactica) unParticipante  || ((=="Accionista").tactica) unParticipante --aca si usaba el $ en las dos condiciones chillaba

comprarUnaPropiedad :: Propiedad -> Participante -> Participante
comprarUnaPropiedad unaPropiedad unParticipante = mapDinero (subtract (precio unaPropiedad)).agregarPropiedad unaPropiedad $ unParticipante
--                                              = mapDinero (+ (- precio unaPropiedad)).agregarPropiedad unaPropiedad $ unParticipante

--                                              LO HICE ASI PORQUE CON EL (- precio unaPropiedad) NO ME DEJABA, porque es esto?

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = mapDinero (+totalACobrar unParticipante) unParticipante

totalACobrar :: Participante -> Dinero
totalACobrar unParticipante = 10 * (cantidadPropiedadesBaratas unParticipante) + 20 * (cantidadPropiedadesCaras unParticipante)

cantidadPropiedadesBaratas :: Participante -> Int
cantidadPropiedadesBaratas unParticipante = length . filter (<150) . map precio . propiedades $ unParticipante

cantidadPropiedadesCaras :: Participante -> Int
cantidadPropiedadesCaras unParticipante = (length.propiedades) unParticipante - cantidadPropiedadesBaratas unParticipante  
--porque no puedo hacer length.propiedades $ unParticipante - cantidadPropiedadesBaratas unParticipante?



pagarAAccionistas :: Accion
pagarAAccionistas unParticipante
    | esAccionista unParticipante    = mapDinero (+200) unParticipante
    | otherwise                      = mapDinero (subtract 100) unParticipante

esAccionista :: Participante -> Bool
esAccionista unParticipante = (=="Accionista").tactica $ unParticipante




hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unParticipante
    | alcanzaElDinero unaPropiedad unParticipante = comprarUnaPropiedad unaPropiedad unParticipante
    | otherwise                                   = hacerBerrinchePor unaPropiedad (mapDinero (+10).gritar $ unParticipante)


alcanzaElDinero :: Propiedad -> Participante -> Bool
alcanzaElDinero unaPropiedad unParticipante = dinero unParticipante >= precio unaPropiedad


--FUNCION PENDIENTE DE HACER
ultimaRonda :: Accion
ultimaRonda unParticipante = unParticipante
-----------------


juegoFinal :: Participante -> Participante -> Participante
juegoFinal unParticipante otroParticipante
    | tieneMasDineroAlFinalizar unParticipante otroParticipante = unParticipante
    | otherwise                                                 = otroParticipante
-- no se contempla el empate en la consigna? tengo que devolver el participante y si empatan no puedo poner un string con el mensaje "empate"


tieneMasDineroAlFinalizar :: Participante -> Participante -> Bool
tieneMasDineroAlFinalizar unParticipante otroParticipante = (dinero.ultimaRonda) unParticipante > (dinero.ultimaRonda) otroParticipante
