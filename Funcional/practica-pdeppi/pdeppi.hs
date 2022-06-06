import Text.Show.Functions()


--------------------- PARTE A --------------------

data Persona = UnaPersona {
    nombrePersona   :: Nombre,
    direccion       :: Direccion,
    dinero          :: Dinero,
    comidaFavorita  :: Comida,
    cupones         :: [Cupon]
} deriving (Show)

data Comida = UnaComida {
    nombreComida    :: Nombre,
    costo           :: Costo,
    ingredientes    :: [Ingrediente]
} deriving (Show)

type Nombre = String
type Direccion = String
type Dinero = Int
type Costo = Int
type Ingrediente = String

paula :: Persona
paula = UnaPersona "Paula" "Thames 1585" 3600 hamburguesaDeluxe []

hamburguesaDeluxe :: Comida
hamburguesaDeluxe = UnaComida "Hamburguesa Deluxe" 350 ["pan","carne","lechuga","tomate","panceta","queso","huevo frito"]

lucas :: Persona
lucas = UnaPersona "Lucas" "UTNBA" 2000 hamburguesaDeluxe [largaDistancia,semanaVegana]




--------------------- MAPPERS, SETTERS y otros --------------------

mapNombrePersona :: (Nombre -> Nombre) -> Persona -> Persona
mapNombrePersona unaFuncion unaPersona = unaPersona { nombrePersona = unaFuncion . nombrePersona $ unaPersona}

mapDireccion :: (Direccion -> Direccion) -> Persona -> Persona
mapDireccion unaFuncion unaPersona = unaPersona { direccion = unaFuncion . direccion $ unaPersona}

mapDinero :: (Dinero -> Dinero) -> Persona -> Persona
mapDinero unaFuncion unaPersona = unaPersona { dinero = unaFuncion . dinero $ unaPersona}

mapComidaFavorita :: (Comida -> Comida) -> Persona -> Persona
mapComidaFavorita unaFuncion unaPersona = unaPersona { comidaFavorita = unaFuncion . comidaFavorita $ unaPersona}

setComidaFavorita :: Comida -> Persona -> Persona
setComidaFavorita = mapComidaFavorita . const

decDinero :: Dinero -> Persona -> Persona
decDinero cantidad = mapDinero (subtract cantidad)


mapNombreComida :: (Nombre -> Nombre) -> Comida -> Comida
mapNombreComida unaFuncion unaComida = unaComida { nombreComida = unaFuncion . nombreComida $ unaComida}

mapCosto :: (Costo -> Costo) -> Comida -> Comida
mapCosto unaFuncion unaComida = unaComida { costo = unaFuncion . costo $ unaComida}

incCosto :: Costo -> Comida -> Comida
incCosto cantidad = mapCosto (+ cantidad)

mapIngredientes :: ([Ingrediente] -> [Ingrediente]) -> Comida -> Comida
mapIngredientes unaFuncion unaComida = unaComida { ingredientes = unaFuncion . ingredientes $ unaComida}




--------------------- PARTE B --------------------


comprar :: Persona -> Comida -> Persona
comprar unaPersona unaComida
    | leAlcanza unaPersona unaComida && comidaBarata unaComida        = decDinero (costo unaComida) unaPersona
    | leAlcanza unaPersona unaComida && (not.comidaBarata) unaComida  = decDinero (costo unaComida) . setComidaFavorita unaComida $ unaPersona
    | otherwise                                                       = unaPersona

leAlcanza :: Persona -> Comida -> Bool
leAlcanza unaPersona unaComida = dinero unaPersona >= costo unaComida

comidaBarata :: Comida -> Bool
comidaBarata = (<200).costo




carritoDeCompras :: [Comida] -> Persona -> Persona
carritoDeCompras unasComidas unaPersona = foldl (\persona comida -> comprar persona comida) (decDinero 100 unaPersona) unasComidas




type Cupon = Comida -> Comida


semanaVegana :: Cupon
semanaVegana unaComida 
    | esVegana unaComida = mapCosto (`div` 2) unaComida
    | otherwise          = unaComida


esVegana :: Comida -> Bool
esVegana unaComida = not (contieneIngrediente "carne" unaComida || contieneIngrediente "huevos" unaComida || contieneIngrediente "queso" unaComida)

contieneIngrediente :: Ingrediente -> Comida -> Bool
contieneIngrediente unIngrediente = elem unIngrediente . ingredientes 



esoNoEsCocaPapi :: Ingrediente -> Cupon
esoNoEsCocaPapi unaBebida unaComida = mapNombreComida (++"Party") . mapIngredientes (unaBebida :) $ unaComida


sinTACCis :: Cupon
sinTACCis unaComida = mapIngredientes (map (++" libre de gluten")) unaComida


findeVegetariano :: Cupon
findeVegetariano unaComida
    | esVegetariana unaComida     = mapCosto ((`div` 100 ) . (*70)) unaComida
    | otherwise                   = unaComida


esVegetariana :: Comida -> Bool
esVegetariana unaComida = not (contieneIngrediente "carne" unaComida)

largaDistancia :: Cupon
largaDistancia unaComida = mapIngredientes ( filter ((<=10).length) ) . incCosto 50 $ unaComida




--------------------- PARTE C --------------------


comprarConCupones :: Persona -> Persona
comprarConCupones unaPersona = comprar unaPersona ( (aplicarCupones (cupones unaPersona) . comidaFavorita) unaPersona)

aplicarCupones :: [Cupon] -> Comida -> Comida
aplicarCupones unosCupones unaComida = foldl (flip ($)) unaComida unosCupones 

{- otra resol:

aplicarCupones :: [Cupon] -> Comida -> Comida
aplicarCupones []      comida = comida
aplicarCupones [x]     comida = x comida
aplicarCupones (x:xs)  comida = aplicarCupones xs (x comida) 

-}


superComida :: [Comida] -> Comida
superComida unasComidas = UnaComida {
    costo           = costoSuperComida unasComidas,
    nombreComida    = eliminarVocales . concatenarNombres $ unasComidas,
    ingredientes    = sinRepetidos . concatenarIngredientes $ unasComidas
}


costoSuperComida :: [Comida] -> Costo
costoSuperComida = sum . map costo


concatenarNombres :: [Comida] -> Nombre
concatenarNombres unasComidas = foldl1 (++) (map nombreComida unasComidas)
--concatenarNombres unasComidas = concatMap ((" "++) . nombreComida) unasComidas

eliminarVocales :: Nombre -> Nombre
eliminarVocales = filter (not.esVocal)

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"



concatenarIngredientes :: [Comida] -> [Ingrediente]
concatenarIngredientes unasComidas = concatMap ingredientes unasComidas

sinRepetidos :: [Ingrediente] -> [Ingrediente]
sinRepetidos []            = []
sinRepetidos (cabeza:cola) = cabeza : sinRepetidos (filter (/= cabeza) cola) 






compraDeluxe :: [Comida] -> Persona -> Persona
compraDeluxe unasComidas unaPersona = comprar unaPersona ((mapCosto (*2)  . superComida . lasDeCostoMenorA400) $ unasComidas)


lasDeCostoMenorA400 :: [Comida] -> [Comida]
lasDeCostoMenorA400 = filter ((<400).costo)
