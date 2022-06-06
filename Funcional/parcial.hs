import Text.Show.Functions()

data Participante = UnParticipante {
    nombre :: Nombre,
    trucos :: [Truco],
    especialidad :: Plato
} deriving (Show)

data Plato = UnPlato {
    dificultad :: Dificultad,
    ingredientes :: [Ingrediente]
} deriving (Eq,Show)

data Ingrediente = UnIngrediente {
    nombreIngrediente :: Nombre,
    peso              :: Peso
} deriving (Eq,Show)

type Nombre = String
type Truco = Plato -> Plato
type Dificultad = Int
type Peso = Int




--------------------MAPPERS, SETTERS y otros-------------------------


mapNombre :: (Nombre -> Nombre) -> Participante -> Participante
mapNombre unaFuncion unParticipante = unParticipante { nombre = unaFuncion . nombre $ unParticipante}

mapTrucos :: ([Truco] -> [Truco]) -> Participante -> Participante
mapTrucos unaFuncion unParticipante = unParticipante { trucos = unaFuncion . trucos $ unParticipante}

mapEspecialidad :: (Plato -> Plato) -> Participante -> Participante
mapEspecialidad unaFuncion unParticipante = unParticipante { especialidad = unaFuncion . especialidad $ unParticipante}


mapDificultad :: (Dificultad -> Dificultad) -> Plato -> Plato
mapDificultad unaFuncion unPlato = unPlato { dificultad = max 0 . min 10 . unaFuncion . dificultad $ unPlato}

setDificultad :: Dificultad -> Plato -> Plato
setDificultad = mapDificultad . const

mapIngredientes :: ([Ingrediente] -> [Ingrediente]) -> Plato -> Plato
mapIngredientes unaFuncion unPlato = unPlato { ingredientes = unaFuncion . ingredientes $ unPlato}


mapNombreIngredientes :: (Nombre -> Nombre) -> Ingrediente -> Ingrediente
mapNombreIngredientes unaFuncion unIngrediente = unIngrediente { nombreIngrediente = unaFuncion . nombreIngrediente $ unIngrediente}

mapPeso :: (Peso -> Peso) -> Ingrediente -> Ingrediente
mapPeso unaFuncion unIngrediente = unIngrediente { peso = unaFuncion . peso $ unIngrediente}




--------------------PARTE A-------------------------


endulzar :: Peso -> Truco
endulzar  = agregarIngrediente "azucar" 

salar :: Peso -> Truco
salar  = agregarIngrediente "sal" 

agregarIngrediente :: Nombre -> Peso -> Plato -> Plato
agregarIngrediente unNombre unPeso = mapIngredientes (UnIngrediente unNombre unPeso :)



darSabor :: Peso -> Peso -> Truco
darSabor unPesoDeSal unPesoDeAzucar = endulzar unPesoDeAzucar . salar unPesoDeSal


duplicarPorcion :: Truco
duplicarPorcion = mapIngredientes (map duplicarPeso)

duplicarPeso :: Ingrediente -> Ingrediente
duplicarPeso = mapPeso (*2)



simplificar :: Truco
simplificar unPlato
    | esComplejo unPlato         = setDificultad 5 . quitarIngredientesMenoresA10Gramos $ unPlato
    | otherwise                  = unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = tieneMasDe5Componentes unPlato && dificultadMayorA7 unPlato

tieneMasDe5Componentes :: Plato -> Bool
tieneMasDe5Componentes = (>5) . length . ingredientes

dificultadMayorA7 :: Plato -> Bool
dificultadMayorA7 = (>7) . dificultad

quitarIngredientesMenoresA10Gramos :: Plato -> Plato
quitarIngredientesMenoresA10Gramos = mapIngredientes (filter ((>=10).peso))





esVegano :: Plato -> Bool
esVegano unPlato = all (esIngredienteVegano.nombreIngrediente)  . ingredientes $ unPlato

esIngredienteVegano :: Nombre -> Bool
esIngredienteVegano unNombre = (unNombre/="carne") && (unNombre/="huevos") && (unNombre/="lacteos")


esSinTacc :: Plato -> Bool
esSinTacc unPlato = all ((/="harina").nombreIngrediente) . ingredientes $ unPlato


noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = (>2) . sumarPesos . ingredientesQueSonSal . ingredientes $ unPlato

ingredientesQueSonSal :: [Ingrediente] -> [Ingrediente]
ingredientesQueSonSal = filter ((=="sal").nombreIngrediente)

sumarPesos :: [Ingrediente] -> Peso
sumarPesos = sum . map peso 



-------------------- PARTE B -------------------------

pepeRonccino :: Participante
pepeRonccino = UnParticipante "Pepe Ronccino" [darSabor 2 5, simplificar, duplicarPorcion] especialidadPepe

especialidadPepe :: Plato
especialidadPepe = UnPlato 9 [  UnIngrediente "sal" 5, 
                                UnIngrediente "huevos" 3, 
                                UnIngrediente "cebolla" 5, 
                                UnIngrediente "pimienta" 1, 
                                UnIngrediente "carne" 300,
                                UnIngrediente "salsa" 3 ] 



-------------------- PARTE C -------------------------


cocinar :: Participante -> Plato
cocinar unParticipante = foldr ($) (especialidad unParticipante) (trucos unParticipante)



esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = tieneMasDificultad unPlato otroPlato && sumatoriaDePesosPlato unPlato < sumatoriaDePesosPlato otroPlato

tieneMasDificultad :: Plato -> Plato -> Bool
tieneMasDificultad unPlato otroPlato = dificultad unPlato > dificultad otroPlato

sumatoriaDePesosPlato :: Plato -> Peso
sumatoriaDePesosPlato = sumarPesos . ingredientes



participanteEstrella :: [Participante] -> Participante
participanteEstrella unosParticipantes = foldl1 dejarElMejor unosParticipantes

dejarElMejor :: Participante -> Participante -> Participante
dejarElMejor unParticipante otroParticipante
    | esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
    | otherwise                                                      = otroParticipante




platinum :: Plato
platinum = UnPlato 10 (map generarIngrediente [1..] )

generarIngrediente :: Int -> Ingrediente
generarIngrediente unNumero = UnIngrediente ("Ingrediente " ++ show unNumero) unNumero



{-¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?

endulzar: Lista infinita que nunca terminara
salar: Lista infinita que nunca terminara
darSabor: Lista infinita que nunca terminara

En estos casos se agrega la sal y/o azucar al principio (porque por lazy evaluation no necesita saber toda la lista para realizarlo) pero luego se genera una lista de ingredientes infinitos que nunca termina.

duplicarPorcion: Lista infinita que nunca terminara

En este caso se realiza el duplicado (porque por lazy evaluation no necesita saber toda la lista para realizarlo) pero luego se muestra la lista de ingredientes infinitos con pesos duplicados que nunca termina

simplificar: se queda colgado, ya que no puede saber si es complejo o no porque no llega a evaluar todos los componentes





¿Cuáles de las preguntas de la Parte A (esVegano, esSinTacc, etc.) se pueden responder sobre el platinum? 


Ninguna, se queda colgado, ya que en todas las preguntas no llega a responder todos los ingredientes de platinum






¿Se puede saber si el platinum es mejor que otro plato?


No, se queda colgado, no puede hacer la sumatoria de los pesos de los ingredientes ya que no llega a saber todos los ingredientes.



-}


