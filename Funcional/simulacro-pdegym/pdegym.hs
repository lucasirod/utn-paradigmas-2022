import Text.Show.Functions ()

data Persona = UnaPersona {
    nombre              :: Nombre,
    calorias            :: Calorias,
    hidratacion         :: Hidratacion,
    tiempo              :: Tiempo,
    equipamiento        :: Equipamiento
} deriving Show

type Nombre = String
type Calorias = Int
type Hidratacion = Int
type Tiempo = Int
type Equipamiento = [String]
type Ejercicio = Persona -> Persona
type Repeticiones = Int
type Peso = Int

type Accion = Persona -> Persona


--MAPPERS y SETTERS

mapNombre :: (Nombre -> Nombre) -> Persona -> Persona
mapNombre    f unaPersona = unaPersona { nombre    = f . nombre    $ unaPersona }

mapCalorias :: (Calorias -> Calorias) -> Persona -> Persona
mapCalorias    f unaPersona = unaPersona { calorias    = f . calorias    $ unaPersona }

mapHidratacion :: (Hidratacion -> Hidratacion)-> Persona -> Persona
mapHidratacion  f unaPersona = unaPersona { hidratacion  = f . hidratacion  $ unaPersona }

mapTiempo :: (Tiempo -> Tiempo)-> Persona -> Persona
mapTiempo  f unaPersona = unaPersona { tiempo  = f . tiempo  $ unaPersona }

mapEquipamiento :: (Equipamiento -> Equipamiento)-> Persona -> Persona
mapEquipamiento  f unaPersona = unaPersona { equipamiento  = f . equipamiento  $ unaPersona }

setEquipamiento :: Equipamiento -> Persona -> Persona
setEquipamiento  = mapEquipamiento . const

setHidratacion :: Hidratacion -> Persona -> Persona
setHidratacion  = mapHidratacion . const


lucas :: Persona
lucas = UnaPersona "Lucas" 1000 90 60 []

maxi :: Persona
maxi = UnaPersona "Maxi" 700 40 120 ["pesa"]



--------------------------------------- PARTE A -----------------------------------------------


--EJERCICIOS

abdominales :: Repeticiones -> Ejercicio
abdominales unasRepeticiones unaPersona = mapCalorias (restarParametroDe 8 unasRepeticiones) $ unaPersona 

flexiones :: Repeticiones -> Ejercicio
flexiones unasRepeticiones unaPersona = mapCalorias (restarParametroDe 16 unasRepeticiones) . mapHidratacion (restarParametroDe 2 (cuantasVecesRestar unasRepeticiones)) $ unaPersona


levantarPesas :: Repeticiones -> Peso -> Ejercicio
levantarPesas unasRepeticiones unPeso unaPersona 
    | tienePesa unaPersona  = mapCalorias (restarParametroDe 32 unasRepeticiones) . mapHidratacion (restarParametroDe unPeso (cuantasVecesRestar unasRepeticiones)) $ unaPersona
    | otherwise             = unaPersona


tienePesa :: Persona -> Bool
tienePesa unaPersona = any (=="pesa") . equipamiento $ unaPersona

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson unaPersona = unaPersona


restarParametroDe :: Int -> Repeticiones -> (Int -> Int)
restarParametroDe unMultiplicador unasRepeticiones = subtract (unMultiplicador * unasRepeticiones)

cuantasVecesRestar :: Repeticiones -> Int
cuantasVecesRestar unasRepeticiones = div unasRepeticiones 10



--ACCIONES

renovarEquipo :: Accion
renovarEquipo unaPersona = mapEquipamiento (map ("Nuevo"++)) unaPersona


volverseYoguista :: Accion
volverseYoguista unaPersona = mapCalorias (`div` 2) . duplicarHidratacion . setEquipamiento ["colchoneta"] $ unaPersona

duplicarHidratacion :: Persona -> Persona
duplicarHidratacion unaPersona
    | sePasaDe100AlDuplicarHidratacion unaPersona  = setHidratacion 100 unaPersona
    | otherwise                                    = mapHidratacion (*2) unaPersona

sePasaDe100AlDuplicarHidratacion :: Persona -> Bool
sePasaDe100AlDuplicarHidratacion unaPersona = (hidratacion.mapHidratacion (*2)) unaPersona > 100


volverseBodyBuilder :: Accion
volverseBodyBuilder unaPersona 
    | tieneTodasPesas unaPersona      = mapNombre ("BB"++) . mapCalorias (*3) $ unaPersona
    | otherwise                       = unaPersona

tieneTodasPesas :: Persona -> Bool
tieneTodasPesas unaPersona = all (=="pesa") . equipamiento $ unaPersona --Si no tiene equipamiento puede volverse Body Builder?


comerUnSandwich :: Accion
comerUnSandwich unaPersona = mapCalorias (+500) . setHidratacion 100 $ unaPersona



--------------------------------------- PARTE B -----------------------------------------------



data Rutina = UnaRutina {
    duracion :: Tiempo,
    ejercicios :: [Ejercicio]
}


hacerRutina :: Rutina -> Persona -> Persona
hacerRutina unaRutina unaPersona 
    | puedeHacerRutina unaRutina unaPersona = foldl (flip ($)) unaPersona (ejercicios unaRutina)
    | otherwise                             = unaPersona

puedeHacerRutina :: Rutina -> Persona -> Bool
puedeHacerRutina unaRutina unaPersona = duracion unaRutina < tiempo unaPersona


esPeligrosa :: Rutina -> Persona -> Bool
esPeligrosa unaRutina unaPersona = quedoAgotada . hacerRutina unaRutina $ unaPersona 

quedoAgotada :: Persona -> Bool
quedoAgotada unaPersona = ((<50).calorias) unaPersona && ((<10).hidratacion) unaPersona


esBalanceada :: Rutina -> Persona -> Bool
esBalanceada unaRutina unaPersona = cumpleBalanceada unaPersona . hacerRutina unaRutina $ unaPersona

cumpleBalanceada :: Persona -> Persona -> Bool
cumpleBalanceada personaAlPrincipio personaAlFinal = ((>80).hidratacion) personaAlFinal && (calorias personaAlFinal < calorias personaAlPrincipio `div` 2)


--elAbominableAbdominal :: Rutina
--elAbominableAbdominal = UnaRutina 60 abdominalesInfinitos


--------------------------------------- PARTE C -----------------------------------------------


type Grupo = [Persona]


seleccionarGrupoDeEjercicio :: Persona -> Grupo -> Grupo
seleccionarGrupoDeEjercicio unaPersona unGrupo = filter ((== tiempo unaPersona).tiempo) unGrupo



type Promedio = (Calorias,Hidratacion)

promedioDeRutina :: Rutina -> Grupo -> Promedio
promedioDeRutina unaRutina unGrupo = ( sacarPromedioDe calorias unGrupo , sacarPromedioDe (hidratacion.hacerRutina unaRutina) unGrupo )

sacarPromedioDe :: (Persona -> Int) -> Grupo -> Int
sacarPromedioDe unaFuncion unGrupo = foldl1 (+) (map unaFuncion unGrupo) `div` length unGrupo
