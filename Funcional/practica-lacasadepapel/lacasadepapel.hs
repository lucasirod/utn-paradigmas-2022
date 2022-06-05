import Text.Show.Functions()

data Ladron = UnLadron {
    nombreLadron      :: Nombre,
    habilidades       :: [Habilidad],
    armas             :: [Arma]
} deriving (Show)

data Rehen = UnRehen {
    nombreRehen       :: Nombre,
    complot           :: Complot,
    miedo             :: Miedo,
    plan              :: Plan
} deriving (Show)

type Nombre = String
type Habilidad = String
type Arma = Rehen -> Rehen
type Complot = Int
type Miedo = Int
type Plan = Ladron -> Ladron


-------- MAPPERS, SETTERS Y OTROS ---------

mapNombreRehen :: (Nombre -> Nombre) -> Rehen -> Rehen
mapNombreRehen unaFuncion unRehen = unRehen { nombreRehen = unaFuncion . nombreRehen $ unRehen }

mapComplot :: (Complot -> Complot) -> Rehen -> Rehen
mapComplot unaFuncion unRehen = unRehen { complot = max 0 . unaFuncion . complot $ unRehen }

mapMiedo :: (Miedo -> Miedo) -> Rehen -> Rehen
mapMiedo unaFuncion unRehen = unRehen { miedo = max 0 . unaFuncion . miedo $ unRehen }

mapPlan :: (Plan -> Plan) -> Rehen -> Rehen
mapPlan unaFuncion unRehen = unRehen { plan = unaFuncion . plan $ unRehen }

setMiedo :: Miedo -> Rehen -> Rehen
setMiedo = mapMiedo . const

setComplot :: Complot -> Rehen -> Rehen
setComplot = mapComplot . const

incMiedo :: Miedo -> Rehen -> Rehen
incMiedo cantidad = mapMiedo (+cantidad)

incComplot :: Complot -> Rehen -> Rehen
incComplot cantidad = mapComplot (+cantidad)

decMiedo :: Miedo -> Rehen -> Rehen
decMiedo cantidad   = mapMiedo (subtract cantidad)

decComplot :: Complot -> Rehen -> Rehen
decComplot cantidad = mapComplot (subtract cantidad)


mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Ladron -> Ladron
mapHabilidades unaFuncion unLadron = unLadron { habilidades = unaFuncion . habilidades $ unLadron }

mapArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
mapArmas unaFuncion unLadron = unLadron { armas = unaFuncion . armas $ unLadron }



------ Modelado de Armas ------

pistola :: Int -> Arma
pistola unCalibre unRehen = decComplot (5*unCalibre) . incMiedo (3*length (nombreRehen unRehen)) $ unRehen

ametralladora :: Int -> Arma
ametralladora balas unRehen = mapComplot (`div` 2) . incMiedo balas $ unRehen




-------------------- PUNTO 1 --------------------------

tokio :: Ladron
tokio = UnLadron "Tokio" ["Trabajo Psicologico","Entrar en Moto"] [pistola 9, ametralladora 30]

profesor :: Ladron
profesor = UnLadron "Profesor" ["Disfrazarse de Linyera","Disfrazarse de Payaso","Estar siempre un paso adelante"] []

pablo :: Rehen
pablo = UnRehen "Pablo" 40 30 esconderse

arturito :: Rehen
arturito = UnRehen "Arturito" 70 50 (atacar pablo . esconderse) 



-------------------- PUNTO 2 --------------------------


esInteligente :: Ladron -> Bool
esInteligente unLadron = ((>2).length.habilidades) unLadron || nombreEs "Profesor" unLadron

nombreEs :: Nombre -> Ladron -> Bool
nombreEs unNombre = (==unNombre).nombreLadron


-------------------- PUNTO 3 --------------------------


conseguirArma :: Arma -> Ladron -> Ladron
conseguirArma unArma = mapArmas (unArma : )





-------------------- PUNTO 4 --------------------------


disparos :: Rehen -> Ladron -> Rehen
disparos unRehen unLadron = (armaDeMasMiedoPara unRehen unLadron) unRehen

armaDeMasMiedoPara :: Rehen -> Ladron -> Arma
armaDeMasMiedoPara unRehen unLadron = maximoSegun (miedo . ($ unRehen)) (armas unLadron)
                                                --el miedo luego de que el arma se aplique al rehen

maximoSegun :: (Arma -> Miedo) -> [Arma] -> Arma
maximoSegun unaFuncion unasArmas = foldl1 (mayorSegun unaFuncion) unasArmas

mayorSegun :: (Arma -> Miedo) -> Arma -> Arma -> Arma
mayorSegun unaFuncion unArma otraArma
  | unaFuncion unArma > unaFuncion otraArma = unArma
  | otherwise                               = otraArma


{-
maximoSegun unaFuncion unaLista= foldl1 (mayorSegun unaFuncion) unaLista

mayorSegun unaFuncion unElemento otroElemento
  | unaFuncion unElemento > unaFuncion otroElemento = unElemento
  | otherwise                                       = otroElemento

-}


hacerseElMalo :: Rehen -> Ladron -> Rehen
hacerseElMalo unRehen unLadron
    | nombreEs "Berlin" unLadron    =  incMiedo (cantidadDeLetrasHabilidades unLadron) unRehen
    | nombreEs "Rio"    unLadron    =  incComplot 20 unRehen
    | otherwise                     =  incMiedo 10 unRehen


cantidadDeLetrasHabilidades :: Ladron -> Int
cantidadDeLetrasHabilidades unLadron = sum . map length . habilidades $ unLadron




-------------------- PUNTO 5 --------------------------


calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas unLadron unosRehenes = filter ((>60).complot) . map (flip disparos unLadron) $ unosRehenes


-------------------- PUNTO 6 --------------------------

puedeEscaparse :: Ladron -> Bool
puedeEscaparse unLadron = any (listaEmpiezaCon "Disfrazarse de") (habilidades unLadron)

listaEmpiezaCon :: String -> String -> Bool
listaEmpiezaCon unaSubLista unaLista = (==unaSubLista) . take (length unaSubLista) $ unaLista --le saco la longitud de la sublista y me deberia quedar la sublista 



-------------------- PUNTO 7 --------------------------

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal unosLadrones unosRehenes = promedioDe complot unosRehenes > (promedioDe miedo unosRehenes * cantidadDeArmas unosLadrones)


promedioDe :: (Rehen -> Int) -> [Rehen] -> Int
promedioDe unaFuncion unosRehenes = ((sum . map unaFuncion) unosRehenes) `div` (length unosRehenes)

cantidadDeArmas :: [Ladron] -> Int
cantidadDeArmas unosLadrones = sum . map length . map armas  $ unosLadrones



-------------------- PUNTO 8 --------------------------

esconderse :: Plan
esconderse unLadron = mapArmas (( drop . flip div 3 . length . habilidades) unLadron) unLadron

atacar :: Rehen -> Plan
atacar unCompaniero unLadron = mapArmas ((drop .flip div 10 . length . nombreRehen) unCompaniero) unLadron



rebelarseContra :: [Rehen] -> Ladron -> Ladron
rebelarseContra unosRehenes unLadron = foldl rebelarse unLadron unosRehenes

rebelarse :: Ladron -> Rehen -> Ladron
rebelarse unLadron unRehen
  | seAnima unRehen      = (plan unRehen) unLadron
  | otherwise            = unLadron

seAnima :: Rehen -> Bool
seAnima unRehen = complot unRehen > miedo unRehen --porque la consigna dice que solo se rebelan si tienen mas complot que miedo



-------------------- PUNTO 9 --------------------------


planValencia :: [Ladron] -> [Rehen] -> Int
planValencia unosLadrones unosRehenes = (*1000000) . sum . map (length.armas) . map (rebelarseContra unosRehenes . conseguirArma (ametralladora 45)) $ unosLadrones



-------------------- PUNTO 10 --------------------------


{-

¿Se puede ejecutar el plan valencia si uno de los ladrones tiene una cantidad infinita de armas? Justifique.

No se puede, el plan valencia necesita saber la cantidad de armas del ladron.
La consulta nunca terminaría de ejecutarse y se colgaría


-------------------- PUNTO 11 --------------------------


¿Se puede ejecutar el plan valencia si uno de los ladrones tiene una cantidad infinita de habilidades? Justifique.


Puede ser sí, como no. Depende del plan que tiene el rehen.
Si el plan es esconderse necesita saber la cantidad de habilidades del ladron y ocurre lo mismo que se explicó anteriormente.

-}
