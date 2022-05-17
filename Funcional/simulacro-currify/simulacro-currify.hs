import Text.Show.Functions()

data Cancion = UnaCancion {
    titulo      :: Titulo,
    genero      :: Genero,
    duracion    :: Duracion
}deriving(Show)

data Artista = UnArtista{
    nombre      :: Nombre,
    canciones   :: [Cancion],
    efecto      :: Efecto
}deriving(Show)

type Titulo = String
type Genero = String
type Duracion = Int
type Nombre = String
type Efecto = Cancion -> Cancion


-------------------------------MAPPERS Y SETTERS------------------------------

mapDuracion :: (Duracion -> Duracion) -> Cancion -> Cancion
mapDuracion unaFuncion  unaCancion = unaCancion{ duracion = unaFuncion.duracion $ unaCancion}

mapTitulo :: (Titulo -> Titulo) -> Cancion -> Cancion
mapTitulo unaFuncion  unaCancion = unaCancion{ titulo = unaFuncion.titulo $ unaCancion}

mapGenero :: (Genero -> Genero) -> Cancion -> Cancion
mapGenero unaFuncion  unaCancion = unaCancion{ genero = unaFuncion.genero $ unaCancion}

setGenero :: Genero -> Cancion -> Cancion
setGenero = mapGenero . const

setDuracion :: Duracion -> Cancion -> Cancion
setDuracion = mapDuracion . const

mapCanciones :: ([Cancion] -> [Cancion]) -> Artista -> Artista
mapCanciones unaFuncion unArtista = unArtista{ canciones = unaFuncion.canciones $ unArtista}

mapNombre :: (Nombre -> Nombre) -> Artista -> Artista
mapNombre unaFuncion unArtista = unArtista{ nombre = unaFuncion.nombre $ unArtista}

setNombre :: Nombre -> Artista -> Artista
setNombre = mapNombre . const



-------------------------------MODELADO DE CANCIONES Y ARTISTAS------------------------------

cafeParaDos :: Cancion
cafeParaDos = UnaCancion "Cafe para Dos" "rock melancolico" 146

fuiHastaAhi :: Cancion
fuiHastaAhi = UnaCancion "Fui hasta ahi" "rock" 279

losEscarabajos :: Artista
losEscarabajos = UnArtista "Los escarabajos" [rocketRaccoon,mientrasMiBateriaFesteja,tomateDeMadera] acortar

rocketRaccoon :: Cancion
rocketRaccoon = UnaCancion "Rocket Racccoon" "metal" 179

mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = UnaCancion "Mientras mi bateria festeja" "heavy metal" 400

tomateDeMadera :: Cancion
tomateDeMadera = UnaCancion "Tomate de madera" "infantil" 100

adela :: Artista
adela = UnArtista "Adela" [teAcordas,unPibeComoVos,daleMechaALaLluvia] remixar

unPibeComoVos :: Cancion
unPibeComoVos = UnaCancion "Un pibe como vos" "romantico" 245

teAcordas :: Cancion
teAcordas = UnaCancion "¿Te Acordas?" "romantico" 210

daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = UnaCancion "Dale mecha a la lluvia" "romantico" 196

elTigreJoaco :: Artista
elTigreJoaco = UnArtista "El Tigre Joaco" [] (acustizar 360)



------------------------------- EFECTOS PARTE A ------------------------------

acortar :: Efecto
acortar unaCancion = mapDuracion (subtract 60) unaCancion

remixar :: Efecto
remixar unaCancion = mapTitulo (++"remix") . mapDuracion (*2) . setGenero "remixado" $ unaCancion

acustizar :: Duracion -> Efecto
acustizar unaDuracion unaCancion
    | noEsAcustica unaCancion = setGenero "acustico".setDuracion unaDuracion $ unaCancion
    | otherwise               = unaCancion

noEsAcustica :: Cancion -> Bool
noEsAcustica unaCancion = (/="acustico").genero $ unaCancion

metaEfecto :: [Efecto] -> Cancion -> Cancion
metaEfecto unosEfectos unaCancion = foldl aplicarEfecto unaCancion unosEfectos
                                    --foldl (flip ($)) unaCancion unosEfectos
                                    --foldr ($) unaCancion unosEfectos

aplicarEfecto :: Cancion -> Efecto -> Cancion
aplicarEfecto unaCancion unEfecto = unEfecto unaCancion




------------------------------- PARTE B ------------------------------


vistazo :: Artista -> [Cancion]
vistazo unArtista = take 3 . filter ((<150).duracion) . canciones $ unArtista

playlist :: Genero -> [Artista] -> [Cancion]
playlist unGenero unosArtistas = filter ((==unGenero).genero) (concatenarCanciones unosArtistas)

concatenarCanciones :: [Artista] -> [Cancion]
concatenarCanciones unosArtistas = foldl1 (++) (map canciones unosArtistas)



------------------------------- PARTE C ------------------------------



hacerseDJ :: Artista -> Artista
hacerseDJ unArtista = mapCanciones (map (efecto unArtista)) unArtista


tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo unArtista = all ((== generoPrimerCancion unArtista).genero) (canciones unArtista)

generoPrimerCancion :: Artista -> Genero
generoPrimerCancion unArtista = genero.head.canciones $ unArtista




formarBanda :: Nombre -> [Artista] -> Artista
formarBanda unNombre unosArtistas = UnArtista{
    nombre=unNombre , 
    canciones = concatenarCanciones unosArtistas, 
    efecto = combinarEfectos unosArtistas
    } 

combinarEfectos :: [Artista] -> Efecto
combinarEfectos unosArtistas = foldl1 (.) (map efecto unosArtistas)



obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = UnaCancion {
    titulo = concatenarTitulos unArtista,
    duracion = sumarDuraciones unArtista,
    genero = obtenerGeneroSuperador unArtista ++ " progresivo"
}

concatenarTitulos :: Artista -> Titulo
concatenarTitulos unArtista = foldl1 (++) (map titulo (canciones unArtista))

sumarDuraciones :: Artista -> Duracion
sumarDuraciones unArtista = foldl1 (+) (map duracion (canciones unArtista))

obtenerGeneroSuperador :: Artista -> Genero
obtenerGeneroSuperador unArtista = foldl1 queGeneroGana (map genero (canciones unArtista)) 


{- 

SOLUCION CON GUARDAS

queGeneroGana :: Genero -> Genero -> Genero
queGeneroGana unGenero otroGenero
    | unGenero=="rock" || otroGenero=="rock" = "rock"
    | unGenero=="reggaeton"                  = otroGenero
    | otroGenero=="reggaeton"                = unGenero
    | length unGenero > length otroGenero    = unGenero
    | otherwise                              = otroGenero

-}

--SOLUCION CON PATTERN MATCHING

queGeneroGana :: Genero -> Genero -> Genero
queGeneroGana "rock"          _         = "rock"
queGeneroGana _             "rock"      = "rock"
queGeneroGana "reggaeton" otroGenero    = otroGenero
queGeneroGana unGenero    "reggaeton"   = unGenero
queGeneroGana unGenero     otroGenero   = queGeneroTieneMasLetras unGenero otroGenero

queGeneroTieneMasLetras :: Genero -> Genero -> Genero
queGeneroTieneMasLetras unGenero otroGenero
    | length unGenero > length otroGenero    = unGenero
    | otherwise                              = otroGenero




------------------------------- PARTE D ------------------------------



{-En la escena apareció una artista que está arrasando con todos los premios gracias a que tiene infinitas canciones, pero antes de agregar su música en Currify te preguntamos:

-- ¿Puede esta nueva artista hacerse dj? --

NO, no puede ya que no termina nunca de aplicar el efecto a todas sus canciones

-- ¿Podemos echar un vistazo a su música? --

Haskell va agarrando cada item de la lista y a eso se lo pasa a la composicion, por lo tanto si yo tuviese: 
take 3.filter (<150) $ unaLista 
entonces si se podria, porque agarra un elemento, lo filtra y hace el take, agarra otro lo filtra y 
hace el take y asi.. una vez que ya tiene los 3 elementos del take termina sin termina de ver toda la lista

-- ¿Podrá crear una obra maestra progresiva? -- 

  NO, ya que el fold nunca terminara

-}



