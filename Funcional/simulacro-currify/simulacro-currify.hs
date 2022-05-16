import Text.Show.Functions

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


cafeParaDos :: Cancion
cafeParaDos = UnaCancion "Cafe para Dos" "rock melancolico" 146

fuiHastaAhi :: Cancion
fuiHastaAhi = UnaCancion "Fui hasta ahi" "rock" 279

--losEscarabajos :: Artista
--losEscarabajos = UnArtista "Los escarabajos" [] [rocketRaccoon,mientrasMiBateriaFesteja,tomateDeMadera] acortar

--rocketRaccoon :: Cancion
--mientrasMiBateriaFesteja :: Cancion
--tomateDeMadera :: Cancion

--adela :: Artista
--adela = UnaCancion "Adela" [unPibeComoVos,daleMechaALaLluvia] remixar

--unPibeComoVos :: Cancion
--daleMechaALaLluvia :: Cancion

elTigreJoaco :: Artista
elTigreJoaco = UnArtista "El Tigre Joaco" [] (acustizar 360)



vistazo :: Artista -> [Cancion]
vistazo unArtista = take 3 . filter ((<150).duracion) . canciones $ unArtista

playlist :: Genero -> [Artista] -> [Cancion]
playlist unGenero unosArtistas = filter (==unGenero) (concatenarCanciones unosArtistas)

concatenarCanciones :: [Artista] -> Cancion
concatenarCanciones unosArtistas = foldl1 (++) (map canciones unosArtistas)



hacerseDJ :: Artista -> Artista
hacerseDJ unArtista = mapCanciones (map (efecto unArtista)) unArtista

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo unArtista = all (== generoPrimerCancion unArtista) (canciones unArtista)

generoPrimerCancion :: Artista -> Genero
generoPrimerCancion unArtista = genero.head.canciones $ unArtista


formarBanda :: Nombre -> [Artista] -> Artista
formarBanda unNombre unosArtistas = UnArtista{
    nombre=unNombre , 
    canciones = concatenarCanciones unosArtistas, 
    efecto = combinarEfectos unosArtistas
    } 

combinarEfectos :: [Artista] -> Efecto
combinarEfectos unosArtistas = foldl1 aplicarEfecto 


obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = UnaCancion {
    titulo = concatenarTitulos unArtista,
    duracion = sumarDuraciones unArtista,
    genero = obtenerGeneroSuperador unArtista ++ " progresivo"
}

concatenarTitulos :: Artista -> Titulo
concatenarTitulos unArtista = foldl1 (++) (map titulo (canciones UnArtista))

sumarDuraciones :: Artista -> Duracion
sumarDuraciones unArtista = foldl1 (+) (map duracion (canciones UnArtista))

obtenerGeneroSuperador :: Artista -> Genero
obtenerGeneroSuperador unArtista = foldl1 queGeneroGana (map genero (canciones UnArtista)) 

--queGeneroGana :: Genero -> Genero -> Genero
--queGeneroGana unGenero otroGenero
--    | unGenero=="rock" || otroGenero=="rock" = "rock"
--    | unGenero=="reggaeton"                  = otroGenero
--    | otroGenero=="reggaeton"                = unGenero
--    | length unGenero > length otroGenero    = unGenero
--    | otherwise                              = otroGenero


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


--En la escena apareció una artista que está arrasando con todos los premios gracias a que tiene infinitas canciones, pero antes de agregar su música en Currify te preguntamos:

-- ¿Puede esta nueva artista hacerse dj?

-- NO, no puede ya que no termina nunca de aplicar el efecto a todas sus canciones

-- ¿Podemos echar un vistazo a su música?

-- Haskell va agarrando cada item de la lista y a eso se lo pasa a la composicion, por lo tanto si yo tuviese: 
-- take 3.filter (<150) $ unaLista 
-- entonces si se podria, porque agarra un elemento, lo filtra y hace el take, agarra otro lo filtra y hace el take y asi.. una vez que ya tiene los 3 elementos del take termina sin termina de ver toda la lista

-- ¿Podrá crear una obra maestra progresiva?

--  NO, ya que el fold nunca terminara





