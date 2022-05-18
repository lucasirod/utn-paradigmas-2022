import Text.Show.Functions ()

-- A.
-- 1.
data Cancion = Cancion {
  titulo   :: String,
  genero   :: Genero,
  duracion :: Int
} deriving Show

type Genero = String

-- 2.
data Artista = Artista {
  nombre          :: String,
  canciones       :: [Cancion],
  efectoPreferido :: Efecto
} deriving Show

type Efecto = Cancion -> Cancion

-- accessors
mapTitulo :: (String -> String) -> Cancion -> Cancion
mapTitulo    f unaCancion = unaCancion { titulo    = f . titulo    $ unaCancion }

mapGenero :: (Genero -> Genero) -> Cancion -> Cancion
mapGenero    f unaCancion = unaCancion { genero    = f . genero    $ unaCancion }

mapDuracion :: (Int -> Int) -> Cancion -> Cancion
mapDuracion  f unaCancion = unaCancion { duracion  = f . duracion  $ unaCancion }

mapCanciones :: ([Cancion] -> [Cancion]) -> Artista -> Artista
mapCanciones f unArtista  = unArtista  { canciones = f . canciones $ unArtista  }

setGenero :: String -> Cancion -> Cancion
setGenero = mapGenero . const
-- accessors

-- 3.
acortar :: Efecto
acortar = mapDuracion (max 0 . subtract 60)

remixar :: Efecto
remixar = mapTitulo (++ " remix") . mapDuracion (* 2) . setGenero "remixado"

acustizar :: Int -> Efecto
acustizar duracion cancion
  | esAcustica cancion = cancion
  | otherwise          = setGenero "acústico" . mapDuracion (const duracion) $ cancion

esAcustica :: Cancion -> Bool
esAcustica = esDelGenero "acústico"

efectoSupremo :: [Efecto] -> Efecto
efectoSupremo unosEfectos unaCancion = foldr ($) unaCancion unosEfectos

-- 4.
cafeParaDos :: Cancion
cafeParaDos = Cancion "Café para dos" "rock melancólico" 146

fuiHastaAhi :: Cancion
fuiHastaAhi = Cancion "Fuí hasta ahí" "rock" 279

losEscarabajos :: Artista
losEscarabajos = Artista "Los escarabajos" [rocketRaccoon, mientrasMiBateriaFesteja, tomateDeMadera] acortar

adela :: Artista
adela = Artista "Adela" [teAcordas, unPibeComoVos, daleMechaALaLluvia] remixar

elTigreJoaco :: Artista
elTigreJoaco = Artista "El tigre Joaco" [] (acustizar 360)

rocketRaccoon :: Cancion
rocketRaccoon = undefined

mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = undefined

tomateDeMadera :: Cancion
tomateDeMadera = undefined

teAcordas :: Cancion
teAcordas = undefined

unPibeComoVos :: Cancion
unPibeComoVos = undefined

daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = undefined

-- B.
-- 1.
vistazo :: Artista -> [Cancion]
vistazo = take 3 . filter esCorta . canciones

esCorta :: Cancion -> Bool
esCorta = (< 150) . duracion

-- 2.
playlist :: Genero -> [Artista] -> [Cancion]
playlist = concatMap . cancionesDelGenero

cancionesDelGenero :: Genero -> Artista -> [Cancion]
cancionesDelGenero unGenero = filter (esDelGenero unGenero) . canciones

esDelGenero :: Genero -> Cancion -> Bool
esDelGenero unGenero = (== unGenero) . genero

-- C.
-- 1.
hacerseDJ :: Artista -> Artista
hacerseDJ unArtista = mapCanciones (map $ efectoPreferido unArtista) unArtista

-- 2.
tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo unArtista = sonTodosIguales . map genero . canciones $ unArtista

sonTodosIguales :: Eq a => [a] -> Bool
sonTodosIguales unaLista = all (== head unaLista) unaLista

-- 3.
formarBanda :: String -> [Artista] -> Artista
formarBanda nombre unosArtistas = Artista {
  nombre          = nombre,
  canciones       = concatMap canciones unosArtistas,
  efectoPreferido = efectoSupremo . map efectoPreferido $ unosArtistas
}

-- 4.
obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva = obraMaestraProgresivaDeCanciones . canciones

obraMaestraProgresivaDeCanciones :: [Cancion] -> Cancion
obraMaestraProgresivaDeCanciones unasCanciones = Cancion {
  titulo   = (++ " progresivo") . concatMap titulo $ unasCanciones,
  duracion = sum . map duracion                    $ unasCanciones,
  genero   = foldl1 generoSuperador . map genero   $ unasCanciones
}

generoSuperador :: Genero -> Genero -> Genero
generoSuperador "rock"      _           = "rock"
generoSuperador _           "rock"      = "rock"
generoSuperador "reggaeton" otroGenero  = otroGenero
generoSuperador otroGenero  "reggaeton" = otroGenero
generoSuperador unGenero    otroGenero  = maxBy length unGenero otroGenero

maxBy :: Ord a => (b -> a) -> b -> b -> b
maxBy criterio unValor otroValor
  | criterio unValor > criterio otroValor = unValor
  | otherwise                             = otroValor

{-
D.
1.
¿Puede esta nueva artista hacerse dj?
Si.
2.
¿Podemos echar un vistazo a su música?
Depende de si tiene por lo menos 3 canciones cortas o no.
3.
¿Podrá crear una obra maestra progresiva?
Si.
-}
