
type Nombre = String
type Precio = Int
type Producto = (Nombre,Precio) 

remera :: Producto
remera = ("Remera",300)

zapas :: Producto
zapas = ("Zapatillaaas",500)

anillo :: Producto
anillo = ("Anillo",1000)

entregaSencilla :: String -> Bool
entregaSencilla unDia = even.length $ unDia

productoDeLujo :: Producto -> Bool
productoDeLujo (nombre,_) = elem 'x' nombre || elem 'z' nombre || elem 'X' nombre || elem 'Z' nombre

productoXL :: Producto -> Producto
productoXL (nombre,precio) =  (nombre ++ "XL",precio)

productoCodiciado :: Producto -> Bool
productoCodiciado (nombre,_) = length nombre >10

productoCorriente :: Producto -> Bool
productoCorriente (nombre,_) = esVocal . head $ nombre

esVocal :: Char -> Bool 
esVocal unaLetra = elem unaLetra "aeiouAEIOU"

descodiciarProducto :: Producto -> Producto
descodiciarProducto (nombre,precio) = (take 10 nombre,precio)

versionBarata :: Producto -> Producto
versionBarata unProducto = (reverse.fst.descodiciarProducto $ unProducto , snd unProducto)

productoDeElite :: Producto -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && (not.productoCorriente) unProducto

aplicarCostoDeEnvio :: Precio -> Int -> Int
aplicarCostoDeEnvio unPrecio unCosto = unPrecio + unCosto

aplicarDescuento :: Precio -> Int -> Int
aplicarDescuento unPrecio unDescuento = unPrecio * (1 - unDescuento)

precioTotal :: Producto -> Int -> Int -> Int -> Int
precioTotal (_,precio) cant desc costoEnvio = aplicarCostoDeEnvio (aplicarDescuento precio desc * cant) costoEnvio
