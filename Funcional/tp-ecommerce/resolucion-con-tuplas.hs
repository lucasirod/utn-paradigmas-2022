
type Nombre = String
type Precio = Float
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

aplicarCostoDeEnvio :: Num(a)=> a -> a-> a
aplicarCostoDeEnvio unPrecio unCosto = unPrecio + unCosto

aplicarDescuento :: Num(a)=> a -> a -> a
aplicarDescuento unPrecio unDescuento = unPrecio * (1 - unDescuento)

-- precioTotal :: Num(a) => Producto -> a -> a -> a -> a
-- precioTotal (_,precio) cant desc costoEnvio = aplicarCostoDeEnvio (aplicarDescuento precio desc * cant) costoEnvio
--                                                                                            ↑↑↑↑
-- DUDA: En precio total me marca error en desc -> Couldn't match expected type `Precio' with actual type `a'
-- No entiendo porque si aplicarDescuento recibe dos num (desc y precio, que al ser float se supone que tambien es num) y devuelve un num, cant tambien es num y aplicarCostoDeEnvio recibe dos num (la salida de aplicarDescuento y el costo) y devuelve otro num