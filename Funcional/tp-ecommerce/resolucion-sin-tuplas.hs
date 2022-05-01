--take :: Int -> String -> String
--drop :: Int -> String -> String
--head :: String -> Char
--elem :: Char -> String -> Bool
--reverse :: String -> String

entregaSencilla :: String -> Bool
entregaSencilla unDia = even.length $ unDia

productoDeLujo :: String -> Bool
productoDeLujo unProducto = elem 'x' unProducto || elem 'z' unProducto

productoXL :: String -> String
productoXL unProducto =  unProducto ++ "XL"

productoCodiciado :: String -> Bool
productoCodiciado unProducto = (length unProducto) >10
-- productoCodiciado unProducto = length unProducto >10

productoCorriente :: String -> Bool
productoCorriente unProducto =  head unProducto == 'A' || head unProducto == 'E' || head unProducto == 'I' || head unProducto == 'O' || head unProducto == 'U'
-- productoCorriente nombreDeProducto = esVocal . head $ nombreDeProducto

-- esVocal :: Char -> Bool 
-- esVocal unaLetra = elem unaLetra "aeiouAEIOU"

aplicarCostoDeEnvio :: Num(a)=> a-> a-> a
aplicarCostoDeEnvio unPrecio unCosto = unPrecio + unCosto

aplicarDescuento :: Num(a)=> a-> a-> a
aplicarDescuento unPrecio unDescuento = unPrecio * (1 - unDescuento)

descodiciarProducto :: String -> String
descodiciarProducto unProducto = take ( length unProducto - ((length unProducto)-10) ) unProducto
-- descodiciarProducto unProducto = take 10 unProducto


versionBarata :: String -> String
versionBarata unProducto = reverse.descodiciarProducto $ unProducto



productoDeElite :: String -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && (not.productoCorriente) unProducto



precioTotal :: Num(a) => a -> a -> a -> a -> a
precioTotal precio cant desc costoEnvio = aplicarCostoDeEnvio ((aplicarDescuento precio desc) * cant) costoEnvio
-- precioTotal precio cant desc costoEnvio = aplicarCostoDeEnvio (aplicarDescuento precio desc * cant) costo
