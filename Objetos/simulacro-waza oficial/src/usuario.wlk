import vehiculo.*

class Usuario {
	
	const PRECIO_POR_LITRO = 40
	const MAX_MONTO_DE_MULTAS_ACUMULADAS = 5000
	
	var property nombreUsuario
	var property dni
	var property dinero
	var property vehiculo
	var property multas = []
		
	method recargarCombustible(unaCantidad) {
		const cantidadARecargar = vehiculo.cantidadARecargar(unaCantidad)
		if(self.puedoPagarCombustible(cantidadARecargar)) {
			vehiculo.recargarCombustible(cantidadARecargar)
		}
	}
	
	method puedoPagarCombustible(unaCantidad) {
		return dinero >= PRECIO_POR_LITRO * unaCantidad
	}
	
	method puedoPagarMulta(unaMulta) {
		return unaMulta.puedeSerPagada(self.dinero())
	}
	
	method pagarMultas() {
		multas.forEach({ multa => self.pagarMulta(multa) })
	}
	
	method pagarMulta(unaMulta) {
		if(self.puedoPagarMulta(unaMulta)) {
			unaMulta.multaPagada()
		} else {
			unaMulta.noSePudoPagar()
		}
	}
	
	method recibirMulta(unaMulta) {
		multas.add(unaMulta)
	}
	
	method tieneDniHabilitadoParaRecorrer() {
		const diaActual = new Date().day()		
		
		return dni.even() && diaActual.even() 
	}
	
	method esEcologico() {
		return vehiculo.esEcologico()
	}
	
	method esComplicado() {
		const multasNoPagadas = multas.filter({ multa => !multa.pagada() })
		const totalMultasNoPagadas = multasNoPagadas.map({ multa => multa.costo() }).sum()
		
		return totalMultasNoPagadas > MAX_MONTO_DE_MULTAS_ACUMULADAS 
	}
}
