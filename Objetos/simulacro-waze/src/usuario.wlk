class Usuario {
	
	const nombreDeUsuario
	const dni
	var dineroEnCuenta
	var multas
	const vehiculo
	
	method restarDinero(unaCantidad){
		dineroEnCuenta -= unaCantidad
	}
	
	method recorrerDistancia(unVehiculo){
		unVehiculo.perderCombustibleBase()
	}
	
	
	method recargarCombustible(unaCantidad){
		const costoRecarga = unaCantidad * 40
		if(dineroEnCuenta > costoRecarga){
			self.restarDinero(costoRecarga)
			vehiculo.recargarCombustible(unaCantidad)
		}
		else{
			self.error("No tiene suficiente dinero en cuenta")
		}
		
	}
	
	method pagarMultas(){
		multas.forEach({multa => self.pagar(multa)})
	}
	
	method pagar(unaMulta){
		const costo = unaMulta.getCosto()
		if(dineroEnCuenta > costo ){
			self.restarDinero(costo)
			unaMulta.setPagada(true)
		}
		else{
			unaMulta.aumentarPorPagoRetrasado()
		}
	}
	
	method montoMultasSinPagar(){
		multas.filter({multa => !multa.getPagada()}).sum({multa => multa.getCosto()})
	}
}