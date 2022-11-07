class Vehiculo {
	
	const capacidad
	var cantidadDeCombustible
	const velocidadPromedio
	const baseDePerdida
	
	method perdidaAdicional()
	
	method recargarCombustible(unaCantidad){
			cantidadDeCombustible = (unaCantidad+cantidadDeCombustible).min(capacidad)
	}
	
	method perderCombustibleBase(){
		cantidadDeCombustible  -= (baseDePerdida + self.perdidaAdicional() )
	}
}



class Camioneta inherits Vehiculo(baseDePerdida = 4) {
	
	override method perdidaAdicional(){
		return 5 * kmRecorridos
	}
	
}

class Deportivo inherits Vehiculo(baseDePerdida = 2) {
	
	override method perdidaAdicional(){
		return 0.2 * velocidadPromedio
	}
}

class Familiar inherits Vehiculo (baseDePerdida = 0){
	
	override method perdidaAdicional(){
		return 0
	}
}
