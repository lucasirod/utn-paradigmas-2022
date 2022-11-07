class Vehiculo {
	
	const PERDIDA_BASE_VEHICULO = 2

	var property combustible
	var property capacidadTanque
	var property velocidadPromedio

	method cantidadARecargar(unaCantidad) {
		return capacidadTanque.min(unaCantidad);
	}
	
	method recargarCombustible(unaCantidad) {
		if(self.superaCapacidadTanque(unaCantidad)) {
			self.error("superando el tamanio del tanque")
		}
		
		combustible += unaCantidad
	}
	
	method superaCapacidadTanque(unaCantidad) {
		return combustible + unaCantidad > capacidadTanque
	}
	
	method recorrer(unaDistancia) {
		const combustibleAUsar = self.perdidaBase(unaDistancia) + self.perdidaAdicional(unaDistancia)
		self.perderCombustible(combustibleAUsar)
	}
	
	method perderCombustible(unaCantidad) {
		const combustibleAPerder = capacidadTanque.min(unaCantidad)
		if(combustible - combustibleAPerder < 0) {
			self.error("combustible negativo")
		}
			
		combustible -= combustibleAPerder
	}
	
	method perdidaBase(unaDistancia) {
		return PERDIDA_BASE_VEHICULO;
	}
	
	method superaVelocidadPromedio(velocidad) {
		return velocidadPromedio > velocidad
	}
	
	method perdidaAdicional(unaDistancia)
	
	method esEcologico()
}

class Camioneta inherits Vehiculo {
	
	const PERDIDA_BASE_CAMIONETA = 4
	const PERDIDA_ADICIONAL_CAMIONETA = 5
	
	override method perdidaBase(unaDistancia) {
		return PERDIDA_BASE_CAMIONETA
	}
	
	override method perdidaAdicional(unaDistancia) {
		return PERDIDA_ADICIONAL_CAMIONETA * unaDistancia
	}
	
	override method esEcologico() {
		return false
	}
}

class Deportivo inherits Vehiculo {
	
	const VELOCIDAD_ECOLOGICA = 120
	const PERDIDA_DEPORTIVA = 0.2
	 
	override method perdidaAdicional(unaDistancia) {
		return PERDIDA_DEPORTIVA * velocidadPromedio
	}
	
	override method esEcologico() {
		return self.superaVelocidadPromedio(VELOCIDAD_ECOLOGICA) 
	}
}

class Familiar inherits Vehiculo {
	
	const PERDIDA_ADICIONAL_FAMILIAR = 0
	
	override method perdidaAdicional(unaDistancia) {
		return PERDIDA_ADICIONAL_FAMILIAR
	}
	
	override method esEcologico() {
		return true
	}
}
