import multa.*

class Control {
	
	method controlarUsuarios(zona, usuarios) {
		usuarios.forEach({ usuario  => self.controlar(zona, usuario) })
	}
	
	method controlar(zona, usuario) {
		if(self.esMultable(zona, usuario)) {
			usuario.recibirMulta(new Multa(self.valorMulta()))
		}
	}
	
	method esMultable(zona, usuario)
	method valorMulta()
}

object controlVelocidad inherits Control {
	
	const VALOR_MULTA_VELOCIDAD = 3000
	
	override method esMultable(zona, usuario) {
		return zona.superaVelocidadPromedio(usuario)
	}
	
	override method valorMulta() {
		return VALOR_MULTA_VELOCIDAD
	}
}

object controlEcologico inherits Control {
	
	const VALOR_MULTA_ECOLOGICO = 1500
	
	override method esMultable(zona, usuario) {
		return usuario.esEcologico()
	}
	
	override method valorMulta() {
		return VALOR_MULTA_ECOLOGICO
	}
}

object controlRegulatorio inherits Control {
	
	const VALOR_MULTA_REGULATORIO = 2000
	
	override method esMultable(zona, usuario) {
		return usuario.tieneDniHabilitadoParaRecorrer()
	}
	
	override method valorMulta() {
		return VALOR_MULTA_REGULATORIO
	}
}
