class Multa {
	
	const INTERESES = 1.1
	
	var property costo
	var property pagada = false
	
	constructor(unCosto) {
		costo = unCosto
	}
	
	method puedeSerPagada(unValor) {
		return unValor >= costo
	}
	
	method multaPagada() {
		pagada = true
	}
	
	method noSePudoPagar() {
		costo *= INTERESES
	}
}
