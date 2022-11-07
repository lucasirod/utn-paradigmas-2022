class Multa {
	
	const intereses = 1.1
	var costo
	var pagada = false
	
	method getCosto() = costo
	method getPagada() = pagada
	
	method setPagada(estado) {
		pagada = estado
	}
	
	method aumentarPorPagoRetrasado(){
		costo = costo * intereses
	}
	
}
