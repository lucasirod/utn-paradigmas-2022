class Pelicula {
	
	const presupuestoExtra = 1.7
	const minimoRecaudado = 1000000
	
	const nombre
	const elenco = #{}
	
	method getNombre() = nombre
	
	method recaudacionExtra()
	
	method presupuesto(){
		return self.sumatoriaSueldosElenco() * presupuestoExtra
	}
	
	method sumatoriaSueldosElenco(){
		return elenco.sum({ artista => artista.sueldo()})
	}
	
	method ganancias(){
		return self.recaudacion() - self.presupuesto()
	}
	
	
	method recaudacion(){
		return minimoRecaudado + self.recaudacionExtra()
	}
	
	method rodarPelicula(){ 
		return elenco.forEach({ artista => artista.actuarEnPelicula() })
	}
	
	
	method esEconomica(){
		return self.presupuesto() < 500000
	}
	
}

class PeliculaDrama inherits Pelicula {
	
	
	override method recaudacionExtra(){
		return nombre.size() * 100000
	}
	
}

class PeliculaAccion inherits Pelicula {
	
	const vidriosRotos
	const presupuestoVidrioRoto = 1000
	
	override method presupuesto(){
		return super() + presupuestoVidrioRoto * vidriosRotos
	} 
	
	override method recaudacionExtra(){
		return elenco.size() * 50000
	}
	
}

class PeliculaTerror inherits Pelicula {
	
	const cuchos
	
	
	override method recaudacionExtra(){
		return cuchos * 20000
	}
}

class PeliculaComedia inherits Pelicula {
	
	
	override method recaudacionExtra(){
		return 0
	}
	
}



