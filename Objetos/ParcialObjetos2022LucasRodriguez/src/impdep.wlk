object impdep {
	
	const peliculas = #{}
	const artistas = #{}
	
	method artistaConMejorPaga(){
		return artistas.max({artista => artista.sueldo() })
	}
	
	method peliculasEconomicas(){
		return peliculas.filter({pelicula => pelicula.esEconomica()})
	}
	
	method nombresDePeliculasEconomicas(){
		return self.peliculasEconomicas().map({pelicula => pelicula.getNombre() }) //Uso map en vez de forEach porque no tiene efecto
	}
	
	method sumaDeGananciasPeliculasEconomicas(){
		return self.peliculasEconomicas().sum({ pelicula => pelicula.ganancias() })
	}
	
	method recategorizarArtistas(){
		artistas.forEach({artista => artista.recategorizarExperiencia()})
	}
}
