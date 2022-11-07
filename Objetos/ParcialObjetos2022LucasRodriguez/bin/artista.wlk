import experiencia.*

class Artista {
	
	var experiencia
	var cantidadDePeliculas
	var ahorros = 0
	
	method getCantidadDePeliculas() = cantidadDePeliculas 

	
	method sueldo(){
		return experiencia.sueldoSegunExperiencia(cantidadDePeliculas)
	}
	
	
	method recategorizarExperiencia(){
		if( experiencia.puedeRecategorizar(cantidadDePeliculas) ){
			experiencia = experiencia.siguienteNivel()
		}
	}

	
	
	method actuarEnPelicula(){
		cantidadDePeliculas += 1
		self.incrementarAhorros( self.sueldo() )
	}
	
	method incrementarAhorros(unaCantidad){
		ahorros += unaCantidad
	}
	
}
