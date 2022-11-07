object amateur {
	
	const sueldoAmateur = 10000
	
	method sueldoSegunExperiencia(cantidadDePeliculas){
		return sueldoAmateur
	}
	
	method puedeRecategorizar(cantidadDePeliculas){
		return cantidadDePeliculas > 10
	}
	
	method siguienteNivel(){
		return establecido
	}
}


object establecido {
	
	const sueldoPocaFama = 15000
	const sueldoMuchaFama = 5000
	
	method sueldoSegunExperiencia(cantidadDePeliculas){
		const nivelFama = self.nivelDeFama(cantidadDePeliculas)
		if( nivelFama < 15 ){
			return sueldoPocaFama
		}
		else{
			return sueldoMuchaFama * nivelFama
		}
	}
	
	method puedeRecategorizar(cantidadDePeliculas){
		return self.nivelDeFama(cantidadDePeliculas) > 10
	}
	
	method siguienteNivel(){
		return estrella
	}
	
	method nivelDeFama(cantidadDePeliculas){
		return cantidadDePeliculas / 2
	}
}


object estrella {
	
	const sueldoEstrellaPorPelicula = 30000
	
	method sueldoSegunExperiencia(cantidadDePeliculas){
		return cantidadDePeliculas * sueldoEstrellaPorPelicula
	}
	
	method puedeRecategorizar(cantidadDePeliculas){
		return false
	}
	
	method siguienteNivel(){
		throw new Exception(message = "No se puede recategorizar")
	}
}