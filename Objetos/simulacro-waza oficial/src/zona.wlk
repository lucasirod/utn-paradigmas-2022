class Zona {
	
	var property velocidadMaxima
	var property usuarios = []
	var property controles = []
	
	method activarControles() {
		controles.forEach({control => control.controlarUsuarios(usuarios)})
	}
	
	method cantidadDeUsuarios() {
		return usuarios.size()
	}
	
	method superaVelocidadPromedio(usuario) {
		return usuario.vehiculo().superaVelocidadPromedio(velocidadMaxima)
	}
}
