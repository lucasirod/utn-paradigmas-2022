object waza {
	
	var property usuarios = []
	var property zonas = []
	
	method pagarMultas() {
		usuarios.forEach({ usuario => usuario.pagarMultas() })
	}
	
	method zonaMasTransitada() {
		return zonas.max({ zona => zona.cantidadDeUsuarios() })
	}
	
	method usuariosComplicados() {
		return usuarios.filter({ usuario => usuario.esComplicado() })
	}
}
