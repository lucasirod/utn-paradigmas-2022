:-style_check(-discontiguous).

% esPersonaje/1 nos permite saber qué personajes tendrá el juego

esPersonaje(aang).
esPersonaje(katara).
esPersonaje(zoka).
esPersonaje(appa).
esPersonaje(momo).
esPersonaje(toph).
esPersonaje(tayLee).
esPersonaje(zuko).
esPersonaje(azula).
esPersonaje(iroh).

% esElementoBasico/1 nos permite conocer los elementos básicos que pueden controlar algunos personajes

esElementoBasico(fuego).
esElementoBasico(agua).
esElementoBasico(tierra).
esElementoBasico(aire).

% elementoAvanzadoDe/2 relaciona un elemento básico con otro avanzado asociado

elementoAvanzadoDe(fuego, rayo).
elementoAvanzadoDe(agua, sangre).
elementoAvanzadoDe(tierra, metal).


% controla/2 relaciona un personaje con un elemento que controla

controla(zuko, rayo).
controla(toph, metal).
controla(katara, sangre).
controla(aang, aire).
controla(aang, agua).
controla(aang, tierra).
controla(aang, fuego).
controla(azula, rayo).
controla(iroh, rayo).


% visito/2 relaciona un personaje con un lugar que visitó. Los lugares son functores que tienen la siguiente forma:
% reinoTierra(nombreDelLugar, estructura)
% nacionDelFuego(nombreDelLugar, soldadosQueLoDefienden)
% tribuAgua(puntoCardinalDondeSeUbica)
% temploAire(puntoCardinalDondeSeUbica)

visito(aang, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(iroh, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(zuko, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(toph, reinoTierra(fortalezaDeGralFong, [cuartel, dormitorios, enfermeria, salaDeGuerra, templo, zonaDeRecreo])).
visito(aang, nacionDelFuego(palacioReal, 1000)).

visito(katara, tribuAgua(norte)).
visito(momo, tribuAgua(norte)).
visito(iroh, tribuAgua(norte)).
visito(toph, tribuAgua(norte)).
visito(aang, tribuAgua(norte)).

visito(katara, tribuAgua(sur)).
visito(aang, temploAire(norte)).
visito(aang, temploAire(oeste)).
visito(aang, temploAire(este)).

visito(aang, temploAire(sur)).
visito(katara, temploAire(sur)).
visito(momo, temploAire(sur)).
visito(iroh, temploAire(sur)).
visito(toph, temploAire(sur)).


%------------PUNTO 1------------%


esElAvatar(Personaje):-
    esPersonaje(Personaje),
    forall( esElementoBasico(Elemento) , controla(Personaje,Elemento)). 
%"para todos" los elementos basicos, son controlados por el avatar


%------------PUNTO 2------------%

noEsMaestro(Personaje):-
    esPersonaje(Personaje),
    not( controlaElementoBasico(Personaje) ),
    not( controlaElementoAvanzado(Personaje) ).

controlaElementoBasico(Personaje):-
    controla(Personaje,Elemento),
    esElementoBasico(Elemento).

controlaElementoAvanzado(Personaje):-
    controla(Personaje,Elemento),
    elementoAvanzadoDe(_,Elemento).


esMaestroPrincipiante(Personaje):-
    controlaElementoBasico(Personaje),
    not( controlaElementoAvanzado(Personaje) ).

esMaestroAvanzado(Personaje):-
    controlaElementoAvanzado(Personaje).

esMaestroAvanzado(Personaje):-
    esElAvatar(Personaje).


%------------PUNTO 3------------%


sigueA(UnPersonaje,OtroPersonaje):-
    esPersonaje(UnPersonaje),
    esPersonaje(OtroPersonaje),
    UnPersonaje \= OtroPersonaje,
    % visito(UnPersonaje,_), %Comento esta línea porque consulté con el docente y considero valido que se pueda seguir a personajes que no visitaron lugares. (lo seguiran todos los otros personajes).
    forall( visito(UnPersonaje,Lugar) , visito(OtroPersonaje,Lugar) ).
%para todos los lugares que visito el primero, tambien los visito el segundo.


sigueA(aang,zuko). % Si zuko sigue a aang quiere decir que visito todos los lugares que visito aang.



%------------PUNTO 4------------%

esDignoDeConocer(temploAire(_)).

esDignoDeConocer(tribuAgua(norte)).

esDignoDeConocer(reinoTierra(Nombre,Estructura)):-
    visito(_,reinoTierra(Nombre,Estructura)),
    not( member(muro,Estructura) ).



%------------PUNTO 5------------%


esPopular(Lugar):- 
    cantidadDeVisitantes(Lugar,Cantidad),
    Cantidad > 4.

cantidadDeVisitantes(Lugar,Cantidad):-
    visito(_,Lugar),
    findall( Personaje , visito(Personaje,Lugar) , PersonajesVisitantesDelLugar ), %todos los personajes que visitaron ese lugar
    length(PersonajesVisitantesDelLugar, Cantidad).


%------------PUNTO 6------------%


esPersonaje(bumi).
controla(bumi,tierra).
visito(bumi,reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).

esPersonaje(suki).
visito(suki,nacionDelFuego(prisionDeMaximaSeguridad, 200)).
