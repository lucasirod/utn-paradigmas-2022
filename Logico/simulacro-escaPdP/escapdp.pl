
:-style_check(-discontiguous).

%persona(Apodo, Edad, Peculiaridades).
persona(ale, 15, [claustrofobia, cuentasRapidas, amorPorLosPerros]).
persona(agus, 25, [lecturaVeloz, ojoObservador, minuciosidad]).
persona(fran, 30, [fanDeLosComics]).
persona(rolo, 12, []).

%esSalaDe(Sala, Empresa).
esSalaDe(elPayasoExorcista, salSiPuedes).
esSalaDe(socorro, salSiPuedes).
esSalaDe(linternas, elLaberintoso).
esSalaDe(guerrasEstelares, escapepepe).
esSalaDe(fundacionDelMulo, escapepepe).

%   FUNCTORES
%terrorifica(CantidadDeSustos, EdadMinima).
%familiar(Tematica, CantidadDeHabitaciones).
%enigmatica(Candados).


%sala(Sala, Experiencia).
sala(elPayasoExorcista, terrorifica(100, 18)).
sala(socorro, terrorifica(20, 12)).
sala(linternas, familiar(comics, 5)).
sala(guerrasEstelares, familiar(futurista, 7)).
sala(fundacionDelMulo, enigmatica([combinacionAlfanumerica, deLlave, deBoton])).


%---------------------------------- PUNTO 1

nivelDeDificultadDeLaSala(Sala,Dificultad):-
    sala(Sala,Experiencia),
    nivelDeDificultadPorExperiencia(Experiencia,Dificultad).

nivelDeDificultadPorExperiencia(terrorifica(CantidadDeSustos, EdadMinima),Dificultad):-
    Dificultad is CantidadDeSustos-EdadMinima.

nivelDeDificultadPorExperiencia(familiar(futurista, _),15).

nivelDeDificultadPorExperiencia(familiar(Tematica, CantidadDeHabitaciones),CantidadDeHabitaciones):-
    Tematica \= futurista.

nivelDeDificultadPorExperiencia(enigmatica(Candados),CantidadDeCandados):-
    length(Candados, CantidadDeCandados).


%---------------------------------- PUNTO 2

puedeSalir(Persona,Sala):-
    nivelDeDificultadDeLaSala(Sala,1),
    not( esClaustrofobica(Persona) ).

puedeSalir(Persona,Sala):-
    nivelDeDificultadDeLaSala(Sala,Dificultad),
    Dificultad < 5,
    persona(Persona,Edad,_),
    Edad > 13,
    not( esClaustrofobica(Persona) ).

esClaustrofobica(Persona):-
    persona(Persona,_,Peculiaridades),
    member(claustrofobia,Peculiaridades).


%---------------------------------- PUNTO 3

tieneSuerte(Persona,Sala):-
    puedeSalir(Persona,Sala),
    persona(Persona,_,[]).


%---------------------------------- PUNTO 4

esMacabra(Empresa):-
    esSalaDe(_,Empresa),
    forall( esSalaDe(Sala,Empresa) , esExperienciaTerrorifica(Sala) ).

esExperienciaTerrorifica(Sala):-
    sala(Sala,terrorifica(_,_)).


%---------------------------------- PUNTO 5


empresaCopada(Empresa):-
    not( esMacabra(Empresa) ),
    promedioDificultadSalas(Empresa,Promedio),
    Promedio < 4.

promedioDificultadSalas(Empresa,Promedio):-
    esSalaDe(_,Empresa), %todas las salas de la empresa
    findall( Dificultad , ( esSalaDe(Sala, Empresa), nivelDeDificultadDeLaSala(Sala, Dificultad) ) , ListaDificultades ),
    promedio(ListaDificultades,Promedio).

promedio(Lista,Promedio):-
    sum_list(Lista,Sumatoria),
    length(Lista,Cantidad),
    Promedio is Sumatoria / Cantidad.


%---------------------------------- PUNTO 6

esSalaDe(estrellaDePeleas, supercelula).
esSalaDe(choqueDeLaRealeza, supercelula).
sala(estrellaDePeleas, familiar(videojuegos, 7)).

esSalaDe(miseriaDeLaNoche, skPista).
sala(miseriaDeLaNoche, terror(150, 21)).
