:-style_check(-discontiguous).

%persona(Apodo, Edad, Características).
persona(ale, 15, [claustrofobia, sumaRapida, amorPorLosPerros]).
persona(agus, 25, [lecturaVeloz, ojoObservador, minuciosidad]).
persona(fran, 30, [fanDeLosComics]).
persona(rolo, 12, []).

%empresa(NombreSala, Empresa).
empresa(elPayasoExorcista, salSiPuedes).
empresa(socorro, salSiPuedes).
empresa(linternas, elLaberintoso).
empresa(guerrasEstelares, escapepepe).
empresa(fundacionDelMulo, escapepepe).

%sala(Nombre, Tipo).
sala(elPayasoExorcista, terror(100, 18)).
sala(socorro, terror(20, 12)).
sala(linternas, familiar(comics, 5)).
sala(guerrasEstelares, familiar(cienciaFiccion, 7)).
sala(fundacionDelMulo, enigmatica([combinacionAlfanumerica, deLlave, deBoton])).

%terror(CantidadDeSustos, EdadMinima).
%familiar(Tematica, CantidadDeHabitaciones).
%enigmatica(Candados).

nivelDeDificultadDeLaSala(Sala, Nivel):-
  sala(Sala, Tipo),
  nivelDeDificultadTipo(Tipo, Nivel).

nivelDeDificultadTipo(terror(CantidadDeSustos, EdadMinima), Nivel):-
  Nivel is CantidadDeSustos - EdadMinima.

nivelDeDificultadTipo(familiar(futurista, _), 15).

nivelDeDificultadTipo(familiar(Tematica, CantidadDeHabitaciones), CantidadDeHabitaciones):-
  Tematica \= futurista.

nivelDeDificultadTipo(enigmatica(Candados), CantidadDeCandados):-
  length(Candados, CantidadDeCandados).

personaNoClaustrofobica(Persona):-
  persona(Persona, _, Caracteristicas),
  not(member(Caracteristicas, claustrofobia)).

puedeSalir(Persona, Sala):-
  personaNoClaustrofobica(Persona),
  nivelDeDificultadDeLaSala(Sala, 1).

puedeSalir(Persona, Sala):-
  personaNoClaustrofobica(Persona),
  persona(Persona, Edad, _),
  Edad > 13,
  nivelDeDificultadDeLaSala(Sala, Nivel),
  Nivel < 5.

sinHabilidades(Persona):-
  persona(Persona, _ , []).

tieneSuerte(Persona, Sala):-
  sinHabilidades(Persona),
  puedeSalir(Persona, Sala).

esMacabra(Empresa):-
  empresa(_, Empresa),
  forall(empresa(Sala, Empresa), esSalaDeTerror(Sala)).

esSalaDeTerror(Sala):-
  sala(Sala, terror(_,_)).

empresaCopada(Empresa):-
  promedioDificultad(Empresa, Promedio),
  Promedio < 4,
  not(esMacabra(Empresa)).

promedioDificultad(Empresa, Promedio):-
  empresa(_, Empresa),
  findall(Nivel, (empresa(Sala, Empresa), nivelDeDificultadDeLaSala(Sala, Nivel)), Niveles),
  average(Niveles, Promedio).

average(Lista, Promedio):-
  length(Lista, Cantidad),
  sumlist(Lista, Sumatoria),
  Promedio is Sumatoria / Cantidad.

% Punto 6
empresa(estrellaDePeleas, supercelula).
empresa(choqueDeLaRealeza, supercelula).
sala(estrellaDePeleas, familiar(videojuegos, 7)).

empresa(miseriaDeLaNoche, skPista).
sala(miseriaDeLaNoche, terror(150, 21)).