:- discontiguous sirveComida/1.
:- discontiguous esDeTipo/2.
:- discontiguous entran/2.
:- discontiguous quedaEn/2.

%quedaEn(Boliche, Localidad)
quedaEn(pachuli, generalLasHeras).
quedaEn(why, generalLasHeras).
quedaEn(chaplin, generalLasHeras).
quedaEn(masDe40, sanLuis).
quedaEn(qma, caba).

%entran(Boliche, CapacidadDePersonas)
entran(pachuli, 500).
entran(why, 1000).
entran(chaplin, 700).
entran(masDe40, 1200).
entran(qma, 800).

%sirveComida(Boliche)
sirveComida(chaplin).
sirveComida(qma).

%tematico(tematica)
%cachengue(listaDeCancionesHabituales)
%electronico(djDeLaCasa, horaQueEmpieza, horaQueTermina)

%esDeTipo(Boliche, Tipo)
esDeTipo(why, cachengue([elYYo, prrrram, biodiesel, buenComportamiento])).
esDeTipo(masDe40, tematico(ochentoso)).
esDeTipo(qma, electronico(djFenich, 2, 5)).

% Punto 1

esPiola(UnBoliche) :-
  sirveComida(UnBoliche),
  esPiola_(UnBoliche).

esPiola_(UnBoliche) :- % nombre que sea sinónimo como estaBueno/1 maybe?
  quedaEn(UnBoliche, generalLasHeras).

esPiola_(UnBoliche) :-
  esGrande(UnBoliche).

esGrande(UnBoliche) :-
  entran(UnBoliche, UnaCapacidad),
  UnaCapacidad > 700.

% Punto 2

soloParaBailar(UnBoliche) :-
  boliche(UnBoliche),
  not(sirveComida(UnBoliche)).

boliche(UnBoliche) :-
  quedaEn(UnBoliche, _).

% Punto 3

podemosIrConEsa(UnaLocalidad) :-
  localidad(UnaLocalidad),
  forall(quedaEn(UnBoliche, UnaLocalidad), esPiola(UnBoliche)).

localidad(UnaLocalidad) :-
  quedaEn(_, UnaLocalidad).

% Punto 4

puntaje(UnBoliche, UnPuntaje) :-
  esDeTipo(UnBoliche, UnTipo),
  puntajeDeTipo(UnTipo, UnPuntaje).

puntajeDeTipo(tematico(ochentoso), 9).
puntajeDeTipo(tematico(UnaTematica), 7) :-
  UnaTematica \= ochentoso.

puntajeDeTipo(electronico(_, HoraALaQueEmpieza, HoraALaQueTermina), UnPuntaje) :-
  UnPuntaje is HoraALaQueEmpieza + HoraALaQueTermina.

puntajeDeTipo(cachengue(CancionesHabituales), 10) :-
  member(biodiesel, CancionesHabituales),
  member(buenaConducta, CancionesHabituales).

% Punto 5

elMasGrande(UnBoliche, UnaLocalidad) :-
  quedaEn(UnBoliche, UnaLocalidad),
  forall(quedaEn(OtroBoliche, UnaLocalidad), esMasGrandeQue(UnBoliche, OtroBoliche)).

esMasGrandeQue(UnBoliche, OtroBoliche) :-
  entran(UnBoliche, UnaCapacidad),
  entran(OtroBoliche, OtraCapacidad),
  UnaCapacidad >= OtraCapacidad. % >= para contemplar la comparación con sigo mismo;
                                 % si hay dos o más boliches con la máxima capacidad ambos cumplen.
                                 % si es preferible que no cumpla ninguno en caso de empate hay que generar sólo los boliches distintos al original.

% Punto 6

puedeAbastecer(UnaLocalidad, UnaCantidadDePersonas) :-
  localidad(UnaLocalidad),
  findall(UnaCapacidad, capacidadEnLocalidad(UnaLocalidad, UnaCapacidad), UnasCapacidades),
  sumlist(UnasCapacidades, UnaCapacidadTotal),
  UnaCapacidadTotal >= UnaCantidadDePersonas.

capacidadEnLocalidad(UnaLocalidad, UnaCapacidad) :-
  quedaEn(UnBoliche, UnaLocalidad),
  entran(UnBoliche, UnaCapacidad).

% Punto 7

% Punto 7.a.

esDeTipo(trabajamosYNosDivertimos, tematico(oficina)).
entran(trabajamosYNosDivertimos, 500).
sirveComida(trabajamosYNosDivertimos).
quedaEn(trabajamosYNosDivertimos, concordia).

% Punto 7.b.

entran(elFinDelMundo, 1500).
quedaEn(elFinDelMundo, ushuaia).
esDeTipo(elFinDelMundo, electronico(luis, 0, 6)).

% Punto 7.c.

entran(misterio, 1000000).
sirveComida(misterio).