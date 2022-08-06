:-style_check(-discontiguous).

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

%   FUNCTORES
%tematico(tematica)
%cachengue(listaDeCancionesHabituales)
%electronico(djDeLaCasa, horaQueEmpieza, horaQueTermina)

%esDeTipo(Boliche, Tipo)
esDeTipo(why, cachengue([elYYo, prrrram, biodiesel, buenComportamiento])).
esDeTipo(masDe40, tematico(ochentoso)).
esDeTipo(qma, electronico(djFenich, 2, 5)).



%---------------------------------- PUNTO 1

esPiola(Boliche):-
    sirveComida(Boliche),
    quedaEn(Boliche,generalLasHeras).

esPiola(Boliche):-
    sirveComida(Boliche),
    esGrande(Boliche).

esGrande(Boliche):-
    entran(Boliche,CapacidadDePersonas),
    CapacidadDePersonas > 700.

%---------------------------------- PUNTO 2

soloParaBailar(Boliche):-  %RECORDAR QUE NOT NO ES INVERSIBLE!!!!!!!!!!!!!! AGREGAR GENERADOR
    quedaEn(Boliche,_),
    not( sirveComida(Boliche) ).


%---------------------------------- PUNTO 3

podemosIrConEsa(Localidad):-
    quedaEn(_,Localidad),
    forall(quedaEn(Boliche,Localidad),esPiola(Boliche)).


%---------------------------------- PUNTO 4

puntaje(Boliche,Puntaje):-
    esDeTipo(Boliche,Tipo),
    puntajeSegunTipo(Tipo,Puntaje).

puntajeSegunTipo(tematico(ochentoso),9).

puntajeSegunTipo(tematico(Tematica),7):-
    Tematica \= ochentoso.

puntajeSegunTipo(electronico(_,HoraQueEmpieza,HoraQueTermina),Puntaje):-
    Puntaje is HoraQueEmpieza + HoraQueTermina.

puntajeSegunTipo(cachengue(Canciones),10):-
    member(biodiesel, Canciones),
    member(buenComportamiento, Canciones).


    
%---------------------------------- PUNTO 5

elMasGrande(UnBoliche, UnaLocalidad) :-
    quedaEn(UnBoliche, UnaLocalidad),
    forall(quedaEn(OtroBoliche, UnaLocalidad), esMasGrandeQue(UnBoliche, OtroBoliche)).
  
esMasGrandeQue(UnBoliche, OtroBoliche) :-
    entran(UnBoliche, UnaCapacidad),
    entran(OtroBoliche, OtraCapacidad),
    UnaCapacidad >= OtraCapacidad. 
% >= para contemplar la comparación con sigo mismo;
% si hay dos o más boliches con la máxima capacidad ambos cumplen.
% si es preferible que no cumpla ninguno en caso de empate hay que generar sólo los boliches distintos al original.



%---------------------------------- PUNTO 6

puedeAbastecer(Localidad,CantidadDePersonas):-
    sumatoriaDeCapacidadesDeBolichesDe(Localidad,Sumatoria),
    Sumatoria >= CantidadDePersonas.

sumatoriaDeCapacidadesDeBolichesDe(Localidad,Sumatoria):-
    quedaEn(_,Localidad),
    findall(Capacidad , ( quedaEn(Boliche,Localidad) , entran(Boliche,Capacidad) ) , ListaCapacidades ),
    sumlist(ListaCapacidades,Sumatoria).


%---------------------------------- PUNTO 7

quedaEn(trabajamosYNosDivertimos,concordia).
entran(trabajamosYNosDivertimos,500).
sirveComida(trabajamosYNosDivertimos).
esDeTipo(trabajamosYNosDivertimos,tematico(oficina)).


quedaEn(elFinDelMundo,ushuaia).
entran(elFinDelMundo,1500).
esDeTipo(elFinDelMundo,electronico(djLuis,0,6)).


entran(misterio,1000000).
sirveComida(misterio).
