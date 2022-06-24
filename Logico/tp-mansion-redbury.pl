
/*Quien mata es porque odia a su víctima y no es más rico que ella. Además, quien mata debe vivir en la mansión Dreadbury.
Tía Agatha, el mayordomo y Charles son las únicas personas que viven en la mansión Dreadbury.
Charles odia a todas las personas de la mansión que no son odiadas por la tía Agatha.
Agatha odia a todos los que viven en la mansión, excepto al mayordomo.
Quien no es odiado por el mayordomo y vive en la mansión, es más rico que tía Agatha
El mayordomo odia a las mismas personas que odia tía Agatha.*/


viveEnLaMansion(tiaAgatha).
viveEnLaMansion(mayordomo).
viveEnLaMansion(charles).

odia(tiaAgatha,Odiado):-
    viveEnLaMansion(Odiado),
    Odiado \= mayordomo.

odia(charles,Odiado):-
    viveEnLaMansion(Odiado),
    not(odia(tiaAgatha,Odiado)).

odia(mayordomo,Odiado):-
    odia(tiaAgatha,Odiado).

esMasRico(Persona,tiaAgatha):-
    viveEnLaMansion(Persona),
    not(odia(mayordomo,Persona)).

mata(Asesino,Victima):-
    viveEnLaMansion(Asesino),
    odia(Asesino,Victima),
    not(esMasRico(Asesino,Victima)).


% ¿Quién mato a la tía Agatha?
%   mata(Asesino,tiaAgatha).
%   Asesino = tiaAgatha 