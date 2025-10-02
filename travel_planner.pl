% --- travel_planner.pl ---
% ruta(Origen, Destino, Medio, HoraSalida, HoraLlegada, CostoUSD, Disponible).
% Horas como números (por ejemplo 8.5 = 8:30). Asumimos misma zona horaria.

ruta(bogota, medellin, avion, 8, 9, 100, si).
ruta(bogota, medellin, bus, 6, 16, 40, si).
ruta(medellin, cartagena, avion, 10, 11.5, 120, no).
ruta(cartagena, santa_marta, bus, 14, 18, 30, si).
ruta(medellin, bogota, bus, 18, 4, 40, si). % ejemplo nocturno (cruzando medianoche)
ruta(bogota, cali, avion, 9, 10.5, 90, si).
ruta(cali, cartagena, bus, 7, 17, 50, si).
ruta(bogota, pereira, bus, 6, 9, 25, si).
ruta(pereira, medellin, bus, 10, 12, 20, si).
ruta(cartagena, bogota, avion, 12, 13.5, 130, si).
ruta(medellin, santa_marta, bus, 13, 21, 45, si).
ruta(santa_marta, cartagena, bus, 8, 10, 20, si).
ruta(cali, bogota, bus, 5, 11, 35, si).
ruta(bogota, bucaramanga, avion, 7, 8, 80, no).
ruta(bucaramanga, medellin, bus, 9, 13, 30, si).

% -------------------------
% Utilidades temporales
% -------------------------

% calcular_duracion(Hs, He, Dur): si He >= Hs => He-Hs, sino (He+24)-Hs
duracion(Hs, He, Dur) :-
    (He >= Hs -> Dur is He - Hs ; Dur is (He + 24) - Hs).

% -------------------------
% Buscar rutas (evitando ciclos)
% path(Origen, Destino, ListaRutas, TiempoTotal, CostoTotal)
% ListaRutas es lista de hechos ruta(...) seleccionados
% -------------------------

% caso base: ruta directa disponible o no (no filtramos disponibilidad aquí)
path(From, To, [ruta(From,To,Med,HS,HE,Cost,Disp)], TotalTime, TotalCost) :-
    ruta(From, To, Med, HS, HE, Cost, Disp),
    duracion(HS, HE, Tdur),
    TotalTime is Tdur,
    TotalCost is Cost.

% caso recursivo:
path(From, To, [ruta(From,Mid,Med,HS,HE,Cost,Disp)|Rest], TotalTime, TotalCost) :-
    ruta(From, Mid, Med, HS, HE, Cost, Disp),
    Mid \= From,
    \+ member(Mid, [From]),    % evita repetición inmediata; global se evita con visited
    path_mid(Mid, To, [From], Rest, Trest, Crest),
    duracion(HS, HE, Tdur),
    TotalTime is Tdur + Trest,
    TotalCost is Cost + Crest.

% Auxiliar con lista de visitados
path_mid(From, To, Visited, [ruta(From,To,Med,HS,HE,Cost,Disp)], TotalTime, TotalCost) :-
    \+ member(To, Visited),
    ruta(From, To, Med, HS, HE, Cost, Disp),
    duracion(HS, HE, Tdur),
    TotalTime is Tdur,
    TotalCost is Cost.

path_mid(From, To, Visited, [ruta(From,Mid,Med,HS,HE,Cost,Disp)|Rest], TotalTime, TotalCost) :-
    ruta(From, Mid, Med, HS, HE, Cost, Disp),
    Mid \= To,
    \+ member(Mid, Visited),
    duracion(HS, HE, Tdur),
    path_mid(Mid, To, [Mid|Visited], Rest, Trest, Crest),
    TotalTime is Tdur + Trest,
    TotalCost is Cost + Crest.

% -------------------------
% Buscar todas las rutas (incluso con escalas) sin ciclos:
% find_all_routes(Origen, Destino, ListaRutas)
% Cada ruta devuelta incluye su ListaRutas, Tiempo y Costo.
% -------------------------

find_all_routes(From, To, Routes) :-
    setof((Path,Time,Cost),
          path(From, To, Path, Time, Cost),
          Routes), !.
find_all_routes(_,_,[]).

% -------------------------
% Filtrado por disponibilidad (Disponible = si)
% only_available(Routes, Filtered)
% -------------------------
only_available(Routes, Filtered) :-
    include(route_all_available, Routes, Filtered).

route_all_available((Path,Time,Cost)) :-
    all_routes_available(Path).

all_routes_available([]).
all_routes_available([ruta(_,_,_,_,_,_,si)|T]) :-
    all_routes_available(T).
all_routes_available([ruta(_,_,_,_,_,_,no)|_]) :- fail.

% -------------------------
% Filtrar por rango de hora de salida:
% keep those routes whose first segment departs between H1 and H2 (inclusive)
% -------------------------
within_departure_range(([ruta(_,_,_,HS,_,_,_)|_],_,_), H1, H2) :-
    HS >= H1, HS =< H2.

filter_by_departure(Routes, H1, H2, Filtered) :-
    include({H1,H2}/[(Path,Time,Cost)>>(within_departure_range((Path,Time,Cost), H1, H2))], Routes, Filtered).

% -------------------------
% Selección óptima: por tiempo o por costo
% best_route(From, To, criterio(COST|TIME), Best)
% -------------------------
best_route(From, To, criterio(time), Best) :-
    find_all_routes(From, To, Routes), Routes \= [],
    % crear lista de (Time, Path, Cost)
    findall((Time,Path,Cost), member((Path,Time,Cost), Routes), L),
    sort(L, Sorted), % ordena por Time asc
    Sorted = [(BestTime,BestPath,BestCost)|_],
    Best = (BestPath, BestTime, BestCost).

best_route(From, To, criterio(cost), Best) :-
    find_all_routes(From, To, Routes), Routes \= [],
    findall((Cost,Path,Time), member((Path,Time,Cost), Routes), L),
    sort(L, Sorted), % ordena por Cost asc
    Sorted = [(BestCost,BestPath,BestTime)|_],
    Best = (BestPath, BestTime, BestCost).

% -------------------------
% Si no hay rutas disponibles, sugerir alternativas:
% - Si no hay rutas directas disponibles (o ninguna disponible), mostrar rutas no disponibles o con escalas.
% suggest_alternatives(From, To, Alternatives)
% -------------------------
suggest_alternatives(From, To, Alternatives) :-
    find_all_routes(From, To, All),
    only_available(All, Available),
    ( Available \= [] -> Alternatives = Available
    ; % si no hay disponibles, mostrar All como alternativas (o rutas parciales)
      Alternatives = All
    ).

% -------------------------
% Ejemplos de uso:
% ?- find_all_routes(bogota, medellin, R).
% ?- best_route(bogota, medellin, criterio(time), Best).
% ?- find_all_routes(bogota, cartagena, R), only_available(R, A).
% ?- find_all_routes(bogota, medellin, R), filter_by_departure(R,6,12, F).
% ?- suggest_alternatives(bogota, bucaramanga, Alt).
