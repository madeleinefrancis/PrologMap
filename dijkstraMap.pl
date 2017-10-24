%The following edge facts represent travel times between the first parameter to the second 
%The parameters 3-6 represent time in minutes from the location in the first parameter,
%to the location in the second parameter. The weights are for biking, walking, driving,
%and taking transit, in that order. 
%A undirected graph is best for finding routes between actual locations, 
%hense there is an edge going each way between connected nodes 

edge(ubc,vandusen,39, 126, 16, 50).
edge(vandusen,ubc,39, 126, 16, 50).

edge(kits,ubc,27,94,15,34).
edge(ubc,kits,27,94,15,34).

edge(ubc,vgh,39,115,17,44).
edge(vgh,ubc,39,115,17,44).

edge(vgh,kits,21,48,12,32).
edge(kits,vgh,21,48,12,32).

edge(vandusen,vgh,15,36,6,12).
edge(vgh,vandusen,15,36,6,12).

edge(oakridge,vandusen,7,18,3,12).
edge(vandusen,oakridge,7,18,3,12).

edge(vgh,oakridge,17,43,10,16).
edge(oakridge,vgh,17,43,10,16).

edge(granville,vgh,9,32,6,20).
edge(vgh,granville,9,32,6,20).

edge(kits,granville,8,25,11,22).
edge(granville,kits,8,25,11,22).

edge(stPauls,kits,12,32,7,11).
edge(kits,stPauls,12,32,7,11).

edge(granville,stPauls,12,19,9,21).
edge(stPauls,granville,12,19,9,21).

edge(stPauls,englishBay,11,22,6,16).
edge(englishBay,stPauls,11,22,6,16).

edge(englishBay,stanley,12,26,8,18).
edge(stanley,englishBay,12,26,8,18).

edge(stanley,cap,12,89,15,40).
edge(cap,stanley,12,89,15,40).

edge(cap,stPauls,40,109,20,38).
edge(cap,stPauls,40,109,20,38).

edge(waterfront,stPauls,11,22,8,14).
edge(stPauls,waterfront,11,22,8,14).

edge(rogers,waterfront,5,14,5,5).
edge(waterfront,rogers,5,14,5,5).

edge(waterfront,lynn,58,123,26,69).
edge(lynn,waterfront,58,123,26,69).

edge(lynn,commercial,74,193,23,74).
edge(commercial,lynn,74,193,23,74).

edge(commercial,troutLake,6,16,4,13).
edge(troutLake,commercial,6,16,4,13).

edge(main,commercial,12,37,6,3).
edge(commercial,main,12,37,6,3).

edge(commercial,vgh,23,54,10,21).
edge(vgh,commercial,23,54,10,21).

edge(main,rogers,8,14,3,3).
edge(rogers,main,8,14,3,3).

edge(vgh,main,15,800,9,26).
edge(main,vgh,15,800,9,26).

edge(rogers,granville,16,36,9,22).
edge(granville,rogers,16,36,9,22).

%Test graph to debug through dijkstra functionality with less headache
edge(a,b,1,1,2,1).
edge(b,f,2,2,2,2).
edge(a,c,1,4,3,5).
edge(a,f,100,100,100,100).
edge(c,f,1,1,1,1).

% Find min path between start location A and
% destination B, using Mode type of transport 
find_paths(A,B,Mode,P,L) :-
    setof([Path, Length], path([A], B, Mode, Path, 0, Length), Set),
    Set = [_|_], %fail if no path between input locations
    minimal(Set,[P,L]), 
    reverse(P, DirectPath), 
    write('We suggest a route going through '),
    printPath(DirectPath),
    writef(' with time of %d\n', [L]),
    write(' minutes').

% Dijkstra's implementation here. From Current node (A), 
% find all outgoing edges with other nodes who are not yet members in visited list. 
% Use Mode variable to determine correct parameter of edge/6 weight for time between nodes 
% - added to NewLength. Recurse until destination node reached (B).
% At base case 
path([B | Rest], B, Mode, [B | Rest], Length, Length).
path([A | Rest], B, Mode, Path, CurrentLength, Length) :-
    edge(A, C, W, X, Y, Z),
    \+member(C, [A | Rest]),
    Mode == bike,
    NewLength is CurrentLength + W,
    path([C, A | Rest], B, Mode, Path, NewLength, Length).
path([A | Rest], B, Mode, Path, CurrentLength, Length) :-
    edge(A, C, W, X, Y, Z),
    \+member(C, [A | Rest]),
    Mode == walk,
    NewLength is CurrentLength + X,
    path([C, A | Rest], B, Mode, Path, NewLength, Length).
path([A | Rest], B, Mode, Path, CurrentLength, Length) :-
    edge(A, C, W, X, Y, Z),
    \+member(C, [A | Rest]),
    Mode == drive,
    NewLength is CurrentLength + Y,
    path([C, A | Rest], B, Mode, Path, NewLength, Length).
path([A | Rest], B, Mode, Path, CurrentLength, Length) :-
    edge(A, C, W, X, Y, Z),
    \+member(C, [A | Rest]),
    Mode == transit,
    NewLength is CurrentLength + Z,
    path([C, A | Rest], B, Mode, Path, NewLength, Length).

% minimum function found at 
minimal([H|T],M) :- min(T,H,M).

%Compairs time of each possible path entry in the Set found by Dijkstra's 
% M represents this minimum in the base case after recursing over set
% minimal path
min([],M,M).
min([[P,L]|T],[_,M],Min) :- L < M, !, min(T,[P,L],Min). 
min([_|T],M,Min) :- min(T,M,Min).
 
%formats path list into comma separted output 
printPath([]).
printPath([X]) :-
    !, write(X).
printPath([X|T]) :-
    write(X),
    write(', '),
    printPath(T).