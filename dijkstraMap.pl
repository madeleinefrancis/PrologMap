% Program uses find_paths(A,B,Mode,P,L) to return P - the shortest path of length L 
% from start node A to node B in a undirected, connected graph. 
% Graph is built using edge(A,B,W,X,Y,Z) facts. 
% A being the start node, B the destination, W-Z are varaibles representing time in minutes
% to get between the nodes by bike, walking, driving, and transit, in that order. 
% The program uses Dijkstra's algroithm to find all possible paths to and selects 
% the path with the shortest time value.

%The following edge facts represent travel times between the first parameter to the second 
%The parameters 3-6 represent time in minutes from the location in the first parameter,
%to the location in the second parameter. The weights are for biking, walking, driving,
%and taking transit, in that order. 
%An undirected graph is best for finding routes between actual locations, 
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
% References https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15A.pl
% http://www.tek-tips.com/viewthread.cfm?qid=1607508

find_paths(A,B,Mode,P,L) :-
    setof([Path, Length], path([A], B, Mode, Path, 0, Length), Set),
    Set = [_|_], %fail if no path between input locations
    minimal(Set,[P,L]), 
    reverse(P, DirectPath), 
    write('We suggest a route going through '),
    printPath(DirectPath),
    writef('. This will take you %d', [L]),
    write(' minutes.').

% From Current node (A), find all edges from A to some other Node. 
% Check to see if other node (C) is in list of already visited nodes.
% If not, use Mode variable to determine correct parameter of edge/6, to use for time between nodes.
% Add this time to NewLength accumuator. Recurse until destination node reached (B).
% Length will take on acummulated time value at the base case. 
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

%Compairs time of each path entry in the Set of all possible paths, found by Dijkstra's algorithm
% M represents the path with mimimum time
% minimal path
min([],M,M).
min([[P,L]|T],[_,M],Min) :- L < M, !, min(T,[P,L],Min). 
min([_|T],M,Min) :- min(T,M,Min).
 
%formats given list into comma separted output 
printPath([]).
printPath([X]) :-
    !, 
    write('and '),
    write(X).
printPath([X|T]) :-
    write(X),
    write(', '),
    printPath(T).