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

% list of the all nodes which represent places in Vancouver, in our graph
nodes([cap,commercial,englishBay,granville,kits,lynn,main,oakridge,rogers,stanley,stPauls,troutLake,waterfront,vandusen,vgh,ubc]).

%Given a node/place S, E returns all places/nodes which are adjacent to S in graph rep of map
adjacentNodes(S, E) :- findall(Y, edge(S,Y,_,_,_,_), E). 

%found at source [3]
% Weve found the minimum
min_in_list([Min],Min).                 
% H is less than or equal to K
min_in_list([H,K|T],M) :-
    H =< K,                             
    min_in_list([H|T],M).               
% H is greater than K
min_in_list([H,K|T],M) :-
    H > K,                              
    min_in_list([K|T],M).               


% Modified from code found at http://okmij.org/ftp/Prolog/shortest_path_weight1.prl
% 
dijks(A,Dest,Mode,ShPaths) :- 
	nodes(Nodes),
	setInfinite(Nodes,A),
	retract(weight(A,1.5NaN)),
	retract(free(A)),

	assert(shortest_path(A,[A],0)),
	assert(to_handle(A)),
	loop(Mode),
	
	findall(shortest_path(X,L,D),shortest_path(X,L,D),ShPaths),
	abolish(shortest_path/3),
	abolish(weight/2),
	abolish(free/1),
	abolish(adjacent/2),
	abolish(to_handle/1).

setInfinite([],_).
setInfinite([H|T],A) :- 
	assert(weight(H,1.5NaN)),
	assert(free(H)),
	setInfinite(T,A).

loop(Mode) :-
	retract(to_handle(A)),
	adjacentNodes(A,E),
	assert(adjacent(A, E)),
	handle(A,E,Mode),
	loop(Mode).
loop(Mode).

handle(A,[],Mode).

handle(A,[H|T],Mode) :-
	retract(free(H)),
	shortest_path(A,Path,Dist),
	findEdgeWeight(A,H,W,Mode),
	Dist1 is Dist + W,
	findNodeWeight(H,NWeight),
	NWeight > Dist1,
	retract(weight(H,_)),
	assert(weight(H,Dist1)),
	assert(shortest_path(H,[H|Path],Dist1)),
	assertz(to_handle(H)),
	handle(A,T,Mode).

handle(A,[H|T],Mode) :-
	retract(free(H)),
	shortest_path(A,Path,Dist),
	findEdgeWeight(A,H,W,Mode),
	Dist1 is Dist + W,
	assert(shortest_path(H,[H|Path],Dist1)),
	assertz(to_handle(H)),
	handle(A,T,Mode).

handle(A,[_|T],Mode) :- handle(A,T,Mode).

findEdgeWeight(A,Dest,Weight,Mode) :- 
	Mode == bike,
 	edge(A,Dest,Weight,X,Y,Z).
findEdgeWeight(A,Dest,Weight,Mode) :- 
 	Mode == walk,
 	edge(A,Dest,X,Weight,Y,Z).
findEdgeWeight(A,Dest,Weight,Mode) :- 
 	Mode == drive,
 	edge(A,Dest,X,Y,Weight,Z).
findEdgeWeight(A,Dest,Weight,Mode) :- 
 	Mode == transit,
 	edge(A,Dest,X,Y,Z,Weight).

findNodeWeight(V,Weight) :- weight(V,Weight).

