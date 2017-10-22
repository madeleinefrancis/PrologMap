edge(a,b,4).
edge(b,d,5).
edge(b,c,6).
edge(a,c,3).
edge(a,e,7).
edge(c,d,11).
edge(c,e,8).
edge(d,e,2).
edge(e,g,5).
edge(d,g,10).
edge(d,f,2).
edge(g,f,3).

edge(b,a,4).
edge(d,b,5).
edge(c,b,6).
edge(c,a,3).
edge(e,a,7).
edge(d,c,11).
edge(e,c,8).
edge(e,d,2).
edge(g,e,5).
edge(g,d,10).
edge(f,d,2).
edge(f,g,3).

nodes([a,b,c,d,e,f,g]).

adjacentNodes(S, E) :- findall(Y, edge(S,Y,_), E). 
adjacentEdges(S, E) :- findall(edge(S,Y,Z), edge(S,Y,Z), E). 
allVertices(E) :- findall(V, edge(V,Y,_),V).
findEdgeWeight(S,Y,W) :- edge(S,Y,W).

%found at source [2]. Finds all nodes that can be reached from X
path(X, Y) :- path(X,Y,[]).
path(X, Y, _ ) :- edge(X,Y,_).
path(X, Y, V) :- \+ member(X, V), edge(X, Z,_), path(Z, Y, [X|V]).
reachableNodes(S,List) :- setof(X, path(S,X), List).

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


dijks(A,ShPaths) :- 
	nodes(Nodes),
	setInfinite(Nodes,A),
	retract(weight(A,1.5NaN)),
	retract(free(A)),
	%p(A),

	assert(shortest_path(A,[A],0)),
	assert(to_handle(A)),
	loop,
	
	setof(shortest_path(X,L,D),shortest_path(X,L,D),ShPaths).

setInfinite([],_).
setInfinite([H|T],A) :- 
	assert(weight(H,1.5NaN)),
	assert(free(H)),
	setInfinite(T,A).

loop :-
	retract(to_handle(A)),
	adjacentNodes(A,E),
	assert(adjacent(A, E)),
	handle(A,E),
	loop.
loop.

handle(A,[]).
handle(A,[H|T]) :-
	retract(free(H)),
	shortest_path(A,Path,Dist),
	findEdgeWeight(A,H,W),
	Dist1 is Dist + W,
	assert(shortest_path(H,[H|Path],Dist1)),
	assertz(to_handle(H)),
	handle(A,T).

handle(A,[_|T]) :- handle(A,T).


p(A) :- adjacentNodes(A,E)),
	writef("%w",[E]).

