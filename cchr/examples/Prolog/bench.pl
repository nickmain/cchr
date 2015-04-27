:- use_module(library(lists)).
:- use_module(dijkstra).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Benchmark

graph2edges([]).
graph2edges([V-NB|R]) :- graph2edges(V,NB), graph2edges(R).
graph2edges(_,[]).
graph2edges(V,[V2-W|R]) :- edge(V,V2,W), graph2edges(V,R).

%generate(N,G) :- generate(N,N,G),!, graph2edges(G),!.
%generate(V,N,G) :- V > 1, makeneighbours(V,N,NB),!, G=[V-NB|R], V2 is V-1, generate(V2,N,R).
%generate(1,N,G) :- makeneighbours(1,N,NB),G=[1-NB].

generate(N,G) :- generate(N,N,G),!.
generate(V,N,G) :- V > 1, !, makeneighbours(V,N,NB),!, graph2edges(V,NB), V2 is V-1, !, generate(V2,N,R), !.
generate(1,N,G) :- makeneighbours(1,N,NB),!,graph2edges(1,NB).

makeneighbours(V,N,NB) :- 
	degree(N,M),
	(V < N -> V1 is V+1 ; V1 = 1),
	NB = [V1-1|RNB],!,
	makeneighbours(N,M,[V,V1],RNB).
makeneighbours(N,M,L,NB) :-
	M>0,
	findrandomneighbour(N,L,B) ->
	random(0,67108864,W_), W is (W_//200000)+1,
	NB = [B-W|RNB],
	M1 is M-1,!,
	makeneighbours(N,M1,[B|L],RNB).
makeneighbours(N,0,L,[]) :- !.
findrandomneighbour(N,L,B) :-
	random(0,67108864,B_), B1 is (N*B_//67108864)+1,!,
        (member(B1,L) -> findrandomneighbour(N,L,B) ; B = B1).



bench :- bench(32).

bench(N) :- 
%	measure(
	generate(N,G),% true, _Utime2,_,_) ->
	measure(dijkstra:dijkstra(1), true, Ufib,GBfib,_) ->
%	measure(min_dist(G,1,Dists), true, Unaive,_,_) ->
%	format('~d ~d ~d ',[N,Ufib,Unaive]),
	format('~d ~d (~d)',[N,Ufib,GBfib]),
%%	writeln(Dists),nl,
%	nl,
%	(measure(dijkstra:checkdists(Dists), true, T,_,_) -> write(ok(T)) ; write('NOT OK') ),
	nl,
	fail.
bench(N) :- N < 500000, !,N1 is N*2, bench(N1).
	
bench(_).


neighbourhood(Graph,V,NB):-
   member(V-NB,Graph),!.

% min_dist(+Graph,+Start,-MinDist)
min_dist(Graph,Start,MinDist):-
   dijkstra(Graph,[],[Start-0],MinDist).
   
% dijkstra(+Graph,+ClosedVertices,+OpenVertices,-Distances)
dijkstra(_,MinDist,[],MinDist).
dijkstra(Graph,Closed,Open,MinDist):-
   choose_v(Open,V-D,RestOpen),
   neighbourhood(Graph,V,NB),  % NB is a list of adjacent vertices+distance to V
   diff(NB,Closed,NewNB),
   merge(NewNB,RestOpen,D,NewOpen),
   dijkstra(Graph,[V-D|Closed],NewOpen,MinDist).
   
% choose_v(+OpenVertices,-VertexToExpand,-RestOpenVertices)
choose_v([H|T],MinV,Rest):-
   choose_minv(T,H,MinV,Rest).
choose_minv([],MinV,MinV,[]).
choose_minv([H|T],M,MinV,[H2|Rest]):-
   H=_V1-D1, M=_V-D,
   (D1<D -> NextM=H,H2=M
          ; NextM=M,H2=H),
   choose_minv(T,NextM,MinV,Rest).
   
% diff(+ListOfVertices,+Closed,-ListOfNonClosedVertices)
diff([],_,[]).
diff([H|T],Closed,L):-
   H=V-_D,
   (member(V-_,Closed) -> L=NewT ; L=[H|NewT]),
   diff(T,Closed,NewT).
   
% merge(+ListOfVertices,+OldOpenVertices,-AllOpenVertices)
merge([],L,_,L).
merge([V1-D1|T],Open,D,NewOpen):-
   (remove(Open,V1-D2,RestOpen)
      -> VD is min(D2,D+D1)
       ; RestOpen=Open,VD is D+D1),
   NewOpen=[V1-VD|SubOpen],
   merge(T,RestOpen,D,SubOpen).
   
remove([H|T],H,T).
remove([H|T],X,[H|NT]):-
   H\=X,
   remove(T,X,NT).

degree(_N,D) :- D is 3.		% sparse graph.
%degree(_N,D) :- D is 10.		% sparse graph.
%degree(N,D) :- D is round(sqrt(N)).	% medium graph.
%degree(N,D) :- D is N//10.	% dense graph.
%degree(N,D) :- D is N//4 -1.	% dense graph.
measure(G,_,Time,GTime,swi) :-
	cputime(X),
	gctime(Y),
%	writeln(time(X)-gtime(Y)),
	call(G),
	cputime(Now),
	gctime(NowG),
	GTime is NowG-Y,
	Time is Now-X-GTime.
cputime(Time) :-
	statistics(runtime, [Time,_]).
gctime(Time) :-
	statistics(garbage_collection, [_,_,Time]).
