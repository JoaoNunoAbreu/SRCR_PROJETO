%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicados Auxiliares
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Predicados Auxiliares para evolução e involuçao
insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ), !,fail.
  
teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.


solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensao do predicado par: X -> {V,F}
par(0).
par(X) :- NX is X-2,NX >= 0,par(NX).

% Extensao do predicado impar: X -> {V,F}
impar(1).
impar(X) :- nao(par(X)).

% Extensao do predicado isB6: X -> {V,F}
isB6(0).
isB6(A) :- 0 is mod(A,4).

% Extensao do predicado fidelizacao: X, Y -> {V,F}
fidelizacao(X,R) :- R is 0.3 * X. 

% Extensao do predicado conta número de digitos de um número: X -> {V,F}

countDigits(0,0) :- !. 
countDigits(X,N) :- NX is div(X,10) ,NX >= 0,countDigits(NX,NN), N is NN + 1.

% Extensao do predicado isAfter: data(X,Y,Z), data(W,Q,S) -> {V,F}
isAfter(data(_,_,A1),data(_,_,A2)) :- A1 > A2.  
isAfter(data(_,M1,A),data(_,M2,A)) :- M1 > M2.
isAfter(data(D1,M,A),data(D2,M,A)) :- D1 > D2.

% Extensao do predicado datimeToData: data(X,Y,Z) -> {V,F}
datimeToData(data(D,M,A)) :- datime(datime(A,M,D,_,_,_)).

% Extensao do predicado data: X, Y, Z -> {V,F} 
data(D,2,A) :- D > 0, 0 < A,(isB6(A)) -> D =< 29; D =< 28.
data(D,M,A) :- M \= 2,M > 0, M =< 12,D > 0, D =< 31, 0 < A.

% Extensao do predicado isData: data(D,M,A) -> {V,F}
isData(data(D,M,A)) :- data(D,M,A).

% Extensao do predicado avancaDias: X, Y, Z -> {V,F} 
avancaDias(_,0,_).
avancaDias(data(D,M,A),Dias,data(Ds,Ms,As)) :-  ((2 is M,isB6(A)) -> ((D + Dias > 29) -> (avancaDias(data(1,3,A),Dias - (29 - D + 1),data(Ds,Ms,As)));
                                                                          (Ds is (D + Dias), Ms is M, As is A) ) );
                                                ((2 is M,par(M),nao(isB6(A) )) -> ((D + Dias > 28) -> (avancaDias(data(1,3,A),Dias - (28 - D + 1),data(Ds,Ms,As)));
                                                                          (Ds is (D + Dias), Ms is M, As is A)) );
                                               ((M =< 7,impar(M)) -> ( (D + Dias > 31) ->  (avancaDias(data(1,M + 1,A),Dias - (31 - D + 1),data(Ds,Ms,As)));
                                                                  (Ds is (D + Dias), Ms is M, As is A) ) );
                                               ((M > 2,M =< 7,par(M)) ->  ( (D + Dias > 30) -> (avancaDias(data(1,M + 1,A),Dias - (30 - D + 1),data(Ds,Ms,As)));
                                                                   (Ds is (D + Dias), Ms is M, As is A) ) );                                          
                                               ((M > 12 ) -> avancaDias(data(1,1,A+1),Dias,data(Ds,Ms,As) ) ); 
                                               ((M >= 8,impar(M)) -> ( (D + Dias > 30 ) ->  (avancaDias(data(1,M + 1,A),Dias - (30 - D + 1),data(Ds,Ms,As)));
                                                                 (Ds is(D + Dias), Ms is M, As is A) ) );
                                               ((M >= 8,par(M)) ->  ((D + Dias > 31 ) -> (avancaDias(data(1,M + 1,A),Dias - (31 - D + 1),data(Ds,Ms,As)));
                                                                  (Ds is (D + Dias), Ms is M, As is A) ))  .
                                


% Extensao do predicado apagaT: [H|T] -> {V,F}
apagaT(_,[],[]).
apagaT(X,[X|T],L) :- apagaT(X,T,L).
apagaT(X,[H|T],[H|L]) :- X \= H, apagaT(X,T,L).      

% Extensao do predicado maxL: [H|T] -> {V,F}
maxL([H],H) :- !. % Bang serve como paragem de modo a evitar ciclo infinito
maxL([H|T],M) :- maxL(T,M), M >= H. % Continua o M pois M > H
maxL([H|T],H) :- maxL(T,M), H > M. % Substitui H pelo M pois H > M

% Extensao do predicado somaList: [H|T] -> {V,F}
somaList([],0).
somaList([Head|Tail],Sum) :- somaList(Tail,NSum),Sum is Head+NSum.

% Extensao do predicado difAno: X, Y, Z, W, Q, S -> {V,F}
difAno(AnoEconomico,AN,AT,TC,D,C) :- solucoes(Ano,contrato(AN,AT,TC,_,D,_,_,_,data(_,_,Ano)),S), maxL(S,J), C is AnoEconomico - J.

% Extensao do predicado acumulaContrato: X, Y, Z -> {V,F}
acumulaContrato(AN,AT,C) :- solucoes(V,contrato(AN,AT,_,_,_,V,_,_,_),S),somaList(S,C). 

