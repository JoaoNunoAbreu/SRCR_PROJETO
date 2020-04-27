%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic adjudicante/3.
:- dynamic adjudicataria/3.
:- dynamic contrato/9.
:- dynamic tipoProcedimento/1.
:- dynamic tipoContrato/1.
:- dynamic localizacao/1.
:- dynamic data/3.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ), !,fail.
	
teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a involucao do conhecimento

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - -  -  -  -  -   -

% Invariantes Universais

% Invariante que garante que não existe conhecimento
% perfeito positivo repetido
+T :: (solucoes(T, T, S),
       comprimento(S, 1)).

% Invariante que garante que não existe conhecimento
% perfeito negativo repetido
+(-T) :: (solucoes(T, -T, S),
          comprimento(S, 1)).

% Invariante que não permite adicionar conhecimento
% perfeito positivo que contradiz conhecimento perfeito negativo
+T :: nao(-T).

% Invariante que não permite adicionar conhecimento
% perfeito negativo que contradiz conhecimento perfeito positivo
+(-T) :: nao(T).

% Invariante que garante que não existem excecoes repetidas
+(excecao(T)) :: (solucoes(T, excecao(T), S),
                  comprimento(S, 1)).

%- - - - - - - - - - - - - - - - - - - - - - - - - - -  -  -  -  -   -

+adjudicante(ID,N,NIF,M)::(solucoes( NIF,( adjudicante(ID,_,NIF,_) ),S ),
				  comprimento( S,1 )).

+adjudicataria(ID,N,NIF,M)::(solucoes( NIF,( adjudicataria(ID,_,NIF,_ ) ),S ),
                  comprimento( S,1 )).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

% Garantir que cada adjudicante é unico
+adjudicante(ID,N,NIF,M)::(solucoes( (ID,N,NIF,M),( adjudicante( ID,N,NIF,M ) ),S ),
                  comprimento( S,N ), 
				  N == 1  ).

% Garantir que cada adjudicataria é unica
+adjudicataria(ID,N,NIF,M)::(solucoes( (ID,N,NIF,M),(adjudicataria( ID,N,NIF,M )),S ),
                  comprimento( S,Num ), 
				  Num == 1  ).

% Garantir que cada contrato é unico
+contrato(AN,AT,TC,TP,D,V,P,L,DT) :: (solucoes((AN,AT,TC,TP,D,V,P,L,DT),(contrato(AN,AT,TC,TP,D,V,P,L,DT)),S),
                  comprimento( S,Num ), 
				  Num == 1  ).

% Garantir que cada procedimento é unico (não sendo possível adicionar um novo tipo)
+tipoProcedimento(T)::(solucoes(T,(tipoProcedimento(T)),S),
				  comprimento( S,Num ),
				  Num == 0  ).

% Invariante Referencial: 
%

% Garantir 
+adjudicante(ID,N,NIF,M)::(solucoes((Ns,Ms),(adjudicante(ID,Ns,NIF,Ms)),S),
				  comprimento(S,Num),
				  Num=<1).

+adjudicataria(ID,N,NIF,M)::(solucoes((Ns,Ms),(adjudicataria(ID,Ns,NIF,Ms) ),S),
				  comprimento( S,N ),
                  Num=<1).

% Garantir que contrato com mesmo adjudicante, adjudicataria, tipo de contrato e tipo de procedimento, tem informação diferente(descrição,valor,prazo,local e data)
+contrato(AN,AT,TC,TP,D,V,P,L,DT)::(solucoes( (Ds,Vs,Ps,Ls,DTs),( contrato(AN,AT,TC,TP,Ds,Vs,Ps,Ls,DTs) ),S ),
                  comprimento( S,N ), 
				  N == 1 ).

% Garantir que para o tipo de procedimento Ajuste Direto não se podem adicionar outros tipos de contrato para além de Aquisição e Locação de bens móveis e Aquisição de Serviços.
+contrato(AN,AT,TC,tipoProcedimento('Ajuste Direto'),D,V,P,L,DT) :: (solucoes(TCs,contrato(_,_,TCs,tipoProcedimento('Ajuste Direto'),_,_,_,_,_),S),
                  apagaT(tipoContrato('Aquisição de bens móveis'),S,L1),
                  apagaT(tipoContrato('Locação de bens móveis'),L1,L2),
                  apagaT(tipoContrato('Aquisição de serviços'),L2,L3),
                  comprimento(L3,Num),
                  Num == 0).

% Garantir que não se pode remover um contrato que não exite na base de conhecimento
-contrato(AN,AT,TC,TP,D,V,P,L,DT) :: (solucoes((AN,AT,TC,TP,D,V,P,L,DT),(contrato(AN,AT,TC,TP,D,V,P,L,DT)),S),
                  comprimento( S,Num ),
				  Num == 0).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -

contrato(AN,AT,TC,TP,D,V,P,L,DT) :-
	tipoProcedimento(TP),
	adjudicante(Z,AN,X),
	adjudicataria(Q,AT,W),
	tipoContrato(TC),
	localizacao(L).

tipoProcedimento('Ajuste Direto').
tipoProcedimento('Consulta Prévia').
tipoProcedimento('Concurso Público').

contrato(AN,AT,TC,tipoProcedimento('Ajuste Direto'),D,V,P,L,DT) :- V =< 5000, P =< 365.

tipoContrato('Aquisição de bens móveis').
tipoContrato('Locação de bens móveis').
tipoContrato('Aquisição de serviços').

contrato(AN,AT,tipoContrato('Aquisição de bens móveis'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).
contrato(AN,AT,tipoContrato('Locação de bens móveis'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).
contrato(AN,AT,tipoContrato('Aquisição de serviços'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).

%- Impedir a inserção de contratos com diferentes tipos dos já definidos, para o tipo de procedimento Ajuste Direto.


% Funções Auxiliares

% Extensao do predicado par: X -> {V,F}

par(0).
par(X) :- NX is X-2,NX >= 1,impar(NX).

% Extensao do predicado impar: X -> {V,F}

impar(1).
impar(X) :- NX is X-2,NX >= 1,impar(NX).

avancaDias(data(D,M,A),Dias,data(Ds,Ms,As)) :- (((M == 2) -> ((D + Dias > 29) -> (avancaDias(data(1,3,A),Dias - (29 - D),data(Ds,3,As)));
                                                            (D + Dias =< 29) -> (Ds is D + Dias)));
                                               ((M =< 7) -> (((impar(M)) -> ((D + Dias > 31) -> Ms is M + 1, avancaDias(data(1,Ms,A),Dias - (31 - D),data(Ds,Ms,As)));
                                                                  ((D + Dias =< 31) -> Ds is D + Dias));
                                                            ((par(M)) ->  ((D + Dias > 30) -> Ms is M + 1, avancaDias(data(1,Ms,A),Dias - (30 - D),data(Ds,Ms,As)));
                                                                  ((D + Dias =< 30) -> Ds is D + Dias))));                                           
                                               ((M >= 8) -> (((impar(M)) -> ((D + Dias > 30) -> Ms is M + 1, avancaDias(data(1,Ms,A),Dias - (30 - D),data(Ds,Ms,As)));
                                                                  ((D + Dias < 30) -> Ds is D + Dias));
                                                            ((par(M)) ->  ((D + Dias > 31) -> Ms is M + 1, avancaDias(data(1,Ms,A),Dias - (31 - D),data(Ds,Ms,As)));
                                                                  ((D + Dias =< 31) -> Ds is D + Dias)))).
                                


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

difAno(AnoEconomico,AN,AT,TC,D,C) :- solucoes(Ano,contrato(AN,AT,TC,_,D,_,_,_,data(_,_,Ano)),S), maxL(S,J), C is AnoEconomico - J.

acumulaContrato(AN,AT,C) :- solucoes(V,contrato(AN,AT,_,_,_,V,_,_,_),S),somaList(S,C). 

% Invariante referencial : 
% Para um contrato entre as mesmas entidades com o mesmo serviço prestado não se pode adicionar dito contrato, caso o valor acumulado de todos os contrato seja maior ou igual que 75000, e
% a diferença entre o ano económico e o ano do ultimo contrato realizado seja menor ou igual que dois
+contrato(AN,AT,tipoContrato('Aquisição de Serviços'),TP,D,V,P,L,data(_,_,Ano)) :: (acumulaContrato(AN,AT,C), C >= 75000, difAno(Ano,AN,AT,tipoContrato('Aquisição de Serviços'),D,S), S =< 2).
