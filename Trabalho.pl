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
%:- dynamic adjudicante/4.
%:- dynamic adjudicataria/4.
%:- dynamic contrato/9.
:- dynamic adjudicante/3.
:- dynamic adjudicataria/3.
:- dynamic contrato/7.

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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+adjudicante(N,NIF,M)::(solucoes( (N,NIF,M),(adjudicante( N,NIF,M )),S ),
                  comprimento( S,N ), 
				  N == 1  ).

+adjudicataria(N,NIF,M)::(solucoes( (N,NIF,M),(adjudicataria( N,NIF,M )),S ),
                  comprimento( S,N ), 
				  N == 1  ).

+contrato(AN,AT,TC,TP,D,V,P,L,DT)::(solucoes( (AN,AT,TC,TP,D,V,P,L,DT),( contrato(AN,AT,TC,TP,D,V,P,L,DT) ),S ),
                  comprimento( S,N ), 
				  N == 1  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -



%--------------------------------- - - - - - - - - - -  -  -  -  -   -