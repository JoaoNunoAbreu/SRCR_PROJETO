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

+adjudicante(N,NIF,M)::(solucoes( NIF,( adjudicante(_,NIF,_) ),S )
				  comprimento( S,1 )).

+adjudicataria(N,NIF,M)::(solucoes( NIF,( adjudicataria( _,NIF,_ ) ),S ),
                  comprimento( S,1 )).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+adjudicante(N,NIF,M)::(solucoes( (N,NIF,M),( adjudicante( N,NIF,M ) ),S ),
                  comprimento( S,N ), 
				  N == 1  ).

+adjudicataria(N,NIF,M)::(solucoes( (N,NIF,M),(adjudicataria( N,NIF,M )),S ),
                  comprimento( S,N ), 
				  N == 1  ).

+contrato(AN,AT,TC,TP,D,V,P,L,DT)::(solucoes( (AN,AT,TC,TP,D,V,P,L,DT),( contrato(AN,AT,TC,TP,D,V,P,L,DT) ),S ),
                  comprimento( S,N ), 
				  N == 1  ).

+tipoProcedimento(T)::solucoes( T,( tipoProcedimento(T) ),S ),
				  comprimento( S,N ),
				  N == 0  ).

% Invariante Referencial: 
%

+adjudicante(N,NIF,M)::(solucoes( (Ns,Ms),( adjudicante(Ns,NIF,Ms) ),S) ),
				  comprimento( S,N ),
				  N<=1 ).

+adjudicataria(N,NIF,M)::(solucoes( (Ns,Ms),( adjudicataria(Ns,NIF,Ms) ),S) ),
				  comprimento( S,N ),
				  N<=1 ).

+contrato(AN,AT,TC,TP,D,V,P,L,DT)::(solucoes( (Ds,Vs,Ps,Ls,DTs),( contrato(AN,AT,TC,TP,Ds,Vs,Ps,Ls,DTs) ),S ),
                  comprimento( S,N ), 
				  N == 1  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

contrato(AN,AT,TC,TP,D,V,P,L,DT):-
	tipoProcedimento(TP),
	adjudicante(Z,AN,X),
	adjudicataria(Q,AT,W),
	tipoContrato(TC),
	localizacao(L).

tipoprocedimento('Ajuste Direto').
tipoprocedimento('Consulta Prévia').
tipoprocedimento('Concurso Público').

contrato(AN,AT,TC,tipoProcedimento('Ajuste Direto'),D,V,P,L,DT):-
	V<=5000, P<=365.

tipoContrato('Aquisição de bens móveis').
tipoContrato('Locação de bens móveis').
tipoContrato('Aquicição de serviços').

contrato(AN,AT,tipoContrato('Aquisição de bens móveis'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).
contrato(AN,AT,tipoContrato('Locação de bens móveis'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).
contrato(AN,AT,tipoContrato('Aquisição de serviços'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).

%- Impedir a inserção de contratos com diferentes tipos dos já definidos, para o tipo de procedimento Ajuste Direto.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -