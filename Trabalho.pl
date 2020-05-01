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
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
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

% Garantir que ID e NIF de adjudicante são únicos.
+adjudicante(ID,N,NIF,M)::(solucoes((Ns,Ms),(adjudicante(ID,Ns,NIF,Ms)),S),
          comprimento(S,Num),
          Num=<1).

% Garantir que ID e NIF de adjudicante são iguais.
+adjudicante(ID,_,NIF,_):: ID == NIF.

% Garantir que ID e NIF de adjudicatária são únicos.
+adjudicataria(ID,N,NIF,M)::(solucoes((Ns,Ms),(adjudicataria(ID,Ns,NIF,Ms) ),S),
          comprimento( S,Num ),
                  Num=<1).

% Garantir que ID e NIF de adjudicatária são iguais.
+adjudicataria(ID,_,NIF,_):: ID == NIF.

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

% Garantir que um contrato está associado a um adjudicante e adjudicatária que existam na base de conhecimento
+contrato(AN,AT,_,_,_,_,_,_,_) :: (solucoes(AN,(adjudicante(AN,_,_,_)),S), 
                  solucoes(AT,(adjudicataria(AT,_,_,_)),R),
                  comprimento( S,1 ),
                  comprimento( R,1 )).

% Garantir que um contrato é válido(campos corretos)
+contrato(AN,AT,TC,TP,D,V,P,L,DT) :: (countDigits(AN,N1), countDigits(AT,N2), tipoContrato(TC), tipoProcedimento(TP), isData(DT), N1 == 9, N2 == 9).

% Garantir que um o ID e NIF de uma adjudicante tem 9 dígitos
+adjudicante(ID,N,NIF,M) :: (countDigits(ID,N1), N1 == 9).

% Garantir que um o ID e NIF de uma adjudicataria tem 9 dígitos
+adjudicataria(ID,N,NIF,M) :: (countDigits(ID,N1), N1 == 9).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

-adjudicante(123456789,'Joana Abrea',123456789,'Amarante').
adjudicante(123456789,'Joana Abreia',123456789,'Amarante').
adjudicataria(234567891,'Nuna Abreia',234567891,'Amarante e arredores').
%contrato(123456789,23456789,tipoContrato('Aquisição de bens móveis'),tipoProcedimento('Ajuste Direto'),'wdcwcwec',200,200,'Amarante',data(29,4,2020)).

contrato(AN,AT,TC,TP,D,V,P,L,DT) :-
  tipoProcedimento(TP),
  adjudicante(Z,AN,X),
  adjudicataria(Q,AT,W),
  tipoContrato(TC),
  localizacao(L).

tipoProcedimento('Ajuste Direto').
tipoProcedimento('Consulta Previa').
tipoProcedimento('Concurso Publico').

contrato(AN,AT,TC,tipoProcedimento('Ajuste Direto'),D,V,P,L,DT) :- V =< 5000, P =< 365.

tipoContrato('Aquisicao de bens moveis').
tipoContrato('Locacao de bens moveis').
tipoContrato('Aquisicao de servicos').

contrato(AN,AT,tipoContrato('Aquisição de bens móveis'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).
contrato(AN,AT,tipoContrato('Locação de bens móveis'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).
contrato(AN,AT,tipoContrato('Aquisição de serviços'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).

%- Impedir a inserção de contratos com diferentes tipos dos já definidos, para o tipo de procedimento Ajuste Direto.


% Funções Auxiliares

% Extensao do predicado par: X -> {V,F}

par(0).
par(X) :- NX is X-2,NX >= 0,par(NX).

% Extensao do predicado impar: X -> {V,F}

impar(1).
impar(X) :- nao(par(X)).

% Extensao do predicado multiplo de quatro: X -> {V,F}

isB6(0).
isB6(A) :- 0 is mod(A,4).

% Extensao do predicado conta número de digitos de um número: X -> {V,F}

countDigits(0,0). 
countDigits(X,N) :- NX is div(X,10) ,NX >= 0,countDigits(NX,NN), N is NN + 1.

% Extensao do predicado adicao,subtracao,multiplicacao,divisao: X,Y -> {V,F}

operationTwo(X,Y,OP) :- ((OP == '+') -> R is X+Y;(OP == '-') -> R is X-Y),write(R).
operationTwo(X,Y,OP) :- ((OP == '*') -> R is X*Y;(OP == '/') -> R is X/Y),write(R).

% Extensao do predicado data: [H|T] -> {V,F}

data(D,2,A) :- D > 0, 0 < A,(isB6(A)) -> D =< 29; D =< 28.
data(D,M,A) :- M \= 2,M > 0, M =< 12,D > 0, D =< 31, 0 < A.

isData(data(D,M,A)) :- data(D,M,A).


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

difAno(AnoEconomico,AN,AT,TC,D,C) :- solucoes(Ano,contrato(AN,AT,TC,_,D,_,_,_,data(_,_,Ano)),S), maxL(S,J), C is AnoEconomico - J.

acumulaContrato(AN,AT,C) :- solucoes(V,contrato(AN,AT,_,_,_,V,_,_,_),S),somaList(S,C). 

% Invariante referencial : 
% Para um contrato entre as mesmas entidades com o mesmo serviço prestado não se pode adicionar dito contrato, caso o valor acumulado de todos os contrato seja maior ou igual que 75000, e
% a diferença entre o ano económico e o ano do ultimo contrato realizado seja menor ou igual que dois
+contrato(AN,AT,tipoContrato('Aquisição de Serviços'),TP,D,V,P,L,data(_,_,Ano)) :: (acumulaContrato(AN,AT,C), C >= 75000, difAno(Ano,AN,AT,tipoContrato('Aquisição de Serviços'),D,S), S =< 2).


% Garantir que não podem ser adicionados adjudicantes com número de NIF negativo.
+adjudicante(_,_,NIF,_) :: (NIF =< 0,solucoes(NIF,adjudicante(_,_,NIF,_),S), comprimento(S,0)).

% Garantir que não podem ser adicionadas adjudicatárias com número de NIF negativo.
+adjudicataria(_,_,NIF,_) :: (NIF =< 0,solucoes(NIF,adjudicataria(_,_,NIF,_),S), comprimento(S,0)).
