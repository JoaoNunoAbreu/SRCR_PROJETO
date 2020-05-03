%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- use_module(library(system)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/9.
:- dynamic tipoProcedimento/1.
:- dynamic tipoContrato/1.
:- dynamic data/3.


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
% Sistema de Inferência

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
                  comprimento( S,Num ), 
          Num == 1  ).

% Garantir que a não inserção de um conhecimento negativo de uma adjudicante que já exista.
+(-adjudicante(ID,N,NIF,M))::(solucoes( (ID,N,NIF,M),( (-adjudicante( ID,N,NIF,M ) )),S ),
                  comprimento( S,Num ), 
          Num == 1  ).

% Garantir que cada adjudicataria é unica
+adjudicataria(ID,N,NIF,M)::(solucoes( (ID,N,NIF,M),(adjudicataria( ID,N,NIF,M )),S ),
                  comprimento( S,Num ), 
          Num == 1  ).

% Garantir que a não inserção de um conhecimento negativo de uma adjudicataria que já exista.
+(-adjudicataria(ID,N,NIF,M))::(solucoes( (ID,N,NIF,M),((-adjudicataria( ID,N,NIF,M ))),S ),
                  comprimento( S,Num ), 
          Num == 1  ).

% Garantir que cada contrato é unico
+contrato(AN,AT,TC,TP,D,V,P,L,DT) :: (solucoes((AN,AT,TC,TP,D,V,P,L,DT),(contrato(AN,AT,TC,TP,D,V,P,L,DT)),S),
                  comprimento( S,Num ), 
          Num == 1  ).

% Garantir que a não inserção de um conhecimento negativo de um contrato que já exista.
+(-contrato(AN,AT,TC,TP,D,V,P,L,DT)) :: (solucoes((AN,AT,TC,TP,D,V,P,L,DT),((-contrato(AN,AT,TC,TP,D,V,P,L,DT))),S),
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

% Garantir que não se pode remover um adjudicante que não exite na base de conhecimento
-adjudicante(ID,N,NIF,M)::(solucoes((ID,N,NIF,M),(adjudicante(ID,N,NIF,M)),S),
          comprimento(S,Num),
          Num == 1).

% Garantir que ID e NIF de adjudicatária são únicos.
+adjudicataria(ID,N,NIF,M)::(solucoes((Ns,Ms),(adjudicataria(ID,Ns,NIF,Ms) ),S),
          comprimento( S,Num ),
                  Num=<1).

% Garantir que apenas se pode retirar da base de conhecimento uma adjudicataria que exista.
-adjudicataria(ID,N,NIF,M)::(solucoes((ID,N,NIF,M),(adjudicataria(ID,N,NIF,M)),S),
          comprimento(S,Num),
          Num == 1).

% Garantir que ID e NIF de adjudicatária são iguais.
+adjudicataria(ID,_,NIF,_):: (ID > 0, ID == NIF).

% Garantir que para o tipo de procedimento Ajuste Direto não se podem adicionar outros tipos de contrato para além de Aquisição e Locação de bens móveis e Aquisição de Serviços.
+contrato(AN,AT,TC,tipoProcedimento('Ajuste Direto'),D,V,P,L,DT) :: (solucoes(TCs,contrato(_,_,TCs,tipoProcedimento('Ajuste Direto'),_,_,_,_,_),S),
                  apagaT(tipoContrato('Aquisiçao de bens moveis'),S,L1),
                  apagaT(tipoContrato('Locacao de bens moveis'),L1,L2),
                  apagaT(tipoContrato('Aquisicao de servicos'),L2,L3),
                  comprimento(L3,Num),
                  Num == 0).

% Para um contrato entre as mesmas entidades com o mesmo serviço prestado não se pode adicionar dito contrato, caso o valor acumulado de todos os contrato seja maior ou igual que 75000, e
% a diferença entre o ano económico e o ano do ultimo contrato realizado seja menor ou igual que dois
+contrato(AN,AT,tipoContrato('Aquisição de Serviços'),TP,D,V,P,L,data(_,_,Ano)) :: (acumulaContrato(AN,AT,C), C >= 75000, difAno(Ano,AN,AT,tipoContrato('Aquisição de Serviços'),D,S), S =< 2).

% Garantir que não se pode remover um contrato que não exite na base de conhecimento
-contrato(AN,AT,TC,TP,D,V,P,L,DT) :: (solucoes((AN,AT,TC,TP,D,V,P,L,DT),(contrato(AN,AT,TC,TP,D,V,P,L,DT)),S),
                  comprimento( S,Num ),
          Num == 0).

%Garantir que só se pode anular um contrato depois de cumprido 30% dos dias do prazo final.
-contrato(AN,AT,TC,TP,D,V,P,L,DT) :: (datimeToData(X),avancaDias(DT,P,F), isAfter(F, X), fidelizacao(P,R), avancaDias(DT,R,RD),isAfter(X,RD)).

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


contrato(AN,AT,tipoContrato('Aquisicao de bens moveis'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).
contrato(AN,AT,tipoContrato('Locacao de bens moveis'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).
contrato(AN,AT,tipoContrato('Aquisicao de servicos'),tipoProcedimento('Ajuste Direto'),D,V,P,L,DT).

%- Impedir a inserção de contratos com diferentes tipos dos já definidos, para o tipo de procedimento Ajuste Direto.

% Pressuposto do domínio fechado
-adjudicante(ID,N,NIF,M) :- nao(adjudicante(ID,N,NIF,M)), nao(excecao(adjudicante(ID,N,NIF,M))).

-adjudicataria(ID,N,NIF,M) :- nao(adjudicataria(ID,N,NIF,M)), nao(excecao(adjudicataria(ID,N,NIF,M))).

-contrato(AN,AT,TC,TP,D,V,P,L,DT) :- nao(contrato(AN,AT,TC,TP,D,V,P,L,DT)), nao(excecao(contrato(AN,AT,TC,TP,D,V,P,L,DT))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funções Auxiliares

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

% Extensao do predicado par: X -> {V,F}
par(0).
par(X) :- NX is X-2,NX >= 0,par(NX).

% Extensao do predicado killMeIDontWannaStudy: X -> {V,F}

killMeIDontWannaStudy('Hugo').

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



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicante: #IdAd, Nome, NIF, Morada -> {V,F}

% Conhecimento Perfeito Positivo.

adjudicante(500745471,'Santa Casa de Lisboa',500745471,'Portugal,Lisboa').
adjudicante(506415082,'Municipio de Coimbra',506415082,'Portugal,Lisboa').
adjudicante(501102752,'Municipio de Amarante',501102752,'Portugal,Amarante').
adjudicante(506770664,'Municipio de Vouzela',506770664,'Portugal,Viseu').
adjudicante(600020339,'Procuradoria Geral da Republica',600020339,'Portugal,Lisboa').
adjudicante(500051070,'Municipio de Lisboa',500051070,'Portugal,Lisboa').

% Conhecimento Perfeito Negativo
% Entidades publicas não têm NIF de privados,pessoas singulares
-adjudicante(276836642,'Pedro' ,276836642,'Braga').

% Conhecimento Imperfeito Incerto
% Não se sabe a morada do adjudicante Municipio de Vinhais
excecao(adjudicante(ID,N,NIF,M)) :- adjudicante(ID,N,NIF,moradaDesconhecida).
adjudicante(223184241,'Municipio de Vinhais',223184241,moradaDesconhecida).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicatária: #IdAda, Nome, NIF, Morada -> {V,F}

% Conhecimento Perfeito Positivo 
adjudicataria(514023708,'Arcos combinados-Arquietetos Associados LDA.',514023708,'Portugal').
adjudicataria(513826602,'VANITYFORMULA - PECAS AUTO, UNIPESSOAL, LDA',513826602,'Portugal').
adjudicataria(508559871,'EDILAGES,S.A.',508559871,'Portugal').
adjudicataria(514495790,'Dream2Fly,Lda',514495790,'Portugal').
adjudicataria(506155676,'Xamane,S.A.',506155676,'Portugal').
adjudicataria(508190495,'Alugal,Lda',508190495,'Portugal').
adjudicataria(505002892,'INESC',505002892,'Portugal,Lisboa').
adjudicataria(500489297,'A.da Costa, Lda.',500489297,'Portugal').

% Conhecimento Perfeito Negativo
% Nif da entidade adjudicataria não pode ser uma pessoa singular
-adjudicataria(203931467,'Patricia',203931467,'Evora').

% Conhecimento Imperfeito Impreciso(morada do adjudicante é desconhecido 
%porém sabe-se é uma de entre um intervalo de respostas)
excecao(adjudicante(264306422,'Municipio de Braga' ,264306422,'Braga,Pacos de Concelho')).
excecao(adjudicante(264306422,'Municipio de Amarante' ,264306422,'Braga,Se')).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado contrato: #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Custo, Prazo, Local, Data -> {V,F} 

% Conhecimento Perfeito Positivo 
contrato(500745471,514023708,'Aquisicao de servicos','Concurso publico','Levantamento Topografico',31500,90,'Portugal',data(24,4,2020)).
contrato(506415082,513826602,'Aquisicao de bens e servicos','Concurso publico','Pecas para viaturas,maquinas e equipamentos',31250,1080,'Portugal,Coimbra',data(27,12,2019)).
contrato(501102752,508559871,'Empreitadas de obras publicas','Concurso publico','Pavilhao Desportivo da EB 2,3',987853,360,'Portugal,Porto,Amarante',data(30,3,2020)).
contrato(506770664,514495790,'Aquisicao de bens moveis','Ajuste direto','Aquisicao de Computadores Portateis',10800,8,'Portugal,Viseu',data(30,4,2020)).
contrato(506770664,506155676,'Aquisicao de bens moveis','Ajuste direto','Equipamento de protecao individual',7241,10,'Portugal,Viseu',data(27,4,2020)).
contrato(501102752,508190495,'Lococao de bens moveis','Ajuste direto','Lococao de monoblocos',6300,181,'Portugal,Porto,Amarante',data(19,12,2019)).

% Conhecimento Perfeito Negativo
% Contrato com um tipo de contrato pre reforma não está previsto no
% código de contrato públicos mas sim de trabalho
-contrato(501102752,508190495,'Pre-reforma','Ajuste direto','Lococao de monoblocos',6.300,181,'Portugal,Porto,Amarante',data(19,12,2019)).

% Conhecimento Imperfeito Impreciso
% Só se sabe que o valor está entre 7000€ e 10000€ a.
excecao(contrato(500051070,500489297,'Aquisicao de bens moveis','Concurso publico','Aquisicao de artigos de fardamento',V,90,'Portugal,Lisboa',data(2,4,2020))) :- V >= 7000,V =< 10000.


% Conhecimento Imperfeito Interdito

% É impossivel saber o valor do contrato cuja execução deve ser acompanhada de medidas especiais de segurança
contrato(202344142,244059039,'Aquisicao de bens moveis','Ajuste Direto','Assessoria juridica',valorSecreto,364,'Alto de Basto',data(29,4,2020)).
excecao(contrato(AN,AT,TC,TP,D,V,P,L,DT)) :- contrato(AN,AT,TC,TP,D,valorSecreto,P,L,DT).
nuloInterdito(valorSecreto).

% Garantir que não é adicionado valor conhecido a um contrato cujo valor deve permanecer desconhecido
+contrato(AN,AT,TC,TP,D,V,P,L,DT) :: (solucoes((AN,AT,TC,TP,D,V,P,L,DT),
                                              (contrato(202344142,244059039,'Aquisicao de bens moveis','Ajuste Direto','Assessoria juridica', valorSecreto,364,'Alto de Basto',data(29,4,2020))
                                              ,nao(nuloInterdito(valorSecreto))),S), 
                                     comprimento( S,0 )).

% É impossivel saber o tipo de contrato,descricao e localizacao  cuja 
% execução deve ser acompanhada de medidas especiais de segurança
% bem como  os interesses essenciais de defesa e 
% segurança do Estado o exigirem

contrato(600020339,505002892,contratoSecreto,'Ajuste Direto',descricaoSecreta,75000,60,localizacaoSecreta,data(29,4,2020)).
excecao(contrato(AN,AT,TC,TP,D,V,P,L,DT)) :- contrato(AN,AT,contratoSecreto,TP,descricaoSecreta,V,P,localizacaoSecreta,DT).

nuloInterdito(contratoSecreto).
nuloInterdito(descricaoSecreta).
nuloInterdito(localizacaoSecreta).

% Garantir que não é adicionado valor conhecido a um contrato cujo valor deve permanecer desconhecido
+contrato(AN,AT,TC,TP,D,V,P,L,DT) :: (solucoes((AN,AT,TC,TP,D,V,P,L,DT),
                                              (contrato(600020339,505002892,contratoSecreto,'Ajuste Direto',descricaoSecreta,75000,60,localizacaoSecreta,data(29,4,2020))
                                              ,nao(nuloInterdito(contratoSecreto))
                                              ,nao(nuloInterdito(descricaoSecreta))
                                              ,nao(nuloInterdito(localizacaoSecreta))),S), 
                                     comprimento( S,0 )).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Evolução e Involução

%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% Extensão do predicado que permite a involucao do conhecimento

% Retira novo conhecimento na base de conhecimento positivo
involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

% Retira novo conhecimento na base de conhecimento negativo
involucao( -Termo ) :-
    solucoes( Invariante,-(-Termo)::Invariante,Lista ),
    remocao( -Termo ),
    teste( Lista ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensão do predicado que permite a evolucao do conhecimento

% Insere novo conhecimento na base de conhecimento positivo
evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

% Insere novo conhecimento na base de conhecimento negativo
evolucao( -Termo) :-
    solucoes( Invariante,+(-Termo)::Invariante,Lista ),
    insercao( -Termo ),
    teste( Lista ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%Adjudicante

% Insere conhecimento imperfeito incerto na base de conhecimento
% no caso de uma entidade adjudicante com morada desconhecida
evolucao(adjudicante(ID,N,NIF,moradaDesconhecida)) :-
    evolucao(adjudicante(ID,N,NIF,moradaDesconhecida)),
    insercao((excecao(adjudicante(IDAd,NAd,NIFAd,MAd)) :-
                    adjudicante(IDAd,NAd,NIFAd,moradaDesconhecida))).

% Remove conhecimento imperfeito incerto na base de conhecimento
% no caso de uma entidade adjudicante com morada desconhecida
involucao(adjudicante(ID,N,NIF,moradaDesconhecida)) :-
    involucao(adjudicante(ID,N,NIF,moradaDesconhecida)),
    remocao((excecao(adjudicante(IDAd,NAd,NIFAd,MAd)) :-
                    adjudicante(IDAd,NAd,NIFAd,moradaDesconhecida))).

%Adjudicataria



% Insere conhecimento imperfeito impreciso na base de conhecimento
% no caso de uma entidade adjudicante com morada desconhecida entre
% duas moradas
evolucao(adjudicataria(ID,N,NIF,MD),M1,M2) :-
    insercao((excecao(adjudicataria(ID,N,NIF,M1)))),
    insercao((excecao(adjudicataria(ID,N,NIF,M2)))).

% Remove conhecimento imperfeito incerto na base de conhecimento
% no caso de uma entidade adjudicante com morada desconhecida
% duas moradas
involucao(adjudicataria(ID,N,NIF,M)) :-
    remocao((excecao(adjudicataria(ID,N,NIF,M)) :-
                    adjudicataria(ID,N,NIF,M))).


%Contrato

evolucao(Termo,imp) :-
    solucoes(Invariante,+Termo::Invariante,Lista),
    insercao(excecao(Termo)),
    teste(Lista).

involucao(Termo, imp) :-
    solucoes(I,-(excecao(Termo))::Invariante,Lista),
    remocao(excecao(Termo)),
    teste(Lista).

% Insere conhecimento imperfeito impreciso na base de conhecimento
% no caso de um contrato entre dois valores

evolucao(contrato(AN,AT,TC,TP,D,ValorD,P,L,DT),imp,LimiteInferior,LimiteSuperior) :-
    insercao((excecao(contrato(AN,AT,TC,TP,D,ValorD,P,L,DT)) :-
                    ValorD >= LimiteInferior, ValorD =< LimiteSuperior)).

% Remove conhecimento imperfeito incerto na base de conhecimento
% no caso de um contrato entre dois valoreS
involucao(contrato(AN,AT,TC,TP,D,ValorD,P,L,DT),LimiteSuperior,LimiteInferior) :-
    remocao((excecao(contrato(AN,AT,TC,TP,D,ValorD,P,L,DT)) :-
                      ValorD >= LimiteInferior, ValorD =< LimiteSuperior)).


% Insere conhecimento imperfeito interdito na base de conhecimento
evolucao(contrato(AN,AT,contratoSecreto,TP,descricaoSecreta,ValorD,P,localizacaoSecreta,DT)) :-
	evolucao(contrato(AN,AT,contratoSecreto,TP,descricaoSecreta,ValorD,P,localizacaoSecreta,DT)),
    insercao((excecao(contrato(ANC,ATC,TCC,TPC,DC,ValorDC,PC,LC,DTC)) :-
    contrato(AN,AT,contratoSecreto,TP,descricaoSecreta,ValorD,P,localizacaoSecreta,DT))),
    insercao((nuloInterdito(contratoSecreto))),
    insercao((nuloInterdito(descricaoSecreta))),
    insercao((nuloInterdito(localizacaoSecreta))).

% Remove conhecimento imperfeito interdio na base de conhecimento
involucao(contrato(AN,AT,contratoSecreto,TP,descricaoSecreta,ValorD,P,localizacaoSecreta,DT)) :-
	involucao(contrato(AN,AT,contratoSecreto,TP,descricaoSecreta,ValorD,P,localizacaoSecreta,DT)),
    remocao((excecao(contrato(ANC,ATC,TCC,TPC,DC,ValorDC,PC,LC,DTC)) :-
    remocao(AN,AT,contratoSecreto,TP,descricaoSecreta,ValorD,P,localizacaoSecreta,DT))),
    remocao((nuloInterdito(contratoSecreto))),
    remocao((nuloInterdito(descricaoSecreta))),
    remocao((nuloInterdito(localizacaoSecreta))).