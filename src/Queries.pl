%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Queries
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Querie de lista de contrato de um adjudicante

listaContratoAN(Entidade,L) :- solucoes(contrato(Entidade,A,B,C,D,E,F,G,H),contrato(Entidade,A,B,C,D,E,F,G,H),L), comprimento(L,S), S >= 0.

% Querie de lista de contrato de um adjudicataria

listaContratoAT(Entidade,L) :-  solucoes(contrato(A,Entidade,B,C,D,E,F,G,H),contrato(A,Entidade,B,C,D,E,F,G,H),L), comprimento(L,S), S >= 0.

% Querie de contratos com o maior valor.

valueOfContrato(contrato(_,_,_,_,_,V,_,_,_),X) :- X is V.

maxValCL([H],X) :- valueOfContrato(H,X), !. 
maxValCL([H|T],M) :- maxValCL(T,M),valueOfContrato(H,X), M >= X.
maxValCL([H|T],X) :- maxValCL(T,M),valueOfContrato(H,X), X > M. 

maxValContrato(Contrato) :- solucoes(contrato(A1,B1,C1,D1,E1,F1,G1,H1,I1),(contrato(A1,B1,C1,D1,E1,F1,G1,H1,I1),nao(nuloInterdito(F1))),L),maxValCL(L,VMax),
                            solucoes(contrato(A2,B2,C2,D2,E2,VMax,F2,G2,H2),contrato(A2,B2,C2,D2,E2,VMax,F2,G2,H2),Contrato).

% Querie que recebe o ID de um adjudicante e retorna o valor que este já pagou.

maxValAdjudicante(L) :- solucoes((A,B,C,D),adjudicante(A,B,C,D),L).

totalGastoAN(IDAN,Sum) :- solucoes(V,contrato(IDAN,_,_,_,_,V,_,_,_),S), sumValor(S,Sum).

% Querie que recebe o ID de uma adjudicataria e retorna o valor que esta já recebeu.

maxValAdjudicataria(L) :- solucoes((A,B,C,D),adjudicataria(A,B,C,D),L).

totalGastoAT(IDAT,Sum) :- solucoes(V,contrato(_,IDAT,_,_,_,V,_,_,_),S), sumValor(S,Sum).