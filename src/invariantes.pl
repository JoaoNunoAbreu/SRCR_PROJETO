%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

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