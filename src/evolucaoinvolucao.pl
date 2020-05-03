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
evolucao(adjudicataria(ID,N,NIF,MD),M1,M2) :-
    remocao((excecao(adjudicataria(ID,N,NIF,M1)))),
    remocao((excecao(adjudicataria(ID,N,NIF,M2)))).


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