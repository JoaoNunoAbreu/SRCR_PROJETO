%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Representação de conhecimento positivo e Negativo
% Representação de conhecimento imperfeito
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


