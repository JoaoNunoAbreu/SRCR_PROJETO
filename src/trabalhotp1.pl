%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

% Sistema de Representação de Conhecimento e Raciocínio com capacidade para
% caracterizar um universo de discurso na área da contratação
% pública para a realização de contratos para a prestação de serviçoes
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
% Includes

:- include('predicadosauxiliares.pl').
:- include('conhecimento.pl').
:- include('evolucaoinvolucao.pl').
:- include('invariantes.pl').
:- include('Queries.pl').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pressuposto do domínio fechado

-adjudicante(ID,N,NIF,M) :- nao(adjudicante(ID,N,NIF,M)), nao(excecao(adjudicante(ID,N,NIF,M))).

-adjudicataria(ID,N,NIF,M) :- nao(adjudicataria(ID,N,NIF,M)), nao(excecao(adjudicataria(ID,N,NIF,M))).

-contrato(AN,AT,TC,TP,D,V,P,L,DT) :- nao(contrato(AN,AT,TC,TP,D,V,P,L,DT)), nao(excecao(contrato(AN,AT,TC,TP,D,V,P,L,DT))).

%contrato(AN,AT,TC,tipoProcedimento('Ajuste Direto'),D,V,P,L,DT) :- V =< 5000, P =< 365.


