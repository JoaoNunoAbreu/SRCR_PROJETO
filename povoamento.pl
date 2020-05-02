:- use_module(library(system)).

adjudicante(202344142,'Municipio de Amarante'  ,202344142,'Amarante').
adjudicante(264306422,'Municipio de Amarante' ,264306422,'Amarante').
adjudicante(223184241,'Municipio de Vinhais'  ,223184241,'Vinhais').
adjudicante(276836642,'Municipio de Braga' ,276836642,'Braga').
adjudicante(256373191,'Municipio da Povoa de Varzim',256373191,'Povoa de Varzim').

adjudicataria(244059039,'Ana'     ,244059039,'Lisboa').
adjudicataria(241341671,'Joana'   ,241341671,'Faro').
adjudicataria(282945490,'Ines'    ,282945490,'Coimbra').
adjudicataria(277891213,'Luisa'   ,277891213,'Porto').
adjudicataria(203931467,'Patricia',203931467,'Evora').

contrato(202344142,244059039,'Aquisicao de bens moveis','Ajuste Direto'   ,'Assessoria juridica'  , 4999,364,'Alto de Basto'     ,data(29,4,2020)).
contrato(264306422,241341671,'Locacao de bens moveis' ,'Consulta previa' ,'Comparacoes de precos', 1231,501,'Leça da Palmeira'  ,data(30,5,2020)).
contrato(223184241,282945490,'Aquisicao de servicos','Concurso publico','Consulta medica'      , 5423, 30,'Viana do Castelo'  ,data( 1,6,2020)).
contrato(276836642,277891213,'Pre-reforma'             ,'Consulta previa' ,'Analise bancaria'     ,  825,100,'Marco de Canaveses',data( 7,7,2020)).
contrato(256373191,203931467,'Termo Incerto'          ,'Concurso publico','Analise de stocks'    ,49275,999,'Vila Verde'        ,data(24,8,2020)).

