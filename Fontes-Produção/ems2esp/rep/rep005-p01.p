/****************************************************************************************** 
** 	   Programa: rep005-p01.p
**   	      Autor: Vando Ribeiro
**   	 Fornecedor: DKP
**    	 Data: 05/11/2018
** Change/Chamado: REQ03
**    Objetivo: Gatilho de Execu‡Æo do programa rep/rep005-rp.p (cria‡Æo de pedidoos de execu‡Æo automatico)
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**

****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: tt-param e tt-digita
** PAR¶METROS DE SAÖDA: p-mot-reprov
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHARACTER NO-UNDO.
DEFINE VARIABLE raw-param AS RAW NO-UNDO.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as INTEGER.

CREATE tt-param.
ASSIGN 
    tt-param.destino          = 3
    tt-param.arquivo          = SESSION:TEMP-DIRECTORY + "rep005rp.txt"
    tt-param.usuario          = c-seg-usuario
    tt-param.data-exec        = TODAY
    tt-param.hora-exec        = TIME.

RAW-TRANSFER tt-param TO raw-param.

DEF TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita  AS RAW.

RUN rep/rep005-rp.p (INPUT raw-param, 
                     INPUT TABLE tt-raw-digita).


