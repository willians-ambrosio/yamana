/****************************************************************************************** 
** 	   Programa: rep005.i
**   	      Autor: Vando Ribeiro
**   	 Fornecedor: Grupo DKP
**    	 Data: 29/10/2018
** Change/Chamado: 
**    Objetivo: Cria temp-table tt-param para ser usado no programa RE1005rp.p
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
** 
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

define temp-table tt-param NO-UNDO
    field destino            as integer
    field arquivo            as char
    field usuario            as char
    field data-exec          as date
    field hora-exec          as integer
    field classifica         as integer
    field c-cod-estabel-ini  as char
    field c-cod-estabel-fim  as char
    field i-cod-emitente-ini as integer
    field i-cod-emitente-fim as integer
    field c-nro-docto-ini    as char
    field c-nro-docto-fim    as char
    field c-serie-docto-ini  as char
    field c-serie-docto-fim  as char
    field c-nat-operacao-ini as char
    field c-nat-operacao-fim as char
    field da-dt-trans-ini    as date
    field da-dt-trans-fim    as DATE.   


    
define temp-table tt-digita NO-UNDO
    field r-docum-est        as rowid.

&GLOBAL-DEFINE bf_devolucao_exportacao YES


/* fim da include */
