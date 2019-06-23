/****************************************************************************************** 
** 	   Programa: rpt_aprop_ctbl_cmg_contab_u00.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: REQ004
**      Objetivo: Chamada das UPCs
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: p-ind-event, p-ind-object, p-wgh-object, p-wgh-frame, p-cod-table e p-row-table
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: rpt_aprop_ctbl_cmg_contab
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID     NO-UNDO.

/* Vando Ribeiro*/
RUN prgfin/upc/rpt_aprop_ctbl_cmg_contab_u01.p (INPUT p-ind-event,
                                                INPUT p-ind-object,
                                                INPUT p-wgh-object,
                                                INPUT p-wgh-frame, 
                                                INPUT p-cod-table, 
                                                INPUT p-row-table).


