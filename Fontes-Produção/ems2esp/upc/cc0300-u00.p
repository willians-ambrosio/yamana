/****************************************************************************************** 
** 	   Programa: cc0300-u00.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: Grupo DKP
**         Data: 15/11/2018
** Change/Chamado: REQ01
**      Objetivo: Chamada das UPCs
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: p-ind-event, p-ind-object, p-wgh-object, p-wgh-frame, p-cod-table e p-row-table
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: cc0300
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.


/*A upc j† existia com o nome cc0300-upc.p, eu adequei ao padr∆o Yamana
  renomeando-a para cc0300_u01.p*/
RUN upc/cc0300-u01.p (INPUT p-ind-event,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame, 
                      INPUT p-cod-table, 
                      INPUT p-row-table).


/* Vando Ribeiro*/
RUN upc/cc0300-u02.p.
