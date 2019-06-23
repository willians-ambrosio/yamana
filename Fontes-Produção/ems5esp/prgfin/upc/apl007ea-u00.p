/****************************************************************************************** 
** 	   Programa: apl007ea-u00.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: Validar a criaá∆o de campo para informar o contrato m∆e na tela da alteraá∆o/modificaá∆o da operaá∆o financeira
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: mod_operac_financ - apl007ea.p
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.


RUN prgfin/upc/apl007ea-u01.p (p-ind-event ,
                               p-ind-object,
                               p-wgh-object,
                               p-wgh-frame ,
                               p-cod-table, 
                               p-row-table ).

RUN prgfin/upc/apl007ea-u02.p (p-ind-event ,
                               p-ind-object,
                               p-wgh-object,
                               p-wgh-frame ,
                               p-cod-table, 
                               p-row-table ).
