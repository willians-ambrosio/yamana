/****************************************************************************************** 
** 	   Programa: apl007aa-u00.p
**   	  Autor: Richardt 
** 	 Fornecedor: DKP
**         Data: 21/12/2017
** Change/Chamado: 
**      Objetivo: Cria bot∆o para permitir vincular contrato ROF e CÉmbio
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: apl007aa - bas_operac_financ
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.

RUN prgfin/upc/apl007aa-u01.p (p-ind-event ,
                               p-ind-object,
                               p-wgh-object,
                               p-wgh-frame ,
                               p-cod-table, 
                               p-row-table ).

RUN prgfin/upc/apl007aa-u02.p (p-ind-event ,
                               p-ind-object,
                               p-wgh-object,
                               p-wgh-frame ,
                               p-cod-table,
                               p-row-table ).
