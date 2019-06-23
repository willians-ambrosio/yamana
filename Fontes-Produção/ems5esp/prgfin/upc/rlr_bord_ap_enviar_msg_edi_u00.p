/****************************************************************************************** 
** 	   Programa: fnc_lote_impl_tit_ap_atualiza.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 21/08/2018
** Change/Chamado: 
**      Objetivo: Criar campo "imposto" e solicitar o c¢digo do tributo qdo for verdadeiro - Usado apenas qdo o 
                  bordorì n∆o for de DARF
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: fnc_lote_impl_tit_ap_atualiza
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{include\i-epc200.i1}

/* DEFINE INPUT  PARAMETER p-ind-event AS CHARACTER   NO-UNDO. */
/* DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-epc.             */
 
  /*
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID     NO-UNDO.  
    */
  
/*     MESSAGE p-ind-event                    */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
