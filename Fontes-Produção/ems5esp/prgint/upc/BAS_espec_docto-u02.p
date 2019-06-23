/****************************************************************************************** 
** 	   Programa: apl007aa-u01.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: Cria o bot∆o para vincular contrato ROF e CÉmbio na operaá∆o Financeira
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.

DEFINE INPUT PARAMETER h-bt-aux                AS WIDGET-HANDLE NO-UNDO.

DEF new GLOBAL SHARED VAR h-fi-cod-espec          AS WIDGET-HANDLE NO-UNDO.
DEF new GLOBAL SHARED VAR h-bt-elimina            AS WIDGET-HANDLE NO-UNDO.
/* campos da tela padr∆o */
DEF NEW GLOBAL SHARED VAR h-bt-des_espec_docto    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-cod_espec_docto    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-ent                AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-tip_espec_docto    AS WIDGET-HANDLE NO-UNDO.
/* fim campos da tela padrao */
DEF new GLOBAL SHARED VAR h-bt-salva              AS WIDGET-HANDLE NO-UNDO.
DEF new GLOBAL SHARED VAR h-bt-ok                 AS WIDGET-HANDLE NO-UNDO.
DEF new GLOBAL SHARED VAR CHBOX-IMP               AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR CHBOX-FGTS              AS widget-handle no-undo.
DEF NEW GLOBAL SHARED VAR CHBOX-GPS              AS widget-handle no-undo.


/* MESSAGE "p-ind-event " p-ind-event        SKIP */
/*         "p-ind-object" p-ind-object       SKIP */
/*         "p-wgh-object" p-wgh-object:NAME  SKIP */
/*         "p-wgh-frame " p-wgh-frame        SKIP */
/*         "p-cod-table " p-cod-table        SKIP */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.     */

IF  p-ind-event = "Apagar" 
AND p-ind-object = "VIEWER" THEN DO:

   DEFINE VARIABLE c-auxApagar AS CHARACTER   NO-UNDO.
   c-auxApagar = h-fi-cod-espec:SCREEN-VALUE.

   APPLY "CHOOSE" TO h-bt-aux.

   
   FIND FIRST ems5.espec_docto NO-LOCK
        WHERE espec_docto.cod_espec_docto = c-auxApagar NO-ERROR.


    IF NOT AVAIL espec_docto THEN DO:  


         FIND FIRST ext_espec_docto exclusive-LOCK
              WHERE ext_espec_docto.cod_espec_docto = c-auxApagar NO-ERROR.


         IF AVAIL ext_espec_docto THEN
             DELETE ext_espec_docto.
    
    END.

     


END.

IF  p-ind-event = "desabilitaFgts"
AND p-ind-object = "VIEWER" THEN DO:

  
    IF CHBOX-IMP:SCREEN-VALUE = "YES" THEN
       ASSIGN CHBOX-FGTS:SCREEN-VALUE = "NO"
              CHBOX-FGTS:SENSITIVE    = NO
              CHBOX-GPS:SCREEN-VALUE  = "NO"
              CHBOX-GPS:SENSITIVE     = NO.
    ELSE 
        ASSIGN CHBOX-FGTS:SENSITIVE   = YES
               CHBOX-GPS:SENSITIVE    = YES.  

END.

IF  p-ind-event = "desabilitaImposto"
AND p-ind-object = "VIEWER" THEN DO:

  
    IF CHBOX-FGTS:SCREEN-VALUE = "YES" THEN
       ASSIGN CHBOX-IMP:SCREEN-VALUE = "NO"
              CHBOX-IMP:SENSITIVE    = NO
              CHBOX-GPS:SCREEN-VALUE = "NO"
              CHBOX-GPS:SENSITIVE    = NO.        
        
    ELSE 
        ASSIGN CHBOX-IMP:SENSITIVE   = YES
               CHBOX-GPS:SENSITIVE   = YES.

END.

IF  p-ind-event = "desabilitaImpFgts"
AND p-ind-object = "VIEWER" THEN DO:

    IF CHBOX-GPS:SCREEN-VALUE = "YES" THEN
      ASSIGN CHBOX-IMP:SCREEN-VALUE  = "NO"
             CHBOX-IMP:SENSITIVE     = NO  
             CHBOX-FGTS:SCREEN-VALUE = "NO"
             CHBOX-FGTS:SENSITIVE    = NO.  

    ELSE
      ASSIGN CHBOX-IMP:SENSITIVE   = YES
             CHBOX-FGTS:SENSITIVE  = YES.

END.
