 /*-----------------------------------------------------------------------
    File        : esap0501a-u01
    Purpose     : Inluir FILL IN de tipo de documento e Valor do Imposto
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i escd0182-u01.p 2.06.00.000}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}       

/* ###########           Parametros obrigatorios para UPC.        ########## */
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

/* ###########           Definicao das Variavel Globais.         ########## */
DEF NEW GLOBAL SHARED VARIABLE wh-escd0182-descricao        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-escd0182-gera-des         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-escd0182-cod-tax          AS WIDGET-HANDLE NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.

/* ###########           Mapeador de Endere¯os de Objeos         ########## */

/*##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####*/
/*      MESSAGE "p-ind-event..:" p-ind-event                  SKIP  */
/*              "p-ind-object.:" p-ind-object                 SKIP  */
/*              "p-cod-table..:" STRING(p-cod-table)          SKIP  */
/*              "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP  */
/*              "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP  */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */
/* ????????????????????????????????????????????????????????????????*/

IF  p-ind-object = "CONTAINER"
AND p-ind-event = "before-INITIALIZE" THEN
DO:

    ASSIGN c-handle-obj = fc-handle-obj("tg-imp-ret,cod-tax",p-wgh-frame).
    ASSIGN wh-escd0182-descricao   = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-escd0182-cod-tax     = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) .    


        CREATE TOGGLE-BOX wh-escd0182-gera-des
        ASSIGN FRAME         = wh-escd0182-descricao:FRAME
               FORMAT        = 'YES/NO'
               COL           = wh-escd0182-descricao:COL 
               ROW           = wh-escd0182-descricao:ROW - 1
               WIDTH         = 12
               SENSITIVE     = NO
               NAME          = "tg-gera-des"
               LABEL         = "Gera DES"
               TOOLTIP       = "Indica se o Imposto Gera DES".



       
END.
  

IF  p-ind-object = "VIEWER"
AND p-ind-event  = "ENABLE" 
AND p-wgh-object:PRIVATE-DATA = "divwr/v04di217.w" THEN DO:

    ASSIGN wh-escd0182-gera-des:SENSITIVE = YES.

END.


IF  p-ind-object = "VIEWER"
AND p-ind-event  = "BEFORE-DISPLAY" 
AND p-wgh-object:PRIVATE-DATA = "divwr/v04di217.w" THEN DO:

    FIND FIRST mgesp.ext-tipo-tax NO-LOCK
         WHERE mgesp.ext-tipo-tax.cod-tax = int(wh-escd0182-cod-tax:SCREEN-VALUE) NO-ERROR.

    IF AVAIL mgesp.ext-tipo-tax 
         AND mgesp.ext-tipo-tax.gera-des = NO THEN
        ASSIGN wh-escd0182-gera-des:SCREEN-VALUE = "NO".
    ELSE
        ASSIGN wh-escd0182-gera-des:SCREEN-VALUE = "YES".
      
END.

IF  p-ind-object = "VIEWER"
AND p-ind-event  = "VALIDATE" 
AND p-wgh-object:PRIVATE-DATA = "divwr/v04di217.w" THEN DO:

    FIND FIRST ext-tipo-tax 
         WHERE ext-tipo-tax.cod-tax = int(wh-escd0182-cod-tax:SCREEN-VALUE) NO-ERROR.

    IF AVAIL ext-tipo-tax THEN
        ASSIGN ext-tipo-tax.gera-des = IF string(wh-escd0182-gera-des:SCREEN-VALUE) = "YES" THEN YES ELSE NO.

END.


IF  p-ind-object = "VIEWER"
AND p-ind-event  = "DISABLE" 
AND p-wgh-object:PRIVATE-DATA = "divwr/v04di217.w" THEN DO:

    ASSIGN wh-escd0182-gera-des:SENSITIVE = NO.

END.
