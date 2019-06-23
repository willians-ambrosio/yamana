 /*-----------------------------------------------------------------------
    File        : escd0606
    Purpose     : Inluir FLAG de gera DES
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
{utp/ut-glob.i}

/* ###########           Parametros obrigatorios para UPC.        ########## */
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

/* ###########           Definicao das Variavel Globais.         ########## */
DEF NEW GLOBAL SHARED VARIABLE wh-escd0606-perc-red-iss     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-escd0606-gera-des         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-escd0606-nat-operacao     AS WIDGET-HANDLE NO-UNDO.
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
AND p-ind-event = "BEFORE-INITIALIZE" THEN
DO:



    ASSIGN c-handle-obj = fc-handle-obj("perc-red-iss,nat-operacao",p-wgh-frame).
    ASSIGN wh-escd0606-perc-red-iss   = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-escd0606-nat-operacao   = WIDGET-HANDLE(ENTRY(2,c-handle-obj)).

 
        CREATE TOGGLE-BOX wh-escd0606-gera-des
        ASSIGN FRAME         = wh-escd0606-perc-red-iss:FRAME
               FORMAT        = 'YES/NO'
               COL           = wh-escd0606-perc-red-iss:COL + 10 /* Fava - Kraft - 06.05.10 */
               ROW           = wh-escd0606-perc-red-iss:ROW      /* Fava - Kraft - 06.05.10 */
               WIDTH         = 12
               SENSITIVE     = NO
               NAME          = "tg-gera-des"
               LABEL         = "Gera DES"
               TOOLTIP       = "Indica se o Imposto Gera DES".

END.
  


IF  p-ind-object = "CONTAINER"
AND p-ind-event  = "BEFORE-ENABLE" /*
AND p-wgh-object:PRIVATE-DATA = "divwr/v04di217.w" */ THEN DO:

    ASSIGN wh-escd0606-gera-des:SENSITIVE = YES.

END.


IF  p-ind-object = "CONTAINER"
AND p-ind-event  = "AFTER-DISPLAY" /*
AND p-wgh-object:PRIVATE-DATA = "divwr/v04di217.w" */ THEN DO:

    IF i-ep-codigo-usuario <> "201" THEN
        NEXT.

    FIND FIRST ext-natur-oper NO-LOCK
         WHERE ext-natur-oper.nat-operacao = wh-escd0606-nat-operacao:SCREEN-VALUE NO-ERROR.

    IF AVAIL ext-natur-oper 
         AND ext-natur-oper.gera-des = NO THEN
        ASSIGN wh-escd0606-gera-des:SCREEN-VALUE = "NO".
    ELSE
        ASSIGN wh-escd0606-gera-des:SCREEN-VALUE = "YES".
      
END.

IF  p-ind-object = "CONTAINER"
AND p-ind-event  = "BEFORE-ASSIGN" /*
AND p-wgh-object:PRIVATE-DATA = "divwr/v04di217.w" */ THEN DO:

    IF i-ep-codigo-usuario <> "201" THEN
        NEXT.
    
    FIND FIRST ext-natur-oper 
         WHERE ext-natur-oper.nat-operacao = wh-escd0606-nat-operacao:SCREEN-VALUE NO-ERROR.

    IF AVAIL ext-natur-oper THEN
        ASSIGN ext-natur-oper.gera-des = IF string(wh-escd0606-gera-des:SCREEN-VALUE) = "YES" THEN YES ELSE NO.

END.


IF  p-ind-object = "CONTAINER"
AND p-ind-event  = "AFTER-DISABLE" /*
AND p-wgh-object:PRIVATE-DATA = "divwr/v04di217.w"  */ THEN DO:

    ASSIGN wh-escd0606-gera-des:SENSITIVE = NO.

END.


