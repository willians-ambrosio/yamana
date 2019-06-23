 /*-----------------------------------------------------------------------
    File        : esre1001c1-u01
    Purpose     : Gera tabela da DES
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esre1001c1-u01.p 2.06.00.000}
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
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001c1-cod-emitente    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001c1-serie-docto     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001c1-nro-docto       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001c1-nat-operacao    AS WIDGET-HANDLE NO-UNDO.


/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.

/* ###########           Mapeador de Endere¯os de Objeos         ########## */


/*##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####*/
/*      MESSAGE "p-ind-event..:" p-ind-event                  SKIP  */
/*              "p-ind-object.:" p-ind-object                 SKIP  */
/*              "p-cod-table..:" STRING(p-cod-table)          SKIP  */
/*              "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP  */
/*              "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP  */
/*              "p-row-table..:" string(p-row-table)                */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */
/* ????????????????????????????????????????????????????????????????*/


IF  p-ind-object = "CONTAINER"
AND p-ind-event = "AFTER-INITIALIZE" THEN
DO:

    ASSIGN c-handle-obj = fc-handle-obj("cod-emitente,serie-docto,nro-docto,nat-operacao",p-wgh-frame).
    ASSIGN wh-esre1001c1-cod-emitente   = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-esre1001c1-serie-docto    = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) 
           wh-esre1001c1-nro-docto      = WIDGET-HANDLE(ENTRY(3,c-handle-obj)) 
           wh-esre1001c1-nat-operacao   = WIDGET-HANDLE(ENTRY(4,c-handle-obj)). 


         
END.

