 /*-----------------------------------------------------------------------
    File        : esap0501j-u01
    Purpose     : Pegar valores
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esap0501j-u01.p 2.06.00.000}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{UTP/UT-GLOB.I}


/* ###########           Parametros obrigatorios para UPC.        ########## */
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

/* ###########           Definicao das Variavel Globais.         ########## */
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-cod-esp   AS WIDGET-HANDLE NO-UNDO.


/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.
DEFINE VARIABLE c-tipo-tax   AS CHAR NO-UNDO.
DEFINE VARIABLE i-qt-tipo    AS INT  NO-UNDO.
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
AND p-ind-event = "INITIALIZE" THEN
DO:
    IF i-ep-codigo-usuario <> "201" THEN NEXT.
    
    ASSIGN c-handle-obj = fc-handle-obj("cod-esp",p-wgh-frame).
    ASSIGN wh-esap0501j-cod-esp           = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).


END.


  

