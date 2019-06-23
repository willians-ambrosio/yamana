 /*-----------------------------------------------------------------------
    File        : esap0501-u01
    Purpose     : Pegar valores
    Syntax      :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esap0501-u01.p 2.06.00.000}
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
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-cod-fornec   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-serie        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-cod-estabel  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-nr-docto     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-vl-orig-me   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-dt-emissao   AS WIDGET-HANDLE NO-UNDO.

/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEF VARIABLE c-esap0501b-nro-docto       AS CHAR FORMAT "x(8)"     NO-UNDO.
DEF VARIABLE i-esap0501b-nro-docto       AS INT FORMAT  "99999999" NO-UNDO.

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
/*              "p-row-table..:" STRING(p-row-table)                */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */
/* ????????????????????????????????????????????????????????????????*/

DEF VARIABLE c-nr-docto       AS CHAR FORMAT "x(8)"     NO-UNDO.
DEF VARIABLE i-nr-docto       AS INT FORMAT  "99999999" NO-UNDO.


IF  p-ind-object = "BROWSER"
AND p-ind-event = "DELETE" THEN
DO:

    FIND FIRST lin-i-ap NO-LOCK 
         WHERE ROWID(lin-i-ap) = p-row-table NO-ERROR.

    ASSIGN c-nr-docto     = lin-i-ap.nr-docto  
           i-nr-docto     = int(c-nr-docto)
           c-nr-docto     = STRING(i-nr-docto,"99999999").

    FIND FIRST es-gera-des 
         WHERE es-gera-des.cod-emitente = lin-i-ap.cod-fornec 
           AND es-gera-des.nro-docto    = c-nr-docto 
           AND es-gera-des.parcela      = lin-i-ap.parcela 
           AND es-gera-des.serie        = lin-i-ap.serie 
           AND es-gera-des.cod-esp      = lin-i-ap.cod-esp NO-ERROR.

    IF AVAIL es-gera-des THEN

        DELETE es-gera-des.

END.

