 /*-----------------------------------------------------------------------
    File        : esre1001a1-u01
    Purpose     : Gera tabela da DES
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esre1001a1-u01.p 2.06.00.000}
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
DEF VARIABLE c-nro-docto       AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VARIABLE i-nro-docto       AS INT FORMAT  "99999999" NO-UNDO.

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
AND p-ind-event = "AFTER-ASSIGN"
AND STRING(p-cod-table) = "docum-est" THEN
DO:

    
    FIND FIRST docum-est NO-LOCK 
         WHERE ROWID(docum-est) = p-row-table NO-ERROR.


    ASSIGN i-nro-docto     = int(docum-est.nro-docto)
           c-nro-docto     = STRING(i-nro-docto,"99999999").


    FIND FIRST es-gera-des 
         WHERE es-gera-des.cod-emitente = docum-est.cod-emitente
           AND es-gera-des.nro-docto    = c-nro-docto
           AND es-gera-des.serie        = docum-est.serie
           AND es-gera-des.nat-operacao = docum-est.nat-operacao NO-ERROR.

    IF AVAIL es-gera-des THEN 
       ASSIGN es-gera-des.tot-valor = docum-est.tot-valor.
    

END.
      

