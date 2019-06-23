 /*-----------------------------------------------------------------------
    File        : esre1001a-u01
    Purpose     : Inluir FILL IN de tipo de documento
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esre1001a-u02.p 2.06.00.000}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}       

/* ###########           Definicao das Variavel Globais.         ########## */
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-natureza        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-text-tipo-docto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-tipo-docto      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-nom-tipo        AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE c-seg-usuario             AS CHAR          NO-UNDO.

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


FIND FIRST es-tipo-docto NO-LOCK
     WHERE es-tipo-docto.cod-tipo-docto = wh-esre1001a-tipo-docto:SCREEN-VALUE NO-ERROR.

IF AVAIL es-tipo-docto THEN
    
    ASSIGN wh-esre1001a-nom-tipo:SCREEN-VALUE = es-tipo-docto.desc-tipo-docto.
    
    ELSE DO:
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Tipo de Nota Invalido~~" +
                                 "Tipo de Nota nao cadastrado.").
        
        ASSIGN wh-esre1001a-tipo-docto:SCREEN-VALUE = ""
               wh-esre1001a-nom-tipo:SCREEN-VALUE = "". 
        RETURN NO-APPLY.
    END.






