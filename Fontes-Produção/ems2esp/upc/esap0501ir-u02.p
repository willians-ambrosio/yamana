 /*-----------------------------------------------------------------------
    File        : esap0501ir-u01
    Purpose     : Inluir FILL IN de tipo de documento e Valor do Imposto
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esap0501ir-u02.p 2.06.00.000}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}       


/* ###########           Definicao das Variavel Globais.         ########## */
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-c-nr-docto      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-i-cod-imposto   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-text-tipo-docto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-tipo-docto      AS WIDGET-HANDLE NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */
                                         
DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.


FIND FIRST mgesp.ext-tipo-tax NO-LOCK
     WHERE mgesp.ext-tipo-tax.cod-tax = int(wh-esap0501ir-i-cod-imposto:SCREEN-VALUE) NO-ERROR.

IF AVAIL mgesp.ext-tipo-tax 
     AND mgesp.ext-tipo-tax.gera-des = YES THEN DO:

     ASSIGN wh-esap0501ir-tipo-docto:SENSITIVE = YES.

END.
