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
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-cod-recol-iss   AS WIDGET-HANDLE NO-UNDO.  
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-nat-operacao    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-c-nat-operacao  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-cod-tax         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE c-seg-usuario                AS CHAR          NO-UNDO.

/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.

/* ###########           Mapeador de Endere¯os de Objeos         ########## */

    FIND FIRST ext-natur-oper NO-LOCK 
         WHERE ext-natur-oper.nat-operacao = wh-esre1001a-nat-operacao:SCREEN-VALUE NO-ERROR.

    IF AVAIL ext-natur-oper 
         AND ext-natur-oper.gera-des = YES THEN DO:

        ASSIGN wh-esre1001a-tipo-docto:SENSITIVE = YES.

        FIND FIRST natur-oper NO-LOCK 
             WHERE natur-oper.nat-operacao = ext-natur-oper.nat-operacao NO-ERROR. 

        IF AVAIL natur-oper THEN
               ASSIGN wh-esre1001a-c-nat-operacao:SCREEN-VALUE = natur-oper.denominacao.
        ELSE
            ASSIGN wh-esre1001a-c-nat-operacao:SCREEN-VALUE = "".

    END.
    ELSE DO:
        
        ASSIGN wh-esre1001a-tipo-docto:SENSITIVE = NO.

        FIND FIRST natur-oper NO-LOCK 
             WHERE natur-oper.nat-operacao = ext-natur-oper.nat-operacao NO-ERROR. 

        IF AVAIL natur-oper THEN
              ASSIGN wh-esre1001a-c-nat-operacao:SCREEN-VALUE = natur-oper.denominacao.
        ELSE
            ASSIGN wh-esre1001a-c-nat-operacao:SCREEN-VALUE = "".
    END.


