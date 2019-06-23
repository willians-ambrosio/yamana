 /*-----------------------------------------------------------------------
    File        : esre1001c1-u03
    Purpose     : Gera tabela da DES
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esre1001c3-u01.p 2.06.00.000}
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
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001c3-rend-trib       AS WIDGET-HANDLE NO-UNDO.


DEF VARIABLE c-esre1001c1-nro-docto       AS CHAR FORMAT "x(8)"    NO-UNDO.
DEF VARIABLE i-esre1001c1-nro-docto       AS INT FORMAT  "99999999" NO-UNDO.

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

    ASSIGN c-handle-obj = fc-handle-obj("rend-trib",p-wgh-frame).
    ASSIGN wh-esre1001c3-rend-trib    = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) .


END.


IF p-ind-event  = "AFTER-ASSIGN" AND 
   p-ind-object = "CONTAINER" THEN DO:

    FIND FIRST dupli-imp NO-LOCK 
         WHERE ROWID(dupli-imp) = p-row-table NO-ERROR.

    FIND FIRST mgesp.ext-tipo-tax NO-LOCK
         WHERE mgesp.ext-tipo-tax.cod-tax = dupli-imp.cod-retencao NO-ERROR.

    IF mgesp.ext-tipo-tax.gera-des = YES THEN DO:
      
        ASSIGN c-esre1001c1-nro-docto = wh-esre1001c1-nro-docto:SCREEN-VALUE
               i-esre1001c1-nro-docto = int(c-esre1001c1-nro-docto)
               c-esre1001c1-nro-docto = STRING(i-esre1001c1-nro-docto,"99999999").
           
    
        FIND FIRST es-gera-des 
             WHERE es-gera-des.cod-emitente = int(wh-esre1001c1-cod-emitente:SCREEN-VALUE)
               AND es-gera-des.nro-docto    = c-esre1001c1-nro-docto
               AND es-gera-des.serie        = wh-esre1001c1-serie-docto:SCREEN-VALUE
               AND es-gera-des.nat-operacao = wh-esre1001c1-nat-operacao:SCREEN-VALUE NO-ERROR.
    
        IF AVAIL es-gera-des THEN DO:
    
           ASSIGN es-gera-des.tot-valor-base = dec(wh-esre1001c3-rend-trib:SCREEN-VALUE).
        
        END.

    END.
         
END.
