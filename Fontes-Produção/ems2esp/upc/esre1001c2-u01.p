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
DEF INPUT PARAM p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAM p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAM p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAM p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAM p-row-table  AS ROWID             NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

/* ###########           Definicao das Variavel Globais.         ########## */
DEF NEW GLOBAL SHARED VAR wh-esre1001c1-cod-emitente    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esre1001c1-serie-docto     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esre1001c1-nro-docto       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esre1001c1-nat-operacao    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esre1001c2-btok            AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esre1001c2-btok-f          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esre1001c2-int-1           AS WIDGET-HANDLE NO-UNDO.

DEF VAR c-esre1001c2-int-1           AS CHAR FORMAT "x(5)"     NO-UNDO.
DEF VAR i-esre1001c2-int-1           AS INT  FORMAT "99999"    NO-UNDO.
DEF VAR c-esre1001c1-nro-docto       AS CHAR FORMAT "x(8)"     NO-UNDO.
DEF VAR i-esre1001c1-nro-docto       AS INT  FORMAT "99999999" NO-UNDO.

/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEF VAR c-handle-obj AS CHAR NO-UNDO.

/* ###########           Mapeador de Endere¯os de Objeos         ########## */

/*##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### */
/*      MESSAGE "p-ind-event..:" p-ind-event                  SKIP  */
/*              "p-ind-object.:" p-ind-object                 SKIP  */
/*              "p-cod-table..:" STRING(p-cod-table)          SKIP  */
/*              "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP  */
/*              "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP  */
/*              "p-row-table..:" STRING(p-row-table)                */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */
/* ???????????????????????????????????????????????????????????????? */

IF  p-ind-object = "CONTAINER" AND
    p-ind-event = "AFTER-INITIALIZE" THEN DO:
    ASSIGN c-handle-obj        = fc-handle-obj("btok,int-1",p-wgh-frame)
           wh-esre1001c2-btok  = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-esre1001c2-int-1 = WIDGET-HANDLE(ENTRY(2,c-handle-obj)).
END.

IF  p-ind-event  = "AFTER-ASSIGN" AND 
    p-ind-object = "CONTAINER" THEN DO:
    ASSIGN c-esre1001c2-int-1     = wh-esre1001c2-int-1:SCREEN-VALUE
           i-esre1001c2-int-1     = int(c-esre1001c2-int-1)
           c-esre1001c2-int-1     = STRING(i-esre1001c2-int-1,"99999").

    ASSIGN c-esre1001c1-nro-docto = wh-esre1001c1-nro-docto:SCREEN-VALUE
           i-esre1001c1-nro-docto = int(c-esre1001c1-nro-docto)
           c-esre1001c1-nro-docto = STRING(i-esre1001c1-nro-docto,"99999999").

    ASSIGN c-esre1001c2-int-1 = wh-esre1001c2-int-1:SCREEN-VALUE.

    FIND FIRST es-gera-des 
         WHERE es-gera-des.cod-emitente = int(wh-esre1001c1-cod-emitente:SCREEN-VALUE)
           AND es-gera-des.nro-docto    = c-esre1001c1-nro-docto   
           AND es-gera-des.serie        = wh-esre1001c1-serie-docto :SCREEN-VALUE
           AND es-gera-des.nat-operacao = wh-esre1001c1-nat-operacao:SCREEN-VALUE NO-ERROR.
    IF AVAIL es-gera-des THEN DO:
        FIND FIRST docum-est NO-LOCK 
             WHERE docum-est.cod-emitente = es-gera-des.cod-emitente
               AND docum-est.nro-docto    = wh-esre1001c1-nro-docto:SCREEN-VALUE 
               AND docum-est.serie        = wh-esre1001c1-serie-docto:SCREEN-VALUE 
               AND docum-est.nat-operacao = wh-esre1001c1-nat-operacao:SCREEN-VALUE NO-ERROR.

        FIND FIRST mgesp.ext-tipo-tax NO-LOCK
             WHERE mgesp.ext-tipo-tax.cod-tax = int(c-esre1001c2-int-1) NO-ERROR.

        FIND FIRST dupli-apagar NO-LOCK 
             WHERE dupli-apagar.cod-emitente = docum-est.cod-emitente
               AND dupli-apagar.nro-docto    = docum-est.nro-docto 
               AND dupli-apagar.serie        = docum-est.serie 
               AND dupli-apagar.nat-operacao = docum-est.nat-operacao NO-ERROR.

        IF mgesp.ext-tipo-tax.gera-des THEN
            ASSIGN es-gera-des.cod-tax        = IF AVAIL mgesp.ext-tipo-tax THEN string(mgesp.ext-tipo-tax.cod-tax) ELSE ""
                   es-gera-des.tot-valor      = docum-est.tot-valor
                   es-gera-des.tot-valor-base = IF AVAIL mgesp.ext-tipo-tax THEN docum-est.tot-valor ELSE 0
                   es-gera-des.cod-esp        = dupli-apagar.cod-esp.
    END.
END.
