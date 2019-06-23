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
{include/i-prgvrs.i esap0501ir-u01.p 2.06.00.000}
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
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-c-nr-docto      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-i-cod-imposto   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-c-cod-esp       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-text-tipo-docto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-tipo-docto      AS WIDGET-HANDLE NO-UNDO.

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
/*8377*/
IF  p-ind-object = "CONTAINER"
AND p-ind-event = "BEFORE-INITIALIZE" THEN
DO:

    ASSIGN c-handle-obj = fc-handle-obj("c-nr-docto,i-cod-imposto,c-cod-esp",p-wgh-frame).
    ASSIGN wh-esap0501ir-c-nr-docto      = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-esap0501ir-i-cod-imposto   = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
           wh-esap0501ir-c-cod-esp       = WIDGET-HANDLE(ENTRY(3,c-handle-obj)).

    CREATE TEXT wh-esap0501ir-text-tipo-docto    
    ASSIGN FRAME         = wh-esap0501ir-c-nr-docto:FRAME
           FORMAT        = "x(12)"
           WIDTH         = 12
           HEIGHT-CHARS  = 0.78
           SCREEN-VALUE  = "Tipo Docto:"
           ROW           = wh-esap0501ir-c-nr-docto:ROW + 2
           COL           = wh-esap0501ir-c-nr-docto:COL + 15
           VISIBLE       = YES.


    FOR EACH es-tipo-docto. 
       ASSIGN c-tipo-tax = c-tipo-tax + string(es-tipo-docto.cod-tipo-docto) + ","
              i-qt-tipo = i-qt-tipo + 1 . 
    END.

    CREATE COMBO-BOX wh-esap0501ir-tipo-docto
    ASSIGN FRAME        = wh-esap0501ir-c-nr-docto:FRAME
           DATA-TYPE    = "CHARACTER"
           FORMAT       = "x(5)" 
           WIDTH        = 9
           NAME         = "tipo-docto"
           ROW          = wh-esap0501ir-c-nr-docto:ROW + 2
           COL          = wh-esap0501ir-c-nr-docto:COL + 24
           VISIBLE      = YES
           SENSITIVE    = NO
           INNER-LINES  = i-qt-tipo.
        
    ASSIGN c-tipo-tax = SUBSTRING(c-tipo-tax,1,LENGTH(c-tipo-tax) - 1). /*RETIRA A ULTIMA LINHA EM BRANCO*/ 
    ASSIGN wh-esap0501ir-tipo-docto:LIST-ITEMS  = c-tipo-tax. 


    ON "LEAVE":U OF wh-esap0501ir-c-cod-esp PERSISTENT RUN upc/esap0501ir-u02.p.

END.
  


    
