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
{include/i-prgvrs.i esre1001a-u01.p 2.06.00.000}
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
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-natureza        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-btok            AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-btok-f          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-text-tipo-docto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-tipo-docto      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-nom-tipo        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-nat-operacao    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-c-nat-operacao  AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-text-cod-tax    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-cod-tax         AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-cod-emitente    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-nro-docto       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-serie-docto     AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE c-seg-usuario                AS CHAR          NO-UNDO.

DEF VAR c-tipo-tax AS CHAR NO-UNDO.
DEF VAR i-qt-tipo    AS INT NO-UNDO.

DEF VAR c-cod-tax AS CHAR NO-UNDO.
DEF VAR i-qt-tax    AS INT NO-UNDO.

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


IF  p-ind-object = "CONTAINER"
AND p-ind-event = "BEFORE-INITIALIZE" THEN
DO:

    ASSIGN c-handle-obj = fc-handle-obj("c-natureza-comp,nat-operacao,c-nat-operacao",p-wgh-frame).
    ASSIGN wh-esre1001a-natureza       = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-esre1001a-nat-operacao   = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
           wh-esre1001a-c-nat-operacao = WIDGET-HANDLE(ENTRY(3,c-handle-obj)).

    CREATE TEXT wh-esre1001a-text-tipo-docto     
    ASSIGN FRAME         = wh-esre1001a-natureza:FRAME
           FORMAT        = "x(18)"
           WIDTH         = 10
           HEIGHT-CHARS  = 0.78
           SCREEN-VALUE  = "Tipo Docto:"
           ROW           = wh-esre1001a-natureza:ROW - 2.75  /* Fava - Kraft - 06.05.10 */
           COL           = wh-esre1001a-natureza:COL + 11 /* Fava - Kraft - 06.05.10 */
           VISIBLE       = YES.

    FOR EACH es-tipo-docto. 
       ASSIGN c-tipo-tax = c-tipo-tax + string(es-tipo-docto.cod-tipo-docto) + ","
              i-qt-tipo = i-qt-tipo + 1 . 
    END.

    CREATE COMBO-BOX wh-esre1001a-tipo-docto
    ASSIGN FRAME        = wh-esre1001a-natureza:FRAME
           DATA-TYPE    = "CHARACTER"
           FORMAT       = "x(5)" 
           WIDTH        = wh-esre1001a-natureza:WIDTH
           NAME         = "tipo-docto"
           ROW          = wh-esre1001a-natureza:ROW - 2
           COL          = wh-esre1001a-natureza:COL + 11
           VISIBLE      = YES
           SENSITIVE    = NO
           INNER-LINES  = i-qt-tipo.



    ASSIGN c-tipo-tax = SUBSTRING(c-tipo-tax,1,LENGTH(c-tipo-tax) - 1). /*RETIRA A ULTIMA LINHA EM BRANCO*/ 
    ASSIGN wh-esre1001a-tipo-docto:LIST-ITEMS  = c-tipo-tax. 

    
    CREATE FILL-IN wh-esre1001a-nom-tipo    
    ASSIGN FRAME      =  wh-esre1001a-natureza:FRAME 
           NAME       = 'cod-recol-iss'
           FORMAT     = "x(60)"
           COL        = wh-esre1001a-natureza:COL + 20
           ROW        = wh-esre1001a-natureza:ROW - 2
           WIDTH      = wh-esre1001a-natureza:WIDTH + 30
           HEIGHT     = 0.88
           SENSITIVE  = NO
           VISIBLE    = YES .
    

END.

ON "LEAVE":U OF wh-esre1001a-nat-operacao   PERSISTENT RUN upc/esre1001a-u03.p.

ON "LEAVE":U OF wh-esre1001a-tipo-docto PERSISTENT RUN upc/esre1001a-u02.p.
  

