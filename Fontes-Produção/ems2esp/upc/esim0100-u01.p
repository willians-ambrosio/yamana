 /*-----------------------------------------------------------------------
    File        : esim0100-u01
    Purpose     : Desabilitar botao executar por usuario
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esim0100-u01.p 2.06.00.000}
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
DEF NEW GLOBAL SHARED VARIABLE wh-esim0100-bt-executar   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE c-seg-usuario             AS CHAR          NO-UNDO.

/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.

/* ###########           Mapeador de Endere¯os de Objeos         ########## */

IF  p-ind-object = "CONTAINER"
AND p-ind-event = "INITIALIZE" THEN DO:
       ASSIGN c-handle-obj = fc-handle-obj("bt-executar", p-wgh-frame)
              wh-esim0100-bt-executar           = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
              wh-esim0100-bt-executar:SENSITIVE = YES.
                         
       FIND FIRST ext_usuar_mestre NO-LOCK 
            WHERE ext_usuar_mestre.cod_usuar = c-seg-usuario NO-ERROR.
       IF AVAIL ext_usuar_mestre THEN
           ASSIGN wh-esim0100-bt-executar:SENSITIVE = NO.
END.

/*##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####*/
/*      MESSAGE "p-ind-event..:" p-ind-event                  SKIP */
/*              "p-ind-object.:" p-ind-object                 SKIP */
/*              "p-cod-table..:" STRING(p-cod-table)          SKIP */
/*              "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP */
/*              "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                     */
/* ????????????????????????????????????????????????????????????????*/
