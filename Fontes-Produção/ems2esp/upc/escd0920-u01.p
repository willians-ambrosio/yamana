
/*********************************************************************
** Programa....: escd0920-u01.P
** Descricao...: Manutencao Tipos Receita/ Despesa
** Autor.......: Rafael Batista 
** Data........: 05/2014
*********************************************************************/
/*************************************************************************************
                                   FUN∞ÜES/INCLUDES
*************************************************************************************/
{include/i-prgvrs.i escd0903-u01 2.06.00.000}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}
{utp/ut-glob.i}
/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.
/*************************************************************************************
                                  VARIÊVEIS GLOBAL
*************************************************************************************/
DEF NEW GLOBAL SHARED VAR wh-escd0920-u01-descricao        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-escp0920-u01-ativo            AS WIDGET-HANDLE NO-UNDO.
/*************************************************************************************
                                  VARIÊVEIS LOCAL
*************************************************************************************/
DEF VAR c-handle-obj   AS CHARACTER     NO-UNDO.
/*************************************************************************************
                                  TEMP-TABLES
*************************************************************************************/

/*************************************************************************************
                                  BUFFERS
*************************************************************************************/

/*************************************************************************************
                                    CABE∞ALHO
************************************************************************************/

/*************************************************************************************
                                    MAIN-BLOCK
************************************************************************************/
/* RUN pi-msg. */

IF p-ind-event               = "BEFORE-DISPLAY"   AND
   p-ind-object              = "VIEWER"           AND
   p-wgh-object:PRIVATE-DATA = "advwr/v01ad259.w" THEN
DO:
    ASSIGN c-handle-obj              = fc-handle-obj("descricao",p-wgh-frame)
           wh-escd0920-u01-descricao = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).

    CREATE TOGGLE-BOX wh-escp0920-u01-ativo
         ASSIGN NAME = "wh-escp0920-u01-ativo"
               FRAME = wh-escd0920-u01-descricao:FRAME
               ROW   = wh-escd0920-u01-descricao:ROW + 1
               COL   = wh-escd0920-u01-descricao:COL 
               LABEL = "Ativo"
               WIDTH    = 26
               VISIBLE = YES
               SENSITIVE = NO.

    FIND FIRST tipo-rec-desp WHERE ROWID(tipo-rec-desp) = p-row-table NO-LOCK NO-ERROR.
    IF AVAIL tipo-rec-desp THEN
    DO:
        FIND FIRST es-tipo-rec-desp WHERE es-tipo-rec-desp.tp-codigo = tipo-rec-desp.tp-codigo NO-LOCK NO-ERROR.
        IF AVAIL es-tipo-rec-desp THEN
            ASSIGN wh-escp0920-u01-ativo:CHECKED = es-tipo-rec-desp.log-ativo.
        ELSE
            ASSIGN wh-escp0920-u01-ativo:CHECKED = NO.
    END.
END.

IF  p-ind-event               = "AFTER-ENABLE"     AND
    p-ind-object              = "VIEWER"           AND
    p-wgh-object:PRIVATE-DATA = "advwr/v01ad259.w" THEN
DO:
    ASSIGN wh-escp0920-u01-ativo:SENSITIVE = YES
           wh-escp0920-u01-ativo:CHECKED   = YES.
END.

IF  p-ind-event               = "DISABLE"          AND
    p-ind-object              = "VIEWER"           AND
    p-wgh-object:PRIVATE-DATA = "advwr/v01ad259.w" THEN
DO:
    ASSIGN wh-escp0920-u01-ativo:SENSITIVE = NO.
END.

IF  p-ind-event               = "END-UPDATE" AND
    p-ind-object              = "VIEWER"     THEN
DO:
    FIND FIRST tipo-rec-desp WHERE ROWID(tipo-rec-desp) = p-row-table NO-ERROR.
    IF AVAIL tipo-rec-desp THEN
    DO:
        FIND FIRST es-tipo-rec-desp WHERE es-tipo-rec-desp.tp-codigo = tipo-rec-desp.tp-codigo NO-ERROR.
        IF AVAIL es-tipo-rec-desp THEN
            ASSIGN es-tipo-rec-desp.log-ativo = wh-escp0920-u01-ativo:CHECKED NO-ERROR.
        ELSE
        DO:
            CREATE es-tipo-rec-desp.
            ASSIGN es-tipo-rec-desp.tp-codigo = tipo-rec-desp.tp-codigo
                   es-tipo-rec-desp.log-ativo = wh-escp0920-u01-ativo:CHECKED NO-ERROR.
        END.
    END.
END.
IF p-ind-event               = "DELETE" AND
   p-ind-object              = "VIEWER" AND
   p-wgh-object:PRIVATE-DATA = "advwr/v01ad259.w" THEN
DO:
    FIND FIRST tipo-rec-desp WHERE ROWID(tipo-rec-desp) = p-row-table NO-LOCK NO-ERROR.
    IF AVAIL tipo-rec-desp THEN
    DO:
        FIND FIRST es-tipo-rec-desp WHERE es-tipo-rec-desp.tp-codigo = tipo-rec-desp.tp-codigo NO-LOCK NO-ERROR.
        IF AVAIL es-tipo-rec-desp THEN
            DELETE es-tipo-rec-desp.
    END.
END.

PROCEDURE pi-msg:
  MESSAGE "p-ind-event..:" p-ind-event            SKIP
          "p-ind-object.:" p-ind-object           SKIP
          "p-cod-table..:" STRING(p-cod-table)    SKIP
          "p-row-table..:" STRING(p-row-table)    SKIP
          "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP
          "p-wgh-frame..:" STRING(p-wgh-frame)    SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

IF RETURN-VALUE = 'NOK':U THEN
    RETURN 'NOK':U.
