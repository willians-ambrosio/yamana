/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: CD0910-UPC
** UPC cadastrada para programa: CD0910
** Objetivo: FO 1708.134 - Inserir um novo fill-in 
                           "Horas de Opera‡Æo".
******************************************************************/
{include/i-prgvrs.i CD0910-UPC 2.00.00.000}

/** Parƒmetros **/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.
/** Handle **/
DEFINE VARIABLE h-tx-label          AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fPage1            AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-cod-altern        AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fPage3            AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-cd-equipto        AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-cd-tag            AS HANDLE        NO-UNDO.
/** Global **/
DEFINE NEW GLOBAL SHARED VAR wh-fi-hr-op-0910       AS WIDGET-HANDLE NO-UNDO.

RUN findWidget (INPUT "fPage1",     INPUT "FRAME",   INPUT p-wgh-frame, OUTPUT h-fPage1).
RUN findWidget (INPUT "cod-altern", INPUT "FILL-IN", INPUT p-wgh-frame, OUTPUT h-cod-altern).
RUN findWidget (INPUT "cd-equipto", INPUT "FILL-IN", INPUT p-wgh-frame, OUTPUT h-cd-equipto).
RUN findWidget (INPUT "fPage3",     INPUT "FRAME",   INPUT p-wgh-frame, OUTPUT h-fPage3).
RUN findWidget (INPUT "cd-tag",     INPUT "FILL-IN", INPUT h-fPage3,    OUTPUT h-cd-tag).

IF  p-ind-event  = "AFTER-INITIALIZE":U AND
    p-ind-object = "CONTAINER":U        THEN DO:

    /** Label Fator de Conversao para Indicadores **/
    CREATE TEXT h-tx-label
    ASSIGN FRAME        = h-fPage1
           FORMAT       = "x(18)"
           WIDTH        = 15
           SCREEN-VALUE = "Horas de Opera‡Æo:":U
           ROW          = 3.11
           COL          = 44.72
           VISIBLE      = YES
           FONT         = 1.

    /** Create Fill-in Fator de Conversao para Indicadores **/
    CREATE FILL-IN wh-fi-hr-op-0910
    ASSIGN FRAME     = h-fPage1
           NAME      = "wh-fi-hr-op-0910"
           DATA-TYPE = "DECIMAL"
           FORMAT    = "->>,>>9.99"
           WIDTH     = 12.86
           HEIGHT    = 0.88
           ROW       = 3.04
           COL       = 58.80
           VISIBLE   = YES
           SENSITIVE = NO
           FONT      = 1.

    RUN piAtualiza IN THIS-PROCEDURE.
END.

IF  p-ind-event = "AFTER-ADD":U     OR
    p-ind-event = "AFTER-COPY":U    OR
    p-ind-event = "AFTER-UPDATE":U  OR
    p-ind-event = "AFTER-DISABLE":U THEN DO:
    IF  VALID-HANDLE(wh-fi-hr-op-0910) AND
        VALID-HANDLE(h-cod-altern)     THEN DO:
        ASSIGN wh-fi-hr-op-0910:SENSITIVE = h-cod-altern:SENSITIVE.
    END.
END.

IF p-ind-event = "AFTER-DISPLAY":U THEN DO:
    RUN piAtualiza IN THIS-PROCEDURE.
END.

PROCEDURE piAtualiza:
    IF  VALID-HANDLE(h-cd-equipto)     AND 
        VALID-HANDLE(h-cd-tag)   AND 
        VALID-HANDLE(wh-fi-hr-op-0910) THEN DO:
        /** Tipo = 1 -> Eqpto **/
        FOR FIRST mmi-horas-oper
            WHERE mmi-horas-oper.cd-equipto = h-cd-equipto:SCREEN-VALUE
            AND   mmi-horas-oper.cd-tag     = h-cd-tag:SCREEN-VALUE 
            AND   mmi-horas-oper.tipo       = 1 NO-LOCK:
        END.
        IF AVAIL mmi-horas-oper THEN
            ASSIGN wh-fi-hr-op-0910:SCREEN-VALUE = STRING(mmi-horas-oper.horas-oper-eqpto,"->>,>>9.99").
        ELSE
            ASSIGN wh-fi-hr-op-0910:SCREEN-VALUE = "0,00":U.
    END.
END PROCEDURE.

PROCEDURE findWidget:
    /*
    * PARAMETROS:
    *   c-widget-name:  nome do widget a ser localizado
    *   c-widget-type:  tipo do widget a ser localizado
    *   h-start-widget: container para procurar o widget
    *   h-widget:       widget encontrado 
    */

    define input  parameter c-widget-name  as char   NO-UNDO.
    define input  parameter c-widget-type  as char   NO-UNDO.
    define input  parameter h-start-widget as handle NO-UNDO.
    define output parameter h-widget       as handle NO-UNDO.

    do while valid-handle(h-start-widget):
        if h-start-widget:name = c-widget-name and
           h-start-widget:type = c-widget-type then do:
            assign h-widget = h-start-widget:handle.
            leave.
        end.

        if h-start-widget:type = "field-group":u or
           h-start-widget:type = "frame":u or
           h-start-widget:type = "dialog-box":u then do:
            run findWidget (input  c-widget-name,
                            input  c-widget-type,
                            input  h-start-widget:first-child,
                            output h-widget).
    
            if valid-handle(h-widget) then
                leave.
        end.
        assign h-start-widget = h-start-widget:next-sibling.
    end.
END PROCEDURE.
