/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: CD0581-UPC
** UPC cadastrada para programa: CD0581
** Objetivo: FO 1708.134 - Inserir um novo fill-in 
                           "Horas de Opera‡Æo".
******************************************************************/
{include/i-prgvrs.i CD0581-UPC 2.00.00.000}

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
DEFINE VARIABLE h-cod-tipo          AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-cd-tag            AS HANDLE        NO-UNDO.
/** Global **/
DEFINE NEW GLOBAL SHARED VAR wh-fi-hr-op-0581 AS WIDGET-HANDLE NO-UNDO.

RUN findWidget (INPUT "fPage1",  INPUT "FRAME",   INPUT p-wgh-frame, OUTPUT h-fPage1).
RUN findWidget (INPUT "cod-tipo",INPUT "FILL-IN", INPUT h-fPage1,    OUTPUT h-cod-tipo).
RUN findWidget (INPUT "cd-tag",  INPUT "FILL-IN", INPUT p-wgh-frame, OUTPUT h-cd-tag).

IF  p-ind-event  = "AFTER-INITIALIZE":U AND
    p-ind-object = "CONTAINER":U        THEN DO:

    /** Label Fator de Conversao para Indicadores **/
    CREATE TEXT h-tx-label
    ASSIGN FRAME        = h-fPage1
           FORMAT       = "x(18)"
           WIDTH        = 15
           SCREEN-VALUE = "Horas de Opera‡Æo:":U
           ROW          = 6.28
           COL          = 31.00
           VISIBLE      = YES
           FONT         = 1.

    /** Create Fill-in Fator de Conversao para Indicadores **/
    CREATE FILL-IN wh-fi-hr-op-0581
    ASSIGN FRAME     = h-fPage1
           NAME      = "wh-fi-hr-op-0581"
           DATA-TYPE = "DECIMAL"
           FORMAT    = "->>,>>9.99"
           WIDTH     = 8.86
           HEIGHT    = 0.88
           ROW       = 6.21
           COL       = 45.43
           VISIBLE   = YES
           SENSITIVE = NO
           FONT      = 1.

    RUN piAtualiza IN THIS-PROCEDURE.
END.

IF  p-ind-event = "AFTER-ADD":U     OR
    p-ind-event = "AFTER-COPY":U    OR
    p-ind-event = "AFTER-UPDATE":U  OR
    p-ind-event = "AFTER-DISABLE":U THEN DO:
    IF  VALID-HANDLE(wh-fi-hr-op-0581) AND
        VALID-HANDLE(h-cod-tipo)       THEN DO:
        ASSIGN wh-fi-hr-op-0581:SENSITIVE = h-cod-tipo:SENSITIVE.
    END.
END.

IF p-ind-event = "AFTER-DISPLAY":U THEN DO:
    RUN piAtualiza IN THIS-PROCEDURE.
END.

PROCEDURE piAtualiza:
    IF  VALID-HANDLE(h-cd-tag)         AND
        VALID-HANDLE(wh-fi-hr-op-0581) THEN DO:
        /** Tipo = 2 -> Tag **/
        FOR FIRST mmi-horas-oper
            WHERE mmi-horas-oper.cd-tag = h-cd-tag:SCREEN-VALUE
            AND   mmi-horas-oper.tipo   = 2 NO-LOCK:
        END.
        IF AVAIL mmi-horas-oper THEN
            ASSIGN wh-fi-hr-op-0581:SCREEN-VALUE = STRING(mmi-horas-oper.horas-oper-tag,"->>,>>9.99").
        ELSE
            ASSIGN wh-fi-hr-op-0581:SCREEN-VALUE = "24":U.
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
