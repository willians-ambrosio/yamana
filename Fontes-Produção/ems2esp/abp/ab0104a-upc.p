/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: AB0104A-UPC
** UPC cadastrada para programa: AB0104A
** Objetivo: FO 1708.134 - Inserir Fill-in
**           "Fator de Conversao para Indicadores""          
******************************************************************/
{include/i-prgvrs.i AB0104A-UPC 2.00.00.000}

/** Parƒmetros **/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.
/** Handle **/
DEFINE VARIABLE h-tx-label          AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-un                AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-cod-model         AS HANDLE        NO-UNDO.
/** Global **/
DEFINE NEW GLOBAL SHARED VAR wh-fi-fator-0104 AS WIDGET-HANDLE NO-UNDO.

RUN findWidget (INPUT "un",        INPUT "FILL-IN", INPUT p-wgh-frame, OUTPUT h-un).
RUN findWidget (INPUT "cod-model", INPUT "FILL-IN", INPUT p-wgh-frame, OUTPUT h-cod-model).

IF  p-ind-event  = "AFTER-INITIALIZE":U AND
    p-ind-object = "CONTAINER":U        THEN DO:

    /** Label Fator de Conversao para Indicadores **/
    CREATE TEXT h-tx-label
    ASSIGN FRAME        = p-wgh-frame
           FORMAT       = "x(36)"
           WIDTH        = 25
           SCREEN-VALUE = "Fator de Conversao para Indicadores:":U
           ROW          = 4.75
           COL          = 48
           VISIBLE      = YES
           FONT         = 1.

    /** Create Fill-in Fator de Conversao para Indicadores **/
    CREATE FILL-IN wh-fi-fator-0104
    ASSIGN FRAME     = p-wgh-frame
           NAME      = "wh-fi-fator-0104"
           DATA-TYPE = "DECIMAL"
           FORMAT    = "->>,>>9.99"
           WIDTH     = 8.86
           HEIGHT    = 0.88
           ROW       = 4.63
           COL       = 74
           VISIBLE   = YES
           SENSITIVE = NO
           FONT      = 1.         

    IF  VALID-HANDLE(wh-fi-fator-0104) AND 
        VALID-HANDLE(h-un)             THEN
        ASSIGN wh-fi-fator-0104:SENSITIVE = h-un:SENSITIVE.

    RUN piAtualiza IN THIS-PROCEDURE.
END.

PROCEDURE piAtualiza:
    IF  VALID-HANDLE(h-cod-model)       AND 
        VALID-HANDLE(wh-fi-fator-0104)   THEN DO:
        FOR FIRST mab-model-esp
            WHERE mab-model-esp.cod-model = h-cod-model:SCREEN-VALUE NO-LOCK:
        END.
        IF AVAIL mab-model-esp THEN
            ASSIGN wh-fi-fator-0104:SCREEN-VALUE = STRING(mab-model-esp.fator-conver,"->>,>>9.99").
        ELSE
            ASSIGN wh-fi-fator-0104:SCREEN-VALUE = "1":U.
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

    define input  parameter c-widget-name  as char   no-undo.
    define input  parameter c-widget-type  as char   no-undo.
    define input  parameter h-start-widget as handle no-undo.
    define output parameter h-widget       as handle no-undo.

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
