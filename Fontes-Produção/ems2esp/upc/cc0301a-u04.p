/**============================================================**
** Altera‡Æo...: Cleilton Conte
** Empresa.....: DSC
** Data........: 25/02/2015
** Objetivo....: Disponibilizar de-para dos itens harmonizados
**=============================================================**/
{include/i-prgvrs.i CC0301A-U04 11.5.11.000}

/** Parƒmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.


/** Global **/


/** Vari veis Locais **/
DEFINE VARIABLE hit-codigo      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hfi-descricao   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbt-depara      AS HANDLE      NO-UNDO.


/** Main-Block **/

/* Criar BotÆo DE-PARA */
IF p-ind-event               = "BEFORE-INITIALIZE" AND
   p-ind-object              = "VIEWER"            AND
   p-wgh-object:PRIVATE-DATA = "invwr/v09in274.w"  THEN DO:
    RUN findWidget (INPUT "it-codigo",      INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT hit-codigo).
    IF VALID-HANDLE(hit-codigo)  THEN DO:
        ON "VALUE-CHANGED" OF hit-codigo PERSISTENT RUN upc\cc0301a-u04a.p (INPUT hit-codigo,INPUT p-wgh-frame).
    END.
    RUN findWidget (INPUT "fi-descricao",   INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT hfi-descricao).
    IF VALID-HANDLE(hfi-descricao)  THEN DO:
        CREATE BUTTON hbt-depara
            ASSIGN  ROW         = hfi-descricao:ROW     - 1
                    COLUMN      = hfi-descricao:COLUMN
                    WIDTH       = 10
                    HEIGHT      = 0.88
                    LABEL       = "De-Para Item"
                    FRAME       = hfi-descricao:FRAME
                    TOOLTIP     = "*De-para dos itens harmonizados"
                    HELP        = "De-para dos itens harmonizados"
                    NAME        = "bt-depara"
                    SENSITIVE   = TRUE
                    VISIBLE     = TRUE
                    TAB-STOP    = NO
                    .
        ON "CHOOSE" OF hbt-depara PERSISTENT RUN upc\cc0301a-u04a.p (INPUT hbt-depara,INPUT p-wgh-frame).
    END.
END.

/* RUN pi-msg.  */

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

procedure pi-msg:
message "p-ind-event..:" p-ind-event                  skip 
        "p-ind-object.:" p-ind-object                 skip 
        "p-cod-table..:" STRING(p-cod-table)          skip 
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    skip 
        "p-wgh-frame..:" STRING(p-wgh-frame)          skip 
        "p-row-table..:" string(p-row-table)          skip 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END PROCEDURE.
