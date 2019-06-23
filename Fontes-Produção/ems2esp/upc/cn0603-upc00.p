/**============================================================**
** Altera‡Æo...:  
** Empresa.....:  
** Data........: 
** Objetivo....:  
** ............:  
**=============================================================**/
{include/i-prgvrs.i AB0115A-U01 2.06.00.000}

/** Parƒmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.


/** Global **/
DEFINE NEW GLOBAL SHARED VAR gl-cc0301a-c-cod-comprado  AS CHAR NO-UNDO.
define new global shared VAR c-seg-usuario              as char format 'x(12)' no-undo.

define new global shared var wh-cc0301a-i-ordem        as widget-handle no-undo.
define new global shared VAR wh-cn0603-bt-hist-msg     AS WIDGET-HANDLE NO-UNDO.
define new global shared VAR wh-cn0603-bt-narrativa    AS WIDGET-HANDLE NO-UNDO.
define new global shared VAR wh-cn0603-f-nr-contrato   AS WIDGET-HANDLE NO-UNDO.
define new global shared VAR c-nr-contrato-01             as char          no-undo.



RUN findWidget (INPUT "bt-narrativa",INPUT "BUTTON",INPUT p-wgh-frame, OUTPUT wh-cn0603-bt-narrativa).
RUN findWidget (INPUT "nr-contrato", INPUT "FILL-IN",INPUT p-wgh-frame,OUTPUT wh-cn0603-f-nr-contrato).



/*  RUN pi-msg. */

IF  p-ind-event  = "BEFORE-INITIALIZE":U  THEN DO:
 
    ASSIGN c-nr-contrato-01 = "".

    IF VALID-HANDLE(wh-cn0603-bt-narrativa) THEN DO:
        CREATE BUTTON wh-cn0603-bt-hist-msg 
        ASSIGN FRAME     = wh-cn0603-bt-narrativa:FRAME
               WIDTH     = wh-cn0603-bt-narrativa:WIDTH
               HEIGHT    = wh-cn0603-bt-narrativa:HEIGHT
               LABEL     = "Hist.Beneficios" 
               COLUMN    = wh-cn0603-bt-narrativa:COLUMN + 14
               ROW       = wh-cn0603-bt-narrativa:ROW
               SENSITIVE = YES
               VISIBLE   = YES
               TRIGGERS:
                    ON CHOOSE PERSISTENT RUN esp/ymof0106.w.
               END TRIGGERS.

    END.

END.

IF  p-ind-event  = "DISPLAY":U AND p-ind-object = "VIEWER" THEN DO:
   FIND CONTRATO-FOR WHERE ROWID(CONTRATO-FOR) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL CONTRATO-FOR THEN DO:
      ASSIGN c-nr-contrato-01 = STRING(contrato-for.nr-contrato).
   END.
END.














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
