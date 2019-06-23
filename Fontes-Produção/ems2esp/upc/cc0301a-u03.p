/**============================================================**
** Altera‡Æo...: Thiago Coutinho
** Empresa.....: CSX Solution
** Data........: Maio/2012
** Objetivo....: Permitir a altera‡Æo do Comprador quando a ordem
** ............: ja esta finalizada ou terminada. 
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

define new global shared var wh-cc0301a-c-it-codigo       as widget-handle no-undo.
/* define new global shared var wh-cc0301a-i-seq-ordem    as widget-handle no-undo. */
/* define new global shared var wh-cc0301a-cod-comprado   as widget-handle no-undo. */
/* define new global shared var wh-cc0301a-ordem-servic   as widget-handle no-undo. */
/* DEFINE NEW GLOBAL SHARED VAR wh-cc0301a-bt-ok-ant      AS WIDGET-HANDLE NO-UNDO. */
/* DEFINE NEW GLOBAL SHARED VAR wh-cc0301a-bt-ok-new      AS WIDGET-HANDLE NO-UNDO. */


RUN findWidget (INPUT "it-codigo",      INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT wh-cc0301a-c-it-codigo).
/* RUN findWidget (INPUT "i-seq-ordem",    INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT wh-cc0301a-i-seq-ordem ).  */
/* RUN findWidget (INPUT "cod-comprado",   INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT wh-cc0301a-cod-comprado).  */
/* RUN findWidget (INPUT "ordem-servic",   INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT wh-cc0301a-ordem-servic).  */
/* RUN findWidget (INPUT "bt-ok",          INPUT "BUTTON",     INPUT p-wgh-frame, OUTPUT wh-cc0301a-bt-ok-ant   ).  */
                                                                                      




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
