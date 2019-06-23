/***************************************************************************************
** Programa: mi0307a-upc.p
** Objetivo:
** Autor...: Gustavo Eduardo Tamanini
** Data....: 04/2008
****************************************************************************************/
{include/i-prgvrs.i mi0307a-upc 2.00.00.000}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTE       NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID          NO-UNDO.

/********************************* HANDLE **************************************/
define variable h-fPage1     as handle no-undo.
define variable h-prioridade as handle no-undo.
/********************************* WIDGET-HANDLE **************************************/
define new global shared variable wh-tg-area-parada-mi0307  as widget-handle  no-undo.
define new global shared variable wh-fi-nr-ord-produ-mi0307 as widget-handle  no-undo.
/**************************************************************************************/
define new global shared variable lMarca-mi0307 as logical no-undo.

RUN findWidget (INPUT "fPage1",     INPUT "FRAME",   INPUT p-wgh-frame, OUTPUT h-fPage1).
RUN findWidget (INPUT "prioridade", INPUT "FILL-IN", INPUT h-fPage1,    OUTPUT h-prioridade).

/** Ao iniciar a tela, eventos para campos espec¡ficos **/ 
if p-ind-event = "AFTER-INITIALIZE":U then do: 
    /** FO 1708.134 **/
    create toggle-box wh-tg-area-parada-mi0307
    assign frame     = h-fPage1
           format    = ":@20)"
           width     = 11.57
           height    = 0.83
           row       = 10.45
           col       = 56
           label     = "·rea Parada"
           visible   = yes
           sensitive = yes
           font      = 1.

    /** lMarca-mi0307 = 
    YES - Traz conforme tabela especifica,
    NO  - Traz desmarcado pois evento e ADD ou COPY. **/
    if lMarca-mi0307 then
        run piAreaParada in this-procedure.
    else
        assign wh-tg-area-parada-mi0307:checked = no.

END. /* AFTER-INITIALIZE */ 

if p-ind-event = "BEFORE-INITIALIZE":U then do:
    /** Default **/
    assign lMarca-mi0307 = yes.
end.

/** Se acao = ADD/COPY deve trazer o toggle-box desmarcado **/
if  p-ind-event = "AFTER-ADD":U  or
    p-ind-event = "AFTER-COPY":U then do:
    assign lMarca-mi0307 = no.
end.

RUN upc/upcmi0307a.p( p-ind-event ,
                      p-ind-object,
                      p-wgh-object,
                      p-wgh-frame ,
                      p-cod-table ,
                      p-row-table ).
                      
/** Atualiza toggle-box conforme informacoes salva em tabela especifica **/
procedure piAreaParada:
    if  valid-handle(wh-tg-area-parada-mi0307)  and
        valid-handle(wh-fi-nr-ord-produ-mi0307) then do:

        for first ext-ord-manut
            where ext-ord-manut.nr-ord-produ = int(wh-fi-nr-ord-produ-mi0307:screen-value) no-lock:
        end.
        if avail ext-ord-manut then
            assign wh-tg-area-parada-mi0307:checked = ext-ord-manut.parada.
        else
            assign wh-tg-area-parada-mi0307:checked = no.
    end.
end procedure.

PROCEDURE findWidget:
    
    DEFINE INPUT  PARAMETER c-widget-name  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER c-widget-type  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER h-start-widget AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER h-widget       AS HANDLE NO-UNDO.
    
    DO WHILE VALID-HANDLE(h-start-widget):
        if h-start-widget:NAME = c-widget-name AND
           h-start-widget:TYPE = c-widget-type THEN DO:
            ASSIGN h-widget = h-start-widget:HANDLE.
            LEAVE.
        END.
    
        IF h-start-widget:TYPE = "field-group":u OR
           h-start-widget:TYPE = "frame":u OR
           h-start-widget:TYPE = "dialog-box":u THEN DO:
            RUN findWidget (INPUT  c-widget-name,
                            INPUT  c-widget-type,
                            INPUT  h-start-widget:FIRST-CHILD,
                            OUTPUT h-widget).
    
            IF VALID-HANDLE(h-widget) THEN
                LEAVE.
        END.
        ASSIGN h-start-widget = h-start-widget:NEXT-SIBLING.
    END.
END PROCEDURE.

return "OK":U.
