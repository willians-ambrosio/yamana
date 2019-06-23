/***************************************************************************************
** Programa: mi0307-upc.p
** Objetivo:
** Autor...: Leonardo Correia Santos Oliveira - Datasul Manufatura
             Gustavo Eduardo Tamanini
** Data....: 04/2008
****************************************************************************************/
{include/i-prgvrs.i mi0307-upc 2.00.00.000}

DEFINE INPUT PARAMETER p-ind-event                          AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object                         AS CHARACTE       NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object                         AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame                          AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table                          AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table                          AS ROWID          NO-UNDO.

/********************************* WIDGET-HANDLE **************************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-button                 AS WIDGET-HANDLE  NO-UNDO.
/** wh-fi-nr-ord-produ-mi0307 - Variavel utilizada na UPC da tela de cadastro da tarefa,
                                favor N«O alterar o nome da mesma. **/
DEFINE NEW GLOBAL SHARED VARIABLE wh-fi-nr-ord-produ-mi0307 AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-brOrdTaref-mi0307      AS WIDGET-HANDLE  NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-fPage1                 AS WIDGET-HANDLE  NO-UNDO.
/** FO 1708.134 **/
define new global shared variable wh-tg-area-parada-mi0307  as widget-handle  no-undo.
/******************************************************************************************/

RUN findWidget (INPUT "nr-ord-produ", INPUT "FILL-IN", INPUT p-wgh-frame, OUTPUT wh-fi-nr-ord-produ-mi0307).
RUN findWidget (INPUT "fPage0",       INPUT "FRAME",   INPUT p-wgh-frame, OUTPUT wh-fPage1).
RUN findWidget (INPUT "brOrdTaref",   INPUT "BROWSE",  INPUT p-wgh-frame, OUTPUT wh-brOrdTaref-mi0307).
RUN findWidget (INPUT "wh-tg-area-parada-mi0307", INPUT "TOGGLE-BOX", INPUT p-wgh-frame, OUTPUT wh-tg-area-parada-mi0307).

/** Ao iniciar a tela, eventos para campos espec°ficos **/ 
if p-ind-event = "AFTER-INITIALIZE":U then do: 

    /*----- Botao Tecnico -----*/
    create button wh-button
    assign frame     = wh-fPage1
           label     = "&TÇcnico":U
           width     = 10.00
           height    = 1.00
           row       = 4.6
           col       = 84.6
           font      = 1
           visible   = yes
           sensitive = yes
        triggers:   
            on choose persistent run mip/mi0307-upca.p (input wh-fi-nr-ord-produ-mi0307,
                                                        input wh-brOrdTaref-mi0307).
        end triggers. 

    run piValidate in this-procedure.

    /** FO 1708.134 **/
    create toggle-box wh-tg-area-parada-mi0307
    assign name      = "wh-tg-area-parada-mi0307":U
           frame     = p-wgh-frame
           format    = ":@20)"
           width     = 17.57
           height    = 0.83
           row       = 3.75
           col       = 53
           label     = "∑rea Parada"
           visible   = yes
           sensitive = no
           font      = 1.
    /***/
    run piAreaParada in this-procedure.

END. /* AFTER-INITIALIZE */ 

if  p-ind-event = "AFTER-DISPLAY":U or
    p-ind-event = "AFTER-DELETE":U  then do: 
    run piValidate in this-procedure.
    run piAreaParada in this-procedure.
END.

RUN upc/upcmi0307.p( p-ind-event ,
                     p-ind-object,
                     p-wgh-object,
                     p-wgh-frame ,
                     p-cod-table ,
                     p-row-table ).

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

procedure piValidate:
    /** Bot∆o fica habilitado apenas se houver registros no browse de tarefa **/
    if valid-handle(wh-brOrdTaref-mi0307) then do:
        if valid-handle(wh-button) then do:
            if wh-brOrdTaref-mi0307:num-selected-rows = 0 then
                assign wh-button:sensitive = no.
            else
                assign wh-button:sensitive = yes.
        end.
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
