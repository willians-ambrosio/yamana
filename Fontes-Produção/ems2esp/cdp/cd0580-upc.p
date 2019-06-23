/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: CD0580-UPC
** UPC cadastrada para programa: CD0580
** Objetivo: FO 1708.134 - Inserir um novo toggle-box 
                           "Corretiva Planejada".
******************************************************************/
{include/i-prgvrs.i CD0580-UPC 2.00.00.000}

/** Parƒmetros **/
define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.
/** Handle **/
define variable h-RECT-5            as handle        no-undo.
define variable h-fi-urgencia       as handle        no-undo.
define variable h-urgencia          as handle        no-undo.
define variable h-RECT-7            as handle        no-undo.
define variable h-fi-forma-ajuste   as handle        no-undo.
define variable h-forma-ajuste      as handle        no-undo.
define variable h-gera-ordem        as handle        no-undo.
define variable l-sfc-mudanca       as log init no   no-undo.
define variable h-cd-tipo           as handle        no-undo.
DEFINE VARIABLE h-dest-manut        AS HANDLE        NO-UNDO.
/** Global **/
define new global shared var wh-tg-corre-plan-cd0580 as widget-handle no-undo.
define new global shared var wh-tg-om-invest-cd0580  as widget-handle no-undo.

run findWidget (input "cd-tipo",         input "FILL-IN",   input p-wgh-frame, output h-cd-tipo).

run findWidget (input "RECT-5",          input "RECTANGLE", input p-wgh-frame, output h-RECT-5).
run findWidget (input "fi-urgencia",     input "TEXT",      input p-wgh-frame, output h-fi-urgencia).
run findWidget (input "urgencia",        input "RADIO-SET", input p-wgh-frame, output h-urgencia).

run findWidget (input "RECT-7",          input "RECTANGLE",  input p-wgh-frame, output h-RECT-7).
run findWidget (input "fi-forma-ajuste", input "TEXT",       input p-wgh-frame, output h-fi-forma-ajuste).
run findWidget (input "forma-ajuste",    input "RADIO-SET",  input p-wgh-frame, output h-forma-ajuste).
run findWidget (input "gera-ordem",      input "TOGGLE-BOX", input p-wgh-frame, output h-gera-ordem).

run findWidget (input "dest-manut",      input "RADIO-SET",  input p-wgh-frame, output h-dest-manut).

if  p-ind-event  = "AFTER-INITIALIZE":U and
    p-ind-object = "CONTAINER":U        then do:

    run sfc/sf9999.p (output l-sfc-mudanca).

    if l-sfc-mudanca then do:        
        if valid-handle(h-RECT-5) then
            assign h-RECT-5:row = 9.58.
    
        if valid-handle(h-fi-urgencia) then
            assign h-fi-urgencia:row = 9.29.
        
        if valid-handle(h-urgencia) then
            assign h-urgencia:row = 10.00.
    
        if valid-handle(h-RECT-7) then
            assign h-RECT-7:row = 11.54.
    
        if valid-handle(h-fi-urgencia) then
            assign h-fi-forma-ajuste:row = 11.21.
        
        if valid-handle(h-urgencia) then
            assign h-forma-ajuste:row = 12.00.
    end.

    create toggle-box wh-tg-corre-plan-cd0580
    assign frame     = p-wgh-frame
           format    = ":@20)"
           width     = 17.57
           height    = 0.83
           row       = if not l-sfc-mudanca then 7.92 else 8.71
           col       = 57.43
           label     = "Corretiva Planejada"
           visible   = yes
           sensitive = no
           font      = 1.

    IF VALID-HANDLE(h-dest-manut) THEN
        ASSIGN h-dest-manut:WIDTH = 28.

    create toggle-box wh-tg-om-invest-cd0580
    assign frame     = p-wgh-frame
           format    = ":@20)"
           width     = 27.5
           height    = 0.83
           row       = 15.25
           col       = 50
           label     = "Obrigado Informar O.M. Investimento"
           visible   = yes
           sensitive = no
           font      = 1.

    run piAtualizaCampos in this-procedure.
end.

if  p-ind-event  = "AFTER-ENABLE":U  or
    p-ind-event  = "AFTER-DISPLAY":U or 
    p-ind-event  = "AFTER-DISABLE":U then do:

    IF VALID-HANDLE(h-gera-ordem) THEN DO:
        IF VALID-HANDLE(wh-tg-corre-plan-cd0580) THEN
            ASSIGN wh-tg-corre-plan-cd0580:SENSITIVE = h-gera-ordem:SENSITIVE.
        
        IF VALID-HANDLE(wh-tg-corre-plan-cd0580) THEN
            ASSIGN wh-tg-om-invest-cd0580:SENSITIVE = h-gera-ordem:SENSITIVE.
    END.
end.

if p-ind-event = "AFTER-DISPLAY":U then do:
    run piAtualizaCampos in this-procedure.
end.

procedure piAtualizaCampos:
    if  valid-handle(h-cd-tipo) and
        valid-handle(wh-tg-corre-plan-cd0580) then do:
        for first mmv-tipo-manut
            where mmv-tipo-manut.tipo-manut = int(h-cd-tipo:screen-value) no-lock:            
        end.
        if avail mmv-tipo-manut then
            assign wh-tg-corre-plan-cd0580:checked = mmv-tipo-manut.corret-plan
                   wh-tg-om-invest-cd0580:CHECKED  = mmv-tipo-manut.log-livre-1.
        else
            assign wh-tg-corre-plan-cd0580:checked = NO
                   wh-tg-om-invest-cd0580:CHECKED  = NO.
    end.
end procedure.

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
