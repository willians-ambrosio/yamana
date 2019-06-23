/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: CD0738-UPC
** UPC cadastrada para programa: CD0580
** Objetivo: FO 1716.704 - Inserir campo de Tecnico.

******************************************************************/
{include/i-prgvrs.i CD0580-UPC 2.06.00.000}

/** Parƒmetros **/
define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.
/** Handle **/
define variable h-tx-label          as handle        no-undo.
define variable h-cd0738-upc        as handle        no-undo.
define variable h-num-tarefa        as handle        no-undo.
define variable h-nr-requisicao     as handle        no-undo.
define variable h-sequencia         as handle        no-undo.
define variable h-rsOrigem          as handle        no-undo.
/** Global **/
define new global shared var wh-fi-tecnico-0738      as widget-handle no-undo.
define new global shared var wh-fi-desc-tecnico-0738 as widget-handle no-undo.

run findWidget (input "num-tarefa",    input "FILL-IN",   input p-wgh-frame, output h-num-tarefa).
run findWidget (input "nr-requisicao", input "FILL-IN",   input p-wgh-frame, output h-nr-requisicao).
run findWidget (input "sequencia",     input "FILL-IN",   input p-wgh-frame, output h-sequencia).
run findWidget (input "rsOrigem",      input "RADIO-SET", input p-wgh-frame, output h-rsOrigem).

if  p-ind-event  = "BEFORE-INITIALIZE":U and
    p-ind-object = "CONTAINER":U        then do:

    run cdp/cd0738-upc.p persistent set h-cd0738-upc (input "",
                                                      input "",
                                                      input p-wgh-object,
                                                      input p-wgh-frame,
                                                      input "",
                                                      input p-row-table).
    /** Label Tecnico **/
    create text h-tx-label
    assign frame        = p-wgh-frame
           format       = "x(08)"
           width        = 8
           screen-value = "T‚cnico:":U
           row          = 16.17
           col          = 43.10
           visible      = yes
           font         = 1.

    /** Create Fill-in Tecnico **/
    create fill-in wh-fi-tecnico-0738
    assign frame     = p-wgh-frame
           name      = "wh-fi-tecnico-0738"
           format    = "x(07)"
           width     = 11.00
           height    = 0.88
           row       = 16.17
           col       = 49.57
           visible   = yes
           sensitive = no
           font      = 1
           triggers:
                on "MOUSE-SELECT-DBLCLICK":U  persistent run piZoom in h-cd0738-upc.
                on "F5":U persistent run piZoom in h-cd0738-upc.
                on "LEAVE":U persistent run piDescricao in h-cd0738-upc.
           end.
           wh-fi-tecnico-0738:load-mouse-pointer("image~\lupa.cur":U).

    /** Create Fill-in Descricao Tecnico **/
    create fill-in wh-fi-desc-tecnico-0738
    assign frame     = p-wgh-frame
          name      = "wh-fi-desc-tecnico-0738"
          format    = "x(50)"
          width     = 29.00
          height    = 0.88
          row       = 16.17
          col       = 60.72
          visible   = yes
          sensitive = no
          font      = 1
          triggers:

          end.

     /** Atualiza Tecnico em Tela **/
     run piTecnico in this-procedure.
end.

if  p-ind-event  = "AFTER-ENABLE":U  or
    p-ind-event  = "AFTER-DISPLAY":U or 
    p-ind-event  = "AFTER-DISABLE":U then do:
    if  valid-handle(wh-fi-tecnico-0738) and
        valid-handle(h-num-tarefa)       then do:
        assign wh-fi-tecnico-0738:sensitive = h-num-tarefa:sensitive.               
    end.
end.

if  p-ind-event  = "AFTER-DISPLAY":U then do:
    /** Atualiza Tecnico em Tela **/
    run piTecnico in this-procedure. 
end.

/** Chamada Zoom do Fornecedor **/
procedure piZoom:
    DEFINE VARIABLE hProgramZoom AS HANDLE NO-UNDO.

    if  valid-handle(h-rsOrigem) then do:
        if h-rsOrigem:screen-value = "1":U then
            run cdp/cd0738-upca.p.
        else do:
            {method/zoomfields.i 
                    &ProgramZoom="frzoom/z01fr032.w"
                    &FieldZoom1="cod-matr"
                    &FieldHandle1="wh-fi-tecnico-0738"
                    &Frame1="fPage0"
                    &EnableImplant="YES"}
        end.
    end.
end procedure.

/** Atualiza Tecnico em Tela **/
procedure piTecnico:
    if  valid-handle(wh-fi-tecnico-0738) and
        valid-handle(h-nr-requisicao)    and
        valid-handle(h-sequencia)        then do:

        for first mmi-tec-req-prod 
            where mmi-tec-req-prod.nr-requisicao = int(h-nr-requisicao:screen-value)
            and   mmi-tec-req-prod.sequencia     = int(h-sequencia:screen-value) no-lock:
        end.
        if avail mmi-tec-req-prod then
            assign wh-fi-tecnico-0738:screen-value = mmi-tec-req-prod.cd-tecnico.
        else
            assign wh-fi-tecnico-0738:screen-value = "":U.

        apply "LEAVE":U to wh-fi-tecnico-0738.
    end.
end procedure.

/** Exibi Nome do Tecnico **/
procedure piDescricao:
    if  valid-handle(wh-fi-tecnico-0738) and
        valid-handle(wh-fi-desc-tecnico-0738) then do:
        if wh-fi-tecnico-0738:screen-value <> "" then do:
            if h-rsOrigem:screen-value = "1":U then do:            
                for first tecn-mi 
                    where tecn-mi.cd-tecnico = wh-fi-tecnico-0738:screen-value no-lock:
                end.
                if avail tecn-mi then
                    assign wh-fi-desc-tecnico-0738:screen-value = tecn-mi.nome-compl.
                else
                    assign wh-fi-desc-tecnico-0738:screen-value = "":U.
            end.
            else do:
                for first mmv-func-ofici 
                    where mmv-func-ofici.cod-matr = wh-fi-tecnico-0738:screen-value no-lock:
                end.
                if avail mmv-func-ofici then
                    assign wh-fi-desc-tecnico-0738:screen-value = mmv-func-ofici.nom-func.
                else
                    assign wh-fi-desc-tecnico-0738:screen-value = "":U.
            end.
        end.
        else
            assign wh-fi-desc-tecnico-0738:screen-value = "":U.
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
