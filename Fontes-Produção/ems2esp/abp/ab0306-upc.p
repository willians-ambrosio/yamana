/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: AB0306-UPC
** UPC cadastrada para programa: AB0306
** Objetivo: FO 1716.716
******************************************************************/
{include/i-prgvrs.i AB0306-UPC 2.06.00.000}
{abp/ab9000.i} /** procedure piFormataHorarioSeg **/

/** Parƒmetros **/
define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.

/** Handle **/
define variable h-dat-movto       as handle no-undo.
define variable h-hra-inicial     as handle no-undo.
define variable h-hra-final       as handle no-undo.
define variable h-val-hodom-horim as handle no-undo.
define variable h-ab0306-upc     as handle no-undo.
define variable h-ep-codigo       as handle no-undo.
define variable h-cod-eqpto       as handle no-undo.
/** Var s **/
define variable cHora      as character format "99:99:99"               no-undo.
define variable dInvertida like mab-movto-km-eqpto.val-dat-hora-invrtda no-undo.

run findWidget (input "dat-movto",       input "FILL-IN", input p-wgh-frame, output h-dat-movto).
run findWidget (input "hra-inicial",     input "FILL-IN", input p-wgh-frame, output h-hra-inicial).
run findWidget (input "hra-final",       input "FILL-IN", input p-wgh-frame, output h-hra-final).
run findWidget (input "val-hodom-horim", input "FILL-IN", input p-wgh-frame, output h-val-hodom-horim).
run findWidget (input "ep-codigo",       input "FILL-IN", input p-wgh-frame, output h-ep-codigo).
run findWidget (input "cod-eqpto",       input "FILL-IN", input p-wgh-frame, output h-cod-eqpto).

if  p-ind-event  = "AFTER-INITIALIZE":U and
    p-ind-object = "CONTAINER":U        then do:

    /** Executa a upc persistente, para poder executar as procedures definidas na UPC **/
    run abp/ab0306-upc.p persistent set h-ab0306-upc (input "",
                                                      input "",
                                                      input p-wgh-object,
                                                      input p-wgh-frame,
                                                      input "",
                                                      input p-row-table).
    
    /** Override na trigger LEAVE / ENTRY **/
    on leave of h-dat-movto       persistent run piSugeriContador  in h-ab0306-upc.
    on leave of h-hra-inicial     persistent run piLeaveHraInicial in h-ab0306-upc.
    on entry of h-val-hodom-horim persistent run piSugeriContador  in h-ab0306-upc.
  /*on leave of h-hra-final       persistent run piLeaveHraFinal   in h-ab0306-upc.*/
end.

/** Sugeri Contador de acordo com o historico de quilometragem do eqpto **/
procedure piSugeriContador:
    if  valid-handle(h-dat-movto)   and
        valid-handle(h-hra-inicial) then
        run converteParaHoraInvertida (input string(date(h-dat-movto:screen-value),"99/99/9999"),
                                       input substring(h-hra-inicial:screen-value,1,2) + substring(h-hra-inicial:screen-value,4,2),
                                       output dInvertida).

    if  valid-handle(h-ep-codigo)       and
        valid-handle(h-cod-eqpto)       and
        valid-handle(h-val-hodom-horim) then do:        
        for last mab-movto-km-eqpto
            where mab-movto-km-eqpto.ep-codigo             = h-ep-codigo:screen-value
            and   mab-movto-km-eqpto.cod-eqpto             = h-cod-eqpto:screen-value
            and   mab-movto-km-eqpto.val-dat-hora-invrtda <= dInvertida no-lock:
            assign h-val-hodom-horim:screen-value = string(mab-movto-km-eqpto.val-hodom-horim).
        end.
        if not avail mab-movto-km-eqpto then
            assign h-val-hodom-horim:screen-value = "0,0":U.
    end.
end procedure.

/** LEAVE do campo Hora Inicial **/
procedure piLeaveHraInicial:
    if  valid-handle(h-hra-inicial) and
        valid-handle(h-hra-final)   then do:
        /** Consiste o hor rio **/
        assign cHora = h-hra-inicial:screen-value.
        run piFormataHorarioSeg (input-output cHora).
        if return-value = "NOK":U then do:
            apply "ENTRY":U to h-hra-inicial.
            return no-apply.
        end.
        assign h-hra-inicial:screen-value  = cHora
               h-hra-final:screen-value    = cHora.
    end.
    run piSugeriContador in this-procedure.
end procedure.

/*
/** LEAVE do campo Hora Final **/
procedure piLeaveHraFinal:
    if valid-handle(h-hra-final) then do:
        /** Consiste o hor rio **/
        assign cHora = h-hra-final:screen-value.
        run piFormataHorarioSeg (input-output cHora).
        if return-value = "NOK":U then do:
            apply "ENTRY":U to h-hra-final.
            return no-apply.
        end.
        assign h-hra-final:screen-value = cHora. 
    end.
end procedure.
*/

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

