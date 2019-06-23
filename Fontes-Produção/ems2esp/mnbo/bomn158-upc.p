/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Programa: BOMN158-UPC
** UPC cadastrada para programa: BOMN158
*******************************************************/
{include/i-prgvrs.i BOMN158-UPC 2.00.00.000}
{include/i-epc200.i}

define temp-table tt-tipo-manut no-undo like tipo-manut
    field r-Rowid as rowid.

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

define variable h-bo   as handle              no-undo.
define variable cTexto as char format "x(50)" no-undo.
/** Variaveis do programa CD0580-UPC **/
define new global shared var wh-tg-corre-plan-cd0580 as widget-handle no-undo.
define new global shared var wh-tg-om-invest-cd0580  as widget-handle no-undo.

find first tt-epc 
     where tt-epc.cod-event     = p-ind-event
     and   tt-epc.cod-parameter = "OBJECT-HANDLE" no-lock no-error.

if avail tt-epc then do:
    assign h-bo = widget-handle(tt-epc.val-parameter).
end.

/** FO 1708.134 - Criar tabela especifica para guardar
    o Tipo Manutencao e se ela ‚ Corretiva Planejada. **/
if (p-ind-event = "afterCreateRecord":U  or
    p-ind-event = "afterUpdateRecord":U) then do:

    run getRecord in h-bo (output table tt-tipo-manut).
    
    for first tt-tipo-manut no-lock:
        if not can-find(first mmv-tipo-manut
                        where mmv-tipo-manut.tipo-manut  = tt-tipo-manut.cd-tipo no-lock) then do:
            create mmv-tipo-manut.
            assign mmv-tipo-manut.corret-plan = wh-tg-corre-plan-cd0580:checked
                   mmv-tipo-manut.tipo-manut  = tt-tipo-manut.cd-tipo
                   mmv-tipo-manut.log-livre-1 = wh-tg-om-invest-cd0580:CHECKED.
        end.
        else do:
            for first mmv-tipo-manut
                where mmv-tipo-manut.tipo-manut  = tt-tipo-manut.cd-tipo exclusive-lock:
                assign mmv-tipo-manut.corret-plan = wh-tg-corre-plan-cd0580:CHECKED
                       mmv-tipo-manut.log-livre-1 = wh-tg-om-invest-cd0580:CHECKED.
            end.
        end.
    end.
end.

if p-ind-event = "afterDeleteRecord":U then do:
    run getRecord in h-bo (output table tt-tipo-manut).
    for first tt-tipo-manut no-lock:
        for first mmv-tipo-manut
            where mmv-tipo-manut.tipo-manut = tt-tipo-manut.cd-tipo exclusive-lock:
            delete mmv-tipo-manut.
        end.
    end.
end.

procedure pi-cria-erro:
define input parameter msg as char no-undo.
    create tt-epc.
    assign tt-epc.cod-event     = "ERROR"
           tt-epc.cod-parameter = "EPC-ERROR" 
           tt-epc.val-parameter = msg.
end.

return "OK":U.
