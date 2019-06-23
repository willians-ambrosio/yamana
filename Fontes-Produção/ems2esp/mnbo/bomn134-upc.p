/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Programa: BOMN134-UPC
** UPC cadastrada para programa: BOMN134
*******************************************************/
{include/i-prgvrs.i BOMN134-UPC 2.00.00.000}
{include/i-epc200.i}

define temp-table tt-ord-manut no-undo like ord-manut
    field r-Rowid as rowid.

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

define variable h-bo   as handle              no-undo.
define variable cTexto as char format "x(50)" no-undo.
/** Variaveis do programa MI0307A-UPC **/
define new global shared variable wh-tg-area-parada-mi0307  as widget-handle  no-undo.

find first tt-epc 
     where tt-epc.cod-event     = p-ind-event
     and   tt-epc.cod-parameter = "OBJECT-HANDLE" no-lock no-error.

if avail tt-epc then do:
    assign h-bo = widget-handle(tt-epc.val-parameter).
end.

/** FO 1708.134 - Cria tabela especifica **/
if (p-ind-event = "afterCreateRecord":U  or
    p-ind-event = "afterUpdateRecord":U) then do:

    run getRecord in h-bo (output table tt-ord-manut).
    
    if  valid-handle(wh-tg-area-parada-mi0307) then do:
        for first tt-ord-manut no-lock:
            if not can-find(first ext-ord-manut
                            where ext-ord-manut.nr-ord-produ = tt-ord-manut.nr-ord-produ no-lock) then do:
                create ext-ord-manut.
                assign ext-ord-manut.nr-ord-produ = tt-ord-manut.nr-ord-produ
                       ext-ord-manut.parada       = wh-tg-area-parada-mi0307:checked.
            end.
            else do:
                for first ext-ord-manut
                    where ext-ord-manut.nr-ord-produ = tt-ord-manut.nr-ord-produ exclusive-lock:
                    assign ext-ord-manut.parada = wh-tg-area-parada-mi0307:checked.
                end.
            end.
        end.
    end.
end.

/** FO 1708.134 - Deleta tabela especifica **/
if p-ind-event = "afterDeleteRecord":U then do:
    run getRecord in h-bo (output table tt-ord-manut).
    for first tt-ord-manut no-lock:
        for first ext-ord-manut
            where ext-ord-manut.nr-ord-produ = tt-ord-manut.nr-ord-produ exclusive-lock:
            delete ext-ord-manut.
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
