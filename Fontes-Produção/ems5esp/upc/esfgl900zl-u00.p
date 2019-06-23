def temp-table tt_epc no-undo
    field cod_event     as char
    field cod_parameter as char
    field val_parameter as char
    index id is primary cod_parameter cod_event.

def input        param p-ind-event as char no-undo.
def input-output param table for tt_epc.

run upc/esfgl900zl-u01.p(input p-ind-event,
                         input-output table tt_epc).

if  return-value = "NOK" then
    return "NOK":U.
