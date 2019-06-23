/* ================================================
=== Programa.....: upc/esfgl900zo-u00.p
=== Prog.Cadastro: api_lote_ctbl_recebto_2 (prgfin/fgl/fgl900zo.py)
=== Autor........: Bruno Bertulli (DSC)
=== Data.........: 28/05/2013
=== Descri‡Æo....: Alterar cen rio do lote
=================================================*/    
    
def temp-table tt_epc no-undo
    field cod_event     as char
    field cod_parameter as char
    field val_parameter as char
    index id is primary cod_parameter cod_event.

def input        param p-ind-event as char no-undo.
def input-output param table for tt_epc.

run upc/esfgl900zo-u01.p(input p-ind-event,
                         input-output table tt_epc).

if  return-value = "NOK" then
    return "NOK":U.
