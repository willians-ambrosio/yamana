/******************************************************************************
**
**  Programa: upc/ucc0300a-u00.p
**  Data....: Junho - 2013
**  Autor...: Fernando Campos - DSC
**  Objetivo: UPC para cc0300a
**
******************************************************************************/

{include/i-prgvrs.i ucc0300a-u00 2.06.00.001}

/* --- Definicoes --- */

def input parameter p-ind-event     as char           no-undo.
def input parameter p-ind-object    as char           no-undo.
def input parameter p-wgh-object    as handle         no-undo.
def input parameter p-wgh-frame     as widget-handle  no-undo.
def input parameter p-cod-table     as char           no-undo.
def input parameter p-row-table     as rowid          no-undo.
 
/* --- Main Block --- */

RUN upc/cc0300a-upc00.r(input p-ind-event,
                        input p-ind-object,
                        input p-wgh-object,
                        input p-wgh-frame,
                        input p-cod-table,
                        input p-row-table).

if return-value = "NOK" then return "NOK".

run upc/ucc0300a-u01.p(input p-ind-event,
                       input p-ind-object,
                       input p-wgh-object,
                       input p-wgh-frame,
                       input p-cod-table,
                       input p-row-table).

if return-value = "NOK" then return "NOK".

