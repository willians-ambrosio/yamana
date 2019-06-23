/******************************************************************
** Programa: esfgl710ra-u00
** Objetivo: Upc para o Programa fgl710ra
**     Data: Outubro/2007
******************************************************************/

def input Param p-ind-event  as char          no-undo.
def input Param p-ind-object as char          no-undo.
def input Param p-wgh-object as handle        no-undo.
def input Param p-wgh-frame  as widget-handle no-undo.
def input Param p-cod-table  as char          no-undo.
def input Param p-rec-table  as recid         no-undo. 

run upc/esfgl710ra-u01.p(input p-ind-event, 
                         input p-ind-object,
                         input p-wgh-object,
                         input p-wgh-frame, 
                         input p-cod-table, 
                         input p-rec-table).
