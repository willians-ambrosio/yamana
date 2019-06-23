/******************************************************************
** Programa: esutb028aa-u00
** Objetivo: Upc para o Programa prgint/utb/utb028aa.py
**     Data: Fevereiro/2008
******************************************************************/
def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-rec-table  as recid         no-undo. 

run upc/esutb028aa-u01.p(input p-ind-event, 
                         input p-ind-object,
                         input p-wgh-object,
                         input p-wgh-frame, 
                         input p-cod-table, 
                         input p-rec-table).

