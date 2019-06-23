/***************************************************************************************************
**    Programa: upc-cd4035-u00.p
**    Objetivo: Chamador de UPC espec¡fica
**       Autor: Willians Moreira Ambrosio - Grupo DKP
***************************************************************************************************/

{include/i-prgvrs.i upc-cd4035-U00 12.01.19.001} 

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.

run upc/upc-cd4035-u01.p (input p-ind-event,
                          input p-ind-object,
                          input p-wgh-object,
                          input p-wgh-frame,
                          input p-cod-table,
                          input p-row-table).
