/******************************************************************************
**    Programa: esap5510aa-u01.p                                             **
**    Objetivo: Filtrar valores                                              **
**       Autor: CSX Solution                                                 **
**        Data: Abril/2012                                                   **
*******************************************************************************/

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.

OUTPUT TO "D:\temp\Thiago_ap5510.txt" APPEND.
PUT UNFORMATTED "p-ind-event - " p-ind-event " \ " p-wgh-object:FILE-NAME SKIP
                "Evento............: " p-ind-event Skip
                "Objeto............: " p-ind-object Skip
                "Handel do Objeto..: " p-wgh-object:NAME Skip
                "Frame.............: " p-wgh-frame:NAME Skip
                "Nome da tabela....: " p-cod-table Skip
                "Rowid da tabela...: " STRING(p-row-table)SKIP(2).

OUTPUT CLOSE.
