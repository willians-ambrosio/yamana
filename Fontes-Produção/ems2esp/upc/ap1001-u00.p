
{include/i-prgvrs.i ap1001-u00 2.06.00.000}

/********************************************************************
     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS
*********************************************************************/

def input parameter p-ind-event  AS CHARACTER      NO-UNDO.
def input parameter p-ind-object AS CHARACTER      NO-UNDO.
def input parameter p-wgh-object AS HANDLE         NO-UNDO.
def input parameter p-wgh-frame  AS WIDGET-HANDLE  NO-UNDO.
def input parameter p-cod-table  AS CHARACTER      NO-UNDO.
def input parameter p-row-table  AS ROWID          NO-UNDO.

RUN upc/ap1001-u01.p (INPUT p-ind-event,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame,
                      INPUT p-cod-table,
                      INPUT p-row-table).

IF RETURN-VALUE = "NOK" THEN 
   RETURN "NOK":U.

