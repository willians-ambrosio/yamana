/*****************************************************************************
 ** PROGRAMA..: cd0620-upc00.p
 ** OBJETIVO..: UPC Recebimento - cd0620
 ** AUTOR.....: DKP 
 ** CLIENTE...: YAMANA
 ** VERSAO....: 2.00.00.001   
 ** ALTERA€åES:
 ******************************************************************************/

def input param p-ind-event  AS CHAR          no-undo.
def input param p-ind-object AS CHAR          no-undo.
def input param p-wgh-object AS HANDLE        no-undo.
def input param p-wgh-frame  AS WIDGET-HANDLE no-undo.
def input param p-cod-table  AS CHAR          no-undo.
def input param p-row-table  AS ROWID         no-undo.

/* OUTPUT TO D:\temp\dkp\cd0620.txt NO-CONVERT APPEND.                                    */
/*     PUT UNFORMATTED 'p-ind-event               = "' p-ind-event               '"' SKIP */
/*                     'p-ind-object              = "' p-ind-object              '"' SKIP */
/*                     'p-wgh-object:PRIVATE-DATA = "' p-wgh-object:PRIVATE-DATA '"' SKIP */
/*                     'p-wgh-frame:NAME          = "' p-wgh-frame:NAME          '"' SKIP */
/*                     'p-cod-table               = "' p-cod-table               '"' SKIP */
/*                     'p-row-table               = "' STRING(p-row-table)       '"' SKIP */
/*                     '-------------------------'                               SKIP.    */
/* OUTPUT CLOSE.                                                                          */

RUN upc/cd0620-upc01.p
    (input p-ind-event ,
     input p-ind-object,
     input p-wgh-object,
     input p-wgh-frame ,
     input p-cod-table ,
     input p-row-table).

IF RETURN-VALUE = "NOK":U THEN
RETURN "NOK":U.
