/*----------------------------------------------------------
    PROGRAMA : upc-utb028aa-u00.p - bas_mapa_distrib_ccusto
    OBJETIVO : Bot∆o para copiar capa do Mapa de Distribuiá∆o de C.C.
    AUTOR    : Bruno Bertulli
    DATA     : 26/11/2012
------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-rec-table  AS RECID         NO-UNDO.

RUN upc\upc-utb028aa-u01.p (INPUT p-ind-event,
                            INPUT p-ind-object,
                            INPUT p-wgh-object,
                            INPUT p-wgh-frame,
                            INPUT p-cod-table,
                            INPUT p-rec-table).

IF RETURN-VALUE = "NOK":U THEN
  RETURN "NOK":U.
ELSE 
  RETURN "OK":U. 

/* FIM DO PROGRAMA */ 


