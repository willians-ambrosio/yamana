/* =========================================================================== 
   PROGRAMA    : AB0115A-U00.p
   DATA        : ABRIL/2012
   DESENVOLVIDO: Thiago Coutinho - CSX
   VERSAO      : 000
   OBJETIVO    : UPC para chamada abp/ab0115a-u01.p. 
   =========================================================================== */
{include/i-prgvrs.i  ab0115a-u00.P 2.06.00.000}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

RUN abp/ab0115a-u01.p(INPUT p-ind-event,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame,
                      INPUT p-cod-table,
                      INPUT p-row-table). 
                     
IF RETURN-VALUE = "NOK" THEN 
  RETURN "NOK":U.
                                                
RETURN RETURN-VALUE.
