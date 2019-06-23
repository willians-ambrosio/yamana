/* =========================================================================== 
   PROGRAMA    : CD0140-U00.p
   DATA        : 24/05/2010
   DESENVOLVIDO: Marcio Sacramoni - Kraft
   VERSAO      : 000
   OBJETIVO    : UPC no programa Itens x Materiais x estabelecimento. 
   =========================================================================== */
{include/i-prgvrs.i  cd0140-u00.P 2.06.00.000}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

RUN upc/cd0140-u01.p(INPUT p-ind-event,
                     INPUT p-ind-object,
                     INPUT p-wgh-object,
                     INPUT p-wgh-frame,
                     INPUT p-cod-table,
                     INPUT p-row-table). 
                     
IF RETURN-VALUE = "NOK" THEN 
  RETURN "NOK":U.
                                                
RETURN RETURN-VALUE.


