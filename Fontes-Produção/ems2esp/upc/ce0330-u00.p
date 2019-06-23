/* =========================================================================== 
   PROGRAMA    : CE0330-U00.p
   DATA        : 14/03/2011
   DESENVOLVIDO: Daniel Pereira de Lima - ZBR7
   VERSAO      : 000
   OBJETIVO    : ITEM X ESTABELECIMENTO (EXTRATOR/IMPORTATDOR XTIVITY) 
   =========================================================================== */
{include/i-prgvrs.i  ce0330-u00.P 2.06.00.000}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

RUN upc/esce0330.r(INPUT p-ind-event,
                     INPUT p-ind-object,
                     INPUT p-wgh-object,
                     INPUT p-wgh-frame,
                     INPUT p-cod-table,
                     INPUT p-row-table). 

/** Programa desenvolvido pela TOTVS - **/

RUN upc/ce0330-upc.r(INPUT p-ind-event,
                     INPUT p-ind-object,
                     INPUT p-wgh-object,
                     INPUT p-wgh-frame,
                     INPUT p-cod-table,
                     INPUT p-row-table). 

/** Programa desenvolvido pela TOTVS - 10/2010 **/
                     
IF RETURN-VALUE = "NOK" THEN 
  RETURN "NOK":U.
                                                
RETURN RETURN-VALUE.
