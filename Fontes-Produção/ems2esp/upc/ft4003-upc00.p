/*-----------------------------------------------------------------------------------
    PROGRAMA : FT4003-upc00.p
    OBJETIVO : Chamanda de UPC do ft4003
    AUTOR    : Wellington Ap - (DSC)
    DATA     : 25/08/2008
-----------------------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i ft4003-upc00.p 2.06.00.000} 

/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/*************************************************************************************
                                    MAIN-BLOCK
*************************************************************************************/
/* ---> Valida‡Æo de Serie x NF-e  <--- */
RUN   upc/ft4003-upc01.p  (INPUT p-ind-event,
                           INPUT p-ind-object,
                           INPUT p-wgh-object,
                           INPUT p-wgh-frame,
                           INPUT p-cod-table,
                           INPUT p-row-table).


IF RETURN-VALUE = "NOK":U THEN
  RETURN "NOK":U.



