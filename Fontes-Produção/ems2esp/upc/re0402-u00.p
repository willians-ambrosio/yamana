/*-----------------------------------------------------------------------------------
    PROGRAMA  : re0402-u00.p
    OBJETIVO  : Chamanda de UPC do re0402
    AUTOR     : Thiago Coutinho (CSX)
    DATA      : March/2012
    EMPRESA   : CSX Solution
    DESCRICAO : Inclusao da chamada de UPC re0402-u01.        
    
-----------------------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i re0402-u00.p 2.06.00.000} 

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
/* ---> Defini se a Ser X Est ira imprimir NF-e      <--- */ 
RUN upc/re0402-u01.p  (INPUT p-ind-event,
                       INPUT p-ind-object,
                       INPUT p-wgh-object,
                       INPUT p-wgh-frame,
                       INPUT p-cod-table,
                       INPUT p-row-table).


IF RETURN-VALUE = "NOK":U THEN
  RETURN "NOK":U.
