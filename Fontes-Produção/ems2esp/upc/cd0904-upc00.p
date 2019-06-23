/*-----------------------------------------------------------------------------------
    PROGRAMA : cd0904-upc00.p
    OBJETIVO : Chamanda de UPC do cd0904
    AUTOR    : Wellington Aparecido (DSC)
    DATA     : 08|08|2008
-----------------------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i cd0904-upc00.p 2.06.00.000} 

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
/* ---> Controle de Usuario (modifica‡Æo/Elimina‡Æo) <--- */
RUN   upc/cd0904-upc01.p  (INPUT p-ind-event,
                           INPUT p-ind-object,
                           INPUT p-wgh-object,
                           INPUT p-wgh-frame,
                           INPUT p-cod-table,
                           INPUT p-row-table).


IF RETURN-VALUE = "NOK":U THEN
  RETURN "NOK":U.



