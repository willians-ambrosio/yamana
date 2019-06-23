/*-----------------------------------------------------------------------------------
    PROGRAMA : re0402rp-u00.P
    OBJETIVO : UPC de chamada para o programa re0402rp
    AUTOR    : Wellington Aparecido (DSC)
    DATA     : 04/09/2008
-----------------------------------------------------------------------------------*/
/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i re0402rp-u00 2.06.00.000}
{include/i-epc200.i re0402rp} 

/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/
DEFINE INPUT PARAM c-ponto AS  CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.


RUN upc/re0402rp-u01.p (INPUT              c-ponto,
                        INPUT-OUTPUT TABLE tt-epc).

IF RETURN-VALUE = "NOK":U THEN
  RETURN "NOK":U.  

