/*********************************************************************
** Programa....: ft2200rp-upc00.P
** Descricao...: UPC para cancelamento de Nota Fiscal
** Autor.......: Wellington Ap (DSC)
** Data........: 28/07/2008
*********************************************************************/
{include/i-prgvrs.i ft2200rp-u00.P 2.06.00.000}
{include/i-epc200.i ft2200rp} 
  

DEF INPUT PARAM  p-ind-event AS  CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

  
RUN upc/ft2200rp-upc01.p (INPUT p-ind-event,
                          INPUT-OUTPUT TABLE tt-epc). 

IF RETURN-VALUE = "NOK" THEN 
  RETURN "NOK".
ELSE
  RETURN "OK".


