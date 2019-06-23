/*****************************************************************************************
**
** Programa: upc-re2005rp-u00
**
** Objetivo: Programa chamador de epc
**
** Autor   : Renato Oliveira
**
** Data    : Dezembro/2018
** 
** Versao: 2.12.00.000 - Desenvolvimento Inicial
**
****************************************************************************************/
{include/i-prgvrs.i upc-re2005rp-u00 2.12.00.000}

{include/i-epc200.i re2005rp}

DEF INPUT PARAM  p-ind-event AS  CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

   /*
  FOR EACH tt-epc.                             
    MESSAGE tt-epc.cod-event SKIP             
            tt-epc.cod-parameter SKIP         
            string(tt-epc.val-parameter) SKIP 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.      
  END.                                        
     */

/*  COnsiste processo de alocacao */
RUN upc/upc-re2005rp-u01.p (INPUT p-ind-event,
                            INPUT-OUTPUT TABLE tt-epc).

IF RETURN-VALUE = "NOK" THEN
    RETURN "NOK".

/* Fim do Programa */
