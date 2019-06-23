/*********************************************************************
** Programa....: ft2200rp-u01.P
** Descricao...: UPC para cancelamento de Nota Fiscal
** Autor.......: Wellington Ap (DSC)
** Data........: 28/07/2008
*********************************************************************/
{include/i-prgvrs.i ft2200rp-u01.P 2.06.00.000}
{include/i-epc200.i ft2200rp} 

DEF INPUT PARAM  p-ind-event AS  CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/*
FOR EACH tt-epc.                       

  MESSAGE 
  "p-ind-event             : "  p-ind-event             SKIP
  "tt-epc.cod-event        : "  tt-epc.cod-event        SKIP        
  "tt-epc.cod-parameter    : "  tt-epc.cod-parameter    SKIP    
  "string(tt-epc.val-parameter): "  string(tt-epc.val-parameter)
    VIEW-AS ALERT-BOX INFO BUTTONS OK. 
 END.   */

IF p-ind-event = "ft2200rp" THEN DO:

  FIND FIRST tt-epc NO-LOCK
    WHERE tt-epc.cod-event   = "inicio-ft2200rp"
    AND tt-epc.cod-parameter = "nota-fiscal rowid" NO-ERROR.
  IF AVAIL tt-epc THEN DO:

    FIND FIRST nota-fiscal NO-LOCK 
      WHERE ROWID(nota-fiscal) = TO-ROWID(tt-epc.val-parameter)
      AND nota-fiscal.dt-cancel = ? NO-ERROR.
    IF AVAIL nota-fiscal THEN DO:                 
      FIND FIRST ext-nota-fiscal-nfe OF nota-fiscal NO-LOCK NO-ERROR.
      

      IF ext-nota-fiscal-nfe.codigo <> 2 THEN DO:
        
       
      END.
        
    END.
  END.
END.

        







