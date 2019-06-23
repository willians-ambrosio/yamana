/*-----------------------------------------------------------------------------------
    PROGRAMA : td-wt-it-docto.p
    OBJETIVO : Trigger de Delete para a tabela wt-it-docto
    AUTOR    : Sergio Luiz Neto da Silveira (DSC)
    DATA     : 20/12/2016
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i td-wt-it-docto.p 2.06.00.000}

/* ----------> Variaveis de Parƒmetros <---------- */ 
DEF PARAMETER BUFFER p-table FOR wt-it-docto.

IF AVAIL p-table THEN DO:
   FOR EACH es-wt-it-docto OF p-table
            EXCLUSIVE-LOCK:
       DELETE es-wt-it-docto.
   END.
END.

RETURN "OK".

