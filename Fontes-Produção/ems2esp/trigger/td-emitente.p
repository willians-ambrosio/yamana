/*-----------------------------------------------------------------------------------
    PROGRAMA : td-emitente.p
    OBJETIVO : Trigger de Delete para a tabela Emitente
    AUTOR    : Thiago Coutinho (CSX)
    DATA     : 10/05/2012
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i td-emitente.p 2.06.00.000}
/* ----------> Variaveis de Parƒmetros <---------- */ 
DEF PARAMETER BUFFER p-table FOR emitente.

IF AVAIL p-table THEN DO:
    FIND FIRST dist-emitente EXCLUSIVE-LOCK
         WHERE dist-emitente.cod-emitente = p-table.cod-emitente
         NO-ERROR.
    IF AVAIL dist-emitente THEN
        DELETE dist-emitente.
END.

RETURN "OK".
