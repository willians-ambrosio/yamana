/*-----------------------------------------------------------------------------------
    PROGRAMA : td-unid-feder.p
    OBJETIVO : Trigger de Delete para a tabela  unid-feder
    AUTOR    : Wellington Aparecido (DSC)
    DATA     : 08/08/2008
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i td-unid-feder.p 2.06.00.000}

/* ----------> Variaveis de Parƒmetros <---------- */ 
DEF PARAMETER BUFFER p-table FOR ems2cadme.unid-feder.

IF  AVAIL p-table THEN DO:
    FIND FIRST mgesp.ext-unid-feder EXCLUSIVE-LOCK
         WHERE mgesp.ext-unid-feder.pais   = p-table.pais
           AND mgesp.ext-unid-feder.estado = p-table.estado NO-ERROR.
    IF  AVAIL mgesp.ext-unid-feder THEN
        DELETE mgesp.ext-unid-feder.
END.

RETURN "OK".
