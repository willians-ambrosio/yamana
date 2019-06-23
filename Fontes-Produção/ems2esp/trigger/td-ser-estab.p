/*-----------------------------------------------------------------------------------
    PROGRAMA : td-ser-estab.p
    OBJETIVO : Trigger de Delete para a tabela  ser-estab
    AUTOR    : Wellington Aparecido (DSC)
    DATA     : 08/08/2008
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i td-ser-estab.p 2.06.00.000}

/* ----------> Variaveis de Parƒmetros <---------- */ 
DEF PARAMETER BUFFER p-table FOR ser-estab.

IF AVAIL p-table THEN DO:
  FIND FIRST mgesp.ext-ser-estab EXCLUSIVE-LOCK
    WHERE  mgesp.ext-ser-estab.serie       = p-table.serie
    AND    mgesp.ext-ser-estab.cod-estabel = p-table.cod-estabel NO-ERROR.
  IF AVAIL mgesp.ext-ser-estab THEN
    DELETE mgesp.ext-ser-estab.
END.

RETURN "OK".


