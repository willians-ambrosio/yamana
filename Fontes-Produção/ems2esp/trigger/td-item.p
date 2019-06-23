/*-----------------------------------------------------------------------------------
    PROGRAMA : td-item.p
    OBJETIVO : Trigger de Delete para a tabela  Item
    AUTOR    : Wellington Aparecido (DSC)
    DATA     : 08/08/2008
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i td-item.p 2.06.00.000}
/* ----------> Variaveis de Parƒmetros <---------- */ 
DEF PARAMETER BUFFER p-table FOR item.

IF AVAIL p-table THEN DO:
  FIND FIRST ext-item-servico EXCLUSIVE-LOCK
    WHERE  ext-item-servico.it-codigo = p-table.it-codigo  NO-ERROR.
  IF AVAIL ext-item-servico THEN
    DELETE ext-item-servico.
END.

RETURN "OK".
