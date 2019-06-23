/*-----------------------------------------------------------------------------------
    PROGRAMA : td-item-uni-estab.p
    OBJETIVO : Trigger de Delete para a tabela  item-uni-estab
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i td-item-uni-estab.p 2.06.00.000}
/* ----------> Variaveis de Parƒmetros <---------- */ 
DEF PARAMETER BUFFER p-table FOR item-uni-estab.
/*
IF AVAIL p-table THEN
  FOR EACH ext-audit-item-fam 
     WHERE ext-audit-item-fam.it-codigo   = p-table.it-codigo
       AND ext-audit-item-fam.cod-estabel = p-table.cod-estabel:
    DELETE ext-audit-item-fam.
  END.
*/
RETURN "OK".
