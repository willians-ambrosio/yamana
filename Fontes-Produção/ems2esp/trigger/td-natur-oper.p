/*-----------------------------------------------------------------------------------
    PROGRAMA : td-natur-oper.p
    OBJETIVO : Trigger de Delete para a tabela natur-oper
    AUTOR    : Wellington Aparecido (DSC)
    DATA     : 08/08/2008
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i td-natur-oper.p 2.06.00.000}

/* ----------> Variaveis de Parƒmetros <---------- */ 
DEF PARAMETER BUFFER p-table FOR natur-oper.

IF AVAIL p-table THEN DO:
   FOR EACH ext-nat-oper-sefaz EXCLUSIVE-LOCK
      WHERE ext-nat-oper-sefaz.nat-operacao = p-table.nat-operacao:
       DELETE ext-nat-oper-sefaz.
   END.

   FOR EACH es-conta-cfop 
      WHERE es-conta-cfop.nat-operacao = p-table.nat-operacao
       EXCLUSIVE-LOCK:
       DELETE es-conta-cfop.
   END.

   FOR EACH es-cfop-excecao 
      WHERE es-cfop-excecao.nat-operacao = p-table.nat-operacao
       EXCLUSIVE-LOCK:
       DELETE es-cfop-excecao.
   END.
END.

RETURN "OK".

