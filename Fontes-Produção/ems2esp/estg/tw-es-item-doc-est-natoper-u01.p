/*-----------------------------------------------------------------------------------
    PROGRAMA   : tw-es-item-doc-est-natoper-u01.p
    OBJETIVO   : Cria campo classe na tabela quando n∆o existir
    AUTOR      : Sergio Luiz Neto da Silveira
    DATA       : 11/08/2016
-----------------------------------------------------------------------------------*/

/* --- DEFINICOES --- */

{include/i-prgvrs.i tw-es-item-doc-est-natoper-u01.p 2.06.00.000}

DEFINE PARAMETER BUFFER p-table     FOR es-item-doc-est-natoper.
DEFINE PARAMETER BUFFER p-old-table FOR es-item-doc-est-natoper.


/* --- MAIN BLOCK --- */
IF NOT (p-table.nat-operacao = "1999" OR 
        p-table.nat-operacao = "2999" OR 
        p-table.nat-operacao = "3999" ) THEN DO:
   IF p-table.classe = "" THEN DO:
      FOR FIRST ext-item-cfa FIELDS(classe)
                WHERE ext-item-cfa.it-codigo = p-table.it-codigo
                  AND ext-item-cfa.ep-codigo = p-table.ep-codigo 
                NO-LOCK:
         ASSIGN p-table.classe = ext-item-cfa.classe.
      END.   
   END.
END.

RETURN "OK".
