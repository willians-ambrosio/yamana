/*-----------------------------------------------------------------------------------
    PROGRAMA   : tw-es-item-doc-est-natoper-u00.p
    OBJETIVO   : Cria campo classe na tabela quando n∆o existir
    AUTOR      : Sergio Luiz Neto da Silveira
    DATA       : 11/08/2016
-----------------------------------------------------------------------------------*/

/* --- DEFINICOES --- */

{include/i-prgvrs.i tw-docum-est-u00.p 2.06.00.000}

DEFINE PARAMETER BUFFER p-table     FOR docum-est.
DEFINE PARAMETER BUFFER p-old-table FOR docum-est.

/* --- MAIN BLOCK --- */


/*
if p-table.tot-valor < p-old-table.tot-valor then do:
   p-table.tot-valor = p-old-table.tot-valor.
end.
*/

IF RETURN-VALUE = "NOK" THEN
   RETURN "NOK".

RETURN "OK".
