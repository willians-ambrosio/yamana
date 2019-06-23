/*-----------------------------------------------------------------------------------
    PROGRAMA   : tw-es-item-doc-est-natoper-u00.p
    OBJETIVO   : Cria campo classe na tabela quando n∆o existir
    AUTOR      : Sergio Luiz Neto da Silveira
    DATA       : 11/08/2016
-----------------------------------------------------------------------------------*/

/* --- DEFINICOES --- */

{include/i-prgvrs.i tw-es-item-doc-est-natoper-u00.p 2.06.00.000}

DEFINE PARAMETER BUFFER p-table     FOR es-item-doc-est-natoper.
DEFINE PARAMETER BUFFER p-old-table FOR es-item-doc-est-natoper.

/* --- MAIN BLOCK --- */

RUN estg/tw-es-item-doc-est-natoper-u01.p (BUFFER p-table,
                                           BUFFER p-old-table).

IF RETURN-VALUE = "NOK" THEN
   RETURN "NOK".

RETURN "OK".
