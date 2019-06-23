/******************************************************************
**
** Programa: UPC-ESRA100-U00
**
** Objetivo: Programa chamador de UPC
**
**    Autor: Renato
**
**     Data: Janeiro/2019
**
**   Versao: 12.01.19.000 - Desenvolvimento Inicial
**
******************************************************************/
{include/i-prgvrs.i UPC-ESRA100-U00 12.01.22.000}

/* Definicao de Parametros */
define input parameter p-ind-event   as char          no-undo.
define input parameter p-ind-object  as char          no-undo.
define input parameter p-wgh-object  as handle        no-undo.
define input parameter p-wgh-frame   as widget-handle no-undo.
define input parameter p-cod-table   as char          no-undo.
define input parameter p-row-table   as rowid         no-undo.

/* Passagem de parƒmetros da UPC  */
run upc/upc-esra100-u01.p (input p-ind-event,
                           input p-ind-object,
                           input p-wgh-object,
                           input p-wgh-frame,
                           input p-cod-table,
                           input p-row-table).

if return-value = "NOK":U then
    return "NOK".
