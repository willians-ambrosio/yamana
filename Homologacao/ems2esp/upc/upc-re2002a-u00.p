/******************************************************************
**
** Programa: UPC-RE2002A-U00
**
** Objetivo: Programa chamador de UPC
**
**    Autor: Renato
**
**     Data: Dezembro/2018
**
**   Versao: 2.12.00.000 - Desenvolvimento Inicial
**
******************************************************************/
{include/i-prgvrs.i UPC-RE2002A-U00 2.12.00.000}

define input parameter p-ind-event   as char          no-undo.
define input parameter p-ind-object  as char          no-undo.
define input parameter p-wgh-object  as handle        no-undo.
define input parameter p-wgh-frame   as widget-handle no-undo.
define input parameter p-cod-table   as char          no-undo.
define input parameter p-row-table   as rowid         no-undo.

/* Passagem de parƒmetros da UPC  */
run upc/upc-re2002a-u01.p (input p-ind-event,
                           input p-ind-object,
                           input p-wgh-object,
                           input p-wgh-frame,
                           input p-cod-table,
                           input p-row-table).

if return-value = "NOK":U then
    return "NOK".
