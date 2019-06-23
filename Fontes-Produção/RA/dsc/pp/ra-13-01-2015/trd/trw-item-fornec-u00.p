/***************************************************************************
**    Programa: TRw-item-fornec-u00.P
**    Objetivo: Trigger de Delete da tabela item-fornec
**       Autor: Renato Valle    
** Atualiza‡Æo: 13/03/2012 
**              Alterar tabela de NFE Recebimento nfe-nota-fiscal-rec
****************************************************************************/

DEF PARAM BUFFER p-table        FOR item-fornec.
DEF PARAM BUFFER p-old-table    FOR item-fornec.

RUN dsc/ra/trd/trw-item-fornec.p (BUFFER p-table, BUFFER p-old-table).

RETURN "OK":U.


