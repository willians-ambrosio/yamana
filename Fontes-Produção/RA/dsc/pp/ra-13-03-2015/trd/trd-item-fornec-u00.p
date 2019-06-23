/***************************************************************************
**    Programa: TRD-item-fornec-u00.P
**    Objetivo: Trigger de Delete da tabela item-fornec
**       Autor: Renato Valle    
** Atualiza‡Æo: 13/03/2012 
**              Alterar tabela de NFE Recebimento nfe-nota-fiscal-rec
****************************************************************************/

DEF PARAM BUFFER p-table FOR item-fornec.

RUN dsc/ra/trd/trd-item-fornec.p (BUFFER p-table).

RETURN "OK":U.


