/*************************************************************************
** 
**  Programa: inapi228.I
** 
**  Objetivo: Defini‡Æo das temp-tables usadas na API inapi228.p
**
*************************************************************************/
def temp-table tt-movto-apr-aux like movto-apr
    INDEX ch-codigo IS PRIMARY ep-codigo
                               cod-estabel
                               esp-docto
                               cod-emitente
                               serie-docto
                               nro-docto.

def temp-table tt-movto-apr like movto-apr
    field cod-maq-origem   as   integer format "999"       initial 0
    field num-processo     as   integer format ">>>>>>>>9" initial 0
    field num-sequencia    as   integer format ">>>>>9"    initial 0
    field ind-tipo-movto   as   integer format "99"        initial 1
    INDEX ch-codigo IS PRIMARY  cod-maq-origem
                                num-processo
                                num-sequencia.
 
 
