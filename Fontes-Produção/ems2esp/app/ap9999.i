/*********************************************************************************
***
*** app/ap9999.i - Defini‡Æo Variaveis Tratamento Multimoeda.
***
*********************************************************************************/

{cdp/cdcfgfin.i}

def var de-calc-vl-original   like tit-ap.vl-original  no-undo.
def var de-calc-valor-saldo   like tit-ap.valor-saldo  no-undo.
def var de-calc-vl-liquido    like tit-ap.vl-liquido   no-undo.
def var de-calc-vl-juros-dia  like tit-ap.vl-juros-dia no-undo.
def var de-calc-vl-desconto   like tit-ap.vl-desconto  no-undo.
def var de-calc-diversos      like tit-ap.diversos     no-undo.
def var de-calc-frete         like tit-ap.frete        no-undo.
def var de-calc-valor-mov     like mov-ap.valor-mov    no-undo.
def var de-calc-valor-juros   like mov-ap.valor-juros  no-undo.
&if defined(BF_FIN_MULTA_ABATIMENTO) &then
def var de-calc-valor-multa   like mov-ap.vl-multa      no-undo.
def var de-calc-valor-abat    like mov-ap.vl-abatimento no-undo.
&endif
def var de-calc-vl-antecip    like mov-ap.vl-antecip   no-undo.
def var de-calc-cotacao       like mov-ap.cotacao-dia  no-undo.

/* Fim de Include */
