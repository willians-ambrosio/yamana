/*******************************************************************************
**
**   ESCC0406.i4 - Put da linha da parcela
**
*******************************************************************************/

put prazo-compra.parcela      at 1
    c-sit                     at 7
    ordem-compra.data-emissao at 12
    prazo-compra.data-entrega at 23
    prazo-compra.quantidade   at 34 format ">>>>>,>>9.9999"
    prazo-compra.quant-saldo  at 49 format "->>>>,>>9.99"
    de-preco-merc             at 61
    de-valor-ipi              at 80
    de-preco-total            at 96
    prazo-compra.pedido-clien at 115 format "x(4)"
    prazo-compra.nr-sequencia at 121 format ">>>>9-"
    i-atraso                  at 128 skip.

/* fim do include */
