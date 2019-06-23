/*******************************************************************************
*
*     CC0302.I - Definicao de variaveis shared
*
*******************************************************************************/

{include/tt-edit.i}

def buffer b-cotacao-item for cotacao-item.
def buffer b-contato      for cont-emit.
def buffer b-ordem        for ordem-compra.
def buffer b-ord          for ordem-compra.
def buffer b-ordem-f      for ordem-compra.

def var l-param      like param-global.exp-cep    no-undo.
def var de-quant     like ordem-compra.preco-forn no-undo.
def var c-un         like prazo-compra.un         no-undo.
def var c-moeda      as char    no-undo format "x(10)".
def var c-descricao  as char    no-undo format "x(60)".
def var c-natureza   as char    no-undo format "x(8)".
def var c-lit        as char    no-undo format "x(4)".
def var c-cod-ipi    as char    no-undo format "X".
def var h-acomp      as handle  no-undo.
def var i-ano        as integer no-undo.
def var i-mes        as integer no-undo.
def var i-dia        as integer no-undo.
def var i-cont       as integer no-undo.
def var de-cotacao   as decimal no-undo.
def var de-preco     as decimal no-undo.
def var de-conta     as decimal no-undo.
def var de-indice    as decimal no-undo.
def var c-traco1     as char    no-undo.
def var c-traco2     as char    no-undo.
def var c-traco3     as char    no-undo.
def var c-traco4     as char    no-undo.
def var c-traco5     as char    no-undo.

/* Define variaveis para traduá∆o */
def var c-lb-contato as char no-undo extent 2.
def var c-lb-fone    as char no-undo extent 2.
def var c-lb-ordem   as char no-undo.
def var c-lb-compra  as char no-undo.
def var c-lb-fornec  as char no-undo.
def var c-lb-fax     as char no-undo.
def var c-lb-telex   as char no-undo.
def var c-lb-natur   as char no-undo.
def var c-lb-qtd-ord as char no-undo.
def var c-lb-un      as char no-undo.
def var c-lb-emissao as char no-undo.
def var c-lb-requis  as char no-undo.
def var c-lb-compr   as char no-undo.

form item.it-codigo              colon 12
     item-fornec.item-do-forn    colon 30 skip
     c-descricao                 colon 12
     ordem-compra.conta-contabil colon 93 skip(1)
     with stream-io no-box side-label width 132 frame f-item.

form c-lb-ordem     at 5  format "x(5)"
     c-lb-natur     at 15
     c-lb-qtd-ord   at 24 format "x(15)"
     c-lb-un        at 43 format "x(2)"
     c-lb-emissao   at 46
     c-lb-requis    at 57 format "x(12)"
     c-lb-compr     at 70 format "x(9)"
     "--------- -------- ------------------ -- ----------" at 5
     "------------ ------------"
     with stream-io no-box no-label width 132 frame f-cab-ordem.

form ordem-compra.numero-ordem  at 5
     c-natureza                 at 15
     de-quant                   at 23
     item-fornec.unid-med-for   at 43
     ordem-compra.data-emissao  at 46
     ordem-compra.requisitante  at 57
     ordem-compra.cod-comprado  at 70
     with stream-io no-label no-box width 132 frame f-ordem.

form emitente.nome-abrev
     b-ordem.num-pedido
     b-ordem.data-pedido
     c-natureza
     item.un
     b-ordem.cod-cond-pag
     c-moeda
     b-ordem.preco-unit
     de-quant
     b-ordem.aliquota-ipi space(0) "(" space(0)
     b-ordem.codigo-ipi   format "I/N" no-label space(0) ")" space(0)
     with stream-io no-box width 132 frame f-ult-compra.

form c-lit  no-label
     b-ordem.data-pedido
     c-natureza
     b-ordem.preco-unit
     c-moeda
     b-ordem.cod-cond-pag
     item.un
     b-ordem.num-pedido
     b-ordem.numero-ordem
     b-ordem.aliquota-ipi
     b-ordem.codigo-ipi
     with stream-io no-box width 132 frame f-cotacao-item.

form tt-editor.conteudo format "x(60)"
     with stream-io no-box no-label width 132 frame f-narrativa.

form ordem-compra.narrativa
     item-fornec.narrativa
     narrativa.descricao
     with stream-io frame f-editor.

{utp/ut-liter.i Descriá∆o * r}
assign c-descricao:label in frame f-item = trim(return-value).
{utp/ut-liter.i Fornecedor * r}
assign emitente.nome-abrev:label in frame f-ult-compra = trim(return-value).
{utp/ut-liter.i Data * r}
assign b-ordem.data-pedido:label in frame f-ult-compra = trim(return-value).
{utp/ut-liter.i Natureza * r}
assign c-natureza:label in frame f-ult-compra   = trim(return-value)
       c-natureza:label in frame f-cotacao-item = trim(return-value)
       c-lb-natur = trim(return-value).
{utp/ut-liter.i Moeda * r}
assign c-moeda:label in frame f-ult-compra   = trim(return-value)
       c-moeda:label in frame f-cotacao-item = trim(return-value).
{utp/ut-liter.i CP * r}
assign b-ordem.cod-cond-pag:label in frame f-ult-compra = trim(return-value).
{utp/ut-liter.i Preáo_Unit_For * r}
assign b-ordem.preco-unit:label in frame f-ult-compra = trim(return-value).
{utp/ut-liter.i Quantidade * r}
assign de-quant:label in frame f-ult-compra = trim(return-value).
{utp/ut-liter.i IPI * r}
assign b-ordem.aliquota-ipi:label in frame f-ult-compra = trim(return-value).

{utp/ut-liter.i Ordem * r}
assign c-lb-ordem = trim(return-value).
{utp/ut-liter.i Èltima_Compra * r}
assign c-lb-compra = trim(return-value).
{utp/ut-liter.i Contato * r}
assign c-lb-contato = trim(return-value).
{utp/ut-liter.i Telefone * r}
assign c-lb-fone = trim(return-value).
{utp/ut-liter.i Fornecedor * r}
assign c-lb-fornec = trim(return-value).
{utp/ut-liter.i Telefax * r}
assign c-lb-fax = trim(return-value).
{utp/ut-liter.i Telex * r}
assign c-lb-telex = trim(return-value).
{utp/ut-liter.i Qtdade_da_Ordem * r}
assign c-lb-qtd-ord = trim(return-value).
{utp/ut-liter.i Un * r}
assign c-lb-un = trim(return-value).
{utp/ut-liter.i Emiss∆o * r}
assign c-lb-emissao = trim(return-value).
{utp/ut-liter.i Requisitante * r}
assign c-lb-requis = trim(return-value).
{utp/ut-liter.i Comprador * r}
assign c-lb-compr = trim(return-value).

/* CC0302.I  */
