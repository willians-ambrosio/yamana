/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i CC0302C 2.00.00.011}  /*** 010011 ***/
/*****************************************************************************
**
**       PROGRAMA: CC0302C.P
**
**       DATA....: MARCO DE 1997
**
**       AUTOR...: DATASUL S.A.
**
**       OBJETIVO: Imprime Ficha de Cota‡Æo para Item sem Relacionamento
**                 Item/Fornecedor
**
**       VERSÇO..: 1.00.000 - Sandra Stadelhofer
**
*****************************************************************************/

{ccp/cc0302.i}  /* Definicao de variaveis e frames shared */
{ccp/cc0302.i3} /* Definicao da temp-table tt-param */

def input param l-prazo-compr  as logical no-undo.
def input param r-registro     as rowid   no-undo.

DEF SHARED VAR l-ultima-compra AS LOG NO-UNDO.

{include/i-rpvar.i}

{include/i-rpcab.i}

assign c-programa = "CC/0302"
       c-versao   = "1.00"
       c-revisao  = "000".

find first param-global no-lock no-error.
if  avail param-global then
    assign c-empresa = grupo.

find ordem-compra where rowid(ordem-compra) = r-registro exclusive-lock.
assign de-quant = 0
       i-cont   = 0.
for each prazo-compra
    where prazo-compra.numero-ordem = ordem-compra.numero-ordem no-lock:
    assign de-quant = de-quant + prazo-compra.quantidade
           c-un     = prazo-compra.un.
end.
find item where item.it-codigo = ordem-compra.it-codigo no-lock.
find narrativa of item no-lock no-error.
assign c-descricao = item.desc-item.

disp item.it-codigo
     "" @ item-fornec.item-do-forn
     c-descricao
     ordem-compra.conta-contabil
     with frame f-item.

if  ordem-compra.narrativa <> "" then
    run pi-print-editor (ordem-compra.narrativa, 60).
else
if  avail narrativa
and narrativa.descricao <> "" then
    run pi-print-editor (narrativa.descricao, 60).
else
    put skip(5).

/* Imprime Narrativa da Ordem */
for each tt-editor:
    if  tt-editor.linha = 1 then
        put unformatted
            fill("-",24) " " c-lb-ordem " " fill("-",29) skip.
    disp tt-editor.conteudo with frame f-narrativa.
    down with frame f-narrativa.
end.
put skip(1).
assign c-natureza = {ininc/i01in274.i 04 ordem-compra.natureza}.
disp c-lb-ordem
     c-lb-natur
     c-lb-qtd-ord
     c-lb-un
     c-lb-emissao
     c-lb-requis
     c-lb-compr
     with frame f-cab-ordem.
disp ordem-compra.numero-ordem
     c-natureza
     de-quant
     c-un @ item-fornec.unid-med-for
     ordem-compra.data-emissao
     ordem-compra.requisitante
     ordem-compra.cod-comprado
     with frame f-ordem.
assign i-cont = 0.
for each prazo-compra
    where prazo-compra.numero-ordem = ordem-compra.numero-ordem no-lock:
    assign i-cont = i-cont + 1.
    if  i-cont > 9 then leave.
end.
put skip(1).
if  l-prazo-compr = no or i-cont <= 9 then do i-cont = 1 to 9:
    if  i-cont = 1 then
        find first prazo-compra
            where prazo-compra.numero-ordem = ordem-compra.numero-ordem
            no-lock no-error.
    else
        find next prazo-compra
            where prazo-compra.numero-ordem = ordem-compra.numero-ordem
            no-lock no-error.
    if  avail prazo-compra then do:
        if  i-cont = 1 then
            put ordem-compra.ordem-servic at 3.
        put prazo-compra.parcela      at 17
            prazo-compra.quantidade   at 22
            prazo-compra.data-entrega at 41
            "________________"        at 52
            "____/____/____"          at 69 skip.
    end.
    else put skip(1).
end.
else
for each prazo-compra
    where prazo-compra.numero-ordem = ordem-compra.numero-ordem no-lock:
    assign de-quant = prazo-compra.quantidade * de-indice.
    if  i-cont = 1 then
        put ordem-compra.ordem-servic at 3.
    put prazo-compra.parcela      at 17
        prazo-compra.quantidade   at 22
        prazo-compra.data-entrega at 41
        "________________"        at 52
        "____/____/____"          at 69 skip.
end.

IF l-ultima-compra THEN DO:
    put unformatted
        fill("-", 30) " " c-lb-compra " " fill("-", 30) skip.
    disp "" @ emitente.nome-abrev
         "" @ b-ordem.num-pedido
         "" @ b-ordem.data-pedido
         "" @ c-natureza
         "" @ item.un
         "" @ b-ordem.cod-cond-pag
         "" @ c-moeda
         "" @ b-ordem.preco-unit
         "" @ de-quant
         "" @ b-ordem.aliquota-ipi
         "" @ b-ordem.codigo-ipi
         with frame f-ult-compra.
END.

put unformatted
    fill("-", 30) " " c-lb-fornec " " fill("-", 33) skip(7).

{utp/ut-liter.i Com * r}
assign c-lit = trim(return-value) + ":".
disp c-lit
     "" @ b-ordem.data-pedido
     "" @ c-natureza
     "" @ c-moeda
     "" @ b-ordem.cod-cond-pag
     "" @ item.un
     "" @ b-ordem.num-pedido
     "" @ b-ordem.numero-ordem
     "" @ b-ordem.preco-unit
     "" @ b-ordem.aliquota-ipi
     "" @ b-ordem.codigo-ipi
     with frame f-cotacao-item.
disp c-lit
     "" @ b-ordem.data-pedido
     "" @ c-natureza
     "" @ c-moeda
     "" @ b-ordem.cod-cond-pag
     "" @ item.un
     "" @ b-ordem.num-pedido
     "" @ b-ordem.numero-ordem
     "" @ b-ordem.preco-unit
     "" @ b-ordem.aliquota-ipi
     "" @ b-ordem.codigo-ipi
     with frame f-cotacao-item.
{utp/ut-liter.i Cot * r}
assign c-lit = trim(return-value) + ":".
disp c-lit
     "" @ b-ordem.data-pedido
     "" @ c-natureza
     "" @ c-moeda
     "" @ b-ordem.cod-cond-pag
     "" @ item.un
     "" @ b-ordem.num-pedido
     "" @ b-ordem.numero-ordem
     "" @ b-ordem.preco-unit
     "" @ b-ordem.aliquota-ipi
     "" @ b-ordem.codigo-ipi
     with frame f-cotacao-item.

{include/pi-edit.i}

