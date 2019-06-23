/****************************************************************************
**
**  cc0323.i - Define frames do relat¢rio e variaveis p/ traducao.
**
****************************************************************************/

{include/tt-edit.i}

def var c-lb-dt-cot      as char no-undo format 'x(12)'.
def var c-lb-moeda       as char no-undo format 'x(5)'.
def var c-lb-cot         as char no-undo format 'x(10)'.
def var c-lb-ordem       as char no-undo format 'x(5)'.
def var c-lb-item        as char no-undo format 'x(4)'.
def var c-lb-qtde        as char no-undo format 'x(10)'.
def var c-lb-dt-ent      as char no-undo format 'x(10)'.
def var c-lb-preco       as char no-undo format 'x(5)'.
def var c-lb-per-ipi     as char no-undo format 'x(5)'.
def var c-lb-dados       as char no-undo format 'x(12)'.
def var c-lb-ipi         as char no-undo format 'x(3)'.
def var c-lb-inc         as char no-undo format 'x(7)'.
def var c-lb-ninc        as char no-undo format 'x(11)'.
def var c-lb-valid       as char no-undo format 'x(16)'.
def var c-lb-icms        as char no-undo format 'x(4)'.
def var c-lb-ind         as char no-undo format 'x(16)'.
def var c-lb-cons        as char no-undo format 'x(7)'.
def var c-lb-per-desc    as char no-undo format 'x(19)'.
def var c-lb-aliq-icms   as char no-undo format 'x(13)'.
def var c-lb-vl-desc     as char no-undo format 'x(17)'.
def var c-lb-aliq-iss    as char no-undo format 'x(12)'.
def var c-lb-reajuste    as char no-undo format 'x(8)'.
def var c-lb-sim         as char no-undo format 'x(3)'.
def var c-lb-nao         as char no-undo format 'x(3)'.
def var c-lb-frete       as char no-undo format 'x(5)'.
def var c-lb-prazo       as char no-undo format 'x(13)'.
def var c-lb-emdia       as char no-undo format 'x(7)'.
def var c-lb-vl-frete    as char no-undo format 'x(11)'.
def var c-lb-taxa        as char no-undo format 'x(15)'.
def var c-lb-dia         as char no-undo format 'x(4)'.
def var c-lb-contato     as char no-undo format 'x(7)'.
def var c-lb-cpg         as char no-undo format 'x(21)'.
def var c-lb-tit-sel     as char no-undo.
def var c-lb-tit-imp     as char no-undo.
def var c-lb-destino     as char no-undo.
def var c-lb-usuario     as char no-undo.
def var c-lb-processo    as char no-undo.
def var c-lb-comprador   as char no-undo.
def var c-lb-param        as char no-undo.

form  tt-ficha.ordem       at 02
      tt-ficha.item        at 15
      tt-ficha.quantidade  TO 57 
      /*tt-ficha.parcela     at 61*/
      tt-ficha.un          AT 62
      '________.______.______,______    _______,_____' at 68
      tt-ficha.descricao   AT 15
      with stream-io no-box no-label width 132 frame f-ordem.


form tt-ficha.processo       colon 25
     tt-ficha.comprador      colon 50
     tt-ficha.tel-umater     LABEL "Telefone" COLON 50 SKIP 
     tt-ficha.fax-umater     LABEL "FAX"      COLON 50 SKIP
     tt-ficha.e-mail         LABEL "E-mail"   FORMAT "x(40)" COLON 50 SKIP
     with stream-io row 3 no-box WIDTH 120 side-labels frame f-processo.

form skip(1)
     tt-ficha.fornecedor   at 6 '-'
     emitente.nome-abrev   no-label 
     emitente.telefone[1]  AT 65 LABEL "Telefone" 
     emitente.telefax      AT 96 LABEL "FAX" skip
     c-lb-dt-cot           at 6  no-label space(0) ':__/__/____' SKIP(2)
     c-traco               at 1  no-label
     with stream-io no-box side-label width 132 frame f-fornec.
      
form c-lb-ordem     at 2
     c-lb-item      at 15
     c-lb-qtde      at 37
     /*"Parcela"      AT 62 */
     "Un"           AT 62 
     c-lb-preco     at 68
     c-lb-per-ipi   at 101 SKIP
     ' ----------   ------------------    ---------------------' at 1
     '    --    -----------------------------'  at 58
     '   -------------'                                          at 98
     with stream-io no-box no-label width 150 frame f-cab1.

form skip(1)
     '--------------------------------------------------' at 1
     '--------------------------------------------------' at 51
     '--------------------------------'                   at 101
     c-lb-dados                                           at 55
     '--------------------------------------------------' at 1
     '--------------------------------------------------' at 51
     '--------------------------------'                   at 101
     with stream-io no-box no-label width 132 frame f-cab2.

form skip(1)
     c-lb-ipi          at 19 ': _____________(' space(0)
     c-lb-inc          space(0) '/' space(0) c-lb-ninc space(0) ')'
     c-lb-aliq-icms    at 9  ': ______,____'
     c-lb-aliq-iss     at 10 ': ______,____'
     c-lb-valid        at 06 ': _______'
     /*c-lb-per-desc     at 03 ': ______,______'*/
     c-lb-prazo        at 09 ': _______(' space(0) c-lb-emdia space(0) ')'
     c-lb-contato      at 15 ': ___________________________'
     c-lb-cpg          at 1  ': _____________________________________________'
     '________________________________________________________' at 70 skip
     '_______________________________________________________'  at 25
     '______________________________________________'           at 80 skip
     '_______________________________________________________'  at 25
     '______________________________________________'           at 80 skip
     '_______________________________________________________'  at 25
     '______________________________________________'           at 80 skip(1)
     c-traco                                                    at 1

     /*
     c-lb-icms         at 18 ': _____________(' space(0)
     c-lb-ind          space(0) '/' space(0) c-lb-cons space(0) ')'
     c-lb-vl-desc      at 72 ': ______.______.______,_____'
     c-lb-reajuste     at 81 ': _______(' space(0)
     c-lb-sim          space(0) '/' space(0) c-lb-nao space(0) ')'
     c-lb-frete        at 17 ': _____________'
     c-lb-vl-frete     at 11 ': ______.______.______,_____'
     c-lb-taxa         at 74 ': _____,_____   ' c-lb-dia ': ____'
     */
     
     with stream-io no-box no-label width 132 frame f-det-fornec.   

{utp/ut-liter.i Numero_do_Processo * r}
assign tt-ficha.processo:label in frame f-processo = trim(return-value).
{utp/ut-liter.i Comprador * r}
assign tt-ficha.comprador:label in frame f-processo = trim(return-value)
       c-lb-comprador = trim(return-value).
{utp/ut-liter.i Fornecedor * r}
assign tt-ficha.fornecedor:label in frame f-fornec = trim(return-value).
{utp/ut-liter.i Data_Cotacao * r}
assign c-lb-dt-cot = trim(return-value).
{utp/ut-liter.i Moeda * r}
assign c-lb-moeda = trim(return-value).
{utp/ut-liter.i Cot._Moeda * r}
assign c-lb-cot = trim(return-value).
{utp/ut-liter.i Ordem * r}
assign c-lb-ordem = trim(return-value).
{utp/ut-liter.i Item * r}
assign c-lb-item = trim(return-value).
{utp/ut-liter.i Quantidade * r}
assign c-lb-qtde = trim(return-value).

/*
{utp/ut-liter.i Dt.Entrega * r}
assign c-lb-dt-ent = trim(return-value).
*/

{utp/ut-liter.i Preco * r}
assign c-lb-preco = trim(return-value).
{utp/ut-liter.i %_Ipi * r}
assign c-lb-per-ipi = trim(return-value).
{utp/ut-liter.i Dados_Gerais * r}
assign c-lb-dados = trim(return-value).
{utp/ut-liter.i IPI * r}
assign c-lb-ipi = trim(return-value).
{utp/ut-liter.i Incluso * r}
assign c-lb-inc = trim(return-value).
{utp/ut-liter.i NÆo_Incluso * r}
assign c-lb-ninc = trim(return-value).
{utp/ut-liter.i Dias_de_Validade * r}
assign c-lb-valid = trim(return-value).
{utp/ut-liter.i ICMS * r}
assign c-lb-icms = trim(return-value).
{utp/ut-liter.i Industrializa‡Æo * r}
assign c-lb-ind = trim(return-value).
{utp/ut-liter.i Consumo * r}
assign c-lb-cons = trim(return-value).
{utp/ut-liter.i Percentual_Desconto * r}
assign c-lb-per-desc = trim(return-value).
{utp/ut-liter.i Aliquota_ICMS * r}
assign c-lb-aliq-icms = trim(return-value).
{utp/ut-liter.i Valor_do_Desconto * r}
assign c-lb-vl-desc = trim(return-value).
{utp/ut-liter.i Aliquota_ISS * r}
assign c-lb-aliq-iss = trim(return-value).
{utp/ut-liter.i Reajuste * r}
assign c-lb-reajuste = trim(return-value).
{utp/ut-liter.i Sim * r}
assign c-lb-sim = trim(return-value).
{utp/ut-liter.i NÆo * r}
assign c-lb-nao = trim(return-value).
{utp/ut-liter.i Frete * r}
assign c-lb-frete = trim(return-value).
{utp/ut-liter.i Prazo_Entrega * r}
assign c-lb-prazo = trim(return-value).
{utp/ut-liter.i em_dias * r}
assign c-lb-emdia = trim(return-value).
{utp/ut-liter.i Valor_Frete * r}
assign c-lb-vl-frete = trim(return-value).
{utp/ut-liter.i Taxa_Financeira * r}
assign c-lb-taxa = trim(return-value).
{utp/ut-liter.i Dias * r}
assign c-lb-dia = trim(return-value).
{utp/ut-liter.i Contato * r}
assign c-lb-contato = trim(return-value).
{utp/ut-liter.i Condicao_de_Pagamento * r}
assign c-lb-cpg = trim(return-value).
{utp/ut-liter.i SELE€ÇO * r}
assign c-lb-tit-sel = trim(return-value).
{utp/ut-liter.i IMPRESSÇO * r}
assign c-lb-tit-imp = trim(return-value).
{utp/ut-liter.i PAR¶METROS * r}
assign c-lb-param = trim(return-value).
{utp/ut-liter.i Destino * r}
assign c-lb-destino = trim(return-value).
{utp/ut-liter.i Usuario * r}
assign c-lb-usuario = trim(return-value).
{utp/ut-liter.i Processo * r}
assign c-lb-processo = trim(return-value).


form tt-editor.conteudo format "x(60)"
     with stream-io no-box no-label width 132 frame f-narrativa.

/* fim include */
