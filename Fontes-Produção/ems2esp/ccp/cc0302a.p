/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer moeda       for ems2cadme.moeda.

{include/i-prgvrs.i CC0302A 2.00.00.030}  /*** 010030 ***/
/********************************************************************************
**
**       PROGRAMA: CC0302A.P
**
**       DATA....: Mar¯o de 1997
**
**       AUTOR...: DATASUL S.A.
**
**       OBJETIVO: Emissao de Fichas para Cotacao(Reduzida).
**
**       VERS°O..: 1.00.000 - Sandra Stadelhofer
**
********************************************************************************/

{ccp/cc0302.i}  /* Definicao de variaveis e frames shared */
{ccp/cc0302.i3} /* Definicao da temp-table tt-param */
{ccp/cc0302f.i}
{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}
{cdp/cd0669.i}
{utp/ut-glob.i}

def input  param raw-param as raw   no-undo.
def output param table for tt-b2b.

def var r-registro        as rowid no-undo.

/* Define Vari˜veis p/ Tradu¯Êo */
def var c-lb-dt-cot  as char no-undo.
def var c-lb-preco   as char no-undo.
def var c-lb-moeda   as char no-undo.
def var c-lb-frete   as char no-undo.
def var c-lb-ipi     as char no-undo.
def var c-lb-perc    as char no-undo.
def var c-lb-cpg     as char no-undo.
def var c-lb-item    as char no-undo.

DEF SHARED VAR l-ultima-compra AS LOG NO-UNDO.


/*PTI-Status*/
&IF defined(BF_MAT_BLOQUEIO_FORNEC) &THEN
    def var h-api029      as handle no-undo.
    define var i-situacao as character no-undo.
    define var dt-vig-ini as date no-undo.
    define var dt-vig-fim as date no-undo.
&ENDIF

{utp/ut-liter.i Data_Cota¯Êo * r}
assign c-lb-dt-cot = trim(return-value).
{utp/ut-liter.i Pre¯o_Unit_Fornec * r}
assign c-lb-preco = trim(return-value).
{utp/ut-liter.i Moeda * r}
assign c-lb-moeda = trim(return-value).
{utp/ut-liter.i Frete * r}
assign c-lb-frete  = trim(return-value).
{utp/ut-liter.i IPI * r}
assign c-lb-ipi  = trim(return-value).
{utp/ut-liter.i Perc._Desconto * r}
assign c-lb-perc = trim(return-value).
{utp/ut-liter.i Cond._Pagto * r}
assign c-lb-cpg = trim(return-value).
{utp/ut-liter.i Item * r}
assign c-lb-item = trim(return-value).

{include/i-rpvar.i}
{include/i-rpcab.i}

form
    c-lb-dt-cot at 3 format "x(12)" space(0) ":__/__/____   "
    c-lb-preco       format "x(17)" space(0) ":_________,__  "
    c-lb-moeda       format "x(5)"  space(0) ":__________ "
    c-lb-frete  at 3 format "x(5)"  space(0) ":______     "
    c-lb-ipi         format "x(3)"  space(0) ":______   "
    c-lb-perc        format "x(12)" space(0) ":___,__  "
    c-lb-cpg         format "x(11)" space(0) ":___" skip(1)
    with stream-io no-box no-label width 132 frame f-cotacao.

form
    skip(1)
    c-traco1                    format "x(30)"
    c-lb-fornec                 format "x(10)"
    c-traco2                    format "x(33)" skip
    emitente.cod-emitente at 3  "-"
    emitente.nome-abrev         "-"
    emitente.nome-emit          skip
    emitente.cidade       at 12 "-"
    emitente.cep                "-"
    emitente.estado             "-"
    emitente.pais               skip
    c-lb-contato[1]       at 12 format "x(7)" space(0) ":"
    cont-emit.nome              format "x(12)"
    c-lb-fone[1]                space(0) ":"
    cont-emit.telefone          "-"
    cont-emit.ramal             skip(1)
    with stream-io no-box no-label width 100 frame f-emitente.

create tt-param.
raw-transfer raw-param to tt-param.

assign c-programa = "CC/0302"
       c-versao   = "1.00"
       c-revisao  = "000"
       c-traco1   = fill("-", 30)
       c-traco2   = fill("-", 33).

find first param-global no-lock no-error.
if  avail param-global then
    assign c-empresa = grupo.

for each  b-ord no-lock
    where b-ord.impr-ficha    = yes
    and   b-ord.data-emissao >= tt-param.d-data-i
    and   b-ord.data-emissao <= tt-param.d-data-f
    and   b-ord.cod-comprado >= tt-param.c-compr-i
    and   b-ord.cod-comprado <= tt-param.c-compr-f
    and   b-ord.it-codigo    >= tt-param.c-item-i
    and   b-ord.it-codigo    <= tt-param.c-item-f
    and   b-ord.numero-ordem >= tt-param.i-ordem-i
    and   b-ord.numero-ordem <= tt-param.i-ordem-f
    and   b-ord.cod-estabel  >= tt-param.c-estab-i
    and   b-ord.cod-estabel  <= tt-param.c-estab-f
    and   b-ord.situacao     <> 4
    break by b-ord.cod-emitente
          by b-ord.it-codigo:

    find ordem-compra where rowid(ordem-compra) = rowid(b-ord) no-lock no-wait no-error.

    if  not avail ordem-compra
    or  locked ordem-compra then
        next.

    for each tt-fornecedores-envio:
        delete tt-fornecedores-envio.
    end.

    /* Caso nÆo integre bus-to-bus imprime ficha sem fornecedor */
    if  tt-param.l-bus-to-bus = no then do:
        if  ordem-compra.it-codigo = ?
        or  ordem-compra.it-codigo  = "" then do:
            assign de-quant = 0.
            for each prazo-compra no-lock
                where prazo-compra.numero-ordem = ordem-compra.numero-ordem:
                assign de-quant = de-quant + prazo-compra.quantidade
                       c-un     = prazo-compra.un.
            end.
            find item where item.it-codigo = ordem-compra.it-codigo no-lock no-error.
            if  not avail item then 
                next.

            assign c-descricao = item.desc-item
                   c-natureza = {ininc/i01in274.i 04 ordem-compra.natureza}.

            if  line-counter + 19 > page-size then page.
            put unformatted
                fill("-", 57) " " c-lb-item " " fill("-", 60) skip.
            disp item.it-codigo
                 "" @ item-fornec.item-do-forn
                 c-descricao
                 ordem-compra.conta-contabil
                 with frame f-item.
            put unformatted
                fill("-",57) " " c-lb-ordem " " fill("-",59) skip.
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

            IF l-ultima-compra THEN DO:
                put unformatted
                    skip(1) fill("-", 57) " " c-lb-compra " " fill("-", 51) skip.
                disp "" @ emitente.nome-abrev
                     "" @ b-ordem.num-pedido
                     "" @ b-ordem.data-pedido
                     "" @ c-natureza
                     "" @ item.un
                     "" @ b-ordem.cod-cond-pag
                     "" @ b-ordem.preco-unit
                     "" @ c-moeda
                     "" @ de-quant
                     "" @ b-ordem.aliquota-ipi
                     "" @ b-ordem.codigo-ipi
                     with frame f-ult-compra.
            END.
            put unformatted
                fill("-", 57) " " c-lb-fornec " " fill("-", 54) skip(3).
            disp c-lb-dt-cot
                 c-lb-preco
                 c-lb-moeda
                 c-lb-frete
                 c-lb-ipi
                 c-lb-perc
                 c-lb-cpg
               with frame f-cotacao.
            assign r-registro = rowid(ordem-compra).
            run ccp/cc0302b.p (input-output r-registro).
            next.
        end.
    end.

    for each  item-fornec
        where item-fornec.it-codigo = ordem-compra.it-codigo
        and   item-fornec.ativo     = yes no-lock
        break by item-fornec.it-codigo:

        &IF defined(bf_mat_bloqueio_fornec) &THEN
            run cdp/cdapi029.p (input c-seg-usuario,
                                input 1,
                                input today,
                                input item-fornec.cod-emitente,
                                output i-situacao,
                                output dt-vig-ini,
                                output dt-vig-fim,
                                output table tt-erro).
            if return-value = "NOK":U then
               next.
        &ENDIF

        find item-fornec-estab
             where item-fornec-estab.it-codigo   = item-fornec.it-codigo
             and   item-fornec-estab.cod-emite   = item-fornec.cod-emite
             and   item-fornec-estab.cod-estabel = b-ord.cod-estabel
             and   item-fornec-estab.ativo       = no
             no-lock no-error.

        if  not avail item-fornec-estab then do:
            find emitente where emitente.cod-emitente = item-fornec.cod-emitente no-lock no-error.

            if  tt-param.l-bus-to-bus = yes
            and avail emitente
            &IF '{&BF_DIS_VERSAO_EMS}' >= '2.05' &THEN
            then do:
                find dist-emitente where dist-emitente.cod-emitente = emitente.cod-emitente no-lock no-error.
                if  avail dist-emitente
                and dist-emitente.parceiro-b2b then do:
            &ELSE
            and  substring(emitente.char-1,11,1) = "1" then do:
            &ENDIF

             /* Os finds abaixo servem para tratar split de ordem. O primeiro find verifica se h 
                alguma cota‡Æo pendente para a ordem, independente de fornecedor. Isso identifica
                se a ordem foi gerada com o parƒmetro "Cria Cota‡Æo Pendente" (cc0104) flegado.
                Se nÆo houver cota‡Æo pendente, o envio de cota‡äes via b2b se basear  no item-fornec.
                Se houver, enviar  apenas para o(s) fornecedor(es) da(s) cota‡Æo(äes) pendente(s).
                Com isso, evita-se de enviar v rias vezes o mesmo item para ser cotado com os mesmos
                fornecedores quando a ordem tiver sido splitada. */
                find first b-cotacao-item
                     where b-cotacao-item.data-cotacao = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 11/11/1111 &ENDIF
                     and   b-cotacao-item.numero-ordem = ordem-compra.numero-ordem
                     no-lock no-error.

                if  avail b-cotacao-item then
                    find first cotacao-item
                         where cotacao-item.data-cotacao = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 11/11/1111 &ENDIF
                         and   cotacao-item.numero-ordem = ordem-compra.numero-ordem
                         and   cotacao-item.cod-emitente = item-fornec.cod-emitente
                         no-lock no-error.

                if  not avail b-cotacao-item
                or  (     avail b-cotacao-item
                     and  avail cotacao-item) then do:
                    create tt-fornecedores-envio.
                    assign tt-fornecedores-envio.numero-ordem       = ordem-compra.numero-ordem
                           tt-fornecedores-envio.cod-emitente       = item-fornec.cod-emitente
                           tt-fornecedores-envio.nome-abrev         = emitente.nome-abrev
                           tt-fornecedores-envio.nome-emit          = emitente.nome-emit
                         /*tt-fornecedores-envio.contato-dest-nome  = cont-emit.nome*/
                           tt-fornecedores-envio.contato-dest-fone  = emitente.telefone[1]
                           tt-fornecedores-envio.contato-dest-ramal = emitente.ramal[1]
                           tt-fornecedores-envio.contato-dest-email = emitente.e-mail
                           tt-fornecedores-envio.contato-dest-fax   = emitente.telefax
                           tt-fornecedores-envio.endereco           = emitente.endereco
                           tt-fornecedores-envio.cidade             = emitente.cidade
                           tt-fornecedores-envio.bairro             = emitente.bairro
                           tt-fornecedores-envio.estado             = emitente.estado
                           tt-fornecedores-envio.pais               = emitente.pais
                           tt-fornecedores-envio.cep                = emitente.cep.
                    next.
                end.
            end.
            &IF '{&BF_DIS_VERSAO_EMS}' >= '2.05' &THEN
            end.
            &ENDIF
            else
            if  tt-param.l-envio = yes then do:
                /**
                ***  Envio: E-Mail 
                **/
                run ccp/cc0302d.p (input raw-param,
                                   input rowid(ordem-compra),
                                   input rowid(item-fornec),
                                   first-of(b-ord.cod-emitente),
                                   last-of(b-ord.cod-emitente),
                                   last(b-ord.cod-emitente)).
                next.
            end.

            assign de-quant = 0
                   i-cont   = 0.
            for each prazo-compra no-lock
                where prazo-compra.numero-ordem = ordem-compra.numero-ordem:
                assign de-quant = de-quant + prazo-compra.quantidade.
            end.
            find item of item-fornec no-lock no-error.
            assign c-descricao = item.desc-item.

            if  line-counter + 19 > page-size then page.
            put unformatted
                fill("-", 57) " " c-lb-item " " fill("-", 60) skip.
            disp item.it-codigo
                 item-fornec.item-do-forn
                 c-descricao
                 ordem-compra.conta-contabil
                 with frame f-item.
            put unformatted
                fill("-",57) " " c-lb-ordem " " fill("-",59) skip.
            assign c-natureza = {ininc/i01in274.i 04 ordem-compra.natureza}
                   de-indice  = item-fornec.fator-conver
                              / exp(10, item-fornec.num-casa-dec)
                   de-quant   = de-quant * de-indice.
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
                 item-fornec.unid-med-for
                 ordem-compra.data-emissao
                 ordem-compra.requisitante
                 ordem-compra.cod-comprado
                 with frame f-ordem.
            
            IF l-ultima-compra THEN DO:
                put unformatted
                    skip(1) fill("-", 57) " " c-lb-compra " " fill("-", 51) skip.
                find last b-ordem use-index compra-item
                    where b-ordem.it-codigo = ordem-compra.it-codigo
                    and   b-ordem.data-pedido <> ? no-lock no-error.

                if  avail b-ordem then do:
                    find emitente
                        where emitente.cod-emit = b-ordem.cod-emit no-lock no-error.
                    find first param-global no-lock no-error.
                    assign de-quant = 0.
                    for each prazo-compra no-lock
                        where prazo-compra.numero-ordem = b-ordem.numero-ordem:
                        assign de-quant = de-quant + prazo-compra.quantidade.
                    end.
                    find moeda where moeda.mo-codigo = b-ordem.mo-codigo no-lock no-error.
                    assign c-moeda = moeda.descricao when avail moeda
                           c-natureza = {ininc/i01in274.i 04 b-ordem.natureza}.       
                    disp emitente.nome-abrev
                         b-ordem.num-pedido
                         b-ordem.data-pedido
                         c-natureza
                         item.un
                         b-ordem.cod-cond-pag
                         b-ordem.preco-unit
                         c-moeda
                         de-quant
                         b-ordem.aliquota-ipi
                         b-ordem.codigo-ipi
                         with frame f-ult-compra.
                end.
                else
                    disp "" @ emitente.nome-abrev
                         "" @ b-ordem.num-pedido
                         "" @ b-ordem.data-pedido
                         "" @ c-natureza
                         "" @ item.un
                         "" @ b-ordem.cod-cond-pag
                         "" @ b-ordem.preco-unit
                         "" @ c-moeda
                         "" @ de-quant
                         "" @ b-ordem.aliquota-ipi
                         "" @ b-ordem.codigo-ipi
                         with frame f-ult-compra.
            END.

            find emitente
                where emitente.cod-emit = item-fornec.cod-emit no-lock no-error.
            find first cont-emit
                where cont-emit.cod-emitente = emitente.cod-emit
                no-lock no-error.
            disp c-traco1
                 c-traco2
                 c-lb-fornec
                 c-lb-contato[1]
                 c-lb-fone[1]
                 emitente.cod-emitente
                 emitente.nome-abrev
                 emitente.nome-emit
                 emitente.cidade
                 emitente.cep
                 emitente.estado
                 emitente.pais
                 cont-emit.nome      when avail cont-emit
                 cont-emit.telefone  when avail cont-emit
                 cont-emit.ramal     when avail cont-emit
                 with frame f-emitente.
            disp c-lb-dt-cot
                 c-lb-preco
                 c-lb-moeda
                 c-lb-frete
                 c-lb-ipi
                 c-lb-perc
                 c-lb-cpg
                 with frame f-cotacao.
            assign r-registro = rowid(ordem-compra).
            run ccp/cc0302b.p (input-output r-registro).
        end.
    end.
    if  can-find(first tt-fornecedores-envio) then do:
        run ccp/cc0302f.p (input rowid(ordem-compra),
                           input table tt-fornecedores-envio,
                           input-output table tt-b2b).
        assign r-registro = rowid(ordem-compra).
        run ccp/cc0302b.p (input-output r-registro).
    end.
end.
