/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer moeda       for ems2cadme.moeda.

{include/i-prgvrs.i CC0302RP 2.00.00.040}  /*** 010040 ***/
/*****************************************************************************
**
**     PROGRAMA: CC0302RP.P
**
**     DATA....: Mar»o de 1997
**
**     AUTOR...: DATASUL S.A.
**
**     OBJETIVO: Emissao de Fichas para Cotação
**
**     VERSÇO..: 1.00.000
**    
*****************************************************************************/

/* Variavel Global criada na p gina de parƒmetros do programa chamador para 
   indicar se imprime ou nÆo dados da £ltima compra */
DEF NEW GLOBAL SHARED VAR gwh-tg-ult-compra AS WIDGET-HANDLE.

{ccp/cc0302.i}    /* Definicao de variaveis e frames shared  */
{ccp/cc0302.i3}   /* Definicao da temp-table tt-param        */
{ccp/cc0327.i}    /* DatasulNet                              */
{ccp/cc0302f.i}   /* Definicao de temp-tables                */
{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}
{cdp/cd0669.i}

def temp-table tt-raw-digita
    field raw-digita as raw.

def var r-registro as rowid no-undo.

/* Variaveis p/ Traducao */
def var c-lb-incluso      as char no-undo extent 3.
def var c-lb-naoincl      as char no-undo extent 3.
def var c-lb-data         as char no-undo extent 2.
def var c-lb-comprador    as char no-undo.
def var c-lb-cotacao      as char no-undo.
def var c-lb-dt-cot       as char no-undo.
def var c-lb-vl-fre       as char no-undo.
def var c-lb-un-med       as char no-undo.
def var c-lb-encarg       as char no-undo.
def var c-lb-prc-for      as char no-undo.
def var c-lb-taxa         as char no-undo.
def var c-lb-moeda        as char no-undo.
def var c-lb-perc         as char no-undo.
def var c-lb-cot-mo       as char no-undo.
def var c-lb-cpg          as char no-undo.
def var c-lb-ipi          as char no-undo.
def var c-lb-prazo        as char no-undo.
def var c-lb-al-ipi       as char no-undo.
def var c-lb-icms         as char no-undo.
def var c-lb-indust       as char no-undo.
def var c-lb-consumo      as char no-undo.
def var c-lb-al-icms      as char no-undo.
def var c-lb-prc-un       as char no-undo.
def var c-lb-al-iss       as char no-undo.
def var c-lb-cot-apr      as char no-undo.
def var c-lb-frete        as char no-undo.
def var c-lb-sim          as char no-undo.
def var c-lb-nao          as char no-undo.
def var c-lb-aprov        as char no-undo.
def var c-lb-coment       as char no-undo.
def var c-lb-gerente      as char no-undo.
def var c-lb-item         as char no-undo.
def var c-lb-fami         as char no-undo.
def var c-lb-estab        as char no-undo.
def var c-lb-tp-rel       as char no-undo.
def var c-lb-ficha        as char no-undo.
def var c-lb-parc         as char no-undo.
def var c-lb-selec        as char no-undo.
def var c-lb-param        as char no-undo.
def var c-lb-impr         as char no-undo.
def var c-lb-dest         as char no-undo.
def var c-lb-usuar        as char no-undo.
def var c-lb-envio        as char no-undo.
def var c-lb-b2b          as char no-undo.
def var c-b2b             as char format "x(40)" no-undo.
def var c-bb-ordem        as char format "x(50)" no-undo.

DEF NEW SHARED VAR l-ultima-compra AS LOG NO-UNDO.

/*PTI-Status*/
&IF defined(BF_MAT_BLOQUEIO_FORNEC) &THEN
    def var h-api029      as handle no-undo.
    define var i-situacao as character no-undo.
    define var dt-vig-ini as date no-undo.
    define var dt-vig-fim as date no-undo.
&ENDIF

{include/i-rpvar.i}
{utp/ut-glob.i}

{utp/ut-liter.i Ficha_para_Cota‡Æo_de_Pre‡os * r}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i Compras * r}
assign c-sistema = trim(return-value).

form c-traco1                format "x(35)"
     c-lb-cotacao            format "x(7)"
     c-traco2                format "x(36)" skip
     c-lb-dt-cot       at 5  format "x(12)" space(0) ": __/__/____"
     c-lb-vl-fre       at 47 format "x(11)" space(0) ": _________,__" skip
     c-lb-un-med       at 1  format "x(16)" space(0) ": __"
     c-lb-encarg       at 38 format "x(20)" space(0) ": __ (" space(0)
     c-lb-incluso[1]         format "x(7)"  space(0) "/" space(0)
     c-lb-naoincl[1]         format "x(11)" space(0) ")" skip
     c-lb-prc-for      at 1  format "x(16)" space(0) ": ___________,__"
     c-lb-taxa         at 43 format "x(15)" space(0) ": ___,__" skip
     c-lb-moeda        at 12 format "x(5)"  space(0) ": __________"
     c-lb-perc         at 39 format "x(19)" space(0) ": ___,__" skip
     c-lb-cot-mo       at 4  format "x(13)" space(0) ": ___________,__"
     c-lb-cpg          at 40 format "x(18)" space(0) ": ___" skip
     c-lb-ipi          at 14 format "x(3)"  space(0) ": __ (" space(0)
     c-lb-incluso[2]         format "x(7)"  space(0) "/" space(0)
     c-lb-naoincl[2]         format "x(11)" space(0) ")"
     c-lb-prazo        at 45 format "x(13)" space(0) ": _____" skip
     c-lb-al-ipi       at 5  format "x(12)" space(0) ": ___,__"
     c-lb-contato      at 51 format "x(7)"  space(0) ": ____________" skip
     c-lb-icms         at 13 format "x(4)"  space(0) ": __ (" space(0)
     c-lb-indust             format "x(6)"  space(0) "/" space(0)
     c-lb-consumo            format "x(7)"  space(0) ")"
     c-lb-comprador    at 49 format "x(9)"  space(0) ": ____________" skip
     c-lb-al-icms      at 4  format "x(13)" space(0) ": ___,__"
     c-lb-prc-un       at 41 format "x(17)" space(0) ": _________,__" skip
     c-lb-al-iss       at 5  format "x(12)" space(0) ": ___,__"
     c-lb-cot-apr      at 42 format "x(16)" space(0) ": ___ (" space(0)
     c-lb-sim                format "x(3)"  space(0) "/" space(0)
     c-lb-nao                format "x(3)"  space(0) ")" skip
     c-lb-frete        at 12 format "x(5)"  space(0) ": __ (" space(0)
     c-lb-incluso[3]         format "x(7)"  space(0) "/"
     c-lb-naoincl[3]         format "x(11)" space(0) ")"
     c-lb-aprov        at 49 format "x(9)"  space(0) ": ____________" skip(1)
     c-lb-coment             format "x(11)" space(0) ":"
     c-traco3          at 14 format "x(67)" skip
     c-traco4          at 14 format "x(67)" skip
     c-traco5          at 14 format "x(67)" skip(1)
     " _________________________ ___/___/___     _________________________"
     "___/___/___" skip
     c-lb-compr        at 10 format "x(9)"
     c-lb-data[1]      at 31
     c-lb-gerente      at 49 format "x(15)"
     c-lb-data[2]      at 73 skip
     with stream-io no-box no-label width 132 frame f-fim.

form skip(1)
     c-traco1                     format "x(30)"
     c-lb-fornec                  format "x(10)"
     c-traco2                     format "x(33)" skip
     emitente.cod-emitente  at 3  "-"
     emitente.nome-abrev          "-"
     emitente.nome-emit           skip
     emitente.endereco      at 12 "-"
     emitente.bairro              skip
     emitente.cidade        at 12 "-"
     emitente.cep                 "-"
     emitente.estado              "-"
     emitente.pais                skip
     c-lb-contato[1]        at 12 format "x(7)" space(0) ":"
     cont-emit.nome               format "x(12)"
     c-lb-fone[1]                 space(0) ":"
     cont-emit.telefone                    "-"
     cont-emit.ramal              skip
     c-lb-contato[2]        at 12 format "x(7)" space(0) ":"
     b-contato.nome               format "x(12)"
     c-lb-fone[2]                 space(0) ":"
     b-contato.telefone                    "-"
     b-contato.ramal              skip
     c-lb-fax               at 12 format "x(7)" space(0) ":"
     emitente.telefax
     c-lb-telex                   format "x(5)" space(0) ":"
     emitente.telex               skip(1)
     with stream-io no-box no-label width 100 frame f-emitente.


form tt-b2b.numero-ordem at 35
     "|"                 at 45  
     tt-b2b.cod-emitente at 47
     "|"                 at 58
     tt-b2b.nome-abrev   at 60
     "|"                 at 76 
     tt-b2b.it-codigo    at 78
     WITH DOWN stream-io no-label no-box CENTERED width 100 frame f-b2b.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Ficha para Cotação de Preços").

{utp/ut-liter.i Incluso * r}
assign c-lb-incluso[1] = trim(return-value)
       c-lb-incluso[2] = trim(return-value)
       c-lb-incluso[3] = trim(return-value).
{utp/ut-liter.i NÆo_Incluso * r}
assign c-lb-naoincl[1] = trim(return-value)
       c-lb-naoincl[2] = trim(return-value)
       c-lb-naoincl[3] = trim(return-value).
{utp/ut-liter.i Data * r}
assign c-lb-data[1] = trim(return-value)
       c-lb-data[2] = trim(return-value).
{utp/ut-liter.i Comprador * r}
assign c-lb-comprador = trim(return-value).
{utp/ut-liter.i Cota‡Æo * r}
assign c-lb-cotacao = trim(return-value).
{utp/ut-liter.i Data_Cota‡Æo * r}
assign c-lb-dt-cot = trim(return-value).
{utp/ut-liter.i Valor_Frete * r}
assign c-lb-vl-fre = trim(return-value).
{utp/ut-liter.i Unid_Medida_Forn * r}
assign c-lb-un-med = trim(return-value).
{utp/ut-liter.i Encargos_Financeiros * r}
assign c-lb-encarg = trim(return-value).
{utp/ut-liter.i Pre‡o_Fornecedor * r}
assign c-lb-prc-for = trim(return-value).
{utp/ut-liter.i Taxa_Financeira * r}
assign c-lb-taxa = trim(return-value).
{utp/ut-liter.i Moeda * r}
assign c-lb-moeda = trim(return-value).
{utp/ut-liter.i Percentual_Desconto * r}
assign c-lb-perc = trim(return-value).
{utp/ut-liter.i Cota‡Æo_Moeda * r}
assign c-lb-cot-mo = trim(return-value).
{utp/ut-liter.i Condi‡Æo_Pagamento * r}
assign c-lb-cpg = trim(return-value).
{utp/ut-liter.i IPI * r}
assign c-lb-ipi = trim(return-value).
{utp/ut-liter.i Prazo_Entrega * r}
assign c-lb-prazo = trim(return-value).
{utp/ut-liter.i Al¡quota_IPI * r}
assign c-lb-al-ipi = trim(return-value).
{utp/ut-liter.i ICMS * r}
assign c-lb-icms = trim(return-value).
{utp/ut-liter.i Indust * r}
assign c-lb-indust = trim(return-value).
{utp/ut-liter.i Consumo * r}
assign c-lb-consumo = trim(return-value).
{utp/ut-liter.i Al¡quota_ICMS * r}
assign c-lb-al-icms = trim(return-value).
{utp/ut-liter.i Pre»o_Unit_Fornec * r}
assign c-lb-prc-un = trim(return-value).
{utp/ut-liter.i Al¡quota_ISS * r}
assign c-lb-al-iss = trim(return-value).
{utp/ut-liter.i Cota‡Æo_Aprovada * r}
assign c-lb-cot-apr = trim(return-value).
{utp/ut-liter.i Sim * r}
assign c-lb-sim = trim(return-value).
{utp/ut-liter.i NÆo * r}
assign c-lb-nao = trim(return-value).
{utp/ut-liter.i Frete * r}
assign c-lb-frete = trim(return-value).
{utp/ut-liter.i Aprovador * r}
assign c-lb-aprov = trim(return-value).
{utp/ut-liter.i Coment rios * r}
assign c-lb-coment = trim(return-value).
{utp/ut-liter.i Gerente_Compras * r}
assign c-lb-gerente = trim(return-value).
{utp/ut-liter.i Item * r}
assign c-lb-item = trim(return-value).
{utp/ut-liter.i Fam¡lia * r}
assign c-lb-fami = trim(return-value).
{utp/ut-liter.i Estabelecimento * r}
assign c-lb-estab = trim(return-value).
{utp/ut-liter.i Tipo_de_Relat¢rio * r}
assign c-lb-tp-rel = trim(return-value).
{utp/ut-liter.i Imprime_Ficha_sem_Fornecedor * r}
assign c-lb-ficha = trim(return-value).
{utp/ut-liter.i Imprime_Todas_as_Parcelas_de_Compra * r}
assign c-lb-parc = trim(return-value).
{utp/ut-liter.i SELE€ÇO * r}
assign c-lb-selec = trim(return-value).
{utp/ut-liter.i PAR¶METROS * r}
assign c-lb-param = trim(return-value).
{utp/ut-liter.i IMPRESSÇO * r}
assign c-lb-impr = trim(return-value).
{utp/ut-liter.i Destino * r}
assign c-lb-dest = trim(return-value).
{utp/ut-liter.i Usuÿrio * r}
assign c-lb-usuar = trim(return-value).
{utp/ut-liter.i Envia * r}
assign c-lb-envio = trim(return-value).
{utp/ut-liter.i Business-to-Business * r}
assign c-lb-b2b = trim(return-value).
create tt-param.
raw-transfer raw-param to tt-param.

for each tt-transfer:
    delete tt-transfer.
end.

for each tt-raw-digita:
    create tt-transfer.
    raw-transfer tt-raw-digita.raw-digita to tt-transfer.
end.

find first param-global no-lock no-error.

assign c-empresa  = (if avail param-global then param-global.grupo else "")
       c-programa = "CC0302"
       c-versao   = "2.04"
       c-revisao  = "00.000"
       c-traco1   = fill("-", 35)
       c-traco2   = fill("-", 36)
       c-traco3   = fill("_", 67)
       c-traco4   = fill("_", 67)
       c-traco5   = fill("_", 67).

{include/i-rpcab.i}

def var c-arq-temp as char no-undo.

ASSIGN l-ultima-compra = IF VALID-HANDLE(gwh-tg-ult-compra) THEN gwh-tg-ult-compra:CHECKED ELSE NO.

{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

if  tt-param.l-eprocurement then
    run ccp/cc0302h.p(input table tt-transfer, yes, tt-param.l-automatico).
else if  tt-param.i-class = 1 then do:
        /**
        ***   Classifica»’o: Item x Fornecedor
        **/
        if tt-param.i-tipo = 2 then
            /**
            ***  Tipo: Resumido
            **/
            run ccp/cc0302a.p (input  raw-param,
                               output table tt-b2b).
        else do for ordem-compra transaction:
            /**
            ***  Tipo: Detalhado
            **/
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
                and   b-ord.situacao     <> 4,
                first item no-lock
                where item.it-codigo  = b-ord.it-codigo
                and   item.fm-codigo >= tt-param.c-fami-i
                and   item.fm-codigo <= tt-param.c-fami-f
                break by b-ord.cod-emitente
                      by b-ord.it-codigo:
                run pi-acompanhar in h-acomp (input b-ord.it-codigo).

                find ordem-compra where rowid(ordem-compra) = rowid(b-ord) exclusive-lock no-wait no-error.
                if  locked ordem-compra then
                    next.

                for each tt-fornecedores-envio:
                    delete tt-fornecedores-envio.
                end.
                /* Caso nÆo integre bus-to-bus imprime ficha sem fornecedor */

                if  not tt-param.l-bus-to-bus then do:
                    if  ordem-compra.it-codigo = ""
                    or  ordem-compra.it-codigo = ? then do:
                        {ccp/cc0302.i2}
                         page.
                        assign r-registro = rowid(ordem-compra).
                        run ccp/cc0302b.p (input-output r-registro).
                        next.
                    end.
                    if   tt-param.l-ficha then do:
                        assign r-registro = rowid(ordem-compra).
                        run ccp/cc0302c.p (input tt-param.l-parcela, input r-registro).
                        disp c-lb-data[1]      c-lb-data[2]
                             c-lb-incluso[1]   c-lb-naoincl[1]
                             c-lb-incluso[2]   c-lb-naoincl[2]
                             c-lb-incluso[3]   c-lb-naoincl[3]
                             c-lb-cotacao      c-lb-dt-cot
                             c-lb-frete        c-lb-contato
                             c-lb-comprador    c-lb-compr
                             c-lb-encarg       c-lb-taxa
                             c-lb-gerente      c-lb-coment
                             c-lb-aprov        c-lb-moeda
                             c-lb-indust       c-lb-consumo
                             c-lb-perc         c-lb-prazo
                             c-lb-un-med       c-lb-cpg
                             c-lb-ipi          c-lb-icms
                             c-lb-sim          c-lb-nao
                             c-lb-vl-fre       c-lb-prc-un
                             c-lb-cot-apr      c-lb-cot-mo
                             c-lb-al-ipi       c-lb-al-icms
                             c-lb-al-iss       c-lb-prc-for
                             c-traco1          c-traco2
                             c-traco3          c-traco4
                             c-traco5
                             with frame f-fim.
                        page.
                        run ccp/cc0302b.p (input-output r-registro).
                        next.
                    end.
                end.

                for each  item-fornec no-lock
                    where item-fornec.it-codigo = b-ord.it-codigo
                    and   item-fornec.ativo     = yes:


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
                         no-lock no-error.

                    if  avail item-fornec-estab
                    and item-fornec-estab.ativo = no then
                        next.

                    find emitente where emitente.cod-emitente = item-fornec.cod-emitente no-lock no-error.

                    if  tt-param.l-bus-to-bus = yes
                    and avail emitente
                    &if '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                    then do:
                        find dist-emitente where dist-emitente.cod-emitente = emitente.cod-emitente no-lock no-error.
                        if  avail dist-emitente
                        and dist-emitente.parceiro-b2b then do:
                    &else
                    and      substring(emitente.char-1,11,1) = "1" then do:
                    &endif
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
                    &if '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                    end.
                    &endif
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

                        assign r-registro = rowid(ordem-compra).
                        run ccp/cc0302b.p (input-output r-registro).
                        next.
                    end.
                    for each  cotacao-item no-lock
                        where cotacao-item.data-cotacao = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 11/11/1111 &ENDIF
                        and   cotacao-item.numero-ordem = ordem-compra.numero-ordem
                        and   cotacao-item.cod-emitente = item-fornec.cod-emitente:
                        {ccp/cc0302.i1}
                        page.
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
        end.
end.
else
    run ccp/cc0302e.p (input raw-param,
                       output table tt-b2b).

find first tt-b2b no-lock no-error.
If avail tt-b2b THEN do:

    page.
    {utp/ut-liter.i Ordens * r}
    assign c-b2b = trim(return-value).
    {utp/ut-liter.i enviadas * r}
    assign c-b2b = c-b2b + " " + trim(return-value).
    {utp/ut-liter.i via * r}
    assign c-b2b = c-b2b + " " + trim(return-value) + " " + "Business-to-Business".

    put "------------------------------------------------------------" at 33 skip.
    put c-b2b at 43 skip.
    put "------------------------------------------------------------" at 33 skip. 
    {utp/ut-liter.i Ordem * r}
    assign c-bb-ordem = trim(return-value).
    {utp/ut-liter.i Fornecedor * r}
    assign c-bb-ordem = c-bb-ordem + "    | " + trim(return-value).
    {utp/ut-liter.i Nome_Abreviado * r}
    assign c-bb-ordem = c-bb-ordem + " | " + trim(return-value).
    {utp/ut-liter.i Item * r}
    assign c-bb-ordem = c-bb-ordem + "  | " + trim(return-value).

    put "------------+------------+-----------------+----------------" at 33 skip.
    put c-bb-ordem  at 36 skip.
    put "------------+------------+-----------------+----------------" at 33 skip.

    for each tt-b2b no-lock:

        IF line-counter > 60 THEN DO:
           page.
           put "------------------------------------------------------------" at 33 skip.
           put c-b2b at 43 skip.
           put "------------------------------------------------------------" at 33 skip. 
           put "------------+------------+-----------------+----------------" at 33 skip.
           put c-bb-ordem  at 36 skip.
           put "------------+------------+-----------------+----------------" at 33 skip.
        END.     
        disp tt-b2b.numero-ordem
             tt-b2b.cod-emitente
             tt-b2b.nome-abrev
             tt-b2b.it-codigo
             with frame f-b2b.
        down 1 with frame f-b2b. 
    end.

end.
run pi-finalizar in h-acomp.

page.
put unformatted
    c-lb-param       skip(1)
    c-lb-tp-rel at 5 ": " tt-param.c-tipo
    c-lb-envio  at 5 ": " tt-param.c-envio.

&IF defined(bf_dis_versao_ems) &Then
    &IF '{&BF_DIS_VERSAO_EMS}' >= '2.02' &THEN
        run utp/ut-neog.p.   /*run utp/ut-neogrid.p.*/
        IF return-value = "OK" THEN DO:
           put unformatted
               c-lb-b2b at 5 ": " tt-param.l-bus-to-bus format "Sim/Nao".
        END.  
    &ENDIF
&ENDIF 

assign l-param = tt-param.l-ficha.
put c-lb-ficha  at 5 format "x(28)" ": " l-param.

assign l-param = tt-param.l-parcela.
put c-lb-parc   at 5 format "x(35)" ": " l-param.

put Skip(1).

put unformatted
    c-lb-selec         skip(1)
    c-lb-item          at 5  ":"
    tt-param.c-item-i  at 22 "|<  >| " at 39 tt-param.c-item-f
    c-lb-estab         at 5  ":"
    tt-param.c-estab-i at 22 "|<  >| " at 39 tt-param.c-estab-f
    c-lb-compr         at 5  ":"
    tt-param.c-compr-i at 22 "|<  >| " at 39 tt-param.c-compr-f
    c-lb-ordem         at 5  ":"
    tt-param.i-ordem-i at 22 "|<  >| " at 39 tt-param.i-ordem-f
    c-lb-data[1]       at 5  ":"
    tt-param.d-data-i  at 22 "|<  >| " at 39 tt-param.d-data-f
    c-lb-fami          at 5  ":"
    tt-param.c-fami-i  at 22 "|<  >| " at 39 tt-param.c-fami-f skip(1)
    c-lb-impr          skip(1)
    c-lb-dest          at 5  ": " tt-param.c-destino " - " tt-param.arquivo
    c-lb-usuar         at 5  ": " tt-param.usuario.

{include/i-rpclo.i}

{include/pi-edit.i}
