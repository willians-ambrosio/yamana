/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i CC0305C 2.00.00.072}  /*** 010072 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i cc0305c MCC}
&ENDIF
/*****************************************************************************
**
**       PROGRAMA: CC0305c
**
**       DATA....: Abril DE 1997
**
**       OBJETIVO: Emissao de Pedido de Compras
**
**       VERSAO..: 2.00.000 - Sandra Stadelhofer
**
******************************************************************************
**      ALTERAÄ«O: Adaptado para MFB em Agosto/03
******************************************************************************/
{cdp/cdcfgmat.i}
{utp/utapi009.i}


def input param raw-param           as raw     no-undo.
def input param r-pedido-compr      as rowid   no-undo.
def input param l-primeiro-emitente as logical no-undo.
def input param l-ultimo-emitente   as logical no-undo.
def input param l-ultimo-cod-emit   as logical no-undo.

def temp-table tt-b2b no-undo
    field nr-pedido          like ordem-compra.num-pedido
    field cod-emitente       like pedido-compr.cod-emitente
    field nome-abrev         like emitente.nome-abrev
    field numero-ordem       like ordem-compra.numero-ordem
    field it-codigo          like ordem-compra.it-codigo
    index pedido-emitente is primary 
          nr-pedido
          cod-emitente.

define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char
    field diretorio         as char
    field usuario           as char
    field data-exec         as date
    field hora-exec         as integer
    field i-pedi-i          as integer
    field i-pedi-f          as integer
    field l-narrativa-item  as logical
    field l-narrativa-ordem as logical
    field l-bus-to-bus      as logical
    field l-descricao       as logical
    field l-impressao       as logical
    field i-param-c         as integer
    field i-ordem-ini       like ordem-compra.numero-ordem
    field i-ordem-fim       like ordem-compra.numero-ordem
    field l-envia           as logical
    field c-destino         as char  
    field l-eprocurement    as LOGICAL
    FIELD l-integra-portal  AS LOGICAL
    FIELD l-ped-compra      AS LOG
    FIELD l-gera-arq-local  AS LOG
    FIELD c-arq-ped-compra  AS CHAR.

/* temp-table para envio dos paramentros para o multiplanta */
define temp-table tt-param-mp no-undo
    field i-ordem-ini       like ordem-compra.numero-ordem
    field i-ordem-fim       like ordem-compra.numero-ordem
    field sit-ordem-contrat like ordem-compra.sit-ordem-contrat.

define temp-table tt_log_erro no-undo
     field ttv_num_cod_erro  as integer   initial ?
     field ttv_des_msg_ajuda as character initial ?
     field ttv_des_msg_erro  as character initial ?.

define temp-table tt_log_erro_aux no-undo like tt_log_erro
       field ttv_tipo as integer initial ?.

def temp-table tt-raw-digita no-undo
    field raw-digita as raw.

{include/tt-edit.i}

{ccp/ccapi201.i}
{ccp/ccapi202.i}
{ccp/ccapi203.i}
{ccp/ccapi204.i}
{ccp/ccapi205.i}
{ccp/ccapi206.i}
{ccp/ccapi207.i}
{cdp/cd0666.i}

def var c-mensagem-e-mail as char format "x(38)" no-undo.
def var c-titulo-erro     as char format "x(56)" no-undo.
def var c-cod             as char format "x(8)"  no-undo.
def var c-desc            as char format "x(13)" no-undo.
def var c-trc-cod         as char format "x(10)" no-undo.
def var c-trc-desc        as char format "x(80)" no-undo.
def var c-cgc             as char format "x(18)".
def var c-cgc-emit        as char format "x(19)".

def buffer b-ord for ordem-compra.

{utp/ut-glob.i}
DEF NEW GLOBAL SHARED VAR l-integra-ems-his AS LOGICAL NO-UNDO.

/*------------ Envio e-mail -------------*/
/* def stream str. */
def var c-arq            as char no-undo.
def var c-assunto-e-mail as char no-undo.
/*---------------------------------------*/

/************* Multi-Planta ***************/
def var i-tipo-movto        as integer no-undo.
def var l-cria              as logical no-undo.

{cdp/cd7300.i1}
{cdp/cd7300.i2}
/**************** Fim *********************/

create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpvar.i}

{ccp/cc0305.i2}
{ccp/cc0305.i6}

form header
     fill("-", 90)       format "x(90)" skip
     c-lb-total    at 52 space(0) ":"
     de-tot-ger    at 66
     c-msg         at 72 skip
     with no-box no-label page-bottom width 95 stream-io frame f-total.

form c-titulo-erro      at 01 skip(1)
     c-cod              at 01
     c-desc             at 13
     c-trc-cod          at 01
     c-trc-desc         at 13
     tt-erros.cod-erro  at 01 format ">>>>>>>>>9"
     tt-erros.desc-erro at 13 format "x(80)"
     with stream-io no-label width 132 frame f-erros-e-mail.

form pedido-compr.num-pedido      colon 24 skip
     c-natureza                   colon 24 skip
     pedido-compr.data-pedido     colon 24 skip
     pedido-compr.cod-cond-pag    colon 24 "-"
     c-desc-var-1                 no-label skip
     /*c-desc-var-2                 at 32 no-label*/
     pedido-compr.cod-transp      colon 24 "-"
     transporte.nome-abrev        no-label 
     transporte.telefone          COLON 60 skip
     pedido-compr.frete           colon 24 skip
     cotacao-item.codigo-icm      colon 24 skip(1)
     emitente.cod-emitente        colon 13 "-"
     emitente.nome-emit           no-label "-"
     emitente.nome-abrev          no-label skip
     emitente.endereco            colon 13 "-"
     emitente.bairro              colon 13 skip
     emitente.cidade              colon 13 "-"
     emitente.cep                 no-label "-"
     emitente.estado              no-label "-"
     emitente.pais                no-label
     emitente.caixa-postal        colon 13 label "x" skip
     emitente.telefone[1]         colon 13 label "x" " / "
     emitente.ramal[1]            no-label space(4)
     emitente.telefone[2]         no-label " / "
     emitente.ramal[2]            no-label
     emitente.telef-fac           colon 13 " / "
     emitente.telefax             no-label 
     emitente.telex               colon 13 skip
     emitente.cgc                 colon 13 "-"
     emitente.ins-estadual        skip(1)
     estabelec.cod-estabel        colon 13 LABEL 'Destinatario' "-"
     estabelec.nome               no-label skip
     estabelec.endereco           colon 13 "-"
     estabelec.bairro             skip
     estabelec.Cidade             colon 13 "-"
     estabelec.cep                no-label "-"
     estabelec.estado             no-label "-"
     estabelec.pais               no-label skip
     estabelec.cgc                colon 13 "-"
     estabelec.ins-estadual       skip(1)
     with no-box side-label 1 down width 95 stream-io frame f-pedido.

form c-desc-contrato              colon 24 skip
     pedido-compr.num-pedido      colon 24 skip
     c-natureza                   colon 24 skip
     pedido-compr.data-pedido     colon 24 skip
     pedido-compr.cod-cond-pag    colon 24 "-"
     c-desc-var-1                 no-label skip
     /*c-desc-var-2                 at 32 no-label*/
     pedido-compr.cod-transp      colon 24 "-"
     transporte.nome-abrev        no-label 
     transporte.telefone          COLON 60 skip
     pedido-compr.frete           colon 24 SKIP
     cotacao-item.codigo-icm      colon 24 skip(1)
     emitente.cod-emitente        colon 13 "-"
     emitente.nome-emit           no-label "-"
     emitente.nome-abrev          no-label skip
     emitente.endereco            colon 13 "-"
     emitente.bairro              colon 13 skip
     emitente.cidade              colon 13 "-"
     emitente.cep                 no-label "-"
     emitente.estado              no-label "-"
     emitente.pais                no-label
     emitente.caixa-postal        colon 13 label "x" skip
     emitente.telefone[1]         colon 13 label "x" " / "
     emitente.ramal[1]            no-label space(4)
     emitente.telefone[2]         no-label " / "
     emitente.ramal[2]            no-label
     emitente.telef-fac           colon 13 " / "
     emitente.telefax             no-label 
     emitente.telex               colon 13 skip
     emitente.cgc                 colon 13 "-"
     emitente.ins-estadual        skip(1)
     estabelec.cod-estabel        colon 13 LABEL 'Destinatario' "-"
     estabelec.nome               no-label skip
     estabelec.endereco           colon 13 "-"
     estabelec.bairro             skip
     estabelec.Cidade             colon 13 "-"
     estabelec.cep                no-label "-"
     estabelec.estado             no-label "-"
     estabelec.pais               no-label skip
     estabelec.cgc                colon 13 "-"
     estabelec.ins-estadual       skip(1)
     with no-box side-label 1 down width 95 stream-io frame f-contrato.

form c-lb-ordem      at 1
     /*c-lb-item-for   at 11*/
     c-lb-item       at 11
     c-lb-ct-cont    at 32
     c-lb-desc       at 50
     c-lb-taxa       at 63
     c-lb-ipi        at 75
     c-lb-vl-ipi     at 85
     c-lb-pa         at 5
     c-lb-qtde       at 13
     c-lb-un         at 24
     c-lb-prc-unit   at 30
     c-lb-vl-desc    at 47
     c-lb-vl-enc     at 63
     c-lb-prc-tot    at 84
     c-lb-data       at 97 
     c-lb-ref        at 108 skip
     "----------------------------------------------------------------" at 1
     "-------------------------------------------------------------" at 65 skip
     with no-box no-label width 132 stream-io frame f-labels.

form c-descricao at 4
     with no-box no-label width 132 stream-io frame f-descricao.

form tt-editor.conteudo at 4
     with no-box no-label width 132 stream-io frame f-narrativa.

form tt-b2b.nr-pedido    at 32
     "|"                 at 42  
     tt-b2b.cod-emitente at 44
     "|"                 at 55
     tt-b2b.nome-abrev   at 57
     "|"                 at 73 
     tt-b2b.numero-ordem at 75
     "|"                 at 87
     tt-b2b.it-codigo    at 89
     WITH DOWN stream-io no-label no-box CENTERED width 132 frame f-b2b.

form header c-traco[1] at 3
     c-traco[2] at 43
     c-traco[3] at 83
     c-nome[1]  at 5
     c-nome[2]  at 45
     c-nome[3]  at 85
     c-cargo[1] at 5
     c-cargo[2] at 45
     c-cargo[3] at 85
     with page-bottom no-box no-label width 132 stream-io frame f-assinatura.

form "Numero do Pedido" at 10
     "|"                at 27
     "Emitente"         at 29
     "|"                at 38
     "Status"           at 40
     with 1 down stream-io no-label no-box centered width 100 frame f-pedido-compr-head.

{utp/ut-liter.i Contrato * r}
assign c-desc-contrato:label in frame f-contrato = trim (return-value).
{utp/ut-liter.i Natureza * r}
assign c-natureza:label in frame f-pedido = trim(return-value)
       c-natureza:label in frame f-contrato = trim(return-value).
{utp/ut-liter.i Destino * r}
assign cotacao-item.codigo-icm:label in frame f-pedido = trim(return-value)
       cotacao-item.codigo-icm:label in frame f-contrato = trim(return-value).
{utp/ut-liter.i Cx.Pos. * r}
assign emitente.caixa-postal:label in frame f-pedido = trim(return-value)
       emitente.caixa-postal:label in frame f-contrato = trim(return-value).
{utp/ut-liter.i Fax * r}
assign emitente.telef-fac:label in frame f-pedido = trim(return-value)
       emitente.telef-fac:label in frame f-contrato = trim(return-value).
{utp/ut-liter.i Destinatario * r}
assign estabelec.cod-estabel:label in frame f-pedido = trim(return-value)
       estabelec.cod-estabel:label in frame f-contrato = trim(return-value).
{utp/ut-liter.i Telefone * r}
assign emitente.telefone[1]:label in frame f-pedido = trim(return-value)
       emitente.telefone[1]:label in frame f-contrato = trim(return-value).       
{utp/ut-liter.i Fornecedor * r}
assign emitente.cod-emitente:label in frame f-pedido = trim(return-value)
       emitente.cod-emitente:label in frame f-contrato = trim(return-value).

find first param-compra no-lock no-error.
find first param-global no-lock no-error.

{utp/ut-liter.i Emiss∆o_Pedidos * L}
assign c-assunto-e-mail = return-value.
{utp/ut-liter.i Arquivo_Anexo_com_dados_do_Pedido: * L}
assign c-mensagem-e-mail = trim(return-value).

assign c-empresa  = (if avail param-global then param-global.grupo else "")
       l-branco   = no.

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Emiss∆o_de_Pedido_de_Compras * r}
assign c-titulo-relat = trim(return-value).

{include/i-rpcab.i}

/* {include/i-rpout.i} */

find first tt-param no-lock no-error.


find pedido-compr where rowid(pedido-compr) = r-pedido-compr exclusive-lock no-error no-wait.

    if  not avail pedido-compr then return "NOK".
    if  locked    pedido-compr then return "NOK".


    if  l-primeiro-emitente = yes then do:        
        output to value(SESSION:TEMP-DIRECTORY + string(pedido-compr.cod-emitente) + ".txt")
            paged page-size 64 convert target 'iso8859-1'.
        view  frame f-cabec.
        view  frame f-rodape.
    end.
    else do:
        output to value(SESSION:TEMP-DIRECTORY + string(pedido-compr.cod-emitente) + ".txt") append
            paged page-size 64 convert target 'iso8859-1'.
        view  frame f-cabec.
        view  frame f-rodape.
    end.

     run utp/ut-acomp.p persistent set h-acomp.
     {utp/ut-liter.i "Emiss∆o_de_Pedidos" *}
     run pi-inicializar in h-acomp (input return-value).
        
     run pi-acompanhar in h-acomp (input string(pedido-compr.num-pedido)).

    
    assign c-pe-aux        = pedido-compr.num-pedido
           de-tot-ger      = 0
           de-total        = 0
           de-total-pedido = 0
           c-desc-var-1    = ""
           c-desc-var-2    = ""
           c-natureza      = {ininc/i01in274.i 04 pedido-compr.natureza}.
    find transporte
        where transporte.cod-transp = pedido-compr.cod-transp no-lock no-error.
    find cond-pagto
        where cond-pagto.cod-cond-pag = pedido-compr.cod-cond-pag
        no-lock no-error.
    if  avail cond-pagto then do:
        if  cond-pagto.cod-cond-pag = 0 then do:
            {utp/ut-liter.i Espec≠fica * r}
            assign c-desc-var-1 = trim(return-value)
                   c-desc-var-2 = "".
        end.
         ELSE
            assign c-desc-var-1 = cond-pagto.descricao
                   c-desc-var-2 = "".
     END.

     find transporte where transporte.cod-transp   = pedido-compr.cod-transp   no-lock no-error.
     find cond-pagto where cond-pagto.cod-cond-pag = pedido-compr.cod-cond-pag no-lock no-error.
 
    find emitente
        where emitente.cod-emitente = pedido-compr.cod-emitente no-lock no-error.
    find estabelec
        where estabelec.cod-estabel = pedido-compr.end-entrega no-lock no-error.

    assign c-cgc = if param-global.formato-id-federal <> "" 
                   then string(estabelec.cgc, param-global.formato-id-federal)
                   else estabelec.cgc
           c-cgc-emit = if param-global.formato-id-federal <> ""
                        then string(emitente.cgc,param-global.formato-id-federal)
                        else emitente.cgc.

    assign l-ordem    = no
           l-imprimiu = no.


    for each ordem-compra use-index pedido
        where ordem-compra.num-pedido = pedido-compr.num-pedido
        and   ordem-compra.situacao  <> 4
        and   ordem-compra.situacao  <> 6 no-lock:

        /* Integraá∆o M¢dulo de Contratos */
        &if defined(bf_mat_contratos) &then
        if param-global.modulo-cn and ordem-compra.nr-contrato <> 0 then do:
        &else
        if param-global.modulo-cn and ordem-compra.nr-contrato <> "" then do:
        &endif
           if ordem-compra.numero-ordem < tt-param.i-ordem-ini or
              ordem-compra.numero-ordem > tt-param.i-ordem-fim then next.
           find contrato-for where contrato-for.nr-contrato =
                ordem-compra.nr-contrato exclusive-lock no-error.
           if avail contrato-for then do:
                assign c-desc-contrato = contrato-for.des-contrat
                       contrato-for.ind-sit-contrat = 2.       

              if tt-param.i-param-c = 1 and
                 ordem-compra.sit-ordem-contrat <> 2 then next.
              else
                    if  tt-param.i-param-c = 2
                    and ordem-compra.sit-ordem-contrat <> 1 then next.
              else
                        if  tt-param.i-param-c = 3
                        and ordem-compra.sit-ordem-contrat = 3 then next.           
           end.
        end.


        /* Fim Integraá∆o Contratos */

        find first cotacao-item
            where cotacao-item.numero-ordem = ordem-compra.numero-ordem
            and   cotacao-item.cod-emitente = ordem-compra.cod-emitente
            and   cotacao-item.cot-aprovada no-lock no-error.
        find item-fornec
            where item-fornec.it-codigo    = ordem-compra.it-codigo
            and   item-fornec.cod-emitente = pedido-compr.cod-emitente
            no-lock no-error.
        find item
            where item.it-codigo = ordem-compra.it-codigo no-lock no-error.   
        find first narrativa
            where narrativa.it-codigo = item.it-codigo no-lock no-error.
        find item
            where item.it-codigo = ordem-compra.it-codigo no-lock no-error.
        assign c-descricao = "".
        if  tt-param.l-descricao
        and avail item then
            assign c-descricao = substring(item.desc-item,1,59).

        if  ordem-compra.mo-codigo > 0 then do:
            run cdp/cd0812.p (input  ordem-compra.mo-codigo,
                              input  0,
                              input  ordem-compra.preco-fornec,
                              input  pedido-compr.data-pedido,
                              output de-preco-conv).
            if  de-preco-conv = ? then
                assign de-preco-conv = ordem-compra.preco-fornec.
        end.
        else
            assign de-preco-conv = ordem-compra.preco-fornec.
        assign de-preco-unit   = de-preco-conv
               de-ipi-tot      = 0.

         if avail estabelec then
             run pi-mostra-dados.

        for each prazo-compra
            where prazo-compra.numero-ordem  = ordem-compra.numero-ordem
            and   prazo-compra.situacao     <> 4
            and   prazo-compra.situacao     <> 6 no-lock:
            assign de-preco-tot-aux = de-preco-conv * prazo-compra.qtd-sal-forn
                   de-preco-calc    = 0
                   de-desc          = 0
                   de-enc           = 0
                   de-ipi           = 0.

            if  param-compra.ipi-sobre-preco = 2 then do:
                if  ordem-compra.perc-descto > 0 then
                    assign de-desc = round(de-preco-tot-aux
                                   * ordem-compra.perc-descto / 100,2).
                if  ordem-compra.taxa-financ = no then do:
                    assign de-enc = de-preco-tot-aux - de-desc.
                    run ccp/cc9020.p (input  yes,
                                      input  ordem-compra.cod-cond-pag,
                                      input  ordem-compra.valor-taxa,
                                      input  ordem-compra.nr-dias-taxa,
                                      input  de-enc,
                                      output de-preco-calc).
                    assign de-enc = round(de-preco-calc - de-enc,2).
                end.
                else assign de-preco-calc = de-preco-tot-aux - de-desc.

                if  ordem-compra.aliquota-ipi > 0
                and ordem-compra.codigo-ipi   = no then
                    assign de-ipi     = de-preco-calc
                                      * ordem-compra.aliquota-ipi / 100
                           de-ipi-tot = de-ipi-tot + de-ipi.
            end.
            else do:
                if  ordem-compra.taxa-financ = no then do:
                    run ccp/cc9020.p (input  yes,
                                      input  ordem-compra.cod-cond-pag,
                                      input  ordem-compra.valor-taxa,
                                      input  ordem-compra.nr-dias-taxa,
                                      input  de-preco-tot-aux,
                                      output de-preco-calc).
                    assign de-enc = round(de-preco-calc - de-preco-tot-aux,2).
                end.
                else
                    assign de-preco-calc = de-preco-tot-aux.

                if  ordem-compra.aliquota-ipi > 0
                and ordem-compra.codigo-ipi   = no then
                    assign de-ipi = de-preco-calc
                                  * ordem-compra.aliquota-ipi / 100
                           de-ipi-tot = de-ipi-tot + de-ipi.
                if  ordem-compra.perc-descto > 0 then
                    assign de-desc = round((de-preco-calc + de-ipi)
                                   * ordem-compra.perc-descto / 100,2).
            end.
            assign de-preco-total  = de-preco-tot-aux + de-enc + de-ipi - de-desc
                   de-total-pedido = de-total-pedido  + de-preco-total.
        end.

        for each prazo-compra
            where prazo-compra.numero-ordem =  ordem-compra.numero-ordem
            and   prazo-compra.situacao    <> 4
            and   prazo-compra.situacao    <> 6 no-lock:

            if  ordem-compra.taxa-financ  then
                assign c-tax-aux = "%"
                       de-enc    = 0.
            else
                assign c-tax-aux = "%".
            assign de-preco-tot-aux = de-preco-conv * prazo-compra.qtd-sal-forn
                   de-preco-calc    = 0
                   de-desc          = 0
                   de-enc           = 0
                   de-ipi           = 0.
            if  param-compra.ipi-sobre-preco = 2 then do:
                if  ordem-compra.perc-descto > 0 then
                    assign de-desc = round(de-preco-tot-aux
                                   * ordem-compra.perc-descto / 100,2).
                if  ordem-compra.taxa-financ = no then do:
                    assign de-enc = de-preco-tot-aux - de-desc.
                    run ccp/cc9020.p (input  yes,
                                      input  ordem-compra.cod-cond-pag,
                                      input  ordem-compra.valor-taxa,
                                      input  ordem-compra.nr-dias-taxa,
                                      input  de-enc,
                                      output de-preco-calc).
                    assign de-enc = round(de-preco-calc - de-enc,2).
                end.
                else de-preco-calc = de-preco-tot-aux - de-desc.

                if  ordem-compra.aliquota-ipi > 0
                and ordem-compra.codigo-ipi = no then
                    assign de-ipi = de-preco-calc
                                  * ordem-compra.aliquota-ipi / 100.
            end.
            else do:
                if  ordem-compra.taxa-financ = no then do:
                    run ccp/cc9020.p (input  yes,
                                      input  ordem-compra.cod-cond-pag,
                                      input  ordem-compra.valor-taxa,
                                      input  ordem-compra.nr-dias-taxa,
                                      input  de-preco-tot-aux,
                                      output de-preco-calc).
                    assign de-enc = round(de-preco-calc
                                  - de-preco-tot-aux,2).
                end.
                else assign de-preco-calc = de-preco-tot-aux.

                if  ordem-compra.aliquota-ipi > 0
                and ordem-compra.codigo-ipi = no then
                    assign de-ipi = de-preco-calc
                                  * ordem-compra.aliquota-ipi / 100.
                if  ordem-compra.perc-descto > 0 then
                    assign de-desc = round((de-preco-calc + de-ipi)
                                   * ordem-compra.perc-descto / 100,2).
            end.

            {utp/ut-liter.i Continua * r}
            assign c-msg = trim(return-value)
                   de-preco-total = de-preco-tot-aux + de-enc + de-ipi - de-desc.

            view  frame f-total.

            assign de-tot-ger = de-tot-ger + de-preco-total.
            disp
                 prazo-compra.parcela       at 2 space(0)
                 prazo-compra.qtd-sal-forn  format ">>>>>>>,>>9.9999" space(1)
                 cotacao-item.un            when available cotacao-item space(0)
                 de-preco-unit              space(0)
                 de-desc                    space(0)
                 de-enc                     space
                 de-preco-total             space(2)
                 prazo-compra.data-entrega  space(1)
                 prazo-compra.cod-refer
                 with no-box no-label width 132 stream-io frame f-parcela.


            assign l-ordem = yes.
        end.

         if line-counter > page-size THEN 
            PAGE.
         ELSE 
            IF line-counter > (page-size - 6) AND 
               (tt-param.l-narrativa-item or tt-param.l-narrativa-ordem) THEN 
               PAGE.

         IF pedido-compr.num-pedido <> c-pe-aux AND l-branco = YES then do:
            PAGE.

            {utp/ut-liter.i Continua... * r}

            ASSIGN c-msg      = TRIM(RETURN-VALUE)
                   de-tot-ger = 0
                   de-total   = 0.
         end.
         /*assign l-branco = yes.*/

         /****** Impress∆o dos Dados da Narrativa ******/ 
         if  tt-param.l-narrativa-item  then do:
             find first narrativa where narrativa.it-codigo = item.it-codigo no-lock no-error. 
             if  avail narrativa 
             and narrativa.descricao <> "" then do:
                 put unformatted  SKIP(1)
                     fill("-", 20) at 4 format "x(20)"
                     c-lb-narra-it  
                     fill("-", 20) format "x(20)".
                 run pi-print-editor (input narrativa.descricao, input 120).
                 for each tt-editor with stream-io frame f-narrativa:
                     disp tt-editor.conteudo with stream-io frame f-narrativa.
                     down  with stream-io frame f-narrativa.
                     if line-counter > (page-size - 4) then do:
                        page.
                     END.   
                 end.                                               
             end.
         end.

         if  tt-param.l-narrativa-ordem
         and ordem-compra.narrativa <> "" then do:
             put unformatted SKIP(1)
                 fill("-", 24) at 4 format "x(24)"
                 c-lb-narra-ord  
                 fill("-", 24) format "x(24)".
             run pi-print-editor (input ordem-compra.narrativa, input 120).
             for each tt-editor with stream-io frame f-narrativa:
                 disp tt-editor.conteudo with stream-io frame f-narrativa.
                 down  with frame f-narrativa.

                 if line-counter > (page-size - 3) then do:
                    page.
                 end.   
             end.                    
         end.                               

        assign c-pe-aux = pedido-compr.num-pedido
               l-ordem  = no.
         if  line-counter > page-size - 2 then page. else put skip(1).

        run ccp/cc0305b.p (input rowid(ordem-compra)). /* atualiza ordem */        
        if avail contrato-for and
           ordem-compra.nr-contrato = contrato-for.nr-contrato and
           ordem-compra.sit-ordem-contrat = 1  then 
           assign ordem-compra.sit-ordem-contrat = 2.
    end.
    if  l-imprimiu  then do:
        if  pedido-compr.cod-cond-pag = 0 then do:
            find first cond-especif
                where pedido-compr.num-pedido = cond-especif.num-pedido
                no-lock no-error.
            if  avail cond-especif then
                disp skip(1)
                     "----------------------------- "
                   + c-lb-cond-pag
                   + " -------------------------------" skip
                     data-pagto[1] at 6 "-"
                     perc-pagto[1] no-label "%" at 41
                     data-pagto[2] "-"
                     perc-pagto[2] no-label "%" at 78 skip
                     data-pagto[3] at 6 "-"
                     perc-pagto[3] no-label "%" at 41
                     data-pagto[4] "-"
                     perc-pagto[4] no-label "%" at 78 skip
                     data-pagto[5] at 6 "-"
                     perc-pagto[5] no-label "%" at 41
                     data-pagto[6] "-"
                     perc-pagto[6] no-label "%" at 78 skip
                     data-pagto[7] at 6 "-"
                     perc-pagto[7] no-label "%" at 41
                     data-pagto[8] "-"
                     perc-pagto[8] no-label "%" at 78 skip
                     data-pagto[9] at 6 "-"
                     perc-pagto[9] no-label "%" at 41
                     data-pagto[10] "-"
                     perc-pagto[10] no-label "%" at 78 skip
                     data-pagto[11] at 6 "-"
                     perc-pagto[11] no-label "%" at 41
                     data-pagto[12] "-"
                     perc-pagto[12] no-label "%" at 78 skip(1)
                     substring(cond-especif.comentarios[1],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[2],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[3],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[4],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[5],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[6],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[7],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[8],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[9],1,72)  format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[10],1,72) format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[11],1,72) format "x(72)" no-label at 6 
                     substring(cond-especif.comentarios[12],1,72) format "x(72)" no-label at 6 
                     with no-box side-label width 95 stream-io frame f-cond-pagto-esp.

        end.
        find mensagem
            where pedido-compr.cod-mensagem = mensagem.cod-mensagem
            no-lock no-error.
        assign l-com1 = no.
        if  pedido-compr.comentarios <> "" then
            assign l-com1 = yes.
        if (line-counter > (page-size - 2) and l-com1) or (line-counter > page-size - 10) then do:
            page.
            view frame f-total.
        end.
        if  avail mensagem then do:
           run pi-print-editor (input mensagem.texto-mensag, input 120).
            for each tt-editor:
               if line-counter > (page-size - 1) then do:
                  page.
                  
                  put "-------------------- " at 4
                       c-lb-msg               at 25 format "x(18)"
                       " -------------------" at 43.
               end.   
               
                if  tt-editor.linha = 1 then
                     put "-------------------- " at 4
                         c-lb-msg               at 25 format "x(18)"
                         " -------------------" at 43.
                disp tt-editor.conteudo when avail tt-editor
                     with frame f-narrativa.
                down with frame f-narrativa.
            end.
        end.
        if (line-counter > (page-size - 2) and l-com1) or (line-counter > (page-size - 4)) then do:
            page.
            view frame f-total.
            disp c-lb-ordem
                /*c-lb-item-for*/
                 c-lb-item
                 c-lb-ct-cont
                 c-lb-desc
                 c-lb-taxa
                 c-lb-ipi
                 c-lb-vl-ipi
                 c-lb-pa
                 c-lb-qtde
                 c-lb-un
                 c-lb-prc-unit
                 c-lb-vl-desc
                 c-lb-vl-enc
                 c-lb-prc-tot
                 c-lb-data
                 c-lb-ref
                 with frame f-labels.
        end.
        if  l-com1 then do:
           run pi-print-editor (input pedido-compr.comentarios, input 120).
            for each tt-editor:
               down  with frame f-narrativa.
               if line-counter > (page-size - 1) then do:
                  page.
                  put "-------------------- " at 4
                      c-lb-coment            at 25 format "x(20)"
                      " -------------------" at 45.
               end.   
               
                if  tt-editor.linha = 1 then
                     put "-------------------- " at 4
                         c-lb-coment            at 25 format "x(20)"
                         " -------------------" at 45.
                disp tt-editor.conteudo when avail tt-editor
                     with frame f-narrativa.
                down with frame f-narrativa.
            end.
        end.
        assign l-r2 = (de-total-pedido > param-compra.limite[1]).
               l-r3 = (de-total-pedido > param-compra.limite[2]).
        if  (line-counter > (page-size - 12)) then do:
            page.
            view frame f-total.
        end.


        assign c-traco[1] = fill("-",36)
               c-traco[2] = ""
               c-traco[3] = ""
               c-nome[1]  = ""
               c-nome[2]  = ""
               c-nome[3]  = ""
               c-cargo[1] = ""
               c-cargo[2] = ""
               c-cargo[3] = "".

        FIND usuar-mater WHERE
             usuar-mater.cod-usuario = pedido-compr.responsavel
             NO-LOCK NO-ERROR.

        ASSIGN c-traco[1] = ''
               c-nome[1]  = 'COMPRADOR: ' + IF AVAIL usuar-mater THEN usuar-mater.nome-usuar  ELSE pedido-compr.responsavel
               c-cargo[1] = ' TELEFONE: ' + IF AVAIL usuar-mater THEN usuar-mater.telefone[1] ELSE ''.

        /* Imprime as Assinaturas (3) uma ao lado da outra */
        view frame f-assinatura.

        assign c-msg        = ""
               de-total     = de-total-pedido
               i-tipo-movto = 0.

        run ccp/cc0305a.p (rowid(pedido-compr)).

        {ccp/cc0305.i5}
        page.
     end. /* imprimiu */

     find first tt-b2b no-lock no-error.
     If avail tt-b2b THEN do:
        page.
        assign c-b2b = "Pedidos Enviados via Business-to-Business".
        
        put "-----------------------------------------------------------------------" at 30 skip.
        put c-b2b at 47 skip.
        put "-----------------------------------------------------------------------" at 30 skip. 
        {utp/ut-liter.i Pedido * r}
        assign c-bb-ordem = trim(return-value).
        {utp/ut-liter.i Emitente * r}
        assign c-bb-ordem = c-bb-ordem + "   | " + trim(return-value).
        {utp/ut-liter.i Nome_Abreviado * r}
        assign c-bb-ordem = c-bb-ordem + "   | " + trim(return-value).
        {utp/ut-liter.i Ordem * r}
        assign c-bb-ordem = c-bb-ordem + "  | " + trim(return-value).
        {utp/ut-liter.i Item * r}
        assign c-bb-ordem = c-bb-ordem + "       | " + trim(return-value).
    
        put "------------+------------+-----------------+-------------+-------------" at 30 skip.
        put c-bb-ordem  at 33 skip.
        put "------------+------------+-----------------+-------------+-------------" at 30 skip.
    
        for each tt-b2b no-lock
            break by tt-b2b.nr-pedido:
            
            IF line-counter > page-size - 2 THEN DO:
               page.
               put "-----------------------------------------------------------------------" at 30 skip.
               put c-b2b at 47 skip.
               put "-----------------------------------------------------------------------" at 30 skip. 
               put "------------+------------+-----------------+-------------+-------------" at 30 skip.
               put c-bb-ordem  at 33 skip.
               put "------------+------------+-----------------+-------------+-------------" at 30 skip.
            END.
            if first-of(tt-b2b.nr-pedido) then
                disp tt-b2b.nr-pedido
                     tt-b2b.cod-emitente
                     tt-b2b.nome-abrev
                     with frame f-b2b.
    
            disp tt-b2b.numero-ordem
                 tt-b2b.it-codigo
                 with frame f-b2b.
            down 1 with frame f-b2b.         
        end.                                    
    end.
    page.
    hide frame f-total.

if  l-ultimo-cod-emit <> yes
or  l-ultimo-emitente <> yes then do:
    run pi-finalizar in h-acomp.
    return.    
end.    

hide frame f-total.


if tt-param.l-impressao then do:
   put unformatted c-lb-tit-par skip(1).

       assign l-param = tt-param.l-narrativa-item.
    put c-lb-narra-it  at 5 format "x(9)" ": " l-param.
    assign l-param = tt-param.l-narrativa-ordem.
    put c-lb-narra-ord  at 5 format "x(9)" ": " l-param.
    assign l-param = tt-param.l-descricao.
    put c-lb-descr  at 5 format "x(9)" ": " l-param.
    assign l-param = tt-param.l-bus-to-bus.
    put c-lb-bus-to-bus at 5 format "x(20)" ": " l-param skip(1).

    put c-lb-tit-sel      skip(1)
        c-lb-pedido       at 5  ":"
        tt-param.i-pedi-i at 15 " |<  >| " tt-param.i-pedi-f skip(1).

    if param-global.modulo-cn then do:
       put c-lb-ordem         at 5  "   :"
           tt-param.i-ordem-ini at 16 " |<  >| " tt-param.i-ordem-fim skip(1).
    end.

    put unformatted    
        c-lb-tit-imp      skip(1)
        c-lb-destino      at 5 ": " tt-param.c-destino " - " tt-param.arquivo
        c-lb-usuario      at 5 ": " tt-param.usuario.
end.
OUTPUT CLOSE.

     if l-ultimo-emitente then do:
        if emitente.e-mail <> "":U then do:
           find usuar-mater where usuar-mater.cod-usuario = pedido-compr.responsavel
               no-lock no-error.
           create tt-envio.
           assign tt-envio.versao-integracao = 1
                  tt-envio.exchange    = param-global.log-1
                  tt-envio.remetente   = if  avail usuar-mater then usuar-mater.e-mail else pedido-compr.responsavel
                  tt-envio.destino     = emitente.e-mail
                  tt-envio.assunto     = c-assunto-e-mail
                  tt-envio.mensagem    = c-mensagem-e-mail
                  tt-envio.importancia = 2
                  tt-envio.log-enviada = yes
                  tt-envio.log-lida    = yes
                  tt-envio.acomp       = yes
                  tt-envio.arq-anexo   = SESSION:TEMP-DIRECTORY + string(emitente.cod-emitente) + ".txt".

            run utp/utapi009.p (input  table tt-envio,
                                output table tt-erros).
        end.                                    
     end.

find first tt-erros no-lock no-error.
if avail tt-erros then do:
   page.   
   disp c-titulo-erro
        c-cod
        c-desc 
        c-trc-cod
        c-trc-desc
        with frame f-erros-e-mail WIDTH 320.
   for each tt-erros no-lock:
       MESSAGE tt-erros.desc-erro
             VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Atená∆o".
       disp tt-erros.cod-erro 
            tt-erros.desc-erro
            with frame f-erros-e-mail WIDTH 320.
   end.
end.


{include/pi-edit.i}

run pi-finalizar in h-acomp.
output close.

PROCEDURE pi-mostra-dados:
    if line-counter < 15 then do:
        IF NOT l-imprimiu THEN DO:
            &if defined(bf_mat_contratos) &then
            if pedido-compr.nr-contrato <> 0 then
            &else
            if pedido-compr.nr-contrato <> "" then
            &endif
                {ccp/cc0305.i3} /* Display do cabeáalho do pedido */
            else
                {ccp/cc0305.i1} /* Display do cabeáalho do pedido */
            assign l-imprimiu = yes.
        END.

        disp c-lb-ordem   /*c-lb-item-for*/
             c-lb-item    c-lb-ct-cont
             c-lb-desc    c-lb-taxa
             c-lb-ipi     c-lb-vl-ipi
             c-lb-pa      c-lb-qtde
             c-lb-un      c-lb-prc-unit
             c-lb-vl-desc c-lb-vl-enc
             c-lb-prc-tot c-lb-ref c-lb-data
             with frame f-labels.

    end.
    {utp/ut-liter.i "Continua..." * r}
    assign c-msg = trim(return-value).
    if  de-ipi-tot > 0 then
        assign de-ipi1 = round(de-ipi-tot,2).
    else
        assign de-ipi1 = 0.

    disp ordem-compra.numero-ordem   /*space(1)*/
         /*item-fornec.item-do-forn    when avail item-fornec*/
         item.it-codigo              /*space(1)*/
         ordem-compra.conta-contabil /*space(1)*/
         ordem-compra.perc-descto    space(0) "%"    /*space(0)*/
         ordem-compra.valor-taxa     format ">>9.99" when not ordem-compra.taxa-financ  /*space(0)*/
         "      "  when ordem-compra.taxa-financ @ ordem-compra.valor-taxa /*space(0)*/
         c-tax-aux when not ordem-compra.taxa-financ  /*space(1)*/
         " "       when ordem-compra.taxa-financ @ c-tax-aux  /*space(1)*/
         ordem-compra.aliquota-ipi space(0) "%" space(2)
         de-ipi1   format ">>>>>>>>,>>9.99" when not ordem-compra.codigo-ipi
         c-lb-inc  format "x(15)"           when ordem-compra.codigo-ipi @ de-ipi1
         with no-box no-label width 132 stream-io frame f-ordem.

    if  tt-param.l-descricao then
        disp c-descricao with stream-io frame f-descricao.
END PROCEDURE.
