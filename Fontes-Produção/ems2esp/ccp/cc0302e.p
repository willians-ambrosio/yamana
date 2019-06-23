/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer moeda for ems2cadme.moeda.

{include/i-prgvrs.i CC0302E 2.00.00.015}  /*** 010015 ***/
/********************************************************************************
**
**   Programa: CC0302e.p
**
********************************************************************************/

{ccp/cc0302.i3} /* Definicao da temp-table tt-param */
{cdp/cdcfgdis.i}
{ccp/cc0302f.i}

def buffer b-ordem        for ordem-compra.
def buffer b-cotacao-item for cotacao-item.

def input  param raw-param as raw no-undo.
def output param table for tt-b2b.

def var r-registro as rowid no-undo.

def var de-indice        as decimal no-undo.
def var de-quant         as decimal no-undo format ">>,>>>,>>>,>>9.99" decimals 2.
def var c-natureza       as char    no-undo format "x(15)".
def var c-moeda          as char    no-undo format "x(10)".
def var i-count          as int     no-undo.
def var c-mod-cot        as char    no-undo.
def var c-mod-cot2       as char    no-undo.
def var c-arq1           as char    no-undo.
def var c-arq2           as char    no-undo.
def var c-prog           as char    no-undo.
def var h-acomp          as handle  no-undo.
def var c-assunto-e-mail as char    no-undo.
def var c-item-fornec    as char    no-undo format "x(20)".
def var c-un             as char    no-undo format "x(02)".
def var c-descricao      as char    no-undo.

/* Define Vari veis para Tradu‡Æo */
def var c-contato  as char no-undo.
def var c-fone     as char no-undo.
def var c-item     as char no-undo.
def var c-it-forn  as char no-undo.
def var c-ordem    as char no-undo.
def var c-cotacao  as char no-undo.
def var c-linha1   as char no-undo.
def var c-linha2   as char no-undo.
def var c-linha3   as char no-undo.
def var c-compra   as char no-undo.
def var c-lin-item as char no-undo.
def stream str.

DEF SHARED VAR l-ultima-compra AS LOG NO-UNDO.

{utp/utapi009.i}

{utp/ut-liter.i Item * r}
assign c-lin-item = trim(return-value).
{utp/ut-liter.i Item_Fornec * r}
assign c-lin-item = c-lin-item + "              " + trim(return-value).
{utp/ut-liter.i Descri‡Æo * r}
assign c-lin-item = c-lin-item + "           " + trim(return-value).
{utp/ut-liter.i Conta-Cont bil * r}
assign c-lin-item = c-lin-item + "                                                     " + trim(return-value).
{utp/ut-liter.i Cota‡Æo_de_Item * L}
assign c-assunto-e-mail = return-value.
{utp/ut-liter.i Contato * r}
assign c-contato = trim(return-value) + ": ".
{utp/ut-liter.i Telefone * r}
assign c-fone = trim(return-value) + ": ".
{utp/ut-liter.i Item * r}
assign c-item = trim(return-value).
{utp/ut-liter.i Item_Fornec * r}
assign c-it-forn = "                      " + trim(return-value) + ": ".
{utp/ut-liter.i Ordem * r}
assign c-ordem  = trim(return-value).
       c-linha1 = c-linha1 + trim(return-value) + "       ".
{utp/ut-liter.i Cota‡Æo * r}
assign c-cotacao = trim(return-value).           
{utp/ut-liter.i Natureza * r}
assign c-linha1 = c-linha1 + trim(return-value) + "         ".
{utp/ut-liter.i Qtdade_da_Ordem * r}
assign c-linha1 = c-linha1 + trim(return-value) + "      ".
{utp/ut-liter.i Un * r}
assign c-linha1 = c-linha1 + trim(return-value) + "   ".
{utp/ut-liter.i EmissÆo * r}
assign c-linha1 = c-linha1 + trim(return-value) + "      ".
{utp/ut-liter.i Requisitante * r}
assign c-linha1 = c-linha1 + trim(return-value) + "   ".
{utp/ut-liter.i Comprador * r}
assign c-linha1 = c-linha1 + trim(return-value).
{utp/ut-liter.i éltima_Compra * r}
assign c-compra = trim(return-value).
{utp/ut-liter.i Fornec * r}
assign c-linha2 = c-linha2 + trim(return-value) + "       ".
{utp/ut-liter.i Pedido * r}
assign c-linha2 = c-linha2 + trim(return-value) + "    ".
{utp/ut-liter.i Data * r}
assign c-linha2 = c-linha2 + trim(return-value) + "       ".
{utp/ut-liter.i Natureza * r}
assign c-linha2 = c-linha2 + trim(return-value) + "       ".
{utp/ut-liter.i Un * r}
assign c-linha2 = c-linha2 + trim(return-value) + " ".
{utp/ut-liter.i Moeda * r}
assign c-linha2 = c-linha2 + trim(return-value) + "     ".
{utp/ut-liter.i Pre‡o_Unit_For * r}
assign c-linha2 = c-linha2 + trim(return-value) + "      ".
{utp/ut-liter.i Quantidade * r}
assign c-linha2 = c-linha2 + trim(return-value) + "        ".
{utp/ut-liter.i IPI * r}
assign c-linha2 = c-linha2 + trim(return-value).
{utp/ut-liter.i Dt._Cota‡Æo * r}
assign c-linha3 = trim(return-value).
{utp/ut-liter.i Pre‡o_Unit_Forn * r}
assign c-linha3 = c-linha3 + "  " + trim(return-value).
{utp/ut-liter.i Moeda * r}
assign c-linha3 = c-linha3 + "  " + trim(return-value).
{utp/ut-liter.i Frete * r}
assign c-linha3 = c-linha3 + "  " + trim(return-value).
{utp/ut-liter.i %_IPI * r}
assign c-linha3 = c-linha3 + "  " + trim(return-value).
{utp/ut-liter.i %_Desc * r}
assign c-linha3 = c-linha3 + "  " + trim(return-value).
{utp/ut-liter.i Cond_Pagto * r}
assign c-linha3 = c-linha3 + "  " + trim(return-value).

form 
    item.it-codigo              at 1 
    c-item-fornec               at 19 
    item.desc-item              at 41  
    ordem-compra.conta-contabil at 103 
    with stream-io no-box no-label width 132 frame f-item.

form 
    ordem-compra.numero-ordem at 1
    c-natureza                at 13
    de-quant                  at 30
    c-un                      at 51
    ordem-compra.data-emissao at 56
    ordem-compra.requisitante at 69
    ordem-compra.cod-comprado at 84
    with stream-io no-box no-label width 132 frame f-ordem.

find first param-compra 
    no-lock no-error.
if avail param-compra then
    assign c-arq1 = trim(substr(param-compra.char-1, 1, 50))
           c-arq2 = trim(substr(param-compra.char-1, 51, 100)).

form 
    emitente.nome-abrev  at 1
    b-ordem.num-pedido   at 14
    b-ordem.data-pedido  at 24
    c-natureza           at 35
    item.un              at 50
    c-moeda              at 53
    b-ordem.preco-unit   at 63 
    de-quant             at 83
    b-ordem.aliquota-ipi at 100
    with stream-io no-box no-label width 132 frame f-ult-compra.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "EmissÆo de Ficha para Cota‡Æo").

create tt-param.
raw-transfer raw-param to tt-param.

for each  ordem-compra no-lock
    where ordem-compra.impr-ficha    = yes
    and   ordem-compra.data-emissao >= tt-param.d-data-i
    and   ordem-compra.data-emissao <= tt-param.d-data-f
    and   ordem-compra.cod-comprado >= tt-param.c-compr-i
    and   ordem-compra.cod-comprado <= tt-param.c-compr-f
    and   ordem-compra.it-codigo    >= tt-param.c-item-i
    and   ordem-compra.it-codigo    <= tt-param.c-item-f
    and   ordem-compra.numero-ordem >= tt-param.i-ordem-i
    and   ordem-compra.numero-ordem <= tt-param.i-ordem-f
    and   ordem-compra.cod-estabel  >= tt-param.c-estab-i
    and   ordem-compra.cod-estabel  <= tt-param.c-estab-f
    and   ordem-compra.situacao     <> 4,
    first item no-lock
    where item.it-codigo  = ordem-compra.it-codigo
    and   item.fm-codigo >= tt-param.c-fami-i
    and   item.fm-codigo <= tt-param.c-fami-f
    break by ordem-compra.cod-emitente
          by ordem-compra.it-codigo:

    for each tt-fornecedores-envio:
        delete tt-fornecedores-envio.
    end.

    for each  item-fornec no-lock
        where item-fornec.it-codigo = ordem-compra.it-codigo
        and   item-fornec.ativo     = yes,
        first emitente no-lock
        where emitente.cod-emite  = item-fornec.cod-emitente
        break by item-fornec.cod-emitente:

    if  tt-param.l-bus-to-bus = yes
    &IF '{&BF_DIS_VERSAO_EMS}' >= '2.05' &THEN
    then do:
        find dist-emitente where dist-emitente.cod-emitente = emitente.cod-emitente no-lock no-error.
        if  avail dist-emitente
        and dist-emitente.parceiro-b2b then do:
    &ELSE
        and substring(emitente.char-1,11,1) = "1" then do:
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
            or     (avail b-cotacao-item
            and     avail cotacao-item) then do:
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
        else
        if  tt-param.l-envio = yes then do:
            /**
            ***  Envio: E-Mail 
            **/
            run ccp/cc0302d.p (input raw-param,
                               input rowid(ordem-compra),
                               input rowid(item-fornec),
                              (first-of(ordem-compra.cod-emitente) or ordem-compra.cod-emitente = 0),
                              (last-of(ordem-compra.cod-emitente)  or ordem-compra.cod-emitente = 0),
                              (last(ordem-compra.cod-emitente)     or ordem-compra.cod-emitente = 0)).

            assign r-registro = rowid(ordem-compra).
            run ccp/cc0302b.p (input-output r-registro).
            next.
        end.

        if  first-of(ordem-compra.cod-emitente) then
            output stream str to value(c-arq1 + string(emitente.cod-emitente) + ".doc").

        run pi-acompanhar in h-acomp (input string(ordem-compra.cod-emitente) + " - "
                                            +      ordem-compra.it-codigo).

        /**
        ***  Imprime relat¢rio
        **/     
        assign de-quant = 0.
        for each  prazo-compra no-lock
            where prazo-compra.numero-ordem = ordem-compra.numero-ordem:
            assign de-quant = de-quant + prazo-compra.quantidade.
        end.
        assign c-natureza    = {ininc/i01in274.i 04 ordem-compra.natureza}
               de-indice     = item-fornec.fator-conver / if item-fornec.num-casa-dec = 0
                               then 1 else exp(10,item-fornec.num-casa-dec)
               de-quant      = de-quant * de-indice
               c-item-fornec = if available item-fornec then item-fornec.item-do-forn else "":U
               c-un          = if available item-fornec then item-fornec.unid-med-for else "":U.

        /**
        ***  Imprime cabe‡alho item
        **/    
        if  line-counter + 19 > page-size then
            page.

        put unformatted fill("-", 57) " " c-item " " fill("-", 60) skip.
        put unformatted c-lin-item skip.
        put unformatted fill("-", 16) "  " fill("-", 20) "  " fill("-", 60) "  " fill("-", 20). 

        disp item.it-codigo
             c-item-fornec   
             item.desc-item 
             ordem-compra.conta-contabil
             with frame f-item no-label.

        /**
        ***  Imprime cabe‡alho ordem compra
        **/    
        put unformatted fill("-",57) " " c-ordem " " fill("-",59) skip.
        put unformatted c-linha1 skip.
        put unformatted "---------   --------------   ---------------      --   --------     ------------   ----------" skip.

        disp ordem-compra.numero-ordem
             c-natureza
             de-quant
             c-un 
             ordem-compra.data-emissao
             ordem-compra.requisitante
             ordem-compra.cod-comprado
             with frame f-ordem no-label.
        /**
        ***  Imprime dados da £ltima compra
        **/
        assign de-quant = 0.
        find last  b-ordem use-index compra-item
             where b-ordem.it-codigo    = ordem-compra.it-codigo
             and   b-ordem.data-pedido <> ? 
             no-lock no-error.

        put unformatted skip(1) fill("-", 57) " " c-compra " " fill("-", 51) skip.
        put unformatted c-linha2 skip.
        put unformatted "------------ --------- ---------- -------------- -- --------- ------------------- ----------------- -----" skip.

        if  avail b-ordem then do:
            for each  prazo-compra
                where prazo-compra.numero-ordem = b-ordem.numero-ordem no-lock:
                assign de-quant = de-quant + prazo-compra.qtd-do-forn.
            end.
            find moeda where moeda.mo-codigo = b-ordem.mo-codigo no-lock no-error.
            assign c-moeda    = (if avail moeda then moeda.descricao else "")
                   c-natureza = {ininc/i01in274.i 04 b-ordem.natureza}.

            IF l-ultima-compra THEN DO:
                disp emitente.nome-abrev
                     b-ordem.num-pedido
                     b-ordem.data-pedido
                     c-natureza
                     item.un
                     c-moeda
                     b-ordem.preco-unit
                     de-quant
                     b-ordem.aliquota-ipi
                     with frame f-ult-compra no-label.
            END.
        end.

        /**
        ***  Imprime dados da cota‡Æo
        **/
        put unformatted skip(1) fill("-", 57) " " c-cotacao " " fill("-", 57) skip.
        put unformatted c-linha3 skip.
        put unformatted "-----------  ---------------  -----  -----  -----  ------  ----------" skip (2).
        put unformatted "-----------  ---------------  -----  -----  -----  ------  ----------" skip (2).

        if  last-of(item-fornec.cod-emitente) then do:
            /*Appword:ActiveDocument:SaveAs(c-arq1 + string(emitente.cod-emitente) + ".doc").*/
            /*Appword:ActiveDocument:Close.*/
            output stream str close.
            find first param-global no-lock no-error.
            if  emitente.e-mail <> "":U then do:
                for each tt-envio:
                    delete tt-envio.
                end.
                create tt-envio.
                assign tt-envio.versao-integracao = 1
                       tt-envio.exchange    = param-global.log-1
                       tt-envio.destino     = emitente.e-mail
                       tt-envio.assunto     = c-assunto-e-mail
                       tt-envio.mensagem    = ""
                       tt-envio.importancia = 2
                       tt-envio.log-enviada = yes
                       tt-envio.log-lida    = yes
                       tt-envio.acomp       = yes.
                       tt-envio.arq-anexo   = c-arq1 + string(emitente.cod-emite) + ".doc".

                run utp/utapi009.p (input  table tt-envio,
                                    output table tt-erros).
            end.                                    
        end.
        assign r-registro = rowid(ordem-compra). 
        run ccp/cc0302b.p (input-output r-registro).
    end.
    &IF '{&BF_DIS_VERSAO_EMS}' >= '2.05' &THEN
    end.
    &ENDIF
    if  can-find(first tt-fornecedores-envio) then do:
        run ccp/cc0302f.p (input rowid(ordem-compra),
                           input table tt-fornecedores-envio,
                           input-output table tt-b2b).
        assign r-registro = rowid(ordem-compra). 
        run ccp/cc0302b.p (input-output r-registro).
    end.
end.
output stream str close.

/*Appword:Quit().*/
/*release object appWord.*/
run pi-finalizar in h-acomp.
