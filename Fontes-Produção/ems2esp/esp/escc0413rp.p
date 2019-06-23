/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i ESCC0413RP 2.06.00.000}
/******************************************************************************
**     Objetivo: Mapa Comparativo de Precos
*******************************************************************************/
def temp-table tt-param no-undo
    field destino                    as int
    field arquivo                    as char format "x(35)"
    field usuario                    as char format "x(12)"
    field data-exec                  as date
    field hora-exec                  as int
    field i-ini-numero-ordem         like ordem-compra.numero-ordem
    field i-fim-numero-ordem         like ordem-compra.numero-ordem
    field dt-ini-emissao             as date format "99/99/9999"
    field dt-fim-emissao             as date format "99/99/9999"
    field c-ini-requisitante         like ordem-compra.requisitante
    field c-fim-requisitante         like ordem-compra.requisitante
    field c-ini-cod-comprado         like ordem-compra.cod-comprado
    field c-fim-cod-comprado         like ordem-compra.cod-comprado
    field c-ini-it-codigo            like ordem-compra.it-codigo
    field c-fim-it-codigo            like ordem-compra.it-codigo
    field l-narrativa-ord-compr      as log
    field l-narrativa-item-ord-compr as log.

def temp-table tt-digita no-undo
    field flag         as char format "x"
    field numero-ordem like ordem-compra.numero-ordem
    field parcela      like prazo-compra.parcela
    field it-codigo    like ordem-compra.it-codigo
    field un           like item.un
    field quantidade   like prazo-compra.quantidade
    field data-entrega like prazo-compra.data-entrega
    index id numero-ordem
    index it flag numero-ordem it-codigo.

def temp-table tt-raw-digita
    field raw-digita as raw.

def temp-table tt-totais no-undo
    field cod-emitente  like emitente.cod-emitente
    field nome-emit     like emitente.nome-emit
    field valores       as dec format ">>>>>,>>9.99"
    FIELD desconto      as dec format ">>>>>,>>9.99"  
    field cond-pag      like cond-pagto.descricao FORMAT "X(35)"
    field transp        like transporte.nome      FORMAT "X(35)"
    field local-entrega like estabelec.nome       FORMAT "X(35)"
    index idx as primary unique cod-emitente.

def temp-table tt-fornecs no-undo
    field cod-emitente like emitente.cod-emitente
    field coluna       AS INT
    index idx  as primary unique cod-emitente coluna
    index colu coluna.

def temp-table tt-linhas-ordem no-undo
    field i-ordem      AS INT
    field qtd-linha    AS INT
    index idx  as primary unique i-ordem.

DEFINE BUFFER bf-ordem-compra FOR ordem-compra.
DEFINE BUFFER bf-cotacao-item FOR cotacao-item.
{utp/ut-glob.i}

{include/tt-edit.i}

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

def var h-acomp       as handle                         no-undo.
def var de-preco-unit as dec  format ">>>>>,>>9.999"    no-undo.
def var de-preco-tot  as dec  format ">>>>>>>>,>>9.999" no-undo.
def var c-cond-pag    as char format "x(40)"            no-undo.

DEF VAR de-vl-desc    as dec  format ">>>>>,>>9.999"    no-undo.      

/* variaveis para gera‡Æo para o excel */
def var chExcelApplication as com-handle no-undo.
def var chWorkbook         as com-handle no-undo.
def var chWorkSheet        as com-handle no-undo.
def var chActiveWorkbook   as com-handle no-undo.
def var c-planilha         as char       no-undo.
def var c-planilha-envio   as char       no-undo.
def var c-range            as char       no-undo.
def var i-linha            as int        no-undo.
def var i-linha-aux        as int        no-undo INIT 2.
def var i-linha-narrativ   as int        no-undo.
def var i-linha-ordem      as int        no-undo.
def var i-coluna           as int        no-undo INIT 5.
def var c-coluna           as char       no-undo EXTENT 256 INIT ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK","AL","AM","AN","AO","AP","AQ","AR","AS","AT","AU","AV","AW","AX","AY","AZ","BA","BB","BC","BD","BE","BF","BG","BH","BI","BJ","BK","BL","BM","BN","BO","BP","BQ","BR","BS","BT","BU","BV","BW","BX","BY","BZ","CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM","CN","CO","CP","CQ","CR","CS","CT","CU","CV","CW","CX","CY","CZ","DA","DB","DC","DD","DE","DF","DG","DH","DI","DJ","DK","DL","DM","DN","DO","DP","DQ","DR","DS","DT","DU","DV","DW","DX","DY","DZ","EA","EB","EC","ED","EE","EF","EG","EH","EI","EJ","EK","EL","EM","EN","EO","EP","EQ","ER","ES","ET","EU","EV","EW","EX","EY","EZ","FA","FB","FC","FD","FE","FF","FG","FH","FI","FJ","FK","FL","FM","FN","FO","FP","FQ","FR","FS","FT","FU","FV","FW","FX","FY","FZ","GA","GB","GC","GD","GE","GF","GG","GH","GI","GJ","GK","GL","GM","GN","GO","GP","GQ","GR","GS","GT","GU","GV","GW","GX","GY","GZ","HA","HB","HC","HD","HE","HF","HG","HH","HI","HJ","HK","HL","HM","HN","HO","HP","HQ","HR","HS","HT","HU","HV","HW","HX","HY","HZ","IA","IB","IC","ID","IE","IF","IG","IH","II","IJ","IK","IL","IM","IN","IO","IP","IQ","IR","IS","IT","IU","IV"].
def var d-quantidade       like prazo-compra.quantidade NO-UNDO.
def var i-ordem            as int        no-undo.
def var i-cont             as int        no-undo INIT 0.
{include/i-rpvar.i}

for each tt-raw-digita no-lock:
    create tt-digita.
    raw-transfer raw-digita TO tt-digita.
end.

create tt-param.
raw-transfer raw-param to tt-param.

find first param-global no-lock no-error.
find first empresa no-lock
     where empresa.ep-codigo = param-global.empresa-prin no-error.


{include/i-rpout.i}

{cdp/cd1234.i} /*Valida‡Æo Decimais*/

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Mapa Comparativo de Pre‡os").

ASSIGN
    FILE-INFO:FILE-NAME = "modelos\ModeloESCC0413.xls"
    c-planilha          = FILE-INFO:FULL-PATHNAME.

PUT "Planilhas Geradas:" AT 1.

for each  tt-digita
    where tt-digita.flag <> "" USE-INDEX it,
    first ordem-compra no-lock
    where ordem-compra.numero-ordem = tt-digita.numero-ordem
    and   ordem-compra.it-codigo    = tt-digita.it-codigo,
    EACH  cotacao-item no-lock use-index cotacao
    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
    and   cotacao-item.it-codigo    = ordem-compra.it-codigo
    and   cotacao-item.preco-unit   > 0,
    first emitente no-lock use-index codigo
    where emitente.cod-emitente = cotacao-item.cod-emitente
    break
    by ordem-compra.nr-processo
    by ordem-compra.numero-ordem
    by cotacao-item.preco-unit:

    IF FIRST-OF(ordem-compra.nr-processo) THEN
        RUN pi-inicia-planilha.

    run pi-acompanhar in h-acomp(input "Proc. "   + string(ordem-compra.nr-processo) +
                                       " \ Item " + string(ordem-compra.it-codigo)).

    /* evita acessos desnecessarios a base */
    if  not avail item then
        find first item no-lock
             where item.it-codigo = ordem-compra.it-codigo no-error.
    else
        if  avail item and
            item.it-codigo <> ordem-compra.it-codigo then
            find first item no-lock
                 where item.it-codigo = ordem-compra.it-codigo no-error.

    if  first-of(ordem-compra.numero-ordem) then
    do:
        assign
            i-linha = i-linha + 1
            i-ordem = i-ordem + 1
            c-range = "A" + string(i-linha)
            chExcelApplication:Range(c-range):value = STRING(ordem-compra.numero-ordem,"zzzzz9,99")
            c-range = "B" + string(i-linha)
            chExcelApplication:Range(c-range):value = ordem-compra.it-codigo.


        if  avail item then
        do:
            assign
                c-range = "C" + string(i-linha)
                chExcelApplication:Range(c-range):WrapText = true
                chExcelApplication:Range(c-range):value = trim(item.desc-item)

            /* Ini - Ultima Compra */
                c-range = "D" + string(i-linha)
                chExcelApplication:Range(c-range):value = "Qtde:"
                c-range = "D" + string(i-linha + 1)
                chExcelApplication:Range(c-range):value = "Vlr. Unit rio:"
                c-range = "D" + string(i-linha + 2)
                chExcelApplication:Range(c-range):value = "Fornecedor:"
                c-range = "D" + string(i-linha + 3)
                chExcelApplication:Range(c-range):value = "Data:"
                c-range = "D" + string(i-linha + 4)
                chExcelApplication:Range(c-range):value = "Ordem:"
                c-range = "D" + string(i-linha + 5)
                chExcelApplication:Range(c-range):value = "Frete:"
                c-range = "D" + string(i-linha + 6)
                chExcelApplication:Range(c-range):value = "ICMS:"
                c-range = "D" + string(i-linha + 7)
                chExcelApplication:Range(c-range):value = "Prazo Entrega:".

            FOR LAST  bf-ordem-compra NO-LOCK
                WHERE bf-ordem-compra.it-codigo = ordem-compra.it-codigo
                AND   (bf-ordem-compra.situacao  = 2
                       OR bf-ordem-compra.situacao  = 6)
                BY bf-ordem-compra.data-pedido:

                ASSIGN
                    d-quantidade = 0.

                FOR EACH prazo-compra no-lock use-index ordem
                    where prazo-compra.numero-ordem = bf-ordem-compra.numero-ordem:
                    ASSIGN 
                        d-quantidade = d-quantidade + prazo-compra.qtd-sal-forn /*prazo-compra.quantidade*/ .
                END.

                ASSIGN 
                    c-range = "E" + string(i-linha)
                    chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
                    chExcelApplication:Range(c-range):value = d-quantidade 
                    c-range = "E" + string(i-linha + 1)
                    chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
/*                     chExcelApplication:Range(c-range):value = bf-ordem-compra.preco-unit */
                    chExcelApplication:Range(c-range):value = bf-ordem-compra.preco-fornec 
                    c-range = "E" + string(i-linha + 2)
                    chExcelApplication:Range(c-range):value = bf-ordem-compra.cod-emitente
                    c-range = "E" + string(i-linha + 3)
                    chExcelApplication:Range(c-range):Numberformat = "@"
                    chExcelApplication:Range(c-range):value = "'" + STRING(bf-ordem-compra.data-pedido,"99/99/9999")
                    c-range = "E" + string(i-linha + 4)
                    chExcelApplication:Range(c-range):Numberformat = "@"
                    chExcelApplication:Range(c-range):value = STRING(bf-ordem-compra.numero-ordem,"zzzzz9,99")
                    c-range = "E" + string(i-linha + 5)
                    chExcelApplication:Range(c-range):value = IF bf-ordem-compra.frete THEN "CIF" ELSE "FOB"
                    c-range = "E" + string(i-linha + 6)
                    chExcelApplication:Range(c-range):value = STRING(bf-ordem-compra.aliquota-icm) + "%"
                    c-range = "E" + string(i-linha + 7)
                    chExcelApplication:Range(c-range):value = bf-ordem-compra.prazo-entreg
                    c-range = "E" + string(i-linha) + ":E" + string(i-linha + 7)
                    chExcelApplication:Range(c-range):HorizontalAlignment = 4.
            END.
            /* Fim - Ultima Compra */

            ASSIGN
                d-quantidade = 0.

            FOR EACH prazo-compra no-lock use-index ordem
                where prazo-compra.numero-ordem = ordem-compra.numero-ordem:
                ASSIGN 
                    d-quantidade = d-quantidade + prazo-compra.qtd-sal-forn /*prazo-compra.quantidade*/ .
            END.


            ASSIGN
                c-range = "F" + string(i-linha)
                chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
                chExcelApplication:Range(c-range):value = d-quantidade 
                c-range = "G" + string(i-linha)
                chExcelApplication:Range(c-range):value = COTACAO-ITEM.UN /*item.un SERGIO*/
                i-linha-narrativ = i-linha + 1
                i-linha-ordem    = 7.

            /* Narrativa do Item */
            if  tt-param.l-narrativa-item-ord-compr then
            do:
                assign i-linha-narrativ = i-linha-narrativ + 1.
                run pi-print-editor(item.narrativa, 89).
                for each tt-editor no-lock:
                    if  tt-editor.conteudo <> "" then
                    DO:
                        assign c-range = "A" + string(i-linha-narrativ) + ":C" + string(i-linha-narrativ).
                        chExcelApplication:Range(c-range):merge.
                        assign c-range = "A" + string(i-linha-narrativ).
                        chExcelApplication:Range(c-range):WrapText = true.
                        chExcelApplication:Range(c-range):value = trim(tt-editor.conteudo).
                    END.
                        assign
                            i-linha-narrativ = i-linha-narrativ + 1
                            i-linha-ordem    = i-linha-ordem    + 1.
                end.
            end.

            /* Narrativa da Ordem */
            if  tt-param.l-narrativa-ord-compr then
            do:
                assign i-linha-narrativ = i-linha-narrativ + 1.
                run pi-print-editor(ordem-compra.narrativa, 89).
                for each tt-editor no-lock:
                    if  tt-editor.conteudo <> "" then
                    DO:
                        assign c-range = "A" + string(i-linha-narrativ) + ":C" + string(i-linha-narrativ).
                        chExcelApplication:Range(c-range):merge.
                        assign c-range = "A" + string(i-linha-narrativ).
                        chExcelApplication:Range(c-range):WrapText = true.
                        chExcelApplication:Range(c-range):value = trim(tt-editor.conteudo).
                    END.
                        assign
                            i-linha-narrativ = i-linha-narrativ + 1
                            i-linha-ordem    = i-linha-ordem    + 1.
                end.
            end.

            /* Narrativa da Cota‡Æo */
            assign
                i-linha-narrativ = i-linha-narrativ + 1
                i-linha-ordem    = i-linha-ordem    + 1
                c-range = "A" + string(i-linha-narrativ) + ":C" + string(i-linha-narrativ).
            chExcelApplication:Range(c-range):merge.
            assign
                c-range = "A" + string(i-linha-narrativ)
                chExcelApplication:Range(c-range):WrapText = true
                chExcelApplication:Range(c-range):value = "Narrativa da Cota‡Æo:"
                chExcelApplication:Range(c-range):font:underline    = true
                i-linha-narrativ = i-linha-narrativ + 1.

            run pi-print-editor(cotacao-item.narrativa, 89).
            for each tt-editor no-lock:
                if  tt-editor.conteudo <> "" then
                DO:
                    assign c-range = "A" + string(i-linha-narrativ) + ":C" + string(i-linha-narrativ).
                    chExcelApplication:Range(c-range):merge.
                    assign c-range = "A" + string(i-linha-narrativ).
                    chExcelApplication:Range(c-range):WrapText = true.
                    chExcelApplication:Range(c-range):value = trim(tt-editor.conteudo).
                END.
                    assign
                        i-linha-narrativ = i-linha-narrativ + 1
                        i-linha-ordem    = i-linha-ordem    + 1.
            end.

            
            /* Motivo da Aprova‡Æo */
            assign
                i-linha-narrativ = i-linha-narrativ + 1
                i-linha-ordem    = i-linha-ordem    + 1
                c-range = "A" + string(i-linha-narrativ) + ":C" + string(i-linha-narrativ).
            chExcelApplication:Range(c-range):merge.
            assign
                c-range = "A" + string(i-linha-narrativ)
                chExcelApplication:Range(c-range):WrapText = true
                chExcelApplication:Range(c-range):value = "Motivo da Aprova‡Æo:"
                chExcelApplication:Range(c-range):font:underline    = true
                i-linha-narrativ = i-linha-narrativ + 1.

            FIND FIRST bf-cotacao-item NO-LOCK
                 WHERE bf-cotacao-item.numero-ordem = cotacao-item.numero-ordem
                 AND   bf-cotacao-item.cod-emitente = cotacao-item.cod-emitente
                 AND   bf-cotacao-item.it-codigo    = cotacao-item.it-codigo
                 AND   bf-cotacao-item.motivo-apr   <> "" NO-ERROR.

            run pi-print-editor(IF AVAIL bf-cotacao-item THEN bf-cotacao-item.motivo-apr ELSE "", 89).
            for each tt-editor no-lock:
                if  tt-editor.conteudo <> "" then
                DO:
                    assign c-range = "A" + string(i-linha-narrativ) + ":C" + string(i-linha-narrativ).
                    chExcelApplication:Range(c-range):merge.
                    assign c-range = "A" + string(i-linha-narrativ).
                    chExcelApplication:Range(c-range):WrapText = true.
                    chExcelApplication:Range(c-range):value = trim(tt-editor.conteudo).
                END.
                    assign
                        i-linha-narrativ = i-linha-narrativ + 1
                        i-linha-ordem    = i-linha-ordem    + 1.
            end.

            /* Coment rios */
            assign
                i-linha-narrativ = i-linha-narrativ + 1
                i-linha-ordem    = i-linha-ordem    + 1
                c-range = "A" + string(i-linha-narrativ) + ":C" + string(i-linha-narrativ).
            chExcelApplication:Range(c-range):merge.
            assign
                c-range = "A" + string(i-linha-narrativ)
                chExcelApplication:Range(c-range):WrapText = true
                chExcelApplication:Range(c-range):value = "Coment rios:"
                chExcelApplication:Range(c-range):font:underline    = true
                i-linha-narrativ = i-linha-narrativ + 1
                i-linha-ordem    = i-linha-ordem    + 1.

        end.

        IF (i-linha-narrativ - i-linha) < 9 THEN
            ASSIGN i-linha-narrativ = i-linha + 9.

        IF i-linha-ordem < 11 THEN
            ASSIGN i-linha-ordem = 11.

        CREATE tt-linhas-ordem.
        ASSIGN
            tt-linhas-ordem.i-ordem    = i-ordem
            tt-linhas-ordem.qtd-linha = i-linha-ordem.


        assign
            c-range = "A" + STRING(i-linha - 1) + ":D" + string(i-linha-narrativ)
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142
            
            c-range = "D" + STRING(i-linha - 1) + ":E" + string(i-linha-narrativ)
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142

            c-range = "F" + STRING(i-linha - 1) + ":G" + string(i-linha-narrativ)
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142
            i-linha-aux = i-linha-aux + 11.

    end.

    FIND FIRST tt-fornecs NO-LOCK
         WHERE tt-fornecs.cod-emitente = emitente.cod-emitente     NO-ERROR.
    IF NOT AVAIL tt-fornecs THEN
    DO:
        CREATE tt-fornecs.
       
        ASSIGN
            tt-fornecs.cod-emitente = emitente.cod-emitente
            i-coluna                = i-coluna + 3
            tt-fornecs.coluna       = i-coluna.

        FIND FIRST cont-emit NO-LOCK
             WHERE cont-emit.cod-emitente = emitente.cod-emitente NO-ERROR.

        ASSIGN
            c-range = c-coluna[tt-fornecs.coluna] + "9"
            chExcelApplication:Range(c-range):value = emitente.nome-abrev + " - " + STRING(emitente.cod-emitente)
            c-range = c-coluna[tt-fornecs.coluna] + "10"
            chExcelApplication:Range(c-range):value = IF AVAIL cont-emit THEN cont-emit.nome + " - " + cont-emit.telefone ELSE ""

            c-range = c-coluna[tt-fornecs.coluna] + "9" + ":" + c-coluna[tt-fornecs.coluna + 2] + "10"
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        ASSIGN
            c-range = c-coluna[tt-fornecs.coluna] + "11"
            chExcelApplication:Range(c-range):value = "Unitario"
            c-range = c-coluna[tt-fornecs.coluna + 1] + "11"
            chExcelApplication:Range(c-range):value = "IPI"
            c-range = c-coluna[tt-fornecs.coluna + 2] + "11"
            chExcelApplication:Range(c-range):value = "TOTAL"

            c-range = c-coluna[tt-fornecs.coluna] + "11" + ":" + c-coluna[tt-fornecs.coluna + 2] + "11"
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

    END.

    
    assign de-preco-unit = 0
           de-vl-desc    = 0.

    if  cotacao-item.mo-codigo = 0 then
/*         assign de-preco-unit = cotacao-item.preco-unit. */
        assign de-preco-unit = cotacao-item.preco-fornec.
    else do:
        run cdp/cd0812.p(input  cotacao-item.mo-codigo,
                         input  0,
                         input  cotacao-item.preco-fornec /*cotacao-item.preco-unit*/,
                         input  cotacao-item.data-cotacao,
                         output de-preco-unit).
    end.

    assign
        de-preco-tot = fn_ajust_dec(de-preco-unit * d-quantidade,0)                         /* Pre‡o total sem IPI e sem Desconto abatido */
        de-preco-tot = de-preco-tot - ((de-preco-tot * cotacao-item.perc-descto) / 100)     /* Pre‡o total sem IPI e com Desconto abatido */
        de-preco-tot = de-preco-tot + (de-preco-tot * (cotacao-item.aliquota-ipi / 100)).   /* Pre‡o total com IPI e com Desconto abatido */
        
    
    /* Solicitado pelo Edilson */ /**/


    assign c-cond-pag = "".
    if  cotacao-item.cod-cond-pag = 0 then
        assign c-cond-pag = "CONDI€ÇO ESPECIAL".
    else do:
/*        assign c-cond-pag = string(pedido-compr.cod-cond-pag). */
        find first cond-pagto no-lock
             where cond-pagto.cod-cond-pag = cotacao-item.cod-cond-pag no-error.
        if  avail cond-pagto then
            assign c-cond-pag = /* c-cond-pag + " - " + */
                                trim(cond-pagto.descricao).
    end.

    ASSIGN de-vl-desc = (((d-quantidade * de-preco-unit)* cotacao-item.perc-descto) / 100). 
    
    ASSIGN i-linha-aux = i-linha.

    ASSIGN
        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
/*         chExcelApplication:Range(c-range):value = cotacao-item.preco-unit                                               /* Pre‡o Unitario */ */
        chExcelApplication:Range(c-range):value = de-preco-unit                                                         /* Pre‡o Unitario */
        c-range = c-coluna[tt-fornecs.coluna + 1] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):value = STRING(cotacao-item.aliquota-ipi) + "%"                               /* IPI */
        c-range = c-coluna[tt-fornecs.coluna + 2] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
        chExcelApplication:Range(c-range):value = de-preco-tot                                                          /* Total */
        i-linha-aux = i-linha-aux + 2
        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):value = IF cotacao-item.codigo-ipi THEN "IPI Incluso" ELSE "IPI NÆo Incluso"  /* IPI incluso */
        c-range = c-coluna[tt-fornecs.coluna + 2] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):value = IF cotacao-item.frete THEN "Frete Incluso" ELSE "Frete NÆo Incluso"   /* Frete */
        i-linha-aux = i-linha-aux + 1
        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux)
/*      chExcelApplication:Range(c-range):value = "Desc: " + string(cotacao-item.valor-descto) + "%"                    /* Desconto */ */
        chExcelApplication:Range(c-range):value = "Desc: " + string(cotacao-item.perc-desc) + "%"  /* Desconto */
        /* 20/08/07 */
        c-range = c-coluna[tt-fornecs.coluna + 2] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
        chExcelApplication:Range(c-range):value = de-vl-desc                                                            /* vl desconto */
        
        i-linha-aux = i-linha-aux + 2
        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):value = IF cotacao-item.frete THEN "CIF" ELSE "FOB"                           /* Frete */
        i-linha-aux = i-linha-aux + 1
        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):value = STRING(cotacao-item.aliquota-icm) + "%"                               /* ICMS */
        i-linha-aux = i-linha-aux + 1
        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):value = cotacao-item.prazo-entreg                                             /* Prazo Entrega */

        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux - 7) + ":" + c-coluna[tt-fornecs.coluna + 2] + STRING(i-linha-aux - 7)
        chExcelApplication:Range(c-range):Interior:ColorIndex = 15

        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux - 5) + ":" + c-coluna[tt-fornecs.coluna + 2 /* cida */ ] + STRING(i-linha-aux - 4)
        chExcelApplication:Range(c-range):Interior:ColorIndex = 15

        c-range = c-coluna[tt-fornecs.coluna + 2] + STRING(i-linha-aux - 5) + ":" + c-coluna[tt-fornecs.coluna + 2] + STRING(i-linha-aux - 5)
        chExcelApplication:Range(c-range):Interior:ColorIndex = 15

        c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux - 2) + ":" + c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux)
        chExcelApplication:Range(c-range):Interior:ColorIndex = 15.

    find first tt-totais
         where tt-totais.cod-emitente = emitente.cod-emitente no-error.
    if  not avail tt-totais then do:

        FIND FIRST transporte NO-LOCK
             WHERE transporte.cod-transp = cotacao-item.cod-transp NO-ERROR.

        FIND FIRST estabelec NO-LOCK
             WHERE estabelec.cod-estabel = ordem-compra.cod-estabel NO-ERROR.

        create tt-totais.
        assign tt-totais.cod-emitente   = emitente.cod-emitente
               tt-totais.nome-emit      = emitente.nome-emit
               tt-totais.cond-pag       = c-cond-pag
               tt-totais.transp         = IF AVAIL transporte THEN transporte.nome ELSE ""
               tt-totais.local-entrega  = IF AVAIL estabelec  THEN estabelec.nome  ELSE "".

    end.
    assign tt-totais.valores = tt-totais.valores   + de-preco-tot
           tt-totais.desconto = tt-totais.desconto + de-vl-desc.


    if  LAST-OF(ordem-compra.numero-ordem) THEN
        ASSIGN
            i-linha = i-linha-narrativ + 1.

    IF LAST-OF(ordem-compra.nr-processo) THEN
        RUN pi-finaliza-planilha.
end.

run pi-finalizar in h-acomp.

{include/i-rpclo.i}

{include/pi-edit.i}

PROCEDURE pi-inicia-planilha:
    
    run pi-planilha-envio.

    create "Excel.Application" chExcelApplication.
    assign chExcelApplication:visible = no /* Nao mostra a planilha Excel na tela enquanto esta sendo criada */
           chWorkbook   = chExcelApplication:Workbooks:ADD(c-planilha). /* Cria uma nova planilha excel */

    assign
        i-linha             = 12
        i-linha-aux         = 2
        i-linha-narrativ    = 0
        i-linha-ordem       = 4
        i-coluna            = 5
        d-quantidade        = 0
        i-ordem             = 0
        i-cont              = 0
        de-preco-unit       = 0
        de-preco-tot        = 0
        de-vl-desc          = 0
        c-cond-pag          = "".

    EMPTY TEMP-TABLE tt-totais.
    EMPTY TEMP-TABLE tt-fornecs.
    EMPTY TEMP-TABLE tt-linhas-ordem.

    ASSIGN
        chExcelApplication:Range("A7"):value = "RazÆo Social: " + empresa.razao-social
        chExcelApplication:Range("A9"):value = "Processo: "     + STRING(ordem-compra.nr-processo,"999,999").

END PROCEDURE.

PROCEDURE pi-finaliza-planilha.
    
    run pi-acompanhar in h-acomp(input "Finalizando Relatorio...").

    /* Fecha quadro dos Fornecs */
    FOR EACH tt-fornecs NO-LOCK
        USE-INDEX colu:

        ASSIGN
            i-linha-aux = 12.

        FOR EACH tt-linhas-ordem BY tt-linhas-ordem.i-ordem:
            ASSIGN
                c-range = c-coluna[tt-fornecs.coluna] + STRING(i-linha-aux) + ":" + c-coluna[tt-fornecs.coluna + 2] + STRING(i-linha-aux + tt-linhas-ordem.qtd-linha - 1)
                chExcelApplication:Range(c-range):Borders:LineStyle = 1
                chExcelApplication:Range(c-range):Borders:Weight    = 3
                chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
                chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142
                i-linha-aux = i-linha-aux + tt-linhas-ordem.qtd-linha.
        END.
    END.

    FOR LAST tt-fornecs NO-LOCK USE-INDEX colu:
        assign c-range = "F1:" + c-coluna[tt-fornecs.coluna + 2] + "7".
        chExcelApplication:Range(c-range):merge.

        ASSIGN
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.
    END.

    RUN pi-totais.

    chExcelApplication:workbooks:item(1):SaveAs(c-planilha-envio,,,,,,).
    chExcelApplication:range("A1"):select.

    chExcelApplication:ActiveSheet:Enableselection ="1".
    chExcelApplication:DisplayAlerts = false.
    if  tt-param.destino = 3 then
        assign chExcelApplication:visible = yes. /* Mostra a planilha Excel na tela */
    else
        chExcelApplication:quit.

    release object chExcelApplication.

    put
        c-planilha-envio format "x(60)" AT 5.

END PROCEDURE.


procedure pi-totais:
    
    assign
        i-linha-aux  = i-linha
        /* c-range = "F" + string(i-linha-aux)  
        chExcelApplication:Range(c-range):value = "SUB-TOTAL" */ 
        c-range = "F" + string(i-linha-aux)               
        chExcelApplication:Range(c-range):value = "DESCONTO"
        c-range = "F" + string(i-linha-aux + 1)
        chExcelApplication:Range(c-range):value = "TOTAL"
        c-range = "F" + string(i-linha-aux + 2)
        chExcelApplication:Range(c-range):value = "Condi‡äes de Pagamento"
        c-range = "F" + string(i-linha-aux + 3)
        chExcelApplication:Range(c-range):value = "Transportador"
        c-range = "F" + string(i-linha-aux + 4)
        chExcelApplication:Range(c-range):value = "Local de Entrega"

        c-range = "F" + string(i-linha-aux) + ":F" + string(i-linha-aux + 4)
        chExcelApplication:Range(c-range):HorizontalAlignment = 4.


    FOR LAST tt-fornecs NO-LOCK USE-INDEX colu:
        assign c-range = "A" + string(i-linha-aux) + ":" + c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux + 5).

        ASSIGN
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.
    END.

    FOR EACH  tt-totais,
        FIRST tt-fornecs NO-LOCK
        WHERE tt-fornecs.cod-emitente = tt-totais.cod-emitente:

        assign
            /*
            i-linha-aux  = i-linha
            c-range = c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux)
            chExcelApplication:Range(c-range):value = tt-totais.valores
            chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
            chExcelApplication:Range(c-range):Interior:ColorIndex = 15  
            */
            
            i-linha-aux  = i-linha /* -aux + 1 */
            c-range = c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux)
            chExcelApplication:Range(c-range):value = tt-totais.desconto 
            chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
            chExcelApplication:Range(c-range):Interior:ColorIndex = 15
            
            i-linha-aux  = i-linha-aux + 1
            c-range = c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux)
            chExcelApplication:Range(c-range):value = tt-totais.valores 
            /* "="   + c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux - 2) + 
                                                      "-((" + c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux - 2) +
                                                      "*"   + c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux - 1) +
                                                      ") / 100)" */
            chExcelApplication:Range(c-range):Numberformat = "#.##0,00"
            chExcelApplication:Range(c-range):Interior:ColorIndex = 15

            i-linha-aux  = i-linha-aux + 1
            c-range = c-coluna[tt-fornecs.coluna] + string(i-linha-aux)
            chExcelApplication:Range(c-range):value = tt-totais.cond-pag

            i-linha-aux  = i-linha-aux + 1
            c-range = c-coluna[tt-fornecs.coluna] + string(i-linha-aux)
            chExcelApplication:Range(c-range):value = tt-totais.transp

            i-linha-aux  = i-linha-aux + 1
            c-range = c-coluna[tt-fornecs.coluna] + string(i-linha-aux)
            chExcelApplication:Range(c-range):value = tt-totais.local-entrega

            i-linha-aux  = i-linha-aux - 3

            c-range = c-coluna[tt-fornecs.coluna] + string(i-linha) + ":" + c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux + 1)
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142

            c-range = c-coluna[tt-fornecs.coluna] + string(i-linha-aux + 1) + ":" + c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux + 3)
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142
            chExcelApplication:Range(c-range):Interior:ColorIndex = 15.
    END.

    ASSIGN
        i-linha-aux  = i-linha-aux + 4
        c-range = "A" + string(i-linha-aux)
        chExcelApplication:Range(c-range):value             = "Aprova‡äes:"
        chExcelApplication:Range(c-range):font:underline    = true

        i-linha-aux  = i-linha-aux + 3
        c-range = "B" + string(i-linha-aux)
        chExcelApplication:Range(c-range):value = "Comprador:  __________________________________________"

        c-range = "D" + string(i-linha-aux)
        chExcelApplication:Range(c-range):value = "Coordenador:  __________________________________________".

    FOR LAST tt-fornecs NO-LOCK USE-INDEX colu:
        ASSIGN
            i-linha-aux  = i-linha-aux - 3

            c-range = c-coluna[tt-fornecs.coluna + 1] + string(i-linha-aux + 3)
            chExcelApplication:Range(c-range):value = "Data: " + STRING(TODAY)

            c-range = "A" + string(i-linha-aux) + ":" + c-coluna[tt-fornecs.coluna + 2] + string(i-linha-aux + 4)
            chExcelApplication:Range(c-range):Borders:LineStyle = 1
            chExcelApplication:Range(c-range):Borders:Weight    = 3
            chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142
            chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.
    END.

END PROCEDURE.

procedure pi-planilha-envio:
    find first usuar_mestre no-lock
         where usuar_mestre.cod_usuario = c-seg-usuario no-error.

    if  avail usuar_mestre then do:
        assign c-planilha-envio = usuar_mestre.nom_dir_spool + "\" +
                                  (if  usuar_mestre.nom_subdir_spool <> "" then
                                      usuar_mestre.nom_subdir_spool + "\"
                                  else "") + "Mapa_Proc_" + STRING(ordem-compra.nr-processo,"999999") + "_" +
                                  trim(c-seg-usuario) + ".xls".
    end.
    else
        assign c-planilha-envio = "d:\temp\Mapa_Proc_" + STRING(ordem-compra.nr-processo,"999999") + "_" +
                                  trim(c-seg-usuario) + ".xls".

    ASSIGN
        c-planilha-envio = replace(c-planilha-envio,"/","\").

    if  search(c-planilha-envio) <> "" and
        search(c-planilha-envio) <> ?  then
        dos silent del value(replace(c-planilha-envio,"/","\")).
end.
