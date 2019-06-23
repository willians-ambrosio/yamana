/*****************************************************************************
**       Programa: cm05088.p
**       Data....: 02/03/06
**       Autor...: DATASUL S.A.
**       Objetivo: CUSTOS EQUIPAMENTO
**       VersÆo..: 1.00.000 - 01950518
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

def var c-prog-gerado as char no-undo init "CM05088".

def new global shared var c-arquivo-log as char format "x(60)" no-undo.
def new global shared var c-prg-vrs     as char                no-undo.
def new global shared var c-prg-obj     as char                no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.06.00.000").

/****************** Definição de Tabelas Temporárias do Relatório **********************/

def temp-table tt-raw-digita
    field raw-digita as raw.

def temp-table tt-param
    field destino              as int
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as int
    field parametro            as log
    field formato              as int
    field v_num_tip_aces_usuar as int
    field ep-codigo            as char
    field da-dt-trans-ini      like movto-estoq.dt-trans
    field da-dt-trans-fim      like movto-estoq.dt-trans
    field i-nr-ord-produ-ini   like movto-estoq.nr-ord-produ
    field i-nr-ord-produ-fim   like movto-estoq.nr-ord-produ
    field c-cod-eqpto-ini      like mmv-movto-mater.cod-eqpto
    field c-cod-eqpto-fim      like mmv-movto-mater.cod-eqpto
    field c-cc-codigo-ini      like mmv-ord-manut.cc-codigo
    field c-cc-codigo-fim      like mmv-ord-manut.cc-codigo
    field c-ct-codigo-ini      like mmv-ord-manut.ct-codigo
    field c-ct-codigo-fim      like mmv-ord-manut.ct-codigo
    field c-it-codigo-ini      like mmv-movto-mater.it-codigo
    field c-it-codigo-fim      like mmv-movto-mater.it-codigo
    field c-cod-compon-ini     like mmv-ord-manut.cod-compon
    field c-cod-compon-fim     like mmv-ord-manut.cod-compon
    field i-cd-tipo-ini        like mmv-ord-manut.cd-tipo
    field i-cd-tipo-fim        like mmv-ord-manut.cd-tipo.

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario     like mguni.empresa.ep-codigo        no-undo.
def new global shared var l-implanta              as log    init no.
def new global shared var c-seg-usuario           as char format "x(12)"              no-undo.
def new global shared var i-num-ped-exec-rpw      as int                              no-undo.
def new global shared var i-pais-impto-usuario    as int format ">>9"                 no-undo.
def new global shared var l-rpc                   as log                              no-undo.
def new global shared var r-registro-atual        as rowid                            no-undo.
def new global shared var c-arquivo-log           as char  format "x(60)"             no-undo.
def new global shared var i-num-ped               as int                              no-undo.
def new global shared var v_cdn_empres_usuar      like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren      like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab      as handle                           no-undo.
def new global shared var v_cod_grp_usuar_lst     as char                             no-undo.
def new global shared var v_num_tip_aces_usuar    as int                              no-undo.
def new global shared var rw-log-exec             as rowid                            no-undo.
def new global shared var c-dir-spool-servid-exec as char                             no-undo.

/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var da-dt-trans-ini    like movto-estoq.dt-trans      format "99/99/9999"  init "01/01/1800"       no-undo.
def new shared var da-dt-trans-fim    like movto-estoq.dt-trans      format "99/99/9999"  init "12/31/9999"       no-undo.
def new shared var i-nr-ord-produ-ini like movto-estoq.nr-ord-produ  format ">>>,>>>,>>9" init 0                  no-undo.
def new shared var i-nr-ord-produ-fim like movto-estoq.nr-ord-produ  format ">>>,>>>,>>9" init 999999999          no-undo.
def new shared var c-cod-eqpto-ini    like mmv-movto-mater.cod-eqpto format "x(16)"       init ""                 no-undo.
def new shared var c-cod-eqpto-fim    like mmv-movto-mater.cod-eqpto format "x(16)"       init "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var c-cc-codigo-ini    like mmv-ord-manut.cc-codigo   format "x(8)"        init ""                 no-undo.
def new shared var c-cc-codigo-fim    like mmv-ord-manut.cc-codigo   format "x(8)"        init "ZZZZZZZZ"         no-undo.
def new shared var c-ct-codigo-ini    like mmv-ord-manut.ct-codigo   format "X(8)"        init ""                 no-undo.
def new shared var c-ct-codigo-fim    like mmv-ord-manut.ct-codigo   format "X(8)"        init "ZZZZZZZZ"         no-undo.
def new shared var c-it-codigo-ini    like mmv-movto-mater.it-codigo format "x(16)"       init ""                 no-undo.
def new shared var c-it-codigo-fim    like mmv-movto-mater.it-codigo format "x(16)"       init "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var c-cod-compon-ini   like mmv-ord-manut.cod-compon  format "x(16)"       init ""                 no-undo.
def new shared var c-cod-compon-fim   like mmv-ord-manut.cod-compon  format "x(16)"       init "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var i-cd-tipo-ini      like mmv-ord-manut.cd-tipo     format ">>>,>>>,>>9" init 0                  no-undo.
def new shared var i-cd-tipo-fim      like mmv-ord-manut.cd-tipo     format ">>>,>>>,>>9" init 999999999          no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

def var de-qtd-item-tt-003    like mmv-movto-mater.qtd-item    no-undo.
def var de-val-mater-1-tt-002 like mmv-movto-mater.val-mater-1 no-undo.
def var de-val-mater-1-tt-001 like mmv-movto-mater.val-mater-1 no-undo.

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var h-FunctionLibrary    as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
def var v-cont-registro      as int    no-undo.
def var v-des-retorno        as char   no-undo.
def var v-des-local-layout   as char   no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form mmv-ord-manut.cod-eqpto      column-label "Eqpto"           format "x(12)"             at 001
     mmv-ord-manut.ct-codigo      column-label "Conta"           format "x(8)"              at 014
     mmv-ord-manut.cc-codigo      column-label "C Custo"         format "x(8)"              at 023
     mmv-movto-mater.it-codigo    column-label "Item"            format "x(11)"             at 032
     item.desc-item               column-label "Descri‡Æo"       format "x(20)"             at 044
     movto-estoq.dt-trans         column-label "Transa‡Æo"       format "99/99/9999"        at 065
     mmv-movto-mater.nr-ord-produ column-label "Ordem"           format ">>>,>>>,>>9"       at 076
     mmv-movto-mater.qtd-item     column-label "Quantidade Item" format "->>>,>>>,>>9.9999" at 088
     mmv-movto-mater.val-mater-1  column-label "Valor"           format "->>>,>>>,>>9.9999" at 106
     mmv-movto-mater.log-saida    column-label "D‚bito"          format "Sim/NÆo"           at 124
     with down width 132 no-box stream-io frame f-relat-09-132.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

def var rw-log-exec     as rowid                          no-undo.
def var c-erro-rpc      as char   format "x(60)" init " " no-undo.
def var c-erro-aux      as char   format "x(60)" init " " no-undo.
def var c-ret-temp      as char                           no-undo.
def var h-servid-rpc    as handle                         no-undo.     
def var c-empresa       as char   format "x(40)"          no-undo.
def var c-titulo-relat  as char   format "x(50)"          no-undo.
def var i-numper-x      as int    format "ZZ"             no-undo.
def var da-iniper-x     as date   format "99/99/9999"     no-undo.
def var da-fimper-x     as date   format "99/99/9999"     no-undo.
def var i-page-size-rel as int                            no-undo.
def var c-programa      as char   format "x(08)"          no-undo.
def var c-versao        as char   format "x(04)"          no-undo.
def var c-revisao       as char   format "999"            no-undo.

def new shared var c-impressora  as char                  no-undo.
def new shared var c-layout      as char                  no-undo.
def new shared var v_num_count   as int                   no-undo.
def new shared var c-arq-control as char                  no-undo.
def new shared var c-sistema     as char format "x(25)"   no-undo.
def new shared var c-rodape      as char                  no-undo.

def new shared buffer b_ped_exec_style    for ped_exec.
def new shared buffer b_servid_exec_style for servid_exec.

def new shared stream str-rp.

assign c-programa     = "cm05088"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "CUSTOS EQUIPAMENTO"
       c-sistema      = "Frotas".

find first mguni.empresa no-lock
     where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
assign c-empresa = if  avail mguni.empresa then mguni.empresa.razao-social else "".

if  tt-param.formato = 2 then do:
    form header
        fill("-", 132) format "x(132)" skip
        c-empresa c-titulo-relat at 50
        "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
        fill("-", 112) format "x(110)" today format "99/99/9999"
        "-" string(time, "HH:MM:SS") skip(1)
        with stream-io width 132 no-labels no-box page-top frame f-cabec.

    form header
        fill("-", 132) format "x(132)" skip
        c-empresa c-titulo-relat at 50
        "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
        "Periodo:" i-numper-x at 08 "-"
        da-iniper-x at 14 "to" da-fimper-x
        fill("-", 74) format "x(72)" today format "99/99/9999"
        "-" string(time, "HH:MM:SS") skip(1)
        with stream-io width 132 no-labels no-box page-top frame f-cabper.

    run grapi/gr2004.p.

    form header
        c-rodape format "x(132)"
        with stream-io width 132 no-labels no-box page-bottom frame f-rodape.
end. /* tt-param.formato = 2 */

run grapi/gr2013c.p(input tt-param.destino,
                   input tt-param.arquivo,
                   input tt-param.usuario,
                   input no).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       da-dt-trans-ini     = tt-param.da-dt-trans-ini
       da-dt-trans-fim     = tt-param.da-dt-trans-fim
       i-nr-ord-produ-ini  = tt-param.i-nr-ord-produ-ini
       i-nr-ord-produ-fim  = tt-param.i-nr-ord-produ-fim
       c-cod-eqpto-ini     = tt-param.c-cod-eqpto-ini
       c-cod-eqpto-fim     = tt-param.c-cod-eqpto-fim
       c-cc-codigo-ini     = tt-param.c-cc-codigo-ini
       c-cc-codigo-fim     = tt-param.c-cc-codigo-fim
       c-ct-codigo-ini     = tt-param.c-ct-codigo-ini
       c-ct-codigo-fim     = tt-param.c-ct-codigo-fim
       c-it-codigo-ini     = tt-param.c-it-codigo-ini
       c-it-codigo-fim     = tt-param.c-it-codigo-fim
       c-cod-compon-ini    = tt-param.c-cod-compon-ini
       c-cod-compon-fim    = tt-param.c-cod-compon-fim.

def var l-imprime as logical no-undo.

assign de-qtd-item-tt-003    = 0
       de-val-mater-1-tt-002 = 0
       l-imprime             = no.

if  tt-param.destino = 1 then
    assign v-cod-destino-impres = "Impressora".
else
    if  tt-param.destino = 2 then
        assign v-cod-destino-impres = "Arquivo".
    else
        assign v-cod-destino-impres = "Terminal".

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

/* gr9020a.p */

for each item no-lock
   where item.it-codigo >= c-it-codigo-ini
     and item.it-codigo <= c-it-codigo-fim,
    each movto-estoq no-lock use-index item-data
   where movto-estoq.it-codigo     = item.it-codigo
     and movto-estoq.dt-trans     >= da-dt-trans-ini
     and movto-estoq.dt-trans     <= da-dt-trans-fim
     and movto-estoq.nr-ord-produ >= i-nr-ord-produ-ini
     and movto-estoq.nr-ord-produ <= i-nr-ord-produ-fim,
    each mmv-movto-mater no-lock use-index mmvmvtmt-03
   where mmv-movto-mater.nr-trans   = movto-estoq.nr-trans
     and mmv-movto-mater.cod-eqpto >= c-cod-eqpto-ini
     and mmv-movto-mater.cod-eqpto <= c-cod-eqpto-fim,
    each mmv-ord-manut no-lock use-index mmvrdmnt-id
   where mmv-ord-manut.nr-ord-produ = mmv-movto-mater.nr-ord-produ
     and mmv-ord-manut.cc-codigo   >= c-cc-codigo-ini
     and mmv-ord-manut.cc-codigo   <= c-cc-codigo-fim
     and mmv-ord-manut.cod-compon  >= c-cod-compon-ini
     and mmv-ord-manut.cod-compon  <= c-cod-compon-fim
     and mmv-ord-manut.ct-codigo   >= c-ct-codigo-ini
     and mmv-ord-manut.ct-codigo   <= c-ct-codigo-fim
     and mmv-ord-manut.cd-tipo     >= i-cd-tipo-ini
     and mmv-ord-manut.cd-tipo     <= i-cd-tipo-fim
    break by mmv-movto-mater.cod-eqpto
          by mmv-movto-mater.nr-ord-produ
          by mmv-ord-manut.cod-compon
          by mmv-movto-mater.log-saida:

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Registros Lidos : " + string(v-num-reg-lidos)).

    if  first-of(mmv-movto-mater.cod-eqpto) then
        assign de-val-mater-1-tt-001 = 0.

    assign de-val-mater-1-tt-001 = de-val-mater-1-tt-001 + mmv-movto-mater.val-mater-1
           de-val-mater-1-tt-002 = de-val-mater-1-tt-002 + mmv-movto-mater.val-mater-1
           de-qtd-item-tt-003    = de-qtd-item-tt-003    + mmv-movto-mater.qtd-item.

    /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/

    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        disp stream str-rp
            mmv-ord-manut.cod-eqpto
            mmv-ord-manut.ct-codigo
            mmv-ord-manut.cc-codigo
            mmv-movto-mater.it-codigo
            item.desc-item
            movto-estoq.dt-trans
            mmv-movto-mater.nr-ord-produ
            mmv-movto-mater.qtd-item
            mmv-movto-mater.val-mater-1
            mmv-movto-mater.log-saida
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.
    end.
    if  last-of(mmv-movto-mater.cod-eqpto) then do:
        if  tt-param.formato = 2 then do:
            disp stream str-rp "-----------------" @ 
                mmv-movto-mater.val-mater-1
                with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.
        end.
        else do:
            disp stream str-rp "-----------------" @ 
                mmv-movto-mater.val-mater-1
                with stream-io frame f-relat-09-80.
            down stream str-rp with frame f-relat-09-80.
        end.
        put stream str-rp "Total por equipamento:" at 041 de-val-mater-1-tt-001 format "->>>,>>>,>>9.9999" to 080.
        put stream str-rp unformatted skip(1).
        put stream str-rp unformatted skip(1).
    end.
end.

if  not l-imprime then do:
    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.
    disp stream str-rp "-----------------" @ 
         mmv-movto-mater.qtd-item
         "-----------------" @ 
         mmv-movto-mater.val-mater-1
         with stream-io frame f-relat-09-132.
    down stream str-rp with frame f-relat-09-132.
        put stream str-rp "Quantiadade  por equipamento:" at 016 de-qtd-item-tt-003 format "->>>,>>>,>>9.9999" to 062.
        put stream str-rp "Custo total:" at 051 de-val-mater-1-tt-002 format "->>>,>>>,>>9.9999" to 080.

if  tt-param.destino <> 1 then
    page stream str-rp.
else do:
    if  tt-param.parametro then
        page stream str-rp.
end.

if  tt-param.parametro then do:
   disp stream str-rp "SELE€ÇO" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
        da-dt-trans-ini    colon 19 "|< >|" at 43 da-dt-trans-fim    no-label
        i-nr-ord-produ-ini colon 19 "|< >|" at 43 i-nr-ord-produ-fim no-label
        c-cod-eqpto-ini    colon 19 "|< >|" at 43 c-cod-eqpto-fim    no-label
        c-cc-codigo-ini    colon 19 "|< >|" at 43 c-cc-codigo-fim    no-label
        c-ct-codigo-ini    colon 19 "|< >|" at 43 c-ct-codigo-fim    no-label
        c-it-codigo-ini    colon 19 "|< >|" at 43 c-it-codigo-fim    no-label
        c-cod-compon-ini   colon 19 "|< >|" at 43 c-cod-compon-fim   no-label
        with stream-io side-labels overlay row 026 frame f-imp-sel.

   put stream str-rp unformatted skip(1) "IMPRESSÇO" skip(1).
   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu rio : " tt-param.usuario.
end.
output stream str-rp close.

procedure pi-print-editor:
    def input param c-editor as char no-undo.
    def input param i-len    as int  no-undo.

    def var i-linha as int  no-undo.
    def var i-aux   as int  no-undo.
    def var c-aux   as char no-undo.
    def var c-ret   as char no-undo.

    for each tt-editor: delete tt-editor. end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.

if  valid-handle(h-acomp) then /*gr9030g*/
    run pi-finalizar in h-acomp no-error.

return 'OK'.

/* fim do programa */
