/*****************************************************************************
**       Programa: CTL0011.p
**       Data....: 07/11/14
**       Autor...: DATASUL S.A.
**       Objetivo: Implantacao de titulos por usuario
**       VersÆo..: 1.00.000 - adm
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/
def buffer fornecedor for ems5.fornecedor.

def var c-prog-gerado as character no-undo initial "CTL0011".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
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
    field c-cod-estabel-ini    like tit_ap.cod_estab
    field c-cod-estabel-fim    like tit_ap.cod_estab
    field da-dt-transacao-ini  like tit_ap.dat_transacao
    field da-dt-transacao-fim  like tit_ap.dat_transacao
    field da-dt-emissao-ini    like tit_ap.dat_emis_docto
    field da-dt-emissao-fim    like tit_ap.dat_emis_docto
    field c-origem-ini         like tit_ap.ind_origin_tit_ap
    field c-origem-fim         like tit_ap.ind_origin_tit_ap
    field c-usuario-ini        like movto_tit_ap.cod_usuario
    field c-usuario-fim        like movto_tit_ap.cod_usuario.

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo  no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-cod-estabel-ini   like tit_ap.cod_estab         format "x(5)"       initial ""             no-undo.
def new shared var c-cod-estabel-fim   like tit_ap.cod_estab         format "x(5)"       initial "ZZZZZ"        no-undo.
def new shared var da-dt-transacao-ini like tit_ap.dat_transacao     format "99/99/9999" initial "01/01/1800"   no-undo.
def new shared var da-dt-transacao-fim like tit_ap.dat_transacao     format "99/99/9999" initial "12/31/9999"   no-undo.
def new shared var da-dt-emissao-ini   like tit_ap.dat_emis_docto    format "99/99/9999" initial "01/01/1800"   no-undo.
def new shared var da-dt-emissao-fim   like tit_ap.dat_emis_docto    format "99/99/9999" initial "12/31/9999"   no-undo.
def new shared var c-origem-ini        like tit_ap.ind_origin_tit_ap format "x(03)"      initial ""             no-undo.
def new shared var c-origem-fim        like tit_ap.ind_origin_tit_ap format "x(03)"      initial "ZZZ"          no-undo.
def new shared var c-usuario-ini       like movto_tit_ap.cod_usuario format "x(12)"      initial ""             no-undo.
def new shared var c-usuario-fim       like movto_tit_ap.cod_usuario format "x(12)"      initial "ZZZZZZZZZZZZ" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

def var de-vl-orig-me-tt-001 like tit_ap.val_origin_tit_ap no-undo.

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

form tit_ap.cod_estab column-label "Est" format "x(3)" at 001
     tit_ap.cod_espec_docto column-label "Esp" format "!!" at 005
     tit_ap.cdn_fornecedor column-label "Fornec" format ">>>>>>>>9" at 009
     fornecedor.nom_abrev column-label "Nome Abrev." format "X(12)" at 019
     movto_tit_ap.dat_transacao column-label "Dt Trans." format "99/99/9999" at 032
     tit_ap.dat_vencto_tit_ap column-label "Dt Vcto" format "99/99/9999" at 043
     movto_tit_ap.dat_gerac_movto column-label "Dt Maq" format "99/99/9999" at 054
     tit_ap.cod_tit_ap column-label "Documento" format "x(16)" at 065
     tit_ap.val_origin_tit_ap column-label "Vl Original ME" format "->>>,>>>,>>9.99" at 082
     tit_ap.ind_origin_tit_ap column-label "Orig" format "x(3)" at 098
     tit_ap.cod_refer column-label "Referˆncia" format "x(10)" at 103
     movto_tit_ap.cod_usuario column-label "Usu rio" format "x(12)" at 114
     with down width 132 no-box stream-io frame f-relat-09-132.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
def var c-empresa       as character format "x(40)"      no-undo.
def var c-titulo-relat  as character format "x(50)"      no-undo.
def var i-numper-x      as integer   format "ZZ"         no-undo.
def var da-iniper-x     as date      format "99/99/9999" no-undo.
def var da-fimper-x     as date      format "99/99/9999" no-undo.
def var i-page-size-rel as integer                       no-undo.
def var c-programa      as character format "x(08)"      no-undo.
def var c-versao        as character format "x(04)"      no-undo.
def var c-revisao       as character format "999"        no-undo.
def new shared var c-impressora   as character                      no-undo.
def new shared var c-layout       as character                      no-undo.
def new shared var v_num_count     as integer                       no-undo.
def new shared var c-arq-control   as character                     no-undo.
def new shared var c-sistema       as character format "x(25)"      no-undo.
def new shared var c-rodape        as character                     no-undo.
def new shared buffer b_ped_exec_style for ped_exec.
def new shared buffer b_servid_exec_style for servid_exec.
def new shared stream str-rp.

assign c-programa     = "CTL0011"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Implantacao de titulos por usuario"
       c-sistema      = "".

find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".

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
       c-cod-estabel-ini   = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim   = tt-param.c-cod-estabel-fim
       da-dt-transacao-ini = tt-param.da-dt-transacao-ini
       da-dt-transacao-fim = tt-param.da-dt-transacao-fim
       da-dt-emissao-ini   = tt-param.da-dt-emissao-ini
       da-dt-emissao-fim   = tt-param.da-dt-emissao-fim
       c-origem-ini        = tt-param.c-origem-ini
       c-origem-fim        = tt-param.c-origem-fim
       c-usuario-ini       = tt-param.c-usuario-ini
       c-usuario-fim       = tt-param.c-usuario-fim.

def var l-imprime as logical no-undo.

assign de-vl-orig-me-tt-001 = 0.
assign l-imprime = no.
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

for each tit_ap no-lock
   where tit_ap.cod_estab         >= c-cod-estabel-ini
     and tit_ap.cod_estab         <= c-cod-estabel-fim
     and tit_ap.dat_emis_docto    >= da-dt-emissao-ini
     and tit_ap.dat_emis_docto    <= da-dt-emissao-fim
     and tit_ap.dat_transacao     >= da-dt-transacao-ini
     and tit_ap.dat_transacao     <= da-dt-transacao-fim
     and tit_ap.ind_origin_tit_ap >= c-origem-ini
     and tit_ap.ind_origin_tit_ap <= c-origem-fim,
   first fornecedor of tit_ap no-lock,
    each movto_tit_ap no-lock
   where movto_tit_ap.cod_estab     = tit_ap.cod_estab
     and movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap
     AND movto_tit_ap.ind_trans_ap = "Implanta‡Æo"
     and movto_tit_ap.cod_usuario  >= c-usuario-ini
     and movto_tit_ap.cod_usuario  <= c-usuario-fim:
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign de-vl-orig-me-tt-001 = de-vl-orig-me-tt-001 + tit_ap.val_origin_tit_ap.

    /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/
    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        disp stream str-rp tit_ap.cod_estab
             tit_ap.cod_espec_docto
             tit_ap.cdn_fornecedor
             fornecedor.nom_abrev
             movto_tit_ap.dat_transacao
             tit_ap.dat_vencto_tit_ap
             movto_tit_ap.dat_gerac_movto
             tit_ap.cod_tit_ap
             tit_ap.val_origin_tit_ap
             tit_ap.ind_origin_tit_ap
             tit_ap.cod_refer
             movto_tit_ap.cod_usuario
            with stream-io frame f-relat-09-132.
        down stream str-rp with frame f-relat-09-132.
    end.
end.

if  l-imprime = no then do:
    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.
    disp stream str-rp "---------------" @ 
         tit_ap.val_origin_tit_ap
        with stream-io frame f-relat-09-132.
    down stream str-rp with frame f-relat-09-132.
    put stream str-rp de-vl-orig-me-tt-001 format "->>>,>>>,>>9.99" to 096.

if  tt-param.destino <> 1 then
    page stream str-rp.
else do:
    if   tt-param.parametro then
         page stream str-rp.
end.

if  tt-param.parametro then do:
    disp stream str-rp "SELE€ÇO" skip(01) with stream-io frame f-imp-sel.
    disp stream str-rp 
         c-cod-estabel-ini colon 17 "|< >|" at 41 c-cod-estabel-fim no-label
         da-dt-transacao-ini colon 17 "|< >|" at 41 da-dt-transacao-fim no-label
         da-dt-emissao-ini colon 17 "|< >|" at 41 da-dt-emissao-fim no-label
         c-origem-ini colon 17 "|< >|" at 41 c-origem-fim no-label
         c-usuario-ini colon 17 "|< >|" at 41 c-usuario-fim no-label
        with stream-io side-labels overlay row 030 frame f-imp-sel.

   disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   Implantacao de titulos por usuario"
        with stream-io side-labels overlay row 030 frame f-imp-cla.

   put stream str-rp unformatted skip(1)
       "IMPRESSÇO" skip(1)
       skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)"
       skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch"
       skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas"
       skip "    " "Usu rio : " tt-param.usuario.
end.
output stream str-rp close.

procedure pi-print-editor:

    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

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

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.

/* fim do programa */
