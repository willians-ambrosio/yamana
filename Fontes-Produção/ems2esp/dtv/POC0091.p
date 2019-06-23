/*****************************************************************************
**       Programa: POC0091.p
**       Data....: 05/02/10
**       Autor...: DATASUL S.A.
**       Objetivo: Titulos Pagos
**       VersÆo..: 1.00.000 - 20100079
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "POC0091".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Definição de Tabelas Temporárias do Relatório **********************/

define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
    field c-cod-estabel-ini    like movto_tit_ap.cod_estab  
    field c-cod-estabel-fim    like movto_tit_ap.cod_estab  
    field i-cod-fornec-ini     like movto_tit_ap.cdn_fornecedor 
    field i-cod-fornec-fim     like movto_tit_ap.cdn_fornecedor 
    field c-nr-docto-ini       like movto_tit_ap.cod_tit_ap 
    field c-nr-docto-fim       like movto_tit_ap.cod_tit_ap 
    field da-dt-pagamento-ini  like tit_ap.dat_liquidac_tit_ap
    field da-dt-pagamento-fim  like tit_ap.dat_liquidac_tit_ap
    field da-dt-vencimen-ini   like tit_ap.dat_vencto_tit_ap
    field da-dt-vencimen-fim   like tit_ap.dat_vencto_tit_ap
.

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
def new global shared var v_cod_empres_usuar   like mguni.empresa.ep-codigo  no-undo.
def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo  no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-cod-estabel-ini   like movto_tit_ap.cod_estab        format "x(5)" initial "" no-undo.
def new shared var c-cod-estabel-fim   like movto_tit_ap.cod_estab        format "x(5)" initial "ZZZZZ" no-undo.
def new shared var i-cod-fornec-ini    like movto_tit_ap.cdn_fornecedor   format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-cod-fornec-fim    like movto_tit_ap.cdn_fornecedor   format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var c-nr-docto-ini      like movto_tit_ap.cod_tit_ap       format "x(16)" initial "" no-undo.
def new shared var c-nr-docto-fim      like movto_tit_ap.cod_tit_ap       format "x(16)" initial "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var da-dt-pagamento-ini like tit_ap.dat_liquidac_tit_ap    format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-pagamento-fim like tit_ap.dat_liquidac_tit_ap    format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var da-dt-vencimen-ini  LIKE tit_ap.dat_vencto_tit_ap      format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-vencimen-fim  like tit_ap.dat_vencto_tit_ap      format "99/99/9999" initial "12/31/9999" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

def var de-valor-juros-tt-002 like mov-ap.valor-juros no-undo.
def var de-valor-mov-tt-001 like mov-ap.valor-mov no-undo.
def var de-vl-abatimento-tt-005 like mov-ap.vl-abatimento no-undo.
def var de-vl-desconto-tt-003 like mov-ap.vl-desconto no-undo.
def var de-vl-multa-tt-004 like mov-ap.vl-multa no-undo.

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

form movto_tit_ap.cod_estab            column-label "Est" format "x(3)" at 001
     movto_tit_ap.ind_trans_ap         column-label "Trans" format ">9" at 005
     movto_tit_ap.cod_espec_docto      column-label "Esp" format "!!" at 011
     movto_tit_ap.cdn_fornecedor       column-label "Fornec" format ">>>>>>>>9" at 015
     emitente.nome-abrev               column-label "Nome Abreviado" format "X(12)" at 025
     tit_ap.cod_tit_ap                 column-label "Documento" format "x(16)" at 040
     tit_ap.cod_ser_docto              column-label "S‚rie" format "x(5)" at 057
     tit_ap.cod_parcela                column-label "/P" format "x(02)" at 063
     tit_ap.dat_emis_docto             column-label "Data EmissÆo" format "99/99/9999" at 066
     movto_tit_ap.dat_transacao        column-label "Dt Transa‡Æo" format "99/99/9999" at 079
     tit_ap.dat_vencto_tit_ap          column-label "Vencimento" format "99/99/9999" at 092
     tit_ap.dat_liquidac_tit_ap        column-label "Pagamento" format "99/99/9999" at 103
/*      movto_tit_ap.nr-bordero column-label "Border“" format ">>>>>>>9" at 116 skip */
/*      movto_tit_ap.docto-ant column-label "Doc Antec" format "x(16)" at 001 */
     movto_tit_ap.val_movto_ap         column-label "Vl Movimento" format ">>>>>>>,>>9.99" at 018
     movto_tit_ap.val_juros            column-label "Vl Juros" format ">>>>>>>,>>9.99" at 033
     movto_tit_ap.val_desconto         column-label "Vl Desconto" format ">>>>>>>,>>9.99" at 048
     movto_tit_ap.val_multa_tit_ap     column-label "Vl Multa" format ">>>>>>>,>>9.99" at 063
     movto_tit_ap.val_abat_tit_ap      column-label "Vl Abatimento" format ">>>>>>>,>>9.99" at 078
     tit_ap.ind_origin_tit_ap          column-label "Orig" format "x(3)" at 093
     tit_ap.cod_refer                  column-label "Referˆncia" format "x(10)" at 098
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
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.
define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.
define new shared buffer b_ped_exec_style for ped_exec.
define new shared buffer b_servid_exec_style for servid_exec.
define new shared stream str-rp.

assign c-programa     = "POC0091"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Titulos Pagos"
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


run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input yes).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       c-cod-estabel-ini = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim = tt-param.c-cod-estabel-fim
       i-cod-fornec-ini = tt-param.i-cod-fornec-ini
       i-cod-fornec-fim = tt-param.i-cod-fornec-fim
       c-nr-docto-ini = tt-param.c-nr-docto-ini
       c-nr-docto-fim = tt-param.c-nr-docto-fim
       da-dt-pagamento-ini = tt-param.da-dt-pagamento-ini
       da-dt-pagamento-fim = tt-param.da-dt-pagamento-fim
       da-dt-vencimen-ini = tt-param.da-dt-vencimen-ini
       da-dt-vencimen-fim = tt-param.da-dt-vencimen-fim
.

def var l-imprime as logical no-undo.


        assign de-valor-juros-tt-002 = 0
               de-valor-mov-tt-001 = 0
               de-vl-abatimento-tt-005 = 0
               de-vl-desconto-tt-003 = 0
               de-vl-multa-tt-004 = 0.
assign l-imprime = no.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

/* gr9020b.p */

FOR each tit_ap no-lock
         where tit_ap.cod_estab      >= c-cod-estabel-ini      and 
               tit_ap.cod_estab      <= c-cod-estabel-fim      and
               tit_ap.cdn_fornecedor   >= i-cod-fornec-ini       and 
               tit_ap.cdn_fornecedor   <= i-cod-fornec-fim       and
               tit_ap.cod_tit_ap        >= c-nr-docto-ini        and 
               tit_ap.cod_tit_ap        <= c-nr-docto-fim        AND
               tit_ap.cod_empresa       = v_cod_empres_usuar and
               tit_ap.dat_vencto_tit_ap >= da-dt-vencimen-ini    AND
               tit_ap.dat_vencto_tit_ap <= da-dt-vencimen-fim    AND
               tit_ap.dat_liquidac_tit_ap >= da-dt-pagamento-ini AND
               tit_ap.dat_liquidac_tit_ap <= da-dt-pagamento-fim,
    EACH movto_tit_ap OF tit_ap NO-LOCK.

    FIND emitente WHERE emitente.cod-emit = tit_ap.cdn_fornecedor NO-LOCK NO-ERROR.
    
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign l-imprime = yes.
    
    put stream str-rp unformatted 
            movto_tit_ap.cod_estab ";"
            movto_tit_ap.ind_trans_ap ";"
            movto_tit_ap.cod_espec_docto ";"
            movto_tit_ap.cdn_fornecedor ";"
            emitente.nome-abrev ";"    
            tit_ap.cod_tit_ap ";"
            tit_ap.cod_ser_docto ";"
            tit_ap.cod_parcela ";"
            tit_ap.dat_emis_docto ";"
            movto_tit_ap.dat_transacao ";"
            tit_ap.dat_vencto_tit_ap ";"
            tit_ap.dat_liquidac_tit_ap ";"
/*             movto_tit_ap.nr-bordero ";" */
/*             movto_tit_ap.docto-ant ";" */
            movto_tit_ap.val_movto_ap ";"
            movto_tit_ap.val_juros ";"
            movto_tit_ap.val_desconto ";"
            movto_tit_ap.val_multa_tit_ap ";"
            movto_tit_ap.val_abat_tit_ap ";"
            tit_ap.ind_origin_tit_ap ";"
            tit_ap.cod_refer skip.
    
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
