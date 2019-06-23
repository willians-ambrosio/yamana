/*****************************************************************************
**       Programa: SU00033.p
**       Data....: 31/08/10
**       Autor...: DATASUL S.A.
**       Objetivo: TOTAL DE COMPRAS RECEBIDAS UF
**       VersÆo..: 1.00.000 - 20100038
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "SU00033".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.06.00.000").

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
    field c-it-codigo-ini like recebimento.it-codigo
    field c-it-codigo-fim like recebimento.it-codigo
    field i-num-pedido-ini like ordem-compra.num-pedido
    field i-num-pedido-fim like ordem-compra.num-pedido
    field da-data-movto-ini like recebimento.data-movto
    field da-data-movto-fim like recebimento.data-movto
    field c-cod-estabel-ini like ordem-compra.cod-estabel
    field c-cod-estabel-fim like ordem-compra.cod-estabel
    field c-cod-comprado-ini like ordem-compra.cod-comprado
    field c-cod-comprado-fim like ordem-compra.cod-comprado
    field i-num-ord-inv-ini like ordem-compra.num-ord-inv
    field i-num-ord-inv-fim like ordem-compra.num-ord-inv
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
def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-it-codigo-ini like recebimento.it-codigo format "X(16)" initial "" no-undo.
def new shared var c-it-codigo-fim like recebimento.it-codigo format "X(16)" initial "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var i-num-pedido-ini like ordem-compra.num-pedido format ">>>>>,>>9" initial 0 no-undo.
def new shared var i-num-pedido-fim like ordem-compra.num-pedido format ">>>>>,>>9" initial 99999999 no-undo.
def new shared var da-data-movto-ini like recebimento.data-movto format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-data-movto-fim like recebimento.data-movto format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var c-cod-estabel-ini like ordem-compra.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-cod-estabel-fim like ordem-compra.cod-estabel format "x(3)" initial "ZZZ" no-undo.
def new shared var c-cod-comprado-ini like ordem-compra.cod-comprado format "X(12)" initial "" no-undo.
def new shared var c-cod-comprado-fim like ordem-compra.cod-comprado format "X(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var i-num-ord-inv-ini like ordem-compra.num-ord-inv format ">>>,>>>,>>9" initial 0 no-undo.
def new shared var i-num-ord-inv-fim like ordem-compra.num-ord-inv format ">>>,>>>,>>9" initial 999999999 no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

def var ems2cademp-item-ext-ind-serv-mat as character no-undo.
def var ems2cademp-item-ext-tipo-contr as character no-undo.

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

def var de-valor-total-tt-001 like recebimento.valor-total no-undo.

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

form recebimento.data-movto column-label "Movimento" format "99/99/9999" at 001
     emitente.nome-abrev column-label "Nome Abreviado" format "X(12)" at 012
     recebimento.cod-emitente column-label "Fornec" format ">>>>>>>>9" at 027
     emitente.estado column-label "UF" format "x(4)" at 037
     emitente.cidade column-label "Cidade" format "X(25)" at 042
     recebimento.it-codigo column-label "Item" format "X(16)" at 068
     ordem-compra.numero-ordem column-label "Ordem" format "zzzzz9,99" at 085
     ordem-compra.nr-contrato column-label "Contrato" format ">>>>>>>>9" at 095
     recebimento.valor-total column-label "Vlr Total" format ">>>,>>>,>>9.9999" at 112 skip
     recebimento.quant-receb column-label "Qtde Receb" format ">>>>,>>9.9999" at 001
     recebimento.quant-rejeit column-label "Qtde Devol" format ">>>>,>>9.9999" at 015
     ordem-compra.conta-contabil column-label "Conta Cont bil" format "x(17)" at 029
     ems2cademp-item-ext-ind-serv-mat column-label "Aplica‡Æo" format "x(008)" at 047
     ems2cademp-item-ext-tipo-contr column-label "Tp Controle" format "x(013)" at 057
     ordem-compra.num-ord-inv COLUMN-LABEL "Ord Inv" format ">>>,>>>,>>9"
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

assign c-programa     = "SU00033"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "TOTAL DE COMPRAS RECEBIDAS UF"
       c-sistema      = "EMS".


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
       c-it-codigo-ini = tt-param.c-it-codigo-ini
       c-it-codigo-fim = tt-param.c-it-codigo-fim
       i-num-pedido-ini = tt-param.i-num-pedido-ini
       i-num-pedido-fim = tt-param.i-num-pedido-fim
       da-data-movto-ini = tt-param.da-data-movto-ini
       da-data-movto-fim = tt-param.da-data-movto-fim
       c-cod-estabel-ini = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim = tt-param.c-cod-estabel-fim
       c-cod-comprado-ini = tt-param.c-cod-comprado-ini
       c-cod-comprado-fim = tt-param.c-cod-comprado-fim
       i-num-ord-inv-ini = tt-param.i-num-ord-inv-ini
       i-num-ord-inv-fim = tt-param.i-num-ord-inv-fim
.

def var l-imprime as logical no-undo.


        assign de-valor-total-tt-001 = 0.
assign l-imprime = no.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

/* gr9020b.p */

for each ordem-compra no-lock
         where ordem-compra.cod-comprado >= c-cod-comprado-ini and 
               ordem-compra.cod-comprado <= c-cod-comprado-fim and
               ordem-compra.cod-estabel >= c-cod-estabel-ini and 
               ordem-compra.cod-estabel <= c-cod-estabel-fim and
               ordem-compra.num-ord-inv >= i-num-ord-inv-ini and 
               ordem-compra.num-ord-inv <= i-num-ord-inv-fim and
               ordem-compra.num-pedido >= i-num-pedido-ini and 
               ordem-compra.num-pedido <= i-num-pedido-fim,
    each recebimento no-lock
         where recebimento.numero-ordem = ordem-compra.numero-ordem and
               recebimento.data-movto >= da-data-movto-ini and 
               recebimento.data-movto <= da-data-movto-fim and
               recebimento.it-codigo >= c-it-codigo-ini and 
               recebimento.it-codigo <= c-it-codigo-fim,
    each item no-lock
         where item.it-codigo = recebimento.it-codigo,
    each emitente no-lock
         where emitente.cod-emitente = recebimento.cod-emitente:
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign ems2cademp-item-ext-ind-serv-mat = 
               entry(item.ind-serv-mat,"Servi‡o,Material")
               
           ems2cademp-item-ext-tipo-contr = 
               entry(item.tipo-contr,"F¡sico,Total,Consignado,D‚bito Direto")
               .
    assign l-imprime = yes.
    
    put stream str-rp unformatted recebimento.data-movto ";"
            emitente.nome-abrev ";"
            recebimento.cod-emitente ";"
            emitente.estado ";"
            emitente.cidade ";"
            recebimento.it-codigo ";"
            ordem-compra.numero-ordem ";"
            ordem-compra.nr-contrato ";"
            recebimento.valor-total ";"
            recebimento.quant-receb ";"
            recebimento.quant-rejeit ";"
            ordem-compra.conta-contabil ";"
            ems2cademp-item-ext-ind-serv-mat ";"
            ems2cademp-item-ext-tipo-contr ";"
            ordem-compra.num-ord-inv
        skip.
    
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
