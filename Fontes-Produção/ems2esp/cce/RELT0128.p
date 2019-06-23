/*****************************************************************************
**       Programa: RELT0128.p
**       Data....: 30/03/12
**       Autor...: DATASUL S.A.
**       Objetivo: PEDIDOS POR COMPRADOR - CT
**       VersÆo..: 1.00.000 - adm
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "RELT0128".

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
    field da-dat-ordem-ini like ordem-compra.dat-ordem
    field da-dat-ordem-fim like ordem-compra.dat-ordem
    field i-cod-emitente-ini like ordem-compra.cod-emitente
    field i-cod-emitente-fim like ordem-compra.cod-emitente
    field c-cod-comprado-ini like ordem-compra.cod-comprado
    field c-cod-comprado-fim like ordem-compra.cod-comprado
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

def new shared var da-dat-ordem-ini like ordem-compra.dat-ordem format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dat-ordem-fim like ordem-compra.dat-ordem format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var i-cod-emitente-ini like ordem-compra.cod-emitente format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-cod-emitente-fim like ordem-compra.cod-emitente format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var c-cod-comprado-ini like ordem-compra.cod-comprado format "X(12)" initial "" no-undo.
def new shared var c-cod-comprado-fim like ordem-compra.cod-comprado format "X(12)" initial "ZZZZZZZZZZZZ" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

def var descricao as character.
def var Vlr_Ttal_fch as decimal label "Vlr Total".

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

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

form ordem-compra.cod-comprado column-label "Comprador" format "X(12)" at 001
     ordem-compra.cod-emitente column-label "Fornec" format ">>>>>>>>9" at 014
     ordem-compra.it-codigo column-label "Item" format "X(16)" at 024
     Vlr_Ttal_fch column-label "Vlr Total" format ">,>>>,>>9.99" at 041
     ordem-compra.num-pedido column-label "Pedido" format ">>>>>,>>9" at 054
     ordem-compra.qt-solic column-label "Qtde" format ">,>>>,>>9.99" at 064
     ordem-compra.preco-unit column-label "Pre‡o Unit" format ">,>>>,>>9.99" at 081
     ordem-compra.numero-ordem column-label "Ordem" format "zzzzz9,99" at 095
     ordem-compra.dat-ordem column-label "Data Ordem" format "99/99/9999" at 105
     ordem-compra.data-pedido column-label "Data Pedido" format "99/99/9999" at 116 skip
     ordem-compra.nr-contrato column-label "Contrato" format ">>>>>>>>9" at 001
     item.desc-item column-label "Descri‡Æo" format "x(60)" at 018
     descricao column-label "Narratia" format "x(30)" at 079
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

assign c-programa     = "RELT0128"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "PEDIDOS POR COMPRADOR - CT"
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
       da-dat-ordem-ini = tt-param.da-dat-ordem-ini
       da-dat-ordem-fim = tt-param.da-dat-ordem-fim
       i-cod-emitente-ini = tt-param.i-cod-emitente-ini
       i-cod-emitente-fim = tt-param.i-cod-emitente-fim
       c-cod-comprado-ini = tt-param.c-cod-comprado-ini
       c-cod-comprado-fim = tt-param.c-cod-comprado-fim
.

def var l-imprime as logical no-undo.

assign l-imprime = no.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

/* gr9020b.p */

for each pedido-compr no-lock,
    each ordem-compra no-lock
         where ordem-compra.num-pedido = pedido-compr.num-pedido and
               ordem-compra.cod-comprado >= c-cod-comprado-ini and 
               ordem-compra.cod-comprado <= c-cod-comprado-fim and
               ordem-compra.cod-emitente >= i-cod-emitente-ini and 
               ordem-compra.cod-emitente <= i-cod-emitente-fim and
               ordem-compra.dat-ordem >= da-dat-ordem-ini and 
               ordem-compra.dat-ordem <= da-dat-ordem-fim,
    each item no-lock
         where item.it-codigo = ordem-compra.it-codigo and
               pedido-compr.nr-contrato  <> 0 and
               ordem-compra.it-codigo  <= "99990001"
    break by ordem-compra.nr-contrato:
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign Vlr_Ttal_fch = (ordem-compra.valor-descto + ordem-compra.preco-unit ) * ordem-compra.qt-solic .
    assign l-imprime = yes.
    descricao = REPLACE(ordem-compra.narrativa,chr(10)," ").
    descricao = REPLACE(descricao,chr(13)," ").
    put stream str-rp ordem-compra.cod-comprado format "X(12)" 
            ordem-compra.cod-emitente format ">>>>>>>>9" 
            ordem-compra.it-codigo format "X(16)" 
            Vlr_Ttal_fch format ">,>>>,>>9.99" 
            ordem-compra.num-pedido format ">>>>>,>>9" 
            ordem-compra.qt-solic format ">,>>>,>>9.99" 
            ordem-compra.preco-unit format ">,>>>,>>9.99" 
            ordem-compra.numero-ordem format "zzzzz9,99" 
            ordem-compra.dat-ordem format "99/99/9999" 
            ordem-compra.data-pedido format "99/99/9999" 
            ordem-compra.nr-contrato format ">>>>>>>>9" 
            item.desc-item format "x(60)" 
            descricao format "x(30)"  skip.
    
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
