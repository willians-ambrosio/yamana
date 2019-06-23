/*****************************************************************************
**       Programa: POC0041.p
**       Data....: 05/02/10
**       Autor...: DATASUL S.A.
**       Objetivo: Notas Fiscais Recebidas - Laurence _ colocar em pr
**       VersÆo..: 1.00.000 - 20100079
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "POC0041".

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
    field i-cod-emitente-ini like docum-est.cod-emitente
    field i-cod-emitente-fim like docum-est.cod-emitente
    field c-nro-docto-ini like docum-est.nro-docto
    field c-nro-docto-fim like docum-est.nro-docto
    field c-nat-operacao-ini like docum-est.nat-operacao
    field c-nat-operacao-fim like docum-est.nat-operacao
    field da-dt-emissao-ini like docum-est.dt-emissao
    field da-dt-emissao-fim like docum-est.dt-emissao
    field da-dt-trans-050-ini like docum-est.dt-trans
    field da-dt-trans-050-fim like docum-est.dt-trans
    field da-dt-emissao-060-ini like dupli-apagar.dt-emissao
    field da-dt-emissao-060-fim like dupli-apagar.dt-emissao
    field da-dt-trans-ini like dupli-apagar.dt-trans
    field da-dt-trans-fim like dupli-apagar.dt-trans
    field da-dt-vencim-ini like dupli-apagar.dt-vencim
    field da-dt-vencim-fim like dupli-apagar.dt-vencim
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

def new shared var i-cod-emitente-ini like docum-est.cod-emitente format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-cod-emitente-fim like docum-est.cod-emitente format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var c-nro-docto-ini like docum-est.nro-docto format "x(16)" initial "" no-undo.
def new shared var c-nro-docto-fim like docum-est.nro-docto format "x(16)" initial "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var c-nat-operacao-ini like docum-est.nat-operacao format "x(06)" initial "" no-undo.
def new shared var c-nat-operacao-fim like docum-est.nat-operacao format "x(06)" initial "ZZZZZZ" no-undo.
def new shared var da-dt-emissao-ini like docum-est.dt-emissao format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-emissao-fim like docum-est.dt-emissao format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var da-dt-trans-050-ini like docum-est.dt-trans format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-trans-050-fim like docum-est.dt-trans format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var da-dt-emissao-060-ini like dupli-apagar.dt-emissao format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-emissao-060-fim like dupli-apagar.dt-emissao format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var da-dt-trans-ini like dupli-apagar.dt-trans format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-trans-fim like dupli-apagar.dt-trans format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var da-dt-vencim-ini like dupli-apagar.dt-vencim format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-vencim-fim like dupli-apagar.dt-vencim format "99/99/9999" initial "12/31/9999" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

def var de-vl-a-pagar-tt-001 like dupli-apagar.vl-a-pagar no-undo.

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

form dupli-apagar.cod-estabel column-label "Est" format "x(3)" at 001
     dupli-apagar.cod-emitente column-label "Emit" format ">>>>>>>>9" at 005
     emitente.nome-abrev column-label "Nome Abreviado" format "X(12)" at 015
     dupli-apagar.cod-esp column-label "Esp" format "!!" at 030
     docum-est.nro-docto column-label "Documento" format "x(16)" at 034
     dupli-apagar.nr-duplic column-label "Duplicata" format "x(16)" at 051
     docum-est.nat-operacao column-label "Nat Oper" format "x(06)" at 068
     dupli-apagar.serie-docto column-label "Ser" format "x(5)" at 077
     dupli-apagar.parcela column-label "Parc" format "x(2)" at 083
     docum-est.dt-emissao column-label "EmissÆo" format "99/99/9999" at 088
     dupli-apagar.dt-emissao column-label "EmissÆo" format "99/99/9999" at 099
     dupli-apagar.dt-trans column-label "Transa‡Æo" format "99/99/9999" at 110
     docum-est.dt-trans column-label "Transa‡Æo" format "99/99/9999" at 121 skip
     dupli-apagar.dt-vencim column-label "Vencimento" format "99/99/9999" at 001
     dupli-apagar.vl-a-pagar column-label "Vlr da Duplicata" format ">>>>>,>>>,>>9.99" at 012
     docum-est.valor-mercad column-label "Vlr Total Mercadorias" format ">>>>>,>>>,>>9.99" at 029
     docum-est.tot-valor column-label "Vlr Total" format ">>>>,>>>,>>>,>>9.99" at 051
     docum-est.ct-transit column-label "Cta Trans" format "x(8)" at 071
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

assign c-programa     = "POC0041"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "Notas Fiscais Recebidas - Laurence _ colocar em pr"
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
       i-cod-emitente-ini = tt-param.i-cod-emitente-ini
       i-cod-emitente-fim = tt-param.i-cod-emitente-fim
       c-nro-docto-ini = tt-param.c-nro-docto-ini
       c-nro-docto-fim = tt-param.c-nro-docto-fim
       c-nat-operacao-ini = tt-param.c-nat-operacao-ini
       c-nat-operacao-fim = tt-param.c-nat-operacao-fim
       da-dt-emissao-ini = tt-param.da-dt-emissao-ini
       da-dt-emissao-fim = tt-param.da-dt-emissao-fim
       da-dt-trans-050-ini = tt-param.da-dt-trans-050-ini
       da-dt-trans-050-fim = tt-param.da-dt-trans-050-fim
       da-dt-emissao-060-ini = tt-param.da-dt-emissao-060-ini
       da-dt-emissao-060-fim = tt-param.da-dt-emissao-060-fim
       da-dt-trans-ini = tt-param.da-dt-trans-ini
       da-dt-trans-fim = tt-param.da-dt-trans-fim
       da-dt-vencim-ini = tt-param.da-dt-vencim-ini
       da-dt-vencim-fim = tt-param.da-dt-vencim-fim
.

def var l-imprime as logical no-undo.


        assign de-vl-a-pagar-tt-001 = 0.
assign l-imprime = no.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

/* gr9020b.p */

for each dupli-apagar no-lock
         where dupli-apagar.dt-emissao >= da-dt-emissao-060-ini and 
               dupli-apagar.dt-emissao <= da-dt-emissao-060-fim and
               dupli-apagar.dt-trans >= da-dt-trans-ini and 
               dupli-apagar.dt-trans <= da-dt-trans-fim and
               dupli-apagar.dt-vencim >= da-dt-vencim-ini and 
               dupli-apagar.dt-vencim <= da-dt-vencim-fim,
    each docum-est no-lock
         where docum-est.serie-docto = dupli-apagar.serie-docto and
               docum-est.nro-docto = dupli-apagar.nro-docto and
               docum-est.cod-emitente = dupli-apagar.cod-emitente and
               docum-est.nat-operacao = dupli-apagar.nat-operacao and
               docum-est.cod-emitente >= i-cod-emitente-ini and 
               docum-est.cod-emitente <= i-cod-emitente-fim and
               docum-est.dt-emissao >= da-dt-emissao-ini and 
               docum-est.dt-emissao <= da-dt-emissao-fim and
               docum-est.dt-trans >= da-dt-trans-050-ini and 
               docum-est.dt-trans <= da-dt-trans-050-fim and
               docum-est.nat-operacao >= c-nat-operacao-ini and 
               docum-est.nat-operacao <= c-nat-operacao-fim and
               docum-est.nro-docto >= c-nro-docto-ini and 
               docum-est.nro-docto <= c-nro-docto-fim,
    each emitente no-lock
         where emitente.cod-emitente = docum-est.cod-emitente:
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign l-imprime = yes.
    
    put stream str-rp unformatted dupli-apagar.cod-estabel ";"
            dupli-apagar.cod-emitente ";"
            emitente.nome-abrev ";"
            dupli-apagar.cod-esp ";"
            docum-est.nro-docto ";"
            dupli-apagar.nr-duplic ";"
            docum-est.nat-operacao ";"
            dupli-apagar.serie-docto ";"
            dupli-apagar.parcela ";"
            docum-est.dt-emissao ";"
            dupli-apagar.dt-emissao ";"
            dupli-apagar.dt-trans ";"
            docum-est.dt-trans ";"
            dupli-apagar.dt-vencim ";"
            dupli-apagar.vl-a-pagar ";"
            docum-est.valor-mercad ";"
            docum-est.tot-valor ";"
            docum-est.ct-transit skip.
    
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
