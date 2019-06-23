/*****************************************************************************
**       Programa: RELT0130.p
**       Data....: 25/11/13
**       Autor...: DATASUL S.A.
**       Objetivo: FOLLOW UP COMPRAS DE INVESTIMENTO
**       VersÆo..: 1.00.000 - adm
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "RELT0130".

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
    field i-cod-emitente-ini like ordem-compra.cod-emitente
    field i-cod-emitente-fim like ordem-compra.cod-emitente
    field c-cod-comprado-ini like ordem-compra.cod-comprado
    field c-cod-comprado-fim like ordem-compra.cod-comprado
    field da-data-pedido-ini like pedido-compr.data-pedido
    field da-data-pedido-fim like pedido-compr.data-pedido
    field c-cod-estabel-ini like pedido-compr.cod-estabel
    field c-cod-estabel-fim like pedido-compr.cod-estabel
    field c-ct-codigo-ini like ordem-compra.ct-codigo
    field c-ct-codigo-fim like ordem-compra.ct-codigo
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

def new shared var i-cod-emitente-ini like ordem-compra.cod-emitente format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-cod-emitente-fim like ordem-compra.cod-emitente format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var c-cod-comprado-ini like ordem-compra.cod-comprado format "X(12)" initial "" no-undo.
def new shared var c-cod-comprado-fim like ordem-compra.cod-comprado format "X(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var da-data-pedido-ini like pedido-compr.data-pedido format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-data-pedido-fim like pedido-compr.data-pedido format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var c-cod-estabel-ini like pedido-compr.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-cod-estabel-fim like pedido-compr.cod-estabel format "x(3)" initial "ZZZ" no-undo.
def new shared var c-ct-codigo-ini like ordem-compra.ct-codigo format "x(17)" initial "" no-undo.
def new shared var c-ct-codigo-fim like ordem-compra.ct-codigo format "x(17)" initial "ZZZZZZZZZZZZZZZZZ" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

def var descricao as character.
def var Vlr_total as decimal label "Total OC Decto".

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

form emitente.nome-emit column-label "Nome" format "X(40)" at 001
     emitente.cod-emitente column-label "C¢digo" format ">>>>>>>>9" at 042
     emitente.telefone[1] column-label "Telefone" format "x(15)" at 052
     with down width 132 no-box stream-io frame f-relat-01-132.

form ordem-compra.cod-comprado column-label "Comprador" format "X(8)" at 001
     ordem-compra.num-pedido column-label "Pedido" format ">>>>>,>>9" at 011
     ordem-compra.data-pedido column-label "Dt Pedido" format "99/99/99" at 021
     ordem-compra.numero-ordem column-label "Ordem" format "zzzzz9,99" at 031
     ordem-compra.dat-ordem column-label "Dt Ordem" format "99/99/99" at 041
     ordem-compra.num-ord-inv column-label "Nr OI" format ">>>,>>9" at 050
     ordem-compra.it-codigo column-label "Item" format "X(10)" at 058
     descricao column-label "Narrativa" format "x(25)" at 068
     ordem-compra.preco-unit column-label "Pre‡o Unit" format ">,>>>,>>9.99" at 094
     ordem-compra.qt-solic column-label "Qtde" format ">>>,>>9.99" at 108
     Vlr_total column-label "Total OC Decto" format ">,>>>,>>9.99" at 119
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

assign c-programa     = "RELT0130"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "FOLLOW UP COMPRAS DE INVESTIMENTO"
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
                    input no).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       i-cod-emitente-ini = tt-param.i-cod-emitente-ini
       i-cod-emitente-fim = tt-param.i-cod-emitente-fim
       c-cod-comprado-ini = tt-param.c-cod-comprado-ini
       c-cod-comprado-fim = tt-param.c-cod-comprado-fim
       da-data-pedido-ini = tt-param.da-data-pedido-ini
       da-data-pedido-fim = tt-param.da-data-pedido-fim
       c-cod-estabel-ini = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim = tt-param.c-cod-estabel-fim
       c-ct-codigo-ini = tt-param.c-ct-codigo-ini
       c-ct-codigo-fim = tt-param.c-ct-codigo-fim
.

def var l-imprime as logical no-undo.

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

for each pedido-compr no-lock
         where pedido-compr.cod-estabel >= c-cod-estabel-ini and 
               pedido-compr.cod-estabel <= c-cod-estabel-fim and
               pedido-compr.data-pedido >= da-data-pedido-ini and 
               pedido-compr.data-pedido <= da-data-pedido-fim,
    each ordem-compra no-lock
         where ordem-compra.num-pedido = pedido-compr.num-pedido and
               ordem-compra.cod-comprado >= c-cod-comprado-ini and 
               ordem-compra.cod-comprado <= c-cod-comprado-fim and
               ordem-compra.cod-emitente >= i-cod-emitente-ini and 
               ordem-compra.cod-emitente <= i-cod-emitente-fim and
               ordem-compra.ct-codigo >= c-ct-codigo-ini and 
               ordem-compra.ct-codigo <= c-ct-codigo-fim,
    each emitente no-lock
         where emitente.cod-emitente = ordem-compra.cod-emitente and
               pedido-compr.nr-contrato  = 0 and
               ordem-compra.situacao  = 2
    break by emitente.nome-emit
          by ordem-compra.num-pedido
          by ordem-compra.numero-ordem:

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign Vlr_total = ordem-compra.preco-unit * ordem-compra.qt-solic .
    /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/

    if  tt-param.formato = 2 then do:

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.

        descricao = REPLACE(ordem-compra.narrativa,chr(10)," ").
descricao = REPLACE(descricao,chr(13)," ").

        if  first-of(emitente.nome-emit) then do:
            display stream str-rp " " with stream-io no-box frame f-branco.
            display stream str-rp emitente.nome-emit
                emitente.cod-emitente
                emitente.telefone[1]
                    with stream-io frame f-relat-01-132.
            display stream str-rp " " with stream-io no-box frame f-branco.
        end.

        display stream str-rp ordem-compra.cod-comprado
            ordem-compra.num-pedido
            ordem-compra.data-pedido
            ordem-compra.numero-ordem
            ordem-compra.dat-ordem
            ordem-compra.num-ord-inv
            ordem-compra.it-codigo
            descricao
            ordem-compra.preco-unit
            ordem-compra.qt-solic
            Vlr_total
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

if  tt-param.destino <> 1 then

    page stream str-rp.

else do:

    if   tt-param.parametro = yes then

         page stream str-rp.

end.

if  tt-param.parametro then do:


   disp stream str-rp "SELE€ÇO" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
      i-cod-emitente-ini colon 20 "|< >|"   at 44 i-cod-emitente-fim no-label
      c-cod-comprado-ini colon 20 "|< >|"   at 44 c-cod-comprado-fim no-label
      da-data-pedido-ini colon 20 "|< >|"   at 44 da-data-pedido-fim no-label
      c-cod-estabel-ini colon 20 "|< >|"   at 44 c-cod-estabel-fim no-label
      c-ct-codigo-ini colon 20 "|< >|"   at 44 c-ct-codigo-fim no-label
        with stream-io side-labels overlay row 030 frame f-imp-sel.

   disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   INVESTIMENTO"
        with stream-io side-labels overlay row 030 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESSÇO" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu rio : " tt-param.usuario.

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
