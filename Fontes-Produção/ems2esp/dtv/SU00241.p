/*****************************************************************************
**       Programa: SU00241.p
**       Data....: 30/09/10
**       Autor...: DATASUL S.A.
**       Objetivo: RELATàRIO PAR¶METRO DE ESTOQUE
**       VersÆo..: 1.00.000 - 20100038
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "SU00241".

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
    field c-fm-codigo-ini like item.fm-codigo
    field c-fm-codigo-fim like item.fm-codigo
    field c-it-codigo-ini like item.it-codigo
    field c-it-codigo-fim like item.it-codigo
    field c-cod-estabel-ini like item-uni-estab.cod-estabel
    field c-cod-estabel-fim like item-uni-estab.cod-estabel
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

def new shared var c-fm-codigo-ini like item.fm-codigo format "x(8)" initial "" no-undo.
def new shared var c-fm-codigo-fim like item.fm-codigo format "x(8)" initial "ZZZZZZZZ" no-undo.
def new shared var c-it-codigo-ini like item.it-codigo format "x(16)" initial "" no-undo.
def new shared var c-it-codigo-fim like item.it-codigo format "x(16)" initial "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var c-cod-estabel-ini like item-uni-estab.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-cod-estabel-fim like item-uni-estab.cod-estabel format "x(3)" initial "ZZZ" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

def var ems2cademp-item--ext-demanda as character no-undo.
def var ems2cademp-item--ext-criticidade as character no-undo.

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

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

form item.it-codigo column-label "Item" format "x(16)" at 001
     item.fm-codigo column-label "Fam¡lia" format "x(8)" at 018
     item-uni-estab.cod-estabel column-label "Est" format "x(3)" at 027
     item.un column-label "Un" format "xx" at 031
     item-uni-estab.deposito-pad column-label "Dep" format "x(3)" at 034
     item-uni-estab.ponto-encomenda column-label "Pto Enc" format ">>>,>>>,>>>,>>9.9999" at 038
     item-uni-estab.ressup-fabri column-label "Res Fabr" format ">>>9" at 059
     item-uni-estab.res-for-comp column-label "Res Forn" format ">>>9" at 068
     item-uni-estab.res-int-comp column-label "Res Cmp" format ">>>9" at 077
     item-uni-estab.tempo-segur column-label "Seg" format ">>9" at 085
     item-uni-estab.consumo-prev column-label "Cons Previsto" format ">>>>,>>9.9999" at 089
     item-uni-estab.quant-segur column-label "Qt Seg" format ">>>>,>>9.9999" at 102
     ems2cademp-item--ext-demanda column-label "Demanda" format "x(012)" at 115
     ems2cademp-item--ext-criticidade column-label "Crit" format "x(001)" at 128 skip
     item.desc-item column-label "Descri‡Æo" format "x(40)" at 001
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

assign c-programa     = "SU00241"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "RELATàRIO PAR¶METRO DE ESTOQUE"
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
                    input no).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       c-fm-codigo-ini = tt-param.c-fm-codigo-ini
       c-fm-codigo-fim = tt-param.c-fm-codigo-fim
       c-it-codigo-ini = tt-param.c-it-codigo-ini
       c-it-codigo-fim = tt-param.c-it-codigo-fim
       c-cod-estabel-ini = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim = tt-param.c-cod-estabel-fim
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

for each item-uni-estab no-lock
         where item-uni-estab.cod-estabel >= c-cod-estabel-ini and 
               item-uni-estab.cod-estabel <= c-cod-estabel-fim,
    each item no-lock
         where item.it-codigo = item-uni-estab.it-codigo and
               item.fm-codigo >= c-fm-codigo-ini and 
               item.fm-codigo <= c-fm-codigo-fim and
               item.it-codigo >= c-it-codigo-ini and 
               item.it-codigo <= c-it-codigo-fim:

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign ems2cademp-item--ext-demanda = 
               entry(item-uni-estab.demanda,"Dependente,Independente")
               
           ems2cademp-item--ext-criticidade = 
               entry(item-uni-estab.criticidade,"X,Y,Z")
               .
    /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/

    if  tt-param.formato = 2 then do:

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp item.it-codigo
            item.fm-codigo
            item-uni-estab.cod-estabel
            item.un
            item-uni-estab.deposito-pad
            item-uni-estab.ponto-encomenda
            item-uni-estab.ressup-fabri
            item-uni-estab.res-for-comp
            item-uni-estab.res-int-comp
            item-uni-estab.tempo-segur
            item-uni-estab.consumo-prev
            item-uni-estab.quant-segur
            ems2cademp-item--ext-demanda
            ems2cademp-item--ext-criticidade
            item.desc-item
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
      c-fm-codigo-ini colon 11 "|< >|"   at 35 c-fm-codigo-fim no-label
      c-it-codigo-ini colon 11 "|< >|"   at 35 c-it-codigo-fim no-label
      c-cod-estabel-ini colon 11 "|< >|"   at 35 c-cod-estabel-fim no-label
        with stream-io side-labels overlay row 034 frame f-imp-sel.

   disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   ITENS"
        with stream-io side-labels overlay row 034 frame f-imp-cla.

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
