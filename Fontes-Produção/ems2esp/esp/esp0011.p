/*****************************************************************************
**
**       Programa: ESP0011.p
**
**       Data....: 25/08/05
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Relat¢rio espec¡fico investimentos
**
**       VersÆo..: 1.00.000 - adm
**
**       OBS.....: Este fonte foi gerado pelo Data Viewer
**

*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "ESP0011".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

def temp-table tt-raw-digita
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
    field data-corte as date
    field l-moeda as integer
    field i-ep-codigo-ini like mginv.proj-inv.ep-codigo
    field i-ep-codigo-fim like mginv.proj-inv.ep-codigo
    field c-cod-est-exec-ini like mginv.proj-inv.cod-est-exec
    field c-cod-est-exec-fim like mginv.proj-inv.cod-est-exec
    field i-num-projeto-ini like mginv.proj-inv.num-projeto
    field i-num-projeto-fim like mginv.proj-inv.num-projeto
    field i-num-ordem-ini like mginv.ordem-inv.num-ordem
    field i-num-ordem-fim like mginv.ordem-inv.num-ordem
    field i-cod-especialidade-ini like mginv.sub-div-ordem.cod-especialidade
    field i-cod-especialidade-fim like mginv.sub-div-ordem.cod-especialidade
    field i-cod-sub-espec-ini like mginv.sub-div-ordem.cod-sub-espec
    field i-cod-sub-espec-fim like mginv.sub-div-ordem.cod-sub-espec
    field i-num-secao-ini like mginv.sub-div-ordem.num-secao
    field i-num-secao-fim like mginv.sub-div-ordem.num-secao
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


/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

def new shared var data-corte as date format "99/99/9999" label "Data Corte".
def var l-moeda-tmp as char no-undo.
def new shared var l-moeda as integer format "9" label "Moeda" view-as radio-set  horizontal radio-buttons "Principal",0, 
"Alternativa 1",1,
"Alternativa 2",3
.

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var i-ep-codigo-ini like mginv.proj-inv.ep-codigo format ">>9" initial 0 no-undo.
def new shared var i-ep-codigo-fim like mginv.proj-inv.ep-codigo format ">>9" initial 999 no-undo.
def new shared var c-cod-est-exec-ini like mginv.proj-inv.cod-est-exec format "x(3)" initial "" no-undo.
def new shared var c-cod-est-exec-fim like mginv.proj-inv.cod-est-exec format "x(3)" initial "ZZZ" no-undo.
def new shared var i-num-projeto-ini like mginv.proj-inv.num-projeto format ">>>>9" initial 0 no-undo.
def new shared var i-num-projeto-fim like mginv.proj-inv.num-projeto format ">>>>9" initial 99999 no-undo.
def new shared var i-num-ordem-ini like mginv.ordem-inv.num-ordem format ">>9" initial 0 no-undo.
def new shared var i-num-ordem-fim like mginv.ordem-inv.num-ordem format ">>9" initial 999 no-undo.
def new shared var i-cod-especialidade-ini like mginv.sub-div-ordem.cod-especialidade format ">9" initial 0 no-undo.
def new shared var i-cod-especialidade-fim like mginv.sub-div-ordem.cod-especialidade format ">9" initial 99 no-undo.
def new shared var i-cod-sub-espec-ini like mginv.sub-div-ordem.cod-sub-espec format ">9" initial 0 no-undo.
def new shared var i-cod-sub-espec-fim like mginv.sub-div-ordem.cod-sub-espec format ">9" initial 99 no-undo.
def new shared var i-num-secao-ini like mginv.sub-div-ordem.num-secao format "9" initial 0 no-undo.
def new shared var i-num-secao-fim like mginv.sub-div-ordem.num-secao format "9" initial 9 no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

def var l-compra-acum as decimal label "Compra Acum".
def var l-comprometer as decimal FORMAT "->>>,>>>,>>9.9999" label "A Comprometer".
def var l-forecast as decimal /*FORMAT "->>>,>>>,>>9.9999"*/ label "Forecast".
def var l-nossa-qtde as decimal label "Nossa Quantidade".
def var l-qtde-acum as decimal label "Qtde Acum".
def var l-qtde-acum2 as decimal label "Qtde Acum".
def var l-valor-realizado as decimal label "Valor Realizado Acum".
def var l-vl-estim-tot as decimal label "Vl Estimado".
def var qtde-item as decimal label "Qtde".

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form mginv.proj-inv.num-projeto column-label "Proj" format ">>>>9" at 001
     mginv.proj-inv.descricao column-label "Descri‡Æo" format "x(40)" at 007
     mginv.ordem-inv.cod-ccusto-benef column-label "C Cust Benef" format ">>>>>>>>>>>>9" at 048
     mginv.ordem-inv.cod-ccusto-resp column-label "C Cust Resp" format ">>>>>>>>>>>>9" at 062
     mginv.ordem-inv.num-ordem column-label "Ordem" format ">>9" at 076
     mginv.ordem-inv.descricao column-label "Descri‡Æo" format "x(40)" at 082
     mginv.secao-inv.num-secao column-label "Se‡Æo" format "9" at 123 skip
     mginv.secao-inv.descricao column-label "Descri‡Æo" format "x(40)" at 001
     mginv.Sub-espec.cod-especialidade column-label "Espec" format ">9" at 042
     mginv.especialidade.descricao column-label "Descri‡Æo" format "x(40)" at 048
     mginv.Sub-espec.cod-sub-espec column-label "Sub Espec" format ">9" at 089 skip
     mginv.Sub-espec.descricao column-label "Descri‡Æo" format "x(40)" at 001
     mginv.sub-div-ordem.num-ord-magnus column-label "Ordem Magnus" format ">>>>>,>>>" at 048
     qtde-item column-label "Qtde" format ">>>>>,>>>,>>9.9999" at 064
     mginv.sub-div-ordem.vl-estimado[1] column-label "Vl Estim" format ">>>>>,>>>,>>>,>>9.99" at 083
     /*mginv.estim-mat.quant-reestim column-label "Qtde Reest" format ">>>>>,>>>,>>9.9999" at 104 skip*/
     mginv.sub-div-ordem.vl-reestimado[1] column-label "Vl Reestim" format ">>>>>,>>>,>>>,>>9.99" at 001
     /*mginv.estim-mat.quant-estim column-label "Qtde Est" format ">>>>>,>>>,>>9.9999" at 022*/
     l-vl-estim-tot column-label "Vl Estimado" format ">>>>>,>>>,>>9.9999" at 041
     l-qtde-acum column-label "Qtde Acum" format ">>,>>>,>>9.99" at 060
     l-compra-acum column-label "Compra Acum" format ">>,>>>,>>9.99" at 074
     l-qtde-acum2 column-label "Qtde Acum" format ">>,>>>,>>9.99" at 088
     l-nossa-qtde column-label "Nossa Quantidade" format ">>>>>,>>9.9999" at 102 skip
     l-valor-realizado column-label "Valor Real Acum" format ">>,>>>,>>>,>>9.99" at 001
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

assign c-programa     = "ESP0011"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Relat¢rio espec¡fico investimentos"
       c-sistema      = "Investimentos".

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
           v_cdn_empres_usuar  = i-ep-codigo-usuario.

    assign data-corte = tt-param.data-corte
           l-moeda = tt-param.l-moeda
           i-ep-codigo-ini = tt-param.i-ep-codigo-ini
           i-ep-codigo-fim = tt-param.i-ep-codigo-fim
           c-cod-est-exec-ini = tt-param.c-cod-est-exec-ini
           c-cod-est-exec-fim = tt-param.c-cod-est-exec-fim
           i-num-projeto-ini = tt-param.i-num-projeto-ini
           i-num-projeto-fim = tt-param.i-num-projeto-fim
           i-num-ordem-ini = tt-param.i-num-ordem-ini
           i-num-ordem-fim = tt-param.i-num-ordem-fim
           i-cod-especialidade-ini = tt-param.i-cod-especialidade-ini
           i-cod-especialidade-fim = tt-param.i-cod-especialidade-fim
           i-cod-sub-espec-ini = tt-param.i-cod-sub-espec-ini
           i-cod-sub-espec-fim = tt-param.i-cod-sub-espec-fim
           i-num-secao-ini = tt-param.i-num-secao-ini
           i-num-secao-fim = tt-param.i-num-secao-fim
.


find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".

/* for each e disp */

def var l-imprime as logical no-undo.

assign l-imprime = no.
put stream str-rp unformatted
"Proj" "#" "Descri‡Æo" "#" "C Custo Benef" "#" "C Custo Resp" "#" "Ordem" "#" "Descri‡Æo" "#" 
"Se‡Æo" "#" "Descri‡Æo" "#" "Espec" "#" "Descri‡Æo" "#" "Sub Espec" "#" 
"Descri‡Æo" "#" "Num Ord EMS" "#" "Qtde" "#" "SITUACAO" "#" "Vl Verba" "#" "Vl Revisado" "#" 
/*"Vl Reestim" "#"*/ /*"Qtde Est" "#"*/ /*"Vl Estimado" "#"*/ /*"Qtde Acum" "#"*/ 
"Compra Acum" "#" /*"Qtde Acum" "#"*/ /*"Qtde Accrued" "#"*/ "Valor Accrued" "#" "A Comprometer" "#" "Forecast" skip.
run grp/gracompx.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

for each mginv.proj-inv no-lock
         where mginv.proj-inv.cod-est-exec >= c-cod-est-exec-ini and 
               mginv.proj-inv.cod-est-exec <= c-cod-est-exec-fim and
               mginv.proj-inv.ep-codigo >= i-ep-codigo-ini and 
               mginv.proj-inv.ep-codigo <= i-ep-codigo-fim and
               mginv.proj-inv.num-projeto >= i-num-projeto-ini and 
               mginv.proj-inv.num-projeto <= i-num-projeto-fim,
    each mginv.ordem-inv no-lock
         where mginv.ordem-inv.ep-codigo = mginv.proj-inv.ep-codigo and
               mginv.ordem-inv.cod-est-exec = mginv.proj-inv.cod-est-exec and
               mginv.ordem-inv.num-projeto = mginv.proj-inv.num-projeto and
               mginv.ordem-inv.num-ordem >= i-num-ordem-ini and 
               mginv.ordem-inv.num-ordem <= i-num-ordem-fim,
    each mginv.sub-div-ordem no-lock
         where mginv.sub-div-ordem.ep-codigo = mginv.ordem-inv.ep-codigo and
               mginv.sub-div-ordem.cod-est-exec = mginv.ordem-inv.cod-est-exec and
               mginv.sub-div-ordem.num-projeto = mginv.ordem-inv.num-projeto and
               mginv.sub-div-ordem.num-ordem = mginv.ordem-inv.num-ordem and
               mginv.sub-div-ordem.cod-especialidade >= i-cod-especialidade-ini and 
               mginv.sub-div-ordem.cod-especialidade <= i-cod-especialidade-fim and
               mginv.sub-div-ordem.cod-sub-espec >= i-cod-sub-espec-ini and 
               mginv.sub-div-ordem.cod-sub-espec <= i-cod-sub-espec-fim and
               mginv.sub-div-ordem.num-secao >= i-num-secao-ini and 
               mginv.sub-div-ordem.num-secao <= i-num-secao-fim,
    each mginv.secao-inv no-lock
         where mginv.secao-inv.ep-codigo = mginv.sub-div-ordem.ep-codigo and
               mginv.secao-inv.cod-est-exec = mginv.sub-div-ordem.cod-est-exec and
               mginv.secao-inv.num-projeto = mginv.sub-div-ordem.num-projeto and
               mginv.secao-inv.num-ordem = mginv.sub-div-ordem.num-ordem and
               mginv.secao-inv.num-secao = mginv.sub-div-ordem.num-secao,
    each mginv.Sub-espec no-lock
         where mginv.Sub-espec.ep-codigo = mginv.sub-div-ordem.ep-codigo and
               mginv.Sub-espec.cod-especialidade = mginv.sub-div-ordem.cod-especialidade and
               mginv.Sub-espec.cod-sub-espec = mginv.sub-div-ordem.cod-sub-espec,
    /*each mginv.estim-mat no-lock
         where mginv.estim-mat.ep-codigo = mginv.sub-div-ordem.ep-codigo and
               mginv.estim-mat.cod-est-exec = mginv.sub-div-ordem.cod-est-exec and
               mginv.estim-mat.num-projeto = mginv.sub-div-ordem.num-projeto and
               mginv.estim-mat.num-ordem = mginv.sub-div-ordem.num-ordem and
               mginv.estim-mat.num-secao = mginv.sub-div-ordem.num-secao and
               mginv.estim-mat.cod-especialidade = mginv.sub-div-ordem.cod-especialidade and
               mginv.estim-mat.cod-sub-espec = mginv.sub-div-ordem.cod-sub-espec and
               mginv.estim-mat.cod-origem = mginv.sub-div-ordem.cod-origem,*/
    each mginv.especialidade no-lock
         where mginv.especialidade.ep-codigo = mginv.Sub-espec.ep-codigo and
               mginv.especialidade.cod-especialidade = mginv.Sub-espec.cod-especialidade :
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    /*assign qtde-item = IF mginv.estim-mat.quant-estim EQ 0 THEN mginv.estim-mat.quant-reestim ELSE mginv.estim-mat.quant-estim .
    assign l-vl-estim-tot = mginv.estim-mat.quant-estim * mginv.estim-mat.vl-unit-estim[1] .*/
    assign l-imprime = yes.
    run pi-retorna-compras-acumuladas (sub-div-ordem.num-ord-magnus,
                                       data-corte, 
                                       OUTPUT l-compra-acum,
                                       OUTPUT l-qtde-acum).
    
    /*assign l-qtde-acum2 = l-qtde-acum - mginv.estim-mat.quant-estim.*/
    
    /* *******Chamada substituida*********
       run pi-accrued (mginv.proj-inv.ep-codigo, 
                    mginv.proj-inv.cod-est-exec,  
                    mginv.proj-inv.num-projeto,
                    l-moeda,
                    OUTPUT l-valor-realizado).*/

    run pi-accrued (sub-div-ordem.num-ord-magnus,
                             data-corte,
                       OUTPUT  l-valor-realizado).
                 
    run pi-nossa-qtde (sub-div-ordem.num-ord-magnus,
                       data-corte, 
                       OUTPUT l-nossa-qtde).
    run pi-comprometer (sub-div-ordem.num-ord-magnus,
                        data-corte, 
                        OUTPUT l-comprometer,
                        OUTPUT l-forecast ).

    
    put stream str-rp unformatted 
            mginv.proj-inv.num-projeto "#"
            mginv.proj-inv.descricao "#"
            mginv.ordem-inv.cod-ccusto-benef "#"
            mginv.ordem-inv.cod-ccusto-resp "#"
            mginv.ordem-inv.num-ordem "#"
            mginv.ordem-inv.descricao "#"
            mginv.secao-inv.num-secao "#"
            mginv.secao-inv.descricao "#"
            mginv.Sub-espec.cod-especialidade "#"
            mginv.especialidade.descricao "#"
            mginv.Sub-espec.cod-sub-espec "#"
            mginv.Sub-espec.descricao "#"
            mginv.sub-div-ordem.num-ord-magnus "#"
            qtde-item "#"
            mginv.ordem-inv.cod-situacao-inv "#"
            mginv.sub-div-ordem.vl-estimado[1] "#"
            /*mginv.estim-mat.quant-reestim "#"*/
            mginv.sub-div-ordem.vl-reestimado[1] "#"
            /*mginv.estim-mat.quant-estim "#"*/
            /*l-vl-estim-tot "#"*/
            /*l-qtde-acum "#"*/
            l-compra-acum "#"
            /*l-qtde-acum2 "#"*/
            /*l-nossa-qtde "#"*/
            l-valor-realizado "#"
            l-comprometer "#"
            l-forecast skip.
    
end.



run pi-finalizar in h-acomp.

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

procedure pi-accrued:
  /*
  DEFINE INPUT  PARAM p-ep-codigo    LIKE base-mensal.ep-codigo       NO-UNDO.
DEFINE INPUT  PARAM p-cod-est-exec LIKE base-mensal.cod-est-exec    NO-UNDO.
DEFINE INPUT  PARAM p-num-projeto  LIKE base-mensal.num-projeto     NO-UNDO.
DEFINE INPUT  PARAM p-mo-codigo    LIKE base-mensal.mo-codigo       NO-UNDO.
DEFINE OUTPUT PARAM p-valor        LIKE base-mensal.vl-realizado[1] NO-UNDO.

DEFINE VARIABLE valor AS DECIMAL NO-UNDO.
DEFINE VARIABLE x     AS INTEGER NO-UNDO.

p-valor = 0.

FOR EACH base-mensal WHERE base-mensal.ep-codigo    = p-ep-codigo
                       AND base-mensal.cod-est-exec = p-cod-est-exec
                       AND base-mensal.num-projeto  = p-num-projeto
                       AND base-mensal.tipo-reg     = "11"
                       AND base-mensal.mo-codigo    = p-mo-codigo NO-LOCK. 

  DO x = 1 to 12:
    p-valor = p-valor +  base-mensal.vl-realizado[x] + base-mensal.vl-realizado-ni[x].
  END.
  
END.*/

  define input  parameter p-ord-magnus  like sub-div-ordem.num-ord-magnus no-undo.
    define input  parameter p-data-corte  as date                           no-undo.
    define output parameter p-valor  like item-doc-est.preco-total[1]      no-undo.

    p-valor = 0.

    for each ordem-compra where ordem-compra.num-ord-inv = p-ord-magnus
                            and (ordem-compra.situacao   = 2
                              or ordem-compra.situacao    = 6)
                            and ordem-compra.data-pedido <= p-data-corte no-lock,
         each item-doc-est of ordem-compra no-lock.

        assign p-valor  = p-valor + (item-doc-est.preco-total[1] + item-doc-est.preco-total[2]).

    END.


/*** p-valor = p-valor / 1000. ***/

end procedure.

procedure pi-nossa-qtde:

      define input  parameter p-ord-magnus  like sub-div-ordem.num-ord-magnus no-undo.
    define input  parameter p-data-corte  as date                           no-undo.
    define output parameter p-nossa-qtde  like item-doc-est.quantidade      no-undo.
    
    p-nossa-qtde = 0.
    
    for each ordem-compra where ordem-compra.num-ord-inv = p-ord-magnus
                            and ordem-compra.situacao    = 2 
                            and ordem-compra.data-pedido <= p-data-corte no-lock,
         each item-doc-est of ordem-compra no-lock.
        
        assign p-nossa-qtde  = p-nossa-qtde + item-doc-est.quantidade.
        
    end.


end procedure.

procedure pi-retorna-compras-acumuladas:

      define input  parameter p-ord-magnus  like sub-div-ordem.num-ord-magnus no-undo.
    define input  parameter p-data-corte  as date                           no-undo.
    define output parameter p-compra-acum like ordem-compra.preco-fornec    no-undo.
    define output parameter p-qtde-acum   like ordem-compra.qt-acum-nec     no-undo.
    
    ASSIGN p-compra-acum = 0
           p-qtde-acum   = 0.
    
    for each ordem-compra where ordem-compra.num-ord-inv = p-ord-magnus
                            and (ordem-compra.situacao   = 2
                            or ordem-compra.situacao    = 6)
                            and ordem-compra.data-pedido <= p-data-corte no-lock.
        /*Gladson*/
        assign p-compra-acum = p-compra-acum + (ordem-compra.preco-fornec * ordem-compra.qt-solic)
               p-qtde-acum   = p-qtde-acum + ordem-compra.qt-solic.
        
    end.


end procedure.



procedure pi-comprometer:

      define input  parameter p-ord-magnus  like sub-div-ordem.num-ord-magnus no-undo.
      define input  parameter p-data-corte  as date                                             no-undo.
      define output parameter p-comprometer like ordem-compra.preco-fornec     no-undo.
      define output parameter p-forecast    like ordem-compra.preco-fornec     no-undo.
      
      define VAR p-comprometer-compra-acum as decimal FORMAT "->>>,>>>,>>9.9999" /*like ordem-compra.preco-fornec*/    no-undo.
      define VAR p-a as decimal FORMAT "->>>,>>>,>>9.9999" no-undo.
      
    
    ASSIGN p-comprometer-compra-acum = 0
                  p-comprometer  = 0.
           p-forecast = 0.
    
    IF mginv.ordem-inv.cod-situacao-inv <> 1 /*Aberto*/ THEN
        p-comprometer = 0.
    ELSE
    DO:

      for each ordem-compra where ordem-compra.num-ord-inv = p-ord-magnus
                            and (ordem-compra.situacao   = 2
                            or ordem-compra.situacao    = 6)
                            and ordem-compra.data-pedido <= p-data-corte no-lock.
        
        assign p-comprometer-compra-acum = p-comprometer-compra-acum + (ordem-compra.preco-fornec * ordem-compra.qt-solic).

        p-a = mginv.sub-div-ordem.vl-reestimado[1] - p-comprometer-compra-acum.
        
        IF  /*(mginv.sub-div-ordem.vl-reestimado[1] - p-comprometer-compra-acum)*/ p-a <= 0 THEN
            p-comprometer = 0.
            
        
        /*IF  (mginv.sub-div-ordem.vl-reestimado[1] - p-comprometer-compra-acum) >= 0  THEN
          p-comprometer = dec(mginv.sub-div-ordem.vl-reestimado[1]) - dec(p-comprometer-compra-acum) .*/
        
        ELSE
        DO:
            p-comprometer = p-a /*dec(mginv.sub-div-ordem.vl-reestimado[1]) - dec(p-comprometer-compra-acum)*/.
        END.

        /**********************Calculo da coluna FORECAST**************************/
        p-forecast = p-comprometer-compra-acum + p-comprometer.
        /**************************************************************************/
        
      end.
    END.
end procedure.

return 'OK'.

/* fim do programa */
