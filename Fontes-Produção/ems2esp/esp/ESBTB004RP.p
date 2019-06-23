/*****************************************************************************
**       Programa: ESBTB004RP.p
**       Data....: 08/12/10
**       Autor...: Daniel Pereira de Lima
**       Objetivo: Exporta para Ilha a Condi��o de Pagamento
**       Vers�o..: 1.00.000
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "ESBTB004RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.06.00.000").

/****************** Defini��o de Tabelas Tempor�rias do Relat�rio **********************/

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
    field i-cod-cond-pag-ini like cond-pagto.cod-cond-pag
    field i-cod-cond-pag-fim like cond-pagto.cod-cond-pag
.

/****************** INCLUDE COM VARI�VEIS GLOBAIS *********************/

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
/****************** Defini�ao de Par�metros do Relat�rio *********************/ 

/****************** Defini�ao de Vari�veis de Sele��o do Relat�rio *********************/ 

def new shared var i-cod-cond-pag-ini like cond-pagto.cod-cond-pag format ">>9" initial 0 no-undo.
def new shared var i-cod-cond-pag-fim like cond-pagto.cod-cond-pag format ">>9" initial 999 no-undo.

/****************** Defini�ao de Vari�veis p/ Campos Virtuais do Relat�rio *******************/ 

/****************** Defini�ao de Vari�veis Campo Calculado do Relat�rio **********************/ 

/****************** Defini�ao de Vari�veis do Relat�rio N�o Pedidas em Tela ******************/ 

/****************** Defini�ao de Vari�veis de Total do Relat�rio *****************************/ 

/****************** Defini�ao de Vari�veis dos Calculos do Relat�rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini�ao de Vari�veis de Processamento do Relat�rio *********************/

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

/****************** Defini�ao de Forms do Relat�rio 132 Colunas ***************************************/ 

form cond-pagto.cod-cond-pag column-label "Cond" format ">>9" at 001
     cond-pagto.descricao column-label "Descri��o" format "x(30)" at 006
     with down width 132 no-box stream-io frame f-relat-09-132.

/****************** Defini�ao de Forms do Relat�rio 80 Colunas ***************************************/ 

form cond-pagto.cod-cond-pag column-label "Cond" format ">>9" at 001
     cond-pagto.descricao column-label "Descri��o" format "x(30)" at 006
     with down width 80 no-box stream-io frame f-relat-09-80.

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

assign c-programa     = "ESBTB004RP"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "Exporta para Ilha a Condi��o de Pagamento"
       c-sistema      = "".

find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".

if  tt-param.formato = 1 then do:
    form header
        fill("-", 80) format "x(80)" skip
        c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
        "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
        fill("-", 60) format "x(58)" today format "99/99/9999"
        "-" string(time, "HH:MM:SS") skip(1)
        with stream-io width 80 no-labels no-box page-top frame f-cabec-80.

    form header
        fill("-", 80) format "x(80)" skip
        c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
        "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
        "Per�odo:" i-numper-x at 08 "-"
        da-iniper-x at 14 "to" da-fimper-x
        fill("-", 22) format "x(20)" today format "99/99/9999"
        "-" string(time, "HH:MM:SS") skip(1)
        with stream-io width 80 no-labels no-box page-top frame f-cabper-80.
    run grapi/gr2005.p.
    form header
        c-rodape format "x(80)"
        with stream-io width 80 no-labels no-box page-bottom frame f-rodape-80.
end. /* tt-param.formato = 1 */ 

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
       i-cod-cond-pag-ini  = tt-param.i-cod-cond-pag-ini
       i-cod-cond-pag-fim  = tt-param.i-cod-cond-pag-fim.

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

run pi-inicializar in h-acomp(input "Acompanhamento Relat�rio").

assign v-num-reg-lidos = 0.

/* Prepara tabela es_cond_pagto para receber os registros */

FIND FIRST es_cond_pagto NO-LOCK NO-ERROR.
IF AVAIL es_cond_pagto THEN DO:
    RUN esp/esidelete.i 'es_cond_pagto'.
END.

/* Prepara tabela es_cond_pagto para receber os registros */


/* gr9020a.p */

for each cond-pagto no-lock
   where cond-pagto.cod-cond-pag >= i-cod-cond-pag-ini
     and cond-pagto.cod-cond-pag <= i-cod-cond-pag-fim:
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).


/* Importar na Ilha */
    CREATE es_cond_pagto NO-ERROR.
    ASSIGN es_cond_pagto.cod_condicao  = cond-pagto.cod-cond-pag
           es_cond_pagto.desc_condicao = cond-pagto.descricao.
/* Importar na Ilha */



    /***  C�DIGO PARA SA�DA EM 80 COLUNAS ***/
    if  tt-param.formato = 1 then do:
        view stream str-rp frame f-cabec-80.
        view stream str-rp frame f-rodape-80.
        assign l-imprime = yes.
        display stream str-rp cond-pagto.cod-cond-pag
            cond-pagto.descricao
                with stream-io frame f-relat-09-80.
            down stream str-rp with frame f-relat-09-80.
    end.

    /***  C�DIGO PARA SA�DA EM 132 COLUNAS ***/
    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp cond-pagto.cod-cond-pag
            cond-pagto.descricao
                with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.
    end.
end.

if  l-imprime = no then do:
    if  tt-param.formato = 1 then do:
        view stream str-rp frame f-cabec-80.
        view stream str-rp frame f-rodape-80.
    end.

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
   disp stream str-rp "SELE��O" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
      i-cod-cond-pag-ini colon 24 "|< >|"   at 48 i-cod-cond-pag-fim no-label
        with stream-io side-labels overlay row 038 frame f-imp-sel.

   disp stream str-rp "CLASSIFICA��O" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   Exporta para Ilha a Condi��o de Pagamento"
        with stream-io side-labels overlay row 038 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESS�O" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execu��o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu�rio : " tt-param.usuario.
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
