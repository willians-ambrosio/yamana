/*****************************************************************************
**       Programa: relat554.p
**       Data....: 18/10/10
**       Autor...: DATASUL S.A.
**       Objetivo: Relat¢rio EMS5 Grupo x Programa x Empresa x User
**       VersÆo..: 1.00.000 - 30100979
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "RELAT554".

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
    field c-cod_grp_usuar-ini like usuar_grp_usuar.cod_grp_usuar
    field c-cod_grp_usuar-fim like usuar_grp_usuar.cod_grp_usuar
    field c-cod_prog_dtsul-ini like prog_dtsul_segur.cod_prog_dtsul
    field c-cod_prog_dtsul-fim like prog_dtsul_segur.cod_prog_dtsul
    field c-cod_usuario-ini like usuar_mestre.cod_usuario
    field c-cod_usuario-fim like usuar_mestre.cod_usuario
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

def new shared var c-cod_grp_usuar-ini like usuar_grp_usuar.cod_grp_usuar format "x(3)" initial "" no-undo.
def new shared var c-cod_grp_usuar-fim like usuar_grp_usuar.cod_grp_usuar format "x(3)" initial "ZZZ" no-undo.
def new shared var c-cod_prog_dtsul-ini like prog_dtsul_segur.cod_prog_dtsul format "x(20)" initial "" no-undo.
def new shared var c-cod_prog_dtsul-fim like prog_dtsul_segur.cod_prog_dtsul format "x(20)" initial "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" no-undo.
def new shared var c-cod_usuario-ini like usuar_mestre.cod_usuario format "x(12)" initial "" no-undo.
def new shared var c-cod_usuario-fim like usuar_mestre.cod_usuario format "x(12)" initial "ZZZZZZZZZZZZ" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

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

form usuar_mestre.cod_usuario column-label "Usu rio" format "x(12)" at 001
     usuar_mestre.nom_usuario column-label "Nome" format "x(32)" at 013
     usuar_univ.cod_empresa column-label "Empresa" format "x(3)" at 046
     prog_dtsul.des_prog_dtsul column-label "Descri‡Æo" format "x(40)" at 052
     usuar_grp_usuar.cod_grp_usuar column-label "Grupo" format "x(3)" at 060
     grp_usuar.des_grp_usuar column-label "Descri‡Æo" format "x(32)" at 067 skip
     prog_dtsul_segur.cod_prog_dtsul column-label "Programa" format "x(50)" at 083
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

assign c-programa     = "relat554"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Relat¢rio EMS5 Grupo x Programa x Empresa x User"
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
       c-cod_grp_usuar-ini = tt-param.c-cod_grp_usuar-ini
       c-cod_grp_usuar-fim = tt-param.c-cod_grp_usuar-fim
       c-cod_prog_dtsul-ini = tt-param.c-cod_prog_dtsul-ini
       c-cod_prog_dtsul-fim = tt-param.c-cod_prog_dtsul-fim
       c-cod_usuario-ini = tt-param.c-cod_usuario-ini
       c-cod_usuario-fim = tt-param.c-cod_usuario-fim
.

def var l-imprime as logical no-undo.

assign l-imprime = no.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Lista usuarios ativos do EMS505").

assign v-num-reg-lidos = 0.

/* gr9020b.p */

for each usuar_mestre no-lock
         where usuar_mestre.cod_usuario >= c-cod_usuario-ini and 
               usuar_mestre.cod_usuario <= c-cod_usuario-fim,
    each usuar_univ no-lock
         where usuar_univ.cod_usuario = usuar_mestre.cod_usuario,
    each usuar_grp_usuar no-lock
         where usuar_grp_usuar.cod_usuario = usuar_mestre.cod_usuario and
               usuar_grp_usuar.cod_grp_usuar >= c-cod_grp_usuar-ini and 
               usuar_grp_usuar.cod_grp_usuar <= c-cod_grp_usuar-fim,
    each grp_usuar no-lock
         where grp_usuar.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar,
    each prog_dtsul_segur no-lock
         where prog_dtsul_segur.cod_grp_usuar = grp_usuar.cod_grp_usuar and
               prog_dtsul_segur.cod_prog_dtsul >= c-cod_prog_dtsul-ini and 
               prog_dtsul_segur.cod_prog_dtsul <= c-cod_prog_dtsul-fim,
    each prog_dtsul no-lock
         where prog_dtsul.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul and
               usuar_mestre.dat_fim_valid  >= TODAY:
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(prog_dtsul.des_prog_dtsul)).

    assign l-imprime = yes.
    
    put stream str-rp unformatted usuar_mestre.cod_usuario ";"
            usuar_mestre.nom_usuario ";"
            usuar_univ.cod_empresa ";"
            prog_dtsul.des_prog_dtsul ";"
            usuar_grp_usuar.cod_grp_usuar ";"
            grp_usuar.des_grp_usuar ";"
            prog_dtsul_segur.cod_prog_dtsul skip.
    
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
