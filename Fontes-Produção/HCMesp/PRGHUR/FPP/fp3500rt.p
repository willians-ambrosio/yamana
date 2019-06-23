/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i FP3500RT 1.02.01.022}  /*** 010122 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i fp3500rt MFP}
&ENDIF

/*********************************************************************************
**       Programa: prghur/fpp/FP3500RT.P
**
**       Data....: Maio/1991.
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem - Emissao Coletiva de Envelopes de Pagamento.
**
*******************************************************************************/
DEF BUFFER empresa FOR mgcad.empresa.

{include/i-rpvar.i}

{prghur/fpp/fp3500tt.i shared}

find first tt-param no-lock no-error.
&if "{&dthrpyc_version}" >= "2.06" &then
&global-define other_clt_99 yes
{prghur/fpp/fp9200.i11}

def shared var v_han_acomp as handle no-undo.

define var c-forma-pgto     as char format "x(30)"                     no-undo.
define var c-tab-trans      as character initial "FP3500"              no-undo.
def var i-col-selec        as dec                                      no-undo.
def var i-col-classif      as dec                                      no-undo.
def var i-col-janela       as dec                                      no-undo.
def var c-titulo-g         as char format "x(20)"                      no-undo. 
def var c-titulo-p         as char format "x(30)"                      no-undo.
def var c-titulo-i         as char format "x(30)"                      no-undo.
def var c-titulo-s         as char format "x(30)"                      no-undo.
def var c-tipo-folha as char                 no-undo.

def var c-demitidos            as char format "x(3)"                   no-undo.
def new shared var l-imprime   as logical                              no-undo.
def var c-msg              as char format "x(50)"                      no-undo.

define var l-retorno  as logical format "Sim/Nao"                      no-undo.

define new shared var i-ordem   as integer               no-undo.
define new shared var i-empresa like empresa.ep-codigo   no-undo.
define new shared var i-ord-aux as int                   no-undo.
define new shared var l-origem  as log format "Coletiva/Individual" no-undo.
define new shared var v_log_folha_educnal as log initial no no-undo.

def new shared var c-imp as cha                      no-undo.
def new shared var c-emp as cha  format "x(40)"      no-undo.
def new shared var c-tit as cha  format "x(50)"      no-undo.
def new shared var i-num as int  format "ZZ"         no-undo.
def new shared var da-in as date format "99/99/9999" no-undo.
def new shared var da-fi as date format "99/99/9999" no-undo.
def new shared var c-rod as cha                      no-undo.
def new shared var c-sis as cha  format "x(25)"      no-undo.
def new shared var c-lay as cha                      no-undo.
def new shared var v_num as int                      no-undo.
def new shared var c-arq as cha                      no-undo.
def new shared var i-pag as int                      no-undo.

assign c-titulo-s = "SELEÄ«O". 

find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.

find param_empres_rh no-lock where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.

find param_folha_educnal no-lock where param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.

if avail param_folha_educnal then assign v_log_folha_educnal = yes.

assign c-versao       = "D.00"
       c-revisao      = "013"
       c-empresa      = empresa.razao-social
       c-sistema      = "FOLHA DE PAGAMENTO"
       c-titulo-relat = "Emiss∆o Coletiva de Envelopes".

  assign i-ordem   = tt-param.classifica.
  assign i-empresa = empresa.ep-codigo
         i-ord-aux = i-ordem
         l-origem  = yes.

  assign c-imp = c-impressora
         c-emp = c-empresa
         c-tit = c-titulo-relat
         i-num = i-numper-x
         da-in = da-iniper-x
         da-fi = da-fimper-x
         c-rod = c-rodape
         c-sis = c-sistema
         c-lay = c-layout
         v_num = v_num_count
         c-arq = c-arq-control
         i-pag = i-page-size-rel.

  RUN VALUE("prghur/fpp/fp3500r" + STRING(tt-param.i-tipo-formula,"9") + ".p").

return "ok".
&endif
