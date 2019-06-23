/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i FP3501RP 1.02.06.037 } /*** 010637 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i fp3501rp MFP}
&ENDIF

{include/i_fnctrad.i}
/*****************************************************************************
**
**       Programa: prghur/fpp/fp3501.p
**
**       Data....: Maio/1991.
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem - Emissao Individual de Envelopes.
**
*****************************************************************************/

{include/i-rpvar.i}

def new shared var v_han_acomp as handle no-undo.
run utp/ut-acomp.p persistent set v_han_acomp.                            

define new shared var l-imprime           as logical                                                  no-undo.
define new shared var i-es-codigo       like rh_estab.cdn_estab  initial 0                            no-undo.
define new shared var c-es-descri       like rh_estab.nom_pessoa_jurid                 initial ""     no-undo.
define new shared var i-fc-codigo       like funcionario.cdn_funcionario initial 0                    no-undo.
define new shared var i-fc-digito       like funcionario.num_digito_verfdor_func       initial 0      no-undo.
define new shared var c-fc-descri       like funcionario.nom_pessoa_fisic              initial ""     no-undo.
define new shared var i-mm-ref          like habilit_calc_fp.num_mes_refer_fp_calcula  initial "00"   no-undo .
define new shared var i-aa-ref          like habilit_calc_fp.num_ano_refer_fp_calcula  initial "0000" no-undo .
define new shared var i-tp-fl             as int format "9"                            initial "1"    no-undo 
                      label "Tipo Folha".
define new shared var i-parc            like habilit_calc_fp.qti_parc_habilit_calc_fp  initial "9"    no-undo 
                      label "Parcela".
define new shared var c-forma           like funcionario.idi_forma_pagto                              no-undo  
                      label "Forma de Pagamento".
define new shared var i-formul            as int format "9"                            initial 1      no-undo 
                      label "Tipo do Formulario".
define new shared var l-emi-dem           as logical  format "Sim/Nao"                 initial yes    no-undo
                      label "Emite Demitido".
define new shared var i-empresa         like mgcad.empresa.ep-codigo                                        no-undo.
define new shared var i-ord-aux           as int                                                      no-undo.
define new shared var l-origem            as log format "Coletiva/Individual"                         no-undo.
define new shared var i-bc-ini          like funcionario.cdn_bco_liq                   initial "001"  no-undo.
define new shared var i-bc-fim          like funcionario.cdn_bco_liq                   initial "999"  no-undo. 
define new shared var i-ag-ini          like funcionario.cdn_agenc_bcia_liq            initial "0001" no-undo.
define new shared var v_log_folha_educnal as log                                       initial no     no-undo.
define new shared var i-ag-fim          like funcionario.cdn_agenc_bcia_liq            initial "9999" no-undo.
define new shared var c-imp               as char                                                     no-undo.
define new shared var c-emp               as char  format "x(40)"                                     no-undo.
define new shared var c-tit               as cha  format "x(50)"                                      no-undo.
define new shared var i-num               as int  format "ZZ"                                         no-undo.
define new shared var da-in               as date format "99/99/9999"                                 no-undo.
define new shared var da-fi               as date format "99/99/9999"                                 no-undo.
define new shared var c-rod               as cha                                                      no-undo.
define new shared var c-sis               as cha  format "x(25)"                                      no-undo.
define new shared var c-lay               as cha                                                      no-undo.
define new shared var v_num               as int                                                      no-undo.
define new shared var c-arq               as cha                                                      no-undo.
define new shared var i-pag               as int                                                      no-undo.

define var i-dv-matric  like funcionario.num_digito_verfdor_func initial 0   no-undo.
define var c-hifen        as character format "x"                initial "-" no-undo.
define var l-selecao      as logical                             initial no  no-undo.
define var l-retorno      as logical format "Sim/Nao"                        no-undo.
define var c-progr        as character                                       no-undo.
def var c-titulo-s        as char format "x(20)"                             no-undo.  
def var c-titulo-p        as char format "x(20)"                             no-undo.    
def var c-titulo-g        as char format "x(20)"                             no-undo. 
def var c-demitidos       as char format "x(3)"                              no-undo.
DEF VAR v-path-arquivo    AS CHAR FORMAT "x(40)"                             NO-UNDO.
DEF VAR c_arquivo         AS CHAR FORMAT "x(120)"                            NO-UNDO.


DEFINE NEW SHARED TEMP-TABLE tt-rel-erros NO-UNDO
    FIELD cdn_empresa      LIKE funcionario.cdn_empresa
    FIELD cdn_estab        LIKE funcionario.cdn_estab
    FIELD cdn_funcionario  LIKE funcionario.cdn_funcionario
    FIELD nom_pessoa_fisic LIKE funcionario.nom_pessoa_fisic
    FIELD status_email     AS CHAR FORMAT "x(11)"
    FIELD email            AS CHAR FORMAT "x(60)".


FORM
    tt-rel-erros.cdn_estab SKIP(1)
    with stream-io SIDE-LABELS no-attr-space no-box width 132 frame f-estab.

FORM 
    tt-rel-erros.cdn_funcionario
    tt-rel-erros.nom_pessoa_fisic
    tt-rel-erros.status_email
    tt-rel-erros.email
    with stream-io DOWN no-attr-space no-box width 132 frame f-status.  

{prghur/fpp/fp3501tt.i new shared}  /* Parametro */
{prghur/fpp/fp9200.i10 new shared}
{prghur/fpp/fp9200.i8}

{utp/ut-liter.i Status_do_Envio MFP R}
ASSIGN tt-rel-erros.status_email:LABEL IN FRAME f-status = RETURN-VALUE.

{utp/ut-liter.i Descriá∆o MFP R}
ASSIGN tt-rel-erros.email:LABEL IN FRAME f-status = RETURN-VALUE.

def temp-table tt-raw-digita
    field raw-digita as raw.
def input parameter  raw-param as raw no-undo.
def input parameter  table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
{prghur/fpp/fp9200.i11}

assign c-titulo-s = "SELEÄ«O"
       c_arquivo  = tt-param.arquivo. 


find mgcad.empresa no-lock where
     empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
find param_empres_rh no-lock where
     param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
if avail param_folha_educnal then
   assign v_log_folha_educnal = yes.          

assign c-versao       = "D.00"    
       c-revisao      = "004"
       c-programa     = "FP/3501"
       c-empresa      = empresa.razao-social
       c-sistema      = "FOLHA DE PAGAMENTO"
       c-titulo-relat = "Emiss∆o Individual de Envelopes".

assign i-empresa       = empresa.ep-codigo
       l-origem        = no
       l-imprime       = no.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.
{utp/ut-liter.i Emiss∆o_Individual_de_Envelopes *}

run pi-inicializar in v_han_acomp (input  Return-value ).

ASSIGN v-path-arquivo = "prghur/fpp/fp3501r" + string(tt-param.i-tipo-formula,"9") + ".r".
IF SEARCH(v-path-arquivo) = ? THEN DO:
    ASSIGN v-path-arquivo = "prghur/fpp/fp3501r" + string(tt-param.i-tipo-formula,"9") + ".p".
    IF SEARCH(v-path-arquivo) = ? THEN DO:
        ASSIGN v-path-arquivo = "prghur/fpp/fp3501r" + string(tt-param.i-tipo-formula,"9") + ".p".
    END.
END.
ELSE DO:
    ASSIGN v-path-arquivo = "prghur/fpp/fp3501r" + string(tt-param.i-tipo-formula,"9") + ".p".
END.

RUN value(v-path-arquivo).


IF tt-param.v_log_enviar_email THEN DO:
    ASSIGN tt-param.arquivo = c_arquivo.
    {include/i-rpcab.i}
    {include/i-rpout.i}
    view frame f-cabec.
    view frame f-rodape.
    
    FOR EACH tt-rel-erros BREAK BY tt-rel-erros.cdn_estab BY tt-rel-erros.cdn_funcionario:
        IF FIRST-OF(tt-rel-erros.cdn_estab) THEN
            DISP tt-rel-erros.cdn_estab WITH FRAME f-estab.
        DISP tt-rel-erros.cdn_funcionario  
             tt-rel-erros.nom_pessoa_fisic 
             tt-rel-erros.status_email     
             tt-rel-erros.email WITH FRAME f-status.
        DOWN WITH FRAME f-status.
        IF LAST-OF(tt-rel-erros.cdn_estab) THEN
            PAGE.
    END.

    view frame f-cabec.
    view frame f-rodape.
    {include/i-rpclo.i}
END.

run pi-finalizar in v_han_acomp.
return.

/*  prghur/fpp/fp3501.p     */


