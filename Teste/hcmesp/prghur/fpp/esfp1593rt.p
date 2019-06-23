/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{include/i-prgvrs.i ESFP1593RT 12.1.13.000 } /*** 010015 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i ESFP1593rt MFP}
&ENDIF

{include/i-func-dtype.i}
/*******************************************************************************
**
**       Programa: prghur/fpp/ESFP1593RP.P
**
**       Data....: NOVEMBRO/1998.
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem Geral de Funcion rios.
**
*******************************************************************************/

{include/i-rpvar.i}

&GLOBAL-DEFINE RTF YES
&GLOBAL-DEFINE safrista YES
{include/tt-edit.i}

/* Temporary Table Definitions ---                                      */

{prghur/fpp/ESFP1593tt.i shared}  /* Parametro */


&if "{&dthrpyc_version}" >= "2.06" &then /*programa disponivel na versao 2.06*/
&global-define other_ponto yes 
find first tt-param.
{prghur/fpp/fp9200.i11}
{prghur/fpp/fp9200.i10}
{prghur/fpp/fp9200.i8}
{prghur/fpp/fp9400.i}

&global-define total-lotac yes
def var v_log_first_lotac as logical init no no-undo.

&global-define outros-fields           ~
        field v_cont_func      as int ~

{prghur/fpp/fp9200.i12}

/* Local Variable Definitions ---                                              */
define shared var v_han_acomp        as handle                                      no-undo.
define var v_log_cancel              as log                                         no-undo.
define var v_des_selecao             as char format "x(15)"                         no-undo.
define var v_cdn_classif             as int  format "99"                            no-undo.
define var v_des_titulo              as char format "x(23)"                         no-undo.
define var v_des_mensagem            as char view-as editor inner-chars 100 inner-lines 1 format "x(1000)" no-undo.
define var c-status                  as char                                        no-undo.
define var v_cont_func               like funcionario.cdn_funcionario               no-undo.
define var v_log_folha_educnal       as log                 init no                 no-undo.
define var v_tit_estab               as char format "x(20)"                         no-undo.
define var v_cdn_estab             like rh_estab.cdn_estab                          no-undo.
define var v_nom_pessoa_jurid      like rh_estab.nom_pessoa_jurid                   no-undo.
define var v_des_tip_cargo_funcao    as char format "x(15)"                         no-undo.
define var v_tit_ccusto              as char format "x(20)"                         no-undo.
define var v_cod_rh_ccusto         like funcionario.cod_rh_ccusto                   no-undo.
define var v_des_rh_ccusto           as char format "x(30)"                         no-undo.
define var v_num_fc                  as char format "x(11)"                         no-undo.
define var v_cdn_turno             like funcionario.cdn_turno_trab                  no-undo.
define var v_des_turno               as char format "x(09)"                         no-undo.
define var v_cdn_categ_sal         like funcionario.cdn_turno_trab                  no-undo.
define var v_des_categ_sal           as char format "x(09)"                         no-undo.
define var v_des_envel               as char format "x(10)"                         no-undo.
define var v_tot_estab               as int  format ">>>,>>9"                       no-undo.
define var v_tot_prest               as int  format ">>>,>>9"                       no-undo.
define var v_tot_ccusto              as int  format ">>>,>>9"                       no-undo.
define var v_tot_emp                 as int  format ">>>,>>9"                       no-undo.
define var v_tot_turno               as int  format ">>>,>>9"                       no-undo.
define var v_tot_unid                as char format "x(44)"                         no-undo. 
define var v_des_tot_ccust           as char format "x(30)"                         no-undo.
define var v_des_tot_turno           as char format "x(31)"                         no-undo.
define var v_des_tot_emp             as char format "x(30)"                         no-undo.
define var v_des_tot_est             as char format "x(30)"                         no-undo.
define var v_des_tot_prest           as char format "x(30)"                         no-undo.
define var v_nome                    as char format "X(30)"                         no-undo.

/* Vari veis utilizadas na pi_histor_sal_func */
define var v_cdn_cargo_basic         as int  format ">>>>9"   label "xxxxxx":U      no-undo.
define var v_cdn_niv_cargo           like funcionario.cdn_niv_cargo                 no-undo.
define var v_val_sal_mes             like histor_sal_func.val_salario_mensal        no-undo.
define var v_val_sal_hor             like histor_sal_func.val_salario_hora          no-undo.
define var v_val_sal_cat             like histor_sal_func.val_salario_categ         no-undo.
define var v_cdn_tab_sal             like histor_sal_func.cdn_tab_sal               no-undo.
define var v_num_fx_sal              like histor_sal_func.num_faixa_sal             no-undo.
define var v_num_niv_sal             like histor_sal_func.num_niv_sal               no-undo.
define var v_cdn_cargo_basic_funcao  like funcionario.cdn_cargo_basic               no-undo.
define var v_cdn_niv_cargo_funcao    like funcionario.cdn_niv_cargo                 no-undo.
define var v_val_sal_mes_funcao      like histor_sal_func.val_salario_mensal        no-undo.
define var v_val_sal_hor_funcao      like histor_sal_func.val_salario_hora          no-undo.
define var v_val_sal_cat_funcao      like histor_sal_func.val_salario_categ         no-undo.
define var v_cdn_tab_sal_funcao      like histor_sal_func.cdn_tab_sal               no-undo.
define var v_num_fx_sal_funcao       like histor_sal_func.num_faixa_sal             no-undo.
define var v_num_niv_sal_funcao      like histor_sal_func.num_niv_sal               no-undo.

def var v_des_envel_func             as char format "x(10)"                         no-undo.
def var v_cdn_estab2                 like funcionario.cdn_estab                     no-undo.
def var v_cod_unid_lotac2            like func_unid_lotac_plano.cod_unid_lotac      no-undo.
def var v_cod_rh_ccusto2             like funcionario.cod_rh_ccusto                 no-undo.
define var v_cdn_turno2              like funcionario.cdn_turno_trab                no-undo.
DEF VAR i-linha AS INT.

DEF VAR nr-apolice AS INT FORMAT "9999999".
DEF VAR nr-fatura  AS INT FORMAT "999".
DEF VAR c-sexo     AS CHAR.
DEF VAR c-estado-civil AS CHAR FORMAT "x(20)".
DEF VAR c-salario  LIKE funcionario.val_salario_atual.
DEF VAR i-idade    AS  DEC.
DEF VAR c-situacao AS CHAR FORMAT "x(10)".

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iTotalNumberOfOrders    AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 3.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE c-descr-afas            AS CHARACTER.

DEFINE NEW GLOBAL SHARED VARIABLE v_dat_refer AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEF VAR COUNT  AS INT.

/* run utp/ut-acomp.p persistent set v_han_acomp. */

form 
    tt-editor.conteudo no-label format "x(100)" at 5
    with stream-io no-labels no-attr-space no-box width 132 frame f-cancela.

form 
    v_des_titulo               no-label colon 56 skip(1)
    with stream-io side-labels no-attr-space no-box width 132 frame f-tit.

form 
    tt-param.cod_rh_ccusto_ini  colon 40 space(1)
    "      |<  >| "
    tt-param.cod_rh_ccusto_fim  no-label skip
    with stream-io side-labels no-attr-space no-box width 132 frame f-sel.

form skip(3)
     v_des_selecao                               colon 20          skip(1)
     tt-param.i-es-ini                           colon 40 
     "       |<  >| "      
     tt-param.i-es-fim                                    no-label
     tt-param.i-fc-ini                           colon 40 space(1)
     "   |<  >| "
     tt-param.i-fc-fim                                    no-label 
     tt-param.v_num_faixa                        colon 40
     tt-param.v_cod_unid_lotac_ini               colon 40 space(1)
     "|<  >| " 
     tt-param.v_cod_unid_lotac_fim                        no-label 
     tt-param.cod_rh_ccusto_ini                  colon 40 space(1)
     "   |<  >| "
     tt-param.cod_rh_ccusto_fim                           no-label 
     tt-param.cdn_tip_contrat_ini                colon 40 space(1)
     "         |<  >| " 
     tt-param.cdn_tip_contrat_fim                         no-label skip(1)
     with stream-io attr-space side-labels  no-box width 132 frame f-selec_educ.

form 
    tt-param.log_mensal       colon 40
    tt-param.log_horista      colon 40
    tt-param.log_semanal      colon 40
    tt-param.log_quinzenal    colon 40
    tt-param.log_tarefa       colon 40
    tt-param.log_diarista     colon 40
    tt-param.log_lista_demit  colon 40 skip
    with stream-io side-labels no-attr-space no-box width 132 frame f-param.

form header
    v_tit_estab                         at  04  space(01)
    v_cdn_estab                                 space(01)
    "-"                                         space(01)
    v_nom_pessoa_jurid                          space(25)
    v_des_tip_cargo_funcao                      space(01)
    tt-param.des_tip_cargo_funcao               skip(01)
    with stream-io no-labels no-box no-attr-space width 132 page-top frame f-cab-estab.

form header
    "Cargo/"                                    at 87   skip
    "Matr¡cula"                                         space(03)
    "Nome"                                      at 13   space(27)
    "C Custo"                                   at 44   space(02)
    "Unid Lotac"                                at 53   space(02)
    "Dat Admiss"                                at 65   space(01)
    "Dt Aposent"                                at 76   space(01)
    "Fun‡Æo"                                    at 87   space(01)
    "Niv"                                       at 94   space(01)
    "Envel Pagto"                               at 98   space(01)
    "Cat Salar"                                 at 110  space(07)
    "Turno"                                     at 126  skip
    "----------- ------------------------------ -------- ----------- ---------- ---------- ------ --- ----------- --------------- -----" skip
    with stream-io attr-space side-labels /*page-top*/ no-box width 132 frame f-cab-dados.

form header
    "Cargo/"                                    at 85   skip
    "Matr¡cula"                                         space(03)
    "Nome"                                      at 13   space(37)
    "C Custo"                                   at 54   space(02)
    "Dat Admiss"                                at 63   space(01)
    "Dt Aposent"                                at 74   space(01)
    "Fun‡Æo"                                    at 85   space(01)
    "Niv"                                       at 92   space(01)
    "Envel Pagto"                               at 96   space(01)
    "Cat Salar"                                 at 108  space(01)
    "Turno"                                     at 124  skip
    "----------- ---------------------------------------- -------- ---------- ---------- ------ --- ----------- --------------- -----" skip
    with stream-io attr-space side-labels /*page-top*/ no-box width 132 frame f-cab-dados-unid.

form header
    v_tit_ccusto                        at  07  space(01)
    v_cod_rh_ccusto                             space(01)
    "-"                                         space(01)
    v_des_rh_ccusto                             space(25)
    v_des_tip_cargo_funcao                      space(01)
    tt-param.des_tip_cargo_funcao               skip(01)
    with stream-io no-labels no-box no-attr-space width 132 page-top frame f-cab-ccusto.

form header
    "Cargo/"                                    at 97   skip
    "Matr¡cula"                                         space(03)
    "Nome"                                      at 13   space(37)
    "C Custo"                                   at 54   space(02)
    "Unid Lotac"                                at 63   space(02)
    "Dat Admiss"                                at 75   space(01)
    "Dt Aposent"                                at 86   space(01)
    "Fun‡Æo"                                    at 97   space(01)
    "Niv"                                       at 104  space(01)
    "Envel Pagto"                               at 108  space(01)
    "Cs"                                        at 120  skip
    "----------- ---------------------------------------- -------- ----------- ---------- ---------- ------ --- ----------- --" skip
     with stream-io attr-space side-labels /*page-top*/ no-box width 132 frame f-cab-dados-turno.

form header
    "Cargo/"                                    at 88   skip
    "Matr¡cula"                                         space(03)
    "Nome"                                      at 13   space(37)
    "Unid Lotac"                                at 54   space(02)
    "Dat Admiss"                                at 66   space(01)
    "Dt Aposent"                                at 77   space(01)
    "Fun‡Æo"                                    at 88   space(01)
    "Niv"                                       at 95   space(01)
    "Envel Pagto"                               at 99   space(01)
    "Cat Salar"                                 at 111  space(07)
    "Turno"                                     at 127  skip
    "----------- ---------------------------------------- ----------- ---------- ---------- ------ --- ----------- --------------- -----" skip
     with stream-io attr-space side-labels /*page-top*/ no-box width 132 frame f-cab-dados-ccusto.

form
    v_cdn_turno                         at 01    space(02)
    "-"                                         space(01)
    v_des_turno                        no-label skip(1)
    with down width 132 no-box no-attr-space side-labels stream-io frame f-turno.

form v_cdn_estab                        at 01         space(13)
     "-"                                           space(01)
     v_nom_pessoa_jurid              no-label      skip(1)
     with side-labels down width 132 no-box stream-io frame f-estab.

form v_num_fc                               at 001
     v_nome                                 at 13
     func_ccusto.cod_rh_ccusto              at 44 FORMAT {prghur/dop/eng010.i}
     func_unid_lotac_plano.cod_unid_lotac   at 53 FORMAT "x(11)"
     funcionario.dat_admis_func             at 65
     funcionario.dat_livre_1                at 76
     v_cdn_cargo_basic                      at 88
     v_cdn_niv_cargo                        at 94
     v_des_envel                            at 98
     v_des_categ_sal                        at 110
     func_turno_trab.cdn_turno_trab         at 127
     with no-label down width 132 no-box no-attr-space stream-io frame f-dados.

form v_num_fc                               at 001
     funcionario.nom_pessoa_fisic           at 13
     func_ccusto.cod_rh_ccusto              at 54 FORMAT {prghur/dop/eng010.i}
     funcionario.dat_admis_func             at 63
     funcionario.dat_livre_1                at 74
     v_cdn_cargo_basic                      at 86
     v_cdn_niv_cargo                        at 92
     v_des_envel                            at 96
     v_des_categ_sal                        at 108
     func_turno_trab.cdn_turno_trab         at 125
     with no-label down width 132 no-box no-attr-space stream-io frame f-dados_unid.

form v_num_fc                               at 001
     funcionario.nom_pessoa_fisic           at 13
     func_ccusto.cod_rh_ccusto              at 54 FORMAT {prghur/dop/eng010.i}
     func_unid_lotac_plano.cod_unid_lotac   at 63 FORMAT "x(11)"
     funcionario.dat_admis_func             at 75
     funcionario.dat_livre_1                at 86
     v_cdn_cargo_basic                      at 98
     v_cdn_niv_cargo                        at 104
     v_des_envel                            at 108
     func_categ_sal.cdn_categ_sal           at 120
     with no-label down width 132 no-box no-attr-space stream-io frame f-dados_turno.

form v_num_fc                               at 001
     funcionario.nom_pessoa_fisic           at 13
     func_unid_lotac_plano.cod_unid_lotac   at 54 FORMAT "x(11)"
     funcionario.dat_admis_func             at 66
     funcionario.dat_livre_1                at 77
     v_cdn_cargo_basic                      at 89
     v_cdn_niv_cargo                        at 95
     v_des_envel                            at 99
     v_des_categ_sal                        at 111
     func_turno_trab.cdn_turno_trab         at 128
     with no-label down width 132 no-box no-attr-space stream-io frame f-dados_ccusto.

{utp/ut-liter.i Listagem_Geral_de_Funcion rios MFP R}
assign c-titulo-relat = return-value.
run pi-inicializar in v_han_acomp (input return-value).
{utp/ut-liter.i Folha_de_Pagamento MFP C}
assign c-sistema = return-value.
{utp/ut-liter.i Sele‡Æo MFP L}
assign v_des_selecao:label in frame f-selec_educ = return-value.
{utp/ut-liter.i Niv_Unid_Lotac MFP L}
assign tt-param.v_num_faixa:label in frame f-selec_educ = return-value.
{utp/ut-liter.i Contrato MFP L}
assign tt-param.cdn_tip_contrat_ini:label in frame f-selec_educ = return-value.
{utp/ut-liter.i Lista_Demitidos * L}
assign tt-param.log_lista_demit:label in frame f-param = return-value.
{utp/ut-liter.i Mensal * L}
assign tt-param.log_mensal:label in frame f-param = return-value.
{utp/ut-liter.i Horista * L}
assign tt-param.log_horista:label in frame f-param = return-value.
{utp/ut-liter.i Semanal * L}
assign tt-param.log_semanal:label in frame f-param = return-value.
{utp/ut-liter.i Quinzenal * L}
assign tt-param.log_quinzenal:label in frame f-param = return-value.
{utp/ut-liter.i Tarefa * L}
assign tt-param.log_tarefa:label in frame f-param = return-value.
{utp/ut-liter.i Diarista * L}
assign tt-param.log_diarista:label in frame f-param = return-value.
{utp/ut-field.i dthrpyc funcionario cdn_estab 1}
assign v_tit_estab = trim(return-value) + ":".
{utp/ut-field.i dthrpyc funcionario cod_rh_ccusto 1}
assign v_tit_ccusto = trim(return-value) + ":".
{utp/ut-liter.i Total_Turno_-_Funcion rios * R}
assign v_des_tot_turno = trim(return-value) + ":".
{utp/ut-liter.i Total_Empresa_-_Funcion rios * R}
assign v_des_tot_emp = trim(return-value) + ":".
{utp/ut-liter.i Total_Estab_-_Funcion rios * R}
assign v_des_tot_est = trim(return-value) + ":".
{utp/ut-liter.i Total_Prest_-_Funcion rios * R}
assign v_des_tot_prest = trim(return-value) + ":".
{utp/ut-liter.i Total_CCusto_-_Funcion rios * R}
assign v_des_tot_ccust = trim(return-value) + ":".
{utp/ut-liter.i Cargo/Fun‡Æo * R}
assign v_des_tip_cargo_funcao = trim(return-value) + ":".


/*  Inicio do Programa   */
find hcm.empresa where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-lock no-error.

assign c-programa     = "ESFP1593"
       c-versao       = "000"
       c-revisao      = "000"
       v_log_cancel   = no.

find first empresa no-lock
    where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
if avail empresa then
  assign c-empresa = empresa.razao-social.
else assign c-empresa = "".

find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
if avail param_folha_educnal then
  assign v_log_folha_educnal = yes.

run utp/ut-trfrrp.p (input frame f-cab-dados:handle).
run utp/ut-trfrrp.p (input frame f-cab-dados-unid:handle).
run utp/ut-trfrrp.p (input frame f-cab-dados-ccusto:handle).
run utp/ut-trfrrp.p (input frame f-cab-dados-turno:handle).
run utp/ut-trfrrp.p (input frame f-estab:handle).
run utp/ut-trfrrp.p (input frame f-turno:handle).
run utp/ut-trfrrp.p (input frame f-cab-ccusto:handle).
run utp/ut-trfrrp.p (input frame f-cab-estab:handle).
run utp/ut-trfrrp.p (input frame f-param:handle).
run utp/ut-trfrrp.p (input frame f-selec_educ:handle).
run utp/ut-trfrrp.p (input frame f-sel:handle).
run utp/ut-trfrrp.p (input frame f-tit:handle).
run utp/ut-trfrrp.p (input frame f-cancela:handle).
{include/i-rpcab.i}

IF NOT tt-param.l-habilitaRtf THEN
    view frame f-cabec.
IF NOT tt-param.l-habilitaRtf THEN
    view frame f-rodape.

if v_log_folha_educnal then
  assign tt-param.i-fc-ini = {prghur/dop/eng005.i &VAR="tt-param.i-fc-ini * 100"}
         tt-param.i-fc-fim = {prghur/dop/eng005.i &VAR="tt-param.i-fc-fim * 100 + 99"}.

assign v_cdn_classif = tt-param.num_classif_terc.

RUN pi-abre-excel.

block1:
do on stop undo block1, leave  transaction:
    if v_cdn_classif = 1 or
       v_cdn_classif = 2 then do:
       view frame f-cab-estab.

       for each funcionario no-lock
          where funcionario.cdn_empresa              = tt-param.v_cdn_empres_usuar         and
                funcionario.cdn_estab               >= tt-param.i-es-ini                   and
                funcionario.cdn_estab               <= tt-param.i-es-fim                   and
                funcionario.cdn_funcionario         >= tt-param.i-fc-ini                   and
                funcionario.cdn_funcionario         <= tt-param.i-fc-fim                   and
                funcionario.dat_admis_func          <= tt-param.v_dat_valid                and
               (funcionario.dat_desligto_func        = ?                                   or
                funcionario.dat_desligto_func        > tt-param.v_dat_valid                or
               (tt-param.log_lista_demit             = yes                                 and
                year(funcionario.dat_desligto_func)  = year(tt-param.v_dat_valid)          and
                month(funcionario.dat_desligto_func) = month(tt-param.v_dat_valid)))       and
               (funcionario.dat_admis_transf_func    = ?                                   or
                funcionario.dat_admis_transf_func   <= tt-param.v_dat_valid)               and
                funcionario.cdn_tip_contrat_func    >= tt-param.cdn_tip_contrat_ini        and
                funcionario.cdn_tip_contrat_func    <= tt-param.cdn_tip_contrat_fim        and
              ((v_log_folha_educnal and funcionario.cdn_tip_contrat_func        >= 0)      or
               (not v_log_folha_educnal)),
           last func_unid_lotac_plano of funcionario no-lock where
                func_unid_lotac_plano.dat_inic_lotac_func <= tt-param.v_dat_valid          and
                func_unid_lotac_plano.dat_fim_lotac_func  >= tt-param.v_dat_valid          and      
                func_unid_lotac_plano.cod_unid_lotac      >= tt-param.v_cod_unid_lotac_ini and
                func_unid_lotac_plano.cod_unid_lotac      <= tt-param.v_cod_unid_lotac_fim,
           last func_ccusto of funcionario no-lock where
                func_ccusto.dat_inic_lotac_func <= tt-param.v_dat_valid       and
                func_ccusto.dat_fim_lotac_func  >= tt-param.v_dat_valid       and      
                func_ccusto.cod_rh_ccusto       >= tt-param.cod_rh_ccusto_ini and
                func_ccusto.cod_rh_ccusto       <= tt-param.cod_rh_ccusto_fim,
           last func_categ_sal of funcionario no-lock where
                func_categ_sal.dat_inic_lotac_func <= tt-param.v_dat_valid                and
                func_categ_sal.dat_fim_lotac_func  >= tt-param.v_dat_valid                and
              ((func_categ_sal.cdn_categ_sal        = 1 and tt-param.log_mensal    = yes) or
               (func_categ_sal.cdn_categ_sal        = 2 and tt-param.log_horista   = yes) or
               (func_categ_sal.cdn_categ_sal        = 3 and tt-param.log_semanal   = yes) or
               (func_categ_sal.cdn_categ_sal        = 4 and tt-param.log_quinzenal = yes) or
               (func_categ_sal.cdn_categ_sal        = 5 and tt-param.log_tarefa    = yes) or
               (func_categ_sal.cdn_categ_sal        = 6 and tt-param.log_diarista  = yes)),
           last func_turno_trab of funcionario no-lock where
                func_turno_trab.dat_inic_lotac_func_turno_trab <= tt-param.v_dat_valid and
                func_turno_trab.dat_term_lotac_func            >= tt-param.v_dat_valid 
          break by funcionario.cdn_empresa
                by funcionario.cdn_estab
                by funcionario.idi_orig_contratac_func
                by funcionario.cdn_prestdor_serv
                by if tt-param.num_classif_terc = 1
                   then string(funcionario.cdn_funcionario,"99999999")
                   else funcionario.nom_pessoa_fisic:

          {prghur/fpp/fp9240.i14}

          &IF "{&func-dtype}" = "INT"  &THEN
          assign v_cont_func = v_cont_func + 1.
          &ENDIF
          run pi-acompanhar in v_han_acomp(input string(v_cont_func)).

          assign v_num_fc        = if v_log_folha_educnal then string(funcionario.cdn_func_centrdor, "zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func, "99") + "-" + string(funcionario.num_digito_verfdor_func)
                                   else string(funcionario.cdn_funcionario, "zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func)
                 v_des_categ_sal = {database/inpy/i01py118.i 04 func_categ_sal.cdn_categ_sal}.

          if first-of(funcionario.cdn_empresa) then do:
            assign v_tot_emp = 0.
          end.

          if first-of(funcionario.cdn_estab) then do:
            assign v_tot_estab = 0.
            find rh_estab of funcionario no-lock no-error.
            assign v_cdn_estab        = rh_estab.cdn_estab
                   v_nom_pessoa_jurid = rh_estab.nom_pessoa_jurid.
          end.

          /* Encontrando o hist¢rico*/
          run pi_histor_sal_func (input funcionario.cdn_estab,
                                  input funcionario.cdn_funcionario,
                                  input tt-param.idi_tip_cargo_funcao /*hist¢rico*/ ,
                                  output v_cdn_cargo_basic,
                                  output v_cdn_niv_cargo,
                                  output v_val_sal_mes,
                                  output v_val_sal_hor,
                                  output v_val_sal_cat,
                                  output v_cdn_tab_sal,
                                  output v_num_fx_sal,
                                  output v_num_niv_sal,
                                  output v_cdn_cargo_basic_funcao,
                                  output v_cdn_niv_cargo_funcao,
                                  output v_val_sal_mes_funcao,
                                  output v_val_sal_hor_funcao,
                                  output v_val_sal_cat_funcao,
                                  output v_cdn_tab_sal_funcao,
                                  output v_num_fx_sal_funcao,
                                  output v_num_niv_sal_funcao).
          /*Encontrando a descri‡Æo do Envelope/Cargo*/
          find first cargo no-lock
               where cargo.cdn_cargo_basic = v_cdn_cargo_basic
                 and cargo.cdn_niv_cargo   = v_cdn_niv_cargo no-error.
          if avail cargo then
              assign v_des_envel = cargo.des_envel_pagto.
          else
              assign v_des_envel = "".

          /*Encontrando a descri‡Æo do Envelope/Fun‡Æo*/
          find first cargo no-lock
               where cargo.cdn_cargo_basic = v_cdn_cargo_basic_funcao
                 and cargo.cdn_niv_cargo   = v_cdn_niv_cargo_funcao no-error.
          if avail cargo then
              assign v_des_envel_func = cargo.des_envel_pagto.
          else
              assign v_des_envel_func = "".

          /* Imprimindo o cabe‡alho */
          if (v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1) then do:
              if v_cdn_estab2 <> funcionario.cdn_estab then do:
                  assign v_cdn_estab2 = funcionario.cdn_estab.
                  if not(first(funcionario.cdn_estab)) then
                      page.
                  view frame f-cab-dados.
              end.
          end.
          assign v_nome = funcionario.nom_pessoa_fisic.

          /* Imprime dados do cargo */
          if v_cdn_cargo_basic > 0 and
             tt-param.idi_tip_cargo_funcao <> 2 then do:
              disp v_num_fc
                   v_nome
                   func_ccusto.cod_rh_ccusto
                   func_unid_lotac_plano.cod_unid_lotac
                   funcionario.dat_admis_func
                   funcionario.dat_livre_1
                   v_cdn_cargo_basic
                   v_cdn_niv_cargo
                   v_des_envel
                   v_des_categ_sal
                   func_turno_trab.cdn_turno_trab
                   with stream-io frame f-dados.
              down with frame f-dados.
          end.
          /* Imprime dados da fun‡Æo */
          if v_cdn_cargo_basic_funcao > 0 and
             tt-param.idi_tip_cargo_funcao <> 1 then do:
              disp v_num_fc when tt-param.idi_tip_cargo_funcao <> 3
                   v_nome when tt-param.idi_tip_cargo_funcao <> 3
                   func_ccusto.cod_rh_ccusto when tt-param.idi_tip_cargo_funcao <> 3
                   func_unid_lotac_plano.cod_unid_lotac when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.dat_admis_func when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.dat_livre_1
                   v_cdn_cargo_basic_funcao @ v_cdn_cargo_basic
                   v_cdn_niv_cargo_funcao   @ v_cdn_niv_cargo
                   v_des_envel_func         @ v_des_envel
                   v_des_categ_sal
                   func_turno_trab.cdn_turno_trab
                   with stream-io frame f-dados.
              down with frame f-dados.
          end.

          RUN pi-imprime-linha. /* sergio */

          /* somando para imprimir o total */
          if (v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1) then
              assign v_tot_emp   = v_tot_emp + 1
                     v_tot_estab = v_tot_estab + 1
                     v_tot_prest = v_tot_prest + 1.

          if tt-param.num_razao_social = 2 and
             last-of(funcionario.cdn_prestdor_serv) and
             v_tot_prest > 0 then do:
             put unformatted skip(1).
             put v_des_tot_prest at 77 v_tot_prest format ">>>>>>>9".
             put unformatted skip(1).
             assign v_tot_prest = 0.
          end.
          if last-of(funcionario.cdn_estab) and
             v_tot_estab > 0 then do:
            put unformatted skip(1).
            put v_des_tot_est at 77 v_tot_estab to 114 format ">>>>>>>9".
            put unformatted skip(1).
          end.
          if last-of(funcionario.cdn_empresa) and
             v_tot_emp > 0 then do:
            put unformatted skip(1).
            put v_des_tot_emp at 75 v_tot_emp to 114 format ">>>>>>>9".
            put unformatted skip(1).
            page.
          end.
       end /* for funcionario */.
       hide frame f-cab-estab.
    end /* if classifica */.

    if v_cdn_classif = 3 or
       v_cdn_classif = 4 then do:
       {prghur/fpp/fp9200.i6} 
       for each tt_lotac_funcionario no-lock,
           each funcionario of tt_lotac_funcionario no-lock  where
                funcionario.cdn_tip_contrat_func >= tt-param.cdn_tip_contrat_ini and
                funcionario.cdn_tip_contrat_func <= tt-param.cdn_tip_contrat_fim and
              ((v_log_folha_educnal and funcionario.cdn_tip_contrat_func >= 0)   or
               (not v_log_folha_educnal)) and
                funcionario.dat_admis_func         <= tt-param.v_dat_valid          and
               (funcionario.dat_desligto_func       = ?                             or
                funcionario.dat_desligto_func       > tt-param.v_dat_valid          or
               (tt-param.log_lista_demit            = yes                           and
               year(funcionario.dat_desligto_func)  = year(tt-param.v_dat_valid)    and
               month(funcionario.dat_desligto_func) = month(tt-param.v_dat_valid))) and
              (funcionario.dat_admis_transf_func    = ?                             or 
               funcionario.dat_admis_transf_func   <= tt-param.v_dat_valid),
           last func_ccusto of funcionario no-lock where
                func_ccusto.dat_inic_lotac_func <= tt-param.v_dat_valid       and
                func_ccusto.dat_fim_lotac_func  >= tt-param.v_dat_valid       and
                func_ccusto.cod_rh_ccusto       >= tt-param.cod_rh_ccusto_ini and
                func_ccusto.cod_rh_ccusto       <= tt-param.cod_rh_ccusto_fim,
           last func_categ_sal of funcionario no-lock where
                func_categ_sal.dat_inic_lotac_func <= tt-param.v_dat_valid           and
                func_categ_sal.dat_fim_lotac_func  >= tt-param.v_dat_valid           and
              ((func_categ_sal.cdn_categ_sal = 1 and tt-param.log_mensal    = yes)  or
               (func_categ_sal.cdn_categ_sal = 2 and tt-param.log_horista   = yes)  or
               (func_categ_sal.cdn_categ_sal = 3 and tt-param.log_semanal   = yes)  or
               (func_categ_sal.cdn_categ_sal = 4 and tt-param.log_quinzenal = yes)  or
               (func_categ_sal.cdn_categ_sal = 5 and tt-param.log_tarefa    = yes)  or
               (func_categ_sal.cdn_categ_sal = 6 and tt-param.log_diarista  = yes)),
           last func_turno_trab of funcionario no-lock where
                func_turno_trab.dat_inic_lotac_func_turno_trab <= tt-param.v_dat_valid and
                func_turno_trab.dat_term_lotac_func            >= tt-param.v_dat_valid 
           break by funcionario.cdn_empresa
                 by funcionario.cdn_estab
                 by tt_lotac_funcionario.num_seq_unid_lotac
                 by tt_lotac_funcionario.num_niv_unid_lotac
                 by tt_lotac_funcionario.cod_unid_lotac
                 by funcionario.idi_orig_contratac_func
                 by funcionario.cdn_prestdor_serv
                 by if tt-param.num_classif_terc = 3
                    then string(funcionario.cdn_funcionario,"99999999")
                    else funcionario.nom_pessoa_fisic:

           {prghur/fpp/fp9240.i14}

           &IF "{&func-dtype}" = "INT" &THEN
           assign v_cont_func = v_cont_func + 1.
           &ENDIF
           run pi-acompanhar in v_han_acomp (input string(v_cont_func)).                 

           assign v_num_fc        = if v_log_folha_educnal then string(funcionario.cdn_func_centrdor, "zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func, "99") + "-" + string(funcionario.num_digito_verfdor_func)
                                    else string(funcionario.cdn_funcionario, "zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func)
                  v_des_categ_sal = {database/inpy/i01py118.i 04 func_categ_sal.cdn_categ_sal}.

          /* Encontrando o hist¢rico*/
          run pi_histor_sal_func (input funcionario.cdn_estab,
                                  input funcionario.cdn_funcionario,
                                  input tt-param.idi_tip_cargo_funcao /*hist¢rico*/ ,
                                  output v_cdn_cargo_basic,
                                  output v_cdn_niv_cargo,
                                  output v_val_sal_mes,
                                  output v_val_sal_hor,
                                  output v_val_sal_cat,
                                  output v_cdn_tab_sal,
                                  output v_num_fx_sal,
                                  output v_num_niv_sal,
                                  output v_cdn_cargo_basic_funcao,
                                  output v_cdn_niv_cargo_funcao,
                                  output v_val_sal_mes_funcao,
                                  output v_val_sal_hor_funcao,
                                  output v_val_sal_cat_funcao,
                                  output v_cdn_tab_sal_funcao,
                                  output v_num_fx_sal_funcao,
                                  output v_num_niv_sal_funcao).
          /*Encontrando a descri‡Æo do Envelope/Cargo*/
          find first cargo no-lock
               where cargo.cdn_cargo_basic = v_cdn_cargo_basic
                 and cargo.cdn_niv_cargo   = v_cdn_niv_cargo no-error.
          if avail cargo then
              assign v_des_envel = cargo.des_envel_pagto.
          else
              assign v_des_envel = "".

          /*Encontrando a descri‡Æo do Envelope/Fun‡Æo*/
          find first cargo no-lock
               where cargo.cdn_cargo_basic = v_cdn_cargo_basic_funcao
                 and cargo.cdn_niv_cargo   = v_cdn_niv_cargo_funcao no-error.
          if avail cargo then
              assign v_des_envel_func = cargo.des_envel_pagto.
          else
              assign v_des_envel_func = "".         

          if first-of(funcionario.cdn_empresa) then do:
             assign v_tot_emp = 0.
          end.

          if first-of(tt_lotac_funcionario.cod_unid_lotac) then
              assign v_log_first_lotac = yes.
          
          if first-of(funcionario.cdn_estab) then do:
            assign v_tot_estab = 0.
            find rh_estab of funcionario no-lock no-error.
            assign v_cdn_estab        = rh_estab.cdn_estab
                   v_nom_pessoa_jurid = rh_estab.nom_pessoa_jurid.
            view frame f-cab-estab.
          end.

          if v_log_first_lotac and
             ((v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1)) then do:
              run pi_report_unid_lotac.
              run pi_acumul_unid_lotac.
              assign v_log_first_lotac = no.
          end.
          if not first-of(tt_lotac_funcionario.cod_unid_lotac) then
             run pi_find_total_unid_lotac.
          if v_log_avail_tot then
              if (v_cdn_cargo_basic > 0 and
                  tt-param.idi_tip_cargo_funcao <> 2) or
                 (v_cdn_cargo_basic_funcao > 0 and
                  tt-param.idi_tip_cargo_funcao <> 1) then do:
                  assign tt_total_unid_lotac.v_cont_func = tt_total_unid_lotac.v_cont_func + 1.
              end.

          /* Imprimindo o cabe‡alho */
          if (v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1) then do:
              if v_cdn_estab2      <> tt_lotac_funcionario.cdn_estab or
                 v_cod_unid_lotac2 <> tt_lotac_funcionario.cod_unid_lotac or
                 line-counter < 2 or
                 line-counter > 63 then do:
                  assign v_cdn_estab2        = tt_lotac_funcionario.cdn_estab
                         v_cod_unid_lotac2 = tt_lotac_funcionario.cod_unid_lotac.
                  view frame f-cab-dados-unid.
              end.
          end.
          /* Imprime dados do cargo */
          if v_cdn_cargo_basic > 0 and
             tt-param.idi_tip_cargo_funcao <> 2 then do:
              disp v_num_fc
                   funcionario.nom_pessoa_fisic
                   func_ccusto.cod_rh_ccusto
                   funcionario.dat_admis_func
                   funcionario.dat_livre_1
                   v_cdn_cargo_basic
                   v_cdn_niv_cargo
                   v_des_envel
                   v_des_categ_sal
                   func_turno_trab.cdn_turno_trab
                   with stream-io frame f-dados_unid.
              down with frame f-dados_unid.
          end.
          /* Imprime dados da fun‡Æo */
          if v_cdn_cargo_basic_funcao > 0 and
             tt-param.idi_tip_cargo_funcao <> 1 then do:
              disp v_num_fc when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.nom_pessoa_fisic when tt-param.idi_tip_cargo_funcao <> 3
                   func_ccusto.cod_rh_ccusto when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.dat_admis_func when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.dat_livre_1
                   v_cdn_cargo_basic_funcao @ v_cdn_cargo_basic
                   v_cdn_niv_cargo_funcao   @ v_cdn_niv_cargo
                   v_des_envel_func         @ v_des_envel
                   v_des_categ_sal
                   func_turno_trab.cdn_turno_trab
                   with stream-io frame f-dados_unid.
              down with frame f-dados_unid.
          end.

          RUN pi-imprime-linha. /* sergio */

          /* somando para imprimir o total */
          if (v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1) then
              assign v_tot_emp   = v_tot_emp + 1
                     v_tot_estab = v_tot_estab + 1
                     v_tot_prest = v_tot_prest + 1.

          if last-of(tt_lotac_funcionario.cod_unid_lotac) then do:
             run pi_report_acumul_unid_lotac.
          end.

          if last-of(funcionario.cdn_estab) then
             run pi_report_estab.

          if tt-param.num_razao_social = 2 and
             last-of(funcionario.cdn_prestdor_serv) and
             v_tot_prest > 0 then do:
              put unformatted skip(1).
              put v_des_tot_prest at 77 v_tot_prest format ">>>>>>>9".
              put unformatted skip(1).
              assign v_tot_prest = 0.
          end.

          if last-of(funcionario.cdn_estab) and
             v_tot_estab > 0 then do:
              put unformatted skip(1).
              put v_des_tot_est at 79 v_tot_estab to 116 format ">>>>>>>9".
              put unformatted skip(1).
          end.
          if last-of(funcionario.cdn_empresa) and
             v_tot_emp > 0 then do:
              put unformatted skip(1).
              put v_des_tot_emp at 77 v_tot_emp to 116 format ">>>>>>>9".
              put unformatted skip(1).
              page.
          end.
       end /* for tt_lotac_funcionario */.
       hide frame f-cab-estab.
    end /* if classifica */.

    if v_cdn_classif = 5 or
       v_cdn_classif = 6 then do:
       view frame f-cab-ccusto.
       for each func_ccusto no-lock where
                func_ccusto.cdn_empresa          = tt-param.v_cdn_empres_usuar and
                func_ccusto.cdn_estab           >= tt-param.i-es-ini           and
                func_ccusto.cdn_estab           <= tt-param.i-es-fim           and
                func_ccusto.cdn_funcionario     >= tt-param.i-fc-ini           and
                func_ccusto.cdn_funcionario     <= tt-param.i-fc-fim           and
                func_ccusto.dat_inic_lotac_func <= tt-param.v_dat_valid        and
                func_ccusto.dat_fim_lotac_func  >= tt-param.v_dat_valid        and
                func_ccusto.cod_rh_ccusto       >= tt-param.cod_rh_ccusto_ini  and
                func_ccusto.cod_rh_ccusto       <= tt-param.cod_rh_ccusto_fim,
           each funcionario of func_ccusto no-lock where
                funcionario.cdn_tip_contrat_func >= tt-param.cdn_tip_contrat_ini and
                funcionario.cdn_tip_contrat_func <= tt-param.cdn_tip_contrat_fim and
              ((v_log_folha_educnal and funcionario.cdn_tip_contrat_func >= 0)   or
               (not v_log_folha_educnal)) and
                funcionario.dat_admis_func         <= tt-param.v_dat_valid          and
               (funcionario.dat_desligto_func       = ?                             or
                funcionario.dat_desligto_func       > tt-param.v_dat_valid          or
               (tt-param.log_lista_demit            = yes                           and
               year(funcionario.dat_desligto_func)  = year(tt-param.v_dat_valid)    and
               month(funcionario.dat_desligto_func) = month(tt-param.v_dat_valid))) and
              (funcionario.dat_admis_transf_func    = ?                             or 
               funcionario.dat_admis_transf_func   <= tt-param.v_dat_valid),
           last func_unid_lotac_plano of funcionario no-lock where
                func_unid_lotac_plano.dat_inic_lotac_func <= tt-param.v_dat_valid          and
                func_unid_lotac_plano.dat_fim_lotac_func  >= tt-param.v_dat_valid          and      
                func_unid_lotac_plano.cod_unid_lotac      >= tt-param.v_cod_unid_lotac_ini and
                func_unid_lotac_plano.cod_unid_lotac      <= tt-param.v_cod_unid_lotac_fim,
           last func_categ_sal of funcionario no-lock where
                func_categ_sal.dat_inic_lotac_func <= tt-param.v_dat_valid                and
                func_categ_sal.dat_fim_lotac_func  >= tt-param.v_dat_valid                and
              ((func_categ_sal.cdn_categ_sal        = 1 and tt-param.log_mensal    = yes) or
               (func_categ_sal.cdn_categ_sal        = 2 and tt-param.log_horista   = yes) or
               (func_categ_sal.cdn_categ_sal        = 3 and tt-param.log_semanal   = yes) or
               (func_categ_sal.cdn_categ_sal        = 4 and tt-param.log_quinzenal = yes) or
               (func_categ_sal.cdn_categ_sal        = 5 and tt-param.log_tarefa    = yes) or
               (func_categ_sal.cdn_categ_sal        = 6 and tt-param.log_diarista  = yes)),
           last func_turno_trab of funcionario no-lock where
                func_turno_trab.dat_inic_lotac_func_turno_trab <= tt-param.v_dat_valid and
                func_turno_trab.dat_term_lotac_func            >= tt-param.v_dat_valid 
          break by funcionario.cdn_empresa
                by func_ccusto.cod_rh_ccusto
                by funcionario.cdn_estab
                by funcionario.idi_orig_contratac_func
                by funcionario.cdn_prestdor_serv
                by if tt-param.num_classif_terc = 5
                   then string(funcionario.cdn_funcionario,"99999999")
                   else funcionario.nom_pessoa_fisic:

          {prghur/fpp/fp9240.i14}

          &IF "{&func-dtype}" = "INT" &THEN
          assign v_cont_func = v_cont_func + 1.
          &ENDIF
          run pi-acompanhar in v_han_acomp(input string(v_cont_func)).

          assign v_num_fc        = if v_log_folha_educnal then string(funcionario.cdn_func_centrdor, "zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func, "99") + "-" + string(funcionario.num_digito_verfdor_func)
                                   else string(funcionario.cdn_funcionario, "zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func)
                 v_des_categ_sal = {database/inpy/i01py118.i 04 func_categ_sal.cdn_categ_sal}.

          if first-of(funcionario.cdn_empresa) then do:
            assign v_tot_emp = 0.
          end.

          if first-of(funcionario.cdn_estab) then do:
            assign v_tot_estab = 0.
            find rh_estab of funcionario no-lock no-error.
            assign v_cdn_estab        = rh_estab.cdn_estab
                   v_nom_pessoa_jurid = rh_estab.nom_pessoa_jurid.
          end.

          if first-of(func_ccusto.cod_rh_ccusto) then do:
            assign v_tot_ccusto = 0.
            find rh_ccusto of func_ccusto no-lock no-error.
            assign v_cod_rh_ccusto = func_ccusto.cod_rh_ccusto
                   v_des_rh_ccusto = rh_ccusto.des_rh_ccusto.
          end.

          /* Encontrando o hist¢rico*/
          run pi_histor_sal_func (input funcionario.cdn_estab,
                                  input funcionario.cdn_funcionario,
                                  input tt-param.idi_tip_cargo_funcao /*hist¢rico*/ ,
                                  output v_cdn_cargo_basic,
                                  output v_cdn_niv_cargo,
                                  output v_val_sal_mes,
                                  output v_val_sal_hor,
                                  output v_val_sal_cat,
                                  output v_cdn_tab_sal,
                                  output v_num_fx_sal,
                                  output v_num_niv_sal,
                                  output v_cdn_cargo_basic_funcao,
                                  output v_cdn_niv_cargo_funcao,
                                  output v_val_sal_mes_funcao,
                                  output v_val_sal_hor_funcao,
                                  output v_val_sal_cat_funcao,
                                  output v_cdn_tab_sal_funcao,
                                  output v_num_fx_sal_funcao,
                                  output v_num_niv_sal_funcao).
          /*Encontrando a descri‡Æo do Envelope/Cargo*/
          find first cargo no-lock
               where cargo.cdn_cargo_basic = v_cdn_cargo_basic
                 and cargo.cdn_niv_cargo   = v_cdn_niv_cargo no-error.
          if avail cargo then
              assign v_des_envel = cargo.des_envel_pagto.
          else
              assign v_des_envel = "".

          /*Encontrando a descri‡Æo do Envelope/Fun‡Æo*/
          find first cargo no-lock
               where cargo.cdn_cargo_basic = v_cdn_cargo_basic_funcao
                 and cargo.cdn_niv_cargo   = v_cdn_niv_cargo_funcao no-error.
          if avail cargo then
              assign v_des_envel_func = cargo.des_envel_pagto.
          else
              assign v_des_envel_func = "".
              
          /* Imprimindo o cabe‡alho */
          if (v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1) then do:
              if v_cod_rh_ccusto2 <> func_ccusto.cod_rh_ccusto then do:
                  assign v_cod_rh_ccusto2 = func_ccusto.cod_rh_ccusto.
                  if not(first(func_ccusto.cod_rh_ccusto)) then do:
                      page.
                      view frame f-cab-ccusto.
                  end.
              end.
                  
              if v_cdn_estab2 <> funcionario.cdn_estab or
                 line-counter < 2 or
                 line-counter > 63 then do:
                  assign v_cdn_estab2 = funcionario.cdn_estab.
                  disp v_cdn_estab
                       v_nom_pessoa_jurid
                       with stream-io frame f-estab.
                  view frame f-cab-dados-ccusto.
              end.
          end.

          /* Imprime dados do cargo */
          if v_cdn_cargo_basic > 0 and
             tt-param.idi_tip_cargo_funcao <> 2 then do:
              disp v_num_fc
                   funcionario.nom_pessoa_fisic
                   func_unid_lotac_plano.cod_unid_lotac
                   funcionario.dat_admis_func
                   funcionario.dat_livre_1
                   v_cdn_cargo_basic
                   v_cdn_niv_cargo
                   v_des_envel
                   v_des_categ_sal
                   func_turno_trab.cdn_turno_trab
                   with stream-io frame f-dados_ccusto.
              down with frame f-dados_ccusto.
          end.
          /* Imprime dados da fun‡Æo */
          if v_cdn_cargo_basic_funcao > 0 and
             tt-param.idi_tip_cargo_funcao <> 1 then do:
              disp v_num_fc when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.nom_pessoa_fisic when tt-param.idi_tip_cargo_funcao <> 3
                   func_unid_lotac_plano.cod_unid_lotac when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.dat_admis_func when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.dat_livre_1
                   v_cdn_cargo_basic_funcao @ v_cdn_cargo_basic
                   v_cdn_niv_cargo_funcao   @ v_cdn_niv_cargo
                   v_des_envel_func         @ v_des_envel
                   v_des_categ_sal
                   func_turno_trab.cdn_turno_trab
                   with stream-io frame f-dados_ccusto.
              down with frame f-dados_ccusto.
          end.

          RUN pi-imprime-linha. /* sergio */

          /* somando para imprimir o total */
          if (v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1) then
              assign v_tot_emp    = v_tot_emp + 1
                     v_tot_estab  = v_tot_estab + 1
                     v_tot_prest  = v_tot_prest + 1
                     v_tot_ccusto = v_tot_ccusto + 1.

          if tt-param.num_razao_social = 2 and
             last-of(funcionario.cdn_prestdor_serv) and
             v_tot_prest > 0 then do:
              put unformatted skip(1).
              put v_des_tot_prest at 77 v_tot_prest format ">>>>>>>9".
              put unformatted skip(1).
              assign v_tot_prest = 0.
          end.
          if last-of(funcionario.cdn_estab) and
             v_tot_estab > 0 then do:
              put unformatted skip(1).
              put v_des_tot_est at 61 v_tot_estab to 98 format ">>>>>>>9".
              put unformatted skip(1).
          end.
          if last-of(func_ccusto.cod_rh_ccusto) and
             v_tot_ccusto > 0 then do:
              put v_des_tot_ccust at 60 v_tot_ccusto to 98 format "zzzzzzz9".
              put unformatted skip(1).
          end.
          if last-of(funcionario.cdn_empresa) and
             v_tot_emp > 0 then do:
              put v_des_tot_emp at 59 v_tot_emp to 98 format ">>>>>>>9".
              put unformatted skip(1).
          end.

       end /* for funcionario */.
       hide frame f-cab-ccusto.
    end /* if classifica */.

    if v_cdn_classif = 7 or
       v_cdn_classif = 8 then do:
       view frame f-cab-estab.

       for each func_turno_trab no-lock where
                func_turno_trab.cdn_empresa                     = tt-param.v_cdn_empres_usuar  and
                func_turno_trab.cdn_estab                      >= tt-param.i-es-ini            and
                func_turno_trab.cdn_estab                      <= tt-param.i-es-fim            and
                func_turno_trab.cdn_funcionario                >= tt-param.i-fc-ini            and
                func_turno_trab.cdn_funcionario                <= tt-param.i-fc-fim            and
                func_turno_trab.dat_inic_lotac_func_turno_trab <= tt-param.v_dat_valid         and
                func_turno_trab.dat_term_lotac_func            >= tt-param.v_dat_valid, 
           each funcionario of func_turno_trab no-lock where
                funcionario.cdn_tip_contrat_func               >= tt-param.cdn_tip_contrat_ini        and
                funcionario.cdn_tip_contrat_func               <= tt-param.cdn_tip_contrat_fim        and
              ((v_log_folha_educnal and funcionario.cdn_tip_contrat_func        >= 0)   or
               (not v_log_folha_educnal)) and
                funcionario.dat_admis_func         <= tt-param.v_dat_valid          and
               (funcionario.dat_desligto_func       = ?                             or
                funcionario.dat_desligto_func       > tt-param.v_dat_valid          or
               (tt-param.log_lista_demit            = yes                           and
               year(funcionario.dat_desligto_func)  = year(tt-param.v_dat_valid)    and
               month(funcionario.dat_desligto_func) = month(tt-param.v_dat_valid))) and
              (funcionario.dat_admis_transf_func    = ?                             or 
               funcionario.dat_admis_transf_func   <= tt-param.v_dat_valid),
           last func_unid_lotac_plano of funcionario no-lock where
                func_unid_lotac_plano.dat_inic_lotac_func <= tt-param.v_dat_valid          and
                func_unid_lotac_plano.dat_fim_lotac_func  >= tt-param.v_dat_valid          and      
                func_unid_lotac_plano.cod_unid_lotac      >= tt-param.v_cod_unid_lotac_ini and
                func_unid_lotac_plano.cod_unid_lotac      <= tt-param.v_cod_unid_lotac_fim,
           last func_ccusto of funcionario no-lock where
                func_ccusto.dat_inic_lotac_func <= tt-param.v_dat_valid       and
                func_ccusto.dat_fim_lotac_func  >= tt-param.v_dat_valid       and      
                func_ccusto.cod_rh_ccusto       >= tt-param.cod_rh_ccusto_ini and
                func_ccusto.cod_rh_ccusto       <= tt-param.cod_rh_ccusto_fim,
           last func_categ_sal of funcionario no-lock where
                func_categ_sal.dat_inic_lotac_func <= tt-param.v_dat_valid                and
                func_categ_sal.dat_fim_lotac_func  >= tt-param.v_dat_valid                and
              ((func_categ_sal.cdn_categ_sal        = 1 and tt-param.log_mensal    = yes) or
               (func_categ_sal.cdn_categ_sal        = 2 and tt-param.log_horista   = yes) or
               (func_categ_sal.cdn_categ_sal        = 3 and tt-param.log_semanal   = yes) or
               (func_categ_sal.cdn_categ_sal        = 4 and tt-param.log_quinzenal = yes) or
               (func_categ_sal.cdn_categ_sal        = 5 and tt-param.log_tarefa    = yes) or
               (func_categ_sal.cdn_categ_sal        = 6 and tt-param.log_diarista  = yes))
          break by funcionario.cdn_empresa
                by funcionario.cdn_estab
                by func_turno_trab.cdn_turno_trab
                by funcionario.idi_orig_contratac_func
                by funcionario.cdn_prestdor_serv
                by if tt-param.num_classif_terc = 7
                   then string(funcionario.cdn_funcionario,"99999999")
                   else funcionario.nom_pessoa_fisic:

          {prghur/fpp/fp9240.i14}

          &IF "{&LAGUAGE-CODE}" = "POR" &THEN
          assign v_cont_func = v_cont_func + 1.
          &ENDIF
          run pi-acompanhar in v_han_acomp(input string(v_cont_func)).

          find turno_trab of funcionario no-lock no-error.
          assign v_num_fc        = if v_log_folha_educnal then string(funcionario.cdn_func_centrdor, "zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func, "99") + "-" + string(funcionario.num_digito_verfdor_func)
                                   else string(funcionario.cdn_funcionario, "zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func)
                 v_cdn_turno = func_turno_trab.cdn_turno_trab
                 v_des_turno = turno_trab.des_turno_trab when avail turno_trab.

          if first-of(funcionario.cdn_empresa) then do:
            assign v_tot_emp = 0.
          end.

          if first-of(funcionario.cdn_estab) then do:
            assign v_tot_estab = 0.
            find rh_estab of funcionario no-lock no-error.
            assign v_cdn_estab        = rh_estab.cdn_estab
                   v_nom_pessoa_jurid = rh_estab.nom_pessoa_jurid.
          end.

          if first-of(func_turno_trab.cdn_turno_trab) then do:
            assign v_tot_turno = 0.
          end.

          /* Encontrando o hist¢rico*/
          run pi_histor_sal_func (input funcionario.cdn_estab,
                                  input funcionario.cdn_funcionario,
                                  input tt-param.idi_tip_cargo_funcao /*hist¢rico*/ ,
                                  output v_cdn_cargo_basic,
                                  output v_cdn_niv_cargo,
                                  output v_val_sal_mes,
                                  output v_val_sal_hor,
                                  output v_val_sal_cat,
                                  output v_cdn_tab_sal,
                                  output v_num_fx_sal,
                                  output v_num_niv_sal,
                                  output v_cdn_cargo_basic_funcao,
                                  output v_cdn_niv_cargo_funcao,
                                  output v_val_sal_mes_funcao,
                                  output v_val_sal_hor_funcao,
                                  output v_val_sal_cat_funcao,
                                  output v_cdn_tab_sal_funcao,
                                  output v_num_fx_sal_funcao,
                                  output v_num_niv_sal_funcao).
          /*Encontrando a descri‡Æo do Envelope/Cargo*/
          find first cargo no-lock
               where cargo.cdn_cargo_basic = v_cdn_cargo_basic
                 and cargo.cdn_niv_cargo   = v_cdn_niv_cargo no-error.
          if avail cargo then
              assign v_des_envel = cargo.des_envel_pagto.
          else
              assign v_des_envel = "".

          /*Encontrando a descri‡Æo do Envelope/Fun‡Æo*/
          find first cargo no-lock
               where cargo.cdn_cargo_basic = v_cdn_cargo_basic_funcao
                 and cargo.cdn_niv_cargo   = v_cdn_niv_cargo_funcao no-error.
          if avail cargo then
              assign v_des_envel_func = cargo.des_envel_pagto.
          else
              assign v_des_envel_func = "".     

          if (v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1) then do:
              if v_cdn_estab2 <> funcionario.cdn_estab then do:
                  assign v_cdn_estab2 = funcionario.cdn_estab.
                  if not(first(funcionario.cdn_estab)) then
                      page.
              end.
              /* Imprimindo o cabe‡alho */
              if v_cdn_turno2 <> func_turno_trab.cdn_turno_trab or
                 line-counter < 2 or
                 line-counter > 63 then do:
                  assign v_cdn_turno2 = func_turno_trab.cdn_turno_trab.
                  disp v_cdn_turno
                       v_des_turno
                       with stream-io frame f-turno.
                  view frame f-cab-dados-turno.
              end.
          end.

          /* Imprime dados do cargo */
          if v_cdn_cargo_basic > 0 and
             tt-param.idi_tip_cargo_funcao <> 2 then do:
              disp v_num_fc
                   funcionario.nom_pessoa_fisic
                   func_ccusto.cod_rh_ccusto
                   func_unid_lotac_plano.cod_unid_lotac
                   funcionario.dat_admis_func
                   funcionario.dat_livre_1
                   v_cdn_cargo_basic
                   v_cdn_niv_cargo
                   v_des_envel
                   func_categ_sal.cdn_categ_sal
                   with stream-io frame f-dados_turno.
              down with frame f-dados_turno.
          end.
          /* Imprime dados da fun‡Æo */
          if v_cdn_cargo_basic_funcao > 0 and
             tt-param.idi_tip_cargo_funcao <> 1 then do:
              disp v_num_fc when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.nom_pessoa_fisic when tt-param.idi_tip_cargo_funcao <> 3
                   func_ccusto.cod_rh_ccusto when tt-param.idi_tip_cargo_funcao <> 3
                   func_unid_lotac_plano.cod_unid_lotac when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.dat_admis_func when tt-param.idi_tip_cargo_funcao <> 3
                   funcionario.dat_livre_1
                   v_cdn_cargo_basic_funcao @ v_cdn_cargo_basic
                   v_cdn_niv_cargo_funcao   @ v_cdn_niv_cargo
                   v_des_envel_func         @ v_des_envel
                   func_categ_sal.cdn_categ_sal
                   with stream-io frame f-dados_turno.
              down with frame f-dados_turno.
          end.

          RUN pi-imprime-linha. /* sergio */

          /* somando para imprimir o total */
          if (v_cdn_cargo_basic > 0 and
              tt-param.idi_tip_cargo_funcao <> 2) or
             (v_cdn_cargo_basic_funcao > 0 and
              tt-param.idi_tip_cargo_funcao <> 1) then
              assign v_tot_emp   = v_tot_emp + 1
                     v_tot_estab = v_tot_estab + 1
                     v_tot_prest = v_tot_prest + 1
                     v_tot_turno = v_tot_turno + 1.

          if tt-param.num_razao_social = 2 and
             last-of(funcionario.cdn_prestdor_serv) and
             v_tot_prest > 0 then do:
              put unformatted skip(1).
              put v_des_tot_prest at 77 v_tot_prest format ">>>>>>>9".
              put unformatted skip(1).
              assign v_tot_prest = 0.
          end.
          if last-of(func_turno_trab.cdn_turno_trab) and
             v_tot_turno > 0 then do:
              put unformatted skip(1).
              put v_des_tot_turno at 75 v_tot_turno to 113 format ">>>>>>>9".
              put unformatted skip(1).
          end.
          if last-of(funcionario.cdn_estab) and
             v_tot_estab > 0 then do:
              put unformatted skip(1).
              put v_des_tot_est at 75 v_tot_estab to 113 format ">>>>>>>9".
              put unformatted skip(1).
          end.
          if last-of(funcionario.cdn_empresa) and
             v_tot_emp > 0 then do:
              put unformatted skip(1).
              put v_des_tot_emp at 73 v_tot_emp to 113 format ">>>>>>>9".
              put unformatted skip(1).
              page.
          end.
       end /* for funcionario */.
       hide frame f-cab-estab.
    end /* if classifica */.

end /* do block1 */.

RUN pi-encerra-excel.
/* {prghur/fpp/fp9240.i11 prghur/fpp/ESFP1593rt.p} */

run pi-retorna-status in v_han_acomp(output c-status).

if c-status = "NOK":U then do:
   run utp/ut-msgs.p (input "msg",
                      input 7575,
                      input "").

   assign v_des_mensagem = return-value.

   run pi-print-editor(v_des_mensagem,100).

   for each tt-editor:
       disp tt-editor.conteudo when avail tt-editor with frame  f-cancela.
       down with frame f-cancela.
   end.
end.
if tt-param.parametro = no then do:
/*     run pi-finalizar in v_han_acomp. */
    {include/i-rpclo.i}
    return "OK":U.
end.
ELSE
   RETURN "OK":U. 

/* run pi-finalizar in v_han_acomp. */


procedure pi_acumul_total_nivel_acima.
    if tt_total_unid_lotac.v_cont_func > 0 then
        assign  b_tt_total_unid_lotac.v_cont_func = b_tt_total_unid_lotac.v_cont_func +
                                                    tt_total_unid_lotac.v_cont_func.
end.

procedure pi_report_total_niv_unid_lotac.
    if tt_total_unid_lotac.v_cont_func > 0 then do:
        if v_log_report_tot then do:
            assign v_tot_unid = v_des_total_lotac.
            display v_tot_unid at 20
                    tt_total_unid_lotac.v_cont_func
                    with no-labels stream-io frame f-totais.                            
            down with frame f-totais.
        end.
    end.
end.

&endif
PROCEDURE pi-abre-excel:
   CREATE "Excel.Application" chExcelApplication.
   chworkbook  = chexcelapplication:workbooks:add.
   chworksheet = chexcelapplication:sheets:item(1).
   chExcelApplication:Visible = FALSE.
/************************************************************/

    /*************** Imprime altera‡äes **************************/
    /*************** Cabe»alho ***********************************/
    chWorkSheet:Range("a1" ):Value = "Mapeamento de Funcionarios   -   " + string(today).
    chWorkSheet:Range("a1" ):Font:Size = 12.
    chWorkSheet:Range("a1" ):Font:Bold = TRUE.
    chWorkSheet:Range("a3" ):Value = "Empresa".
    chWorkSheet:Range("b3" ):Value = "Estabel".
    chWorkSheet:Range("c3" ):Value = "Matricula".
    chWorkSheet:Range("d3" ):Value = "Nome".
    chWorkSheet:Range("e3" ):Value = "Estado Civil".
    chWorkSheet:Range("f3" ):Value = "Sexo".
    chWorkSheet:Range("g3" ):Value = "CPF".
    chWorkSheet:Range("h3" ):Value = "Num.Dependente IRF".
    chWorkSheet:Range("i3" ):Value = "Dt Nascimento". 
    chWorkSheet:Range("j3" ):Value = "Idade". 
    chWorkSheet:Range("k3" ):Value = "Status".   
    chWorkSheet:Range("l3" ):Value = "Descricao da situacao".   
    chWorkSheet:Range("m3" ):Value = "Cod Cargo". 
    chWorkSheet:Range("n3" ):Value = "Nivel Cargo".
    chWorkSheet:Range("o3" ):Value = "Cargo".
    chWorkSheet:Range("p3" ):Value = "Dt Admissao".
    chWorkSheet:Range("q3" ):Value = "Tempo Casa". 
    chWorkSheet:Range("r3" ):Value = "Salario". 
    chWorkSheet:Range("s3" ):Value = "Tb Sal".   
    chWorkSheet:Range("t3" ):Value = "Gr Salarial". 
    chWorkSheet:Range("u3" ):Value = "Step".
    chWorkSheet:Range("v3" ):Value = "Tipo M Obra". 
    chWorkSheet:Range("w3" ):Value = "Turno". 
    chWorkSheet:Range("x3" ):Value = "Turma".
    chWorkSheet:Range("y3" ):Value = "Unid Lotacao".
    chWorkSheet:Range("z3" ):Value = "Descricao Lotacao".
    chWorkSheet:Range("aa3"):Value = "Cod CCusto". 
    chWorkSheet:Range("ab3"):Value = "Descricao CCusto".
    chWorkSheet:Range("ac3"):Value = "Cod Gestor". 
    chWorkSheet:Range("ad3"):Value = "Nome Gestor".

                                                                    
    chWorkSheet:Range("a3:ad3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth  = 08.
    chWorkSheet:Columns("b"):ColumnWidth  = 08.
    chWorkSheet:Columns("C"):ColumnWidth  = 08.
    chWorkSheet:Columns("d"):ColumnWidth  = 40.
    chWorkSheet:Columns("e"):ColumnWidth  = 16.
    chWorkSheet:Columns("f"):ColumnWidth  = 06.
    chWorkSheet:Columns("g"):ColumnWidth  = 12. 
    chWorkSheet:Columns("h"):ColumnWidth  = 18.
    chWorkSheet:Columns("i"):ColumnWidth  = 14.
    chWorkSheet:Columns("j"):ColumnWidth  = 06.
    chWorkSheet:Columns("k"):ColumnWidth  = 10.
    chWorkSheet:Columns("l"):ColumnWidth  = 30.
    chWorkSheet:Columns("m"):ColumnWidth  = 12.
    chWorkSheet:Columns("n"):ColumnWidth  = 12.
    chWorkSheet:Columns("o"):ColumnWidth  = 35.
    chWorkSheet:Columns("p"):ColumnWidth  = 12.
    chWorkSheet:Columns("q"):ColumnWidth  = 12.
    chWorkSheet:Columns("r"):ColumnWidth  = 12.
    chWorkSheet:Columns("s"):ColumnWidth  = 08.
    chWorkSheet:Columns("t"):ColumnWidth  = 12.
    chWorkSheet:Columns("u"):ColumnWidth  = 08.
    chWorkSheet:Columns("v"):ColumnWidth  = 12.
    chWorkSheet:Columns("w"):ColumnWidth  = 06.
    chWorkSheet:Columns("x"):ColumnWidth  = 06.
    chWorkSheet:Columns("y"):ColumnWidth  = 12.
    chWorkSheet:Columns("z"):ColumnWidth  = 40.
    chWorkSheet:Columns("aa"):ColumnWidth = 12.
    chWorkSheet:Columns("ab"):ColumnWidth = 40. 
    chWorkSheet:Columns("ac"):ColumnWidth = 12.
    chWorkSheet:Columns("ad"):ColumnWidth = 40. 

    ASSIGN i-linha = 4.     
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   chExcelApplication:Visible = YES.

   /* release com-handles */
   RELEASE OBJECT chExcelApplication.      
   RELEASE OBJECT chWorkbook.
   RELEASE OBJECT chWorksheet. 
END PROCEDURE.

PROCEDURE pi-imprime-linha:

      FIND rh_pessoa_fisic
         WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-LOCK NO-ERROR.

      FIND unid_lotac
           WHERE unid_lotac.cod_unid_lotac = funcionario.cod_unid_lotac NO-LOCK NO-ERROR.

      FIND cargo_basic WHERE
           cargo_basic.cdn_cargo_basic = funcionario.cdn_cargo_basic NO-LOCK NO-ERROR.

      FIND FIRST cargo NO-LOCK
             WHERE cargo.cdn_cargo_basic =   cargo_basic.cdn_cargo_basic NO-ERROR.

      FIND rh_ccusto WHERE
          rh_ccusto.cdn_empresa   = funcionario.cdn_empresa   AND
          rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.
   
      ASSIGN c-situacao   = "Ativo".
      ASSIGN c-descr-afas = "".

      FOR EACH sit_afast_func 
           WHERE dat_inic_sit_afast            <= today
             AND dat_term_sit_afast            >= today
             and sit_afast_func.cdn_empresa     = funcionario.cdn_empresa
             and sit_afast_func.cdn_estab       = funcionario.cdn_estab
             and sit_afast_func.cdn_funcionario = funcionario.cdn_funcionario  NO-LOCK:
      
        IF sit_afast_func.cdn_sit_afast_func = 90 THEN DO.
           c-situacao = "Ferias".

           FIND FIRST Sit_Afast OF sit_afast_func NO-LOCK NO-ERROR.
           IF AVAIL Sit_Afast THEN DO.
              assign c-descr-afas = Sit_Afast.des_sit_afast_func .
           END.
        END.

        IF sit_afast_func.cdn_sit_afast_func = 25 OR 
           sit_afast_func.cdn_sit_afast_func = 35 OR
           sit_afast_func.cdn_sit_afast_func = 71 OR
           sit_afast_func.cdn_sit_afast_func = 17 OR
           sit_afast_func.cdn_sit_afast_func = 27 OR
           sit_afast_func.cdn_sit_afast_func = 5  OR
           sit_afast_func.cdn_sit_afast_func = 10 OR
           sit_afast_func.cdn_sit_afast_func = 18 OR
           sit_afast_func.cdn_sit_afast_func = 37 OR
           sit_afast_func.cdn_sit_afast_func = 96 THEN DO.

           ASSIGN c-situacao = "Afastado".
          
           FIND FIRST Sit_Afast OF sit_afast_func NO-LOCK NO-ERROR.
           IF AVAIL Sit_Afast THEN DO.
              assign c-descr-afas = Sit_Afast.des_sit_afast_func .
           END.
        END.
      END.
                           

        /* estado civil */
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 1 THEN                                                      
            ASSIGN c-estado-civil =  "CASADO".             
        ELSE                                                                                      
             IF  INT(rh_pessoa_fisic.idi_estado_civil) = 2 THEN                                                 
            ASSIGN c-estado-civil = "SOLTEIRO".           
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 3 THEN                                                     
            ASSIGN c-estado-civil = "DESQUITADO".              
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 4 THEN                                                     
            ASSIGN c-estado-civil = "DIVORCIADO".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 5 THEN                                                     
            ASSIGN c-estado-civil = "VIUVO".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 6 THEN                                                     
            ASSIGN c-estado-civil = "SEPARADO JUD.".  
         ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 7 THEN                                                     
            ASSIGN c-estado-civil = "OUTROS".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 8 THEN                                                     
            ASSIGN c-estado-civil = "UNIÇO ESTAVEL".

        /* sexo */      
        IF  rh_pessoa_fisic.idi_sexo = 1 THEN
            ASSIGN c-sexo = "M".
        ELSE
            ASSIGN c-sexo = "F".
      
        ASSIGN i-idade = INT((((funcionario.dat_nascimento) - TODAY ) * -1) / 365).


        DEFINE VARIABLE i_cdn_gestor LIKE es_gestor.cdn_gestor NO-UNDO.  
        DEFINE VARIABLE c_nom_gestor LIKE es_gestor.nom_gestor NO-UNDO.  

        ASSIGN i_cdn_gestor = 0
               c_nom_gestor = "".

        blk:
        FOR EACH es_histgestor
                 WHERE es_histgestor.cdn_empresa      = funcionario.cdn_empresa     AND
                       es_histgestor.cdn_estab        = funcionario.cdn_estab       AND
                       es_histgestor.cdn_funcionario  = funcionario.cdn_funcionario AND
                       /*es_histgestor.da-inicio       >= v_dat_refer                 AND*/
                       es_histgestor.da-final        >= v_dat_refer
                 NO-LOCK, 
                 FIRST es_gestor OF es_histgestor
                       NO-LOCK:

            ASSIGN i_cdn_gestor = es_gestor.cdn_gestor
                   c_nom_gestor = es_gestor.nom_gestor.
        END.

        ASSIGN chExcelApplication:range( "a"  + STRING(i-linha) ):value = funcionario.cdn_empresa
               chExcelApplication:range( "b"  + STRING(i-linha) ):value = funcionario.cdn_estab 
               chExcelApplication:range( "c"  + STRING(i-linha) ):value = funcionario.cdn_funcionario 
               chExcelApplication:range( "d"  + STRING(i-linha) ):value = funcionario.nom_pessoa_fisic /* funcionario.val_salario_atual*/
               chExcelApplication:range( "e"  + STRING(i-linha) ):value = c-estado-civil
               chExcelApplication:range( "f"  + STRING(i-linha) ):value = c-sexo
               /* chExcelApplication:Range( "f" + STRING(i-linha)):Numberformat = "0000000" */
               chExcelApplication:range( "g"  + STRING(i-linha) ):value = rh_pessoa_fisic.cod_id_feder
               chExcelApplication:Range( "g"  + STRING(i-linha)):Numberformat = "00000000000"
               chExcelApplication:range( "h"  + STRING(i-linha) ):value = funcionario.qti_depend_irf
               chExcelApplication:range( "i"  + STRING(i-linha) ):value = funcionario.dat_nascimento
               chExcelApplication:range( "j"  + STRING(i-linha) ):value = i-idade 
               chExcelApplication:range( "k"  + STRING(i-linha) ):value = c-situacao 

               chExcelApplication:range( "l"  + STRING(i-linha) ):value = c-descr-afas 
               chExcelApplication:range( "m"  + STRING(i-linha) ):value = funcionario.cdn_cargo_basic 
               chExcelApplication:range( "n"  + STRING(i-linha) ):value = funcionario.cdn_niv_cargo  
               chExcelApplication:range( "o"  + STRING(i-linha) ):value = cargo.des_cargo /*cargo_basic.des_cargo_basic*/
               chExcelApplication:range( "p"  + STRING(i-linha) ):value = funcionario.dat_admis_func
               chExcelApplication:range( "q"  + STRING(i-linha) ):value = ((funcionario.dat_admis_func - TODAY) / 365 ) * -1
               chExcelApplication:Range( "q"  + STRING(i-linha)):NumberFormat="#0,0" 
               chExcelApplication:range( "r"  + STRING(i-linha) ):value = funcionario.val_salario_atual
               chExcelApplication:Range( "r"  + STRING(i-linha)):NumberFormat="###.###.##0,00" 
               chExcelApplication:range( "s"  + STRING(i-linha) ):value = funcionario.cdn_tab_sal
               chExcelApplication:range( "t"  + STRING(i-linha) ):value = funcionario.cdn_regiao_sal /*grupo sal*/ 
               chExcelApplication:range( "u"  + STRING(i-linha) ):value = funcionario.num_faixa_sal
               chExcelApplication:range( "v"  + STRING(i-linha) ):value = funcionario.cod_tip_mdo
               chExcelApplication:range( "w"  + STRING(i-linha) ):value = funcionario.cdn_turno_trab
               chExcelApplication:Range( "w"  + STRING(i-linha)):Numberformat = "0000"
               chExcelApplication:range( "x"  + STRING(i-linha) ):value = funcionario.cdn_turma_trab
               chExcelApplication:range( "y"  + STRING(i-linha) ):value = funcionario.cod_unid_lotac
               chExcelApplication:range( "z"  + STRING(i-linha) ):value = unid_lotac.des_unid_lotac 
               chExcelApplication:range( "aa" + STRING(i-linha) ):value = funcionario.cod_rh_ccusto
               chExcelApplication:range( "ab" + STRING(i-linha) ):value = rh_ccusto.des_rh_ccusto
               chExcelApplication:range( "ac" + STRING(i-linha) ):value = i_cdn_gestor
               chExcelApplication:range( "ad" + STRING(i-linha) ):value = c_nom_gestor
            .
               
        ASSIGN i-linha = i-linha + 1.
END PROCEDURE.
