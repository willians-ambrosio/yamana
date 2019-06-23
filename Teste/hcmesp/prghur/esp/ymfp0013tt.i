/******************************************************************************
** Programa       : ymfp0013tt.i
** Altera‡Æo      : 1789506  - 05/01/18 - FSW TOTVS Joinville HCM
** Objetivo       : Exporta‡Æo Planilha PLR
*****************************************************************************/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field v_cdn_empres_usuar as char
    field imprime_parametros as logical
    FIELD tp_func            AS INT
    field tt_ini_cdn_estab like funcionario.cdn_estab
    field tt_fim_cdn_estab like funcionario.cdn_estab
    field tt_ini_cdn_funcionario like funcionario.cdn_funcionario
    field tt_fim_cdn_funcionario like funcionario.cdn_funcionario

    field tt_cdn_param_calc_ppr like param_calc_ppr.cdn_param_calc_ppr
    field tt_num_ano_refer_calc_efetd like movto_ppr.num_ano_refer_calc_efetd
    field tt_num_mes_refer_calc_efetd like movto_ppr.num_mes_refer_calc_efetd

    field tt_ind_tip_exporta as int
    field tt_des_ind_exporta as char format "x(13)" 

    field tt_individual_factor as logical format "Sim/NÆo"

    field tt_arquivo_exporta as char format "x(100)"
    FIELD v_cod_id_feder_ini LIKE funcionario.cod_id_feder
    FIELD v_cod_id_feder_fim LIKE funcionario.cod_id_feder
    FIELD v_cdn_individual_factor LIKE tiph_func_indiv_factor_pag.cdn_indiv_factor

    .

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */
def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Functions */
function fn_nome_mes returns char (input p_mes as int) :

    &scoped-define NOMES-MES Janeiro,Fevereiro,Mar‡o,Abril,Maio,Junho,Julho,Agosto,Setembro,Outubro,Novembro,Dezembro
    
    return entry(p_mes, "{&NOMES-MES}").


end function.

&IF defined(YMFP0013RP) > 0 &THEN
def temp-table tt-tip-cash-awards no-undo
    field des_unid_lotac like unid_lotac.des_unid_lotac
    field cod_rh_ccusto like rh_ccusto.cod_rh_ccusto 
    field des_rh_ccusto like rh_ccusto.des_rh_ccusto
    field cdn_estab like rh_estab.cdn_estab
    field log_corporativo like tip_indiv_factor.log_corporativo
    field cod_id_feder like funcionario.cod_id_feder
    field nom_pessoa_fisic like funcionario.nom_pessoa_fisic
    field des_cargo_basic like cargo_basic.des_cargo_basic
    field dat_inic_lotac_func like func_unid_lotac_plano.dat_inic_lotac_func
    field dat_fim_lotac_func like func_unid_lotac_plano.dat_fim_lotac_func
    field des_nivel_lotacao as char
    field val_salario like funcionario.val_salario_atual
    field margem like tip_est_contrib_margin_prov.perc_margem
    field resultado like tip_est_targets_prov.result
    field fator_margem like tip_contrib_margin.margem
    field result_sal like tip_targets.result
    field proporcao as int

    field cdn_tip_grupo_cargo like ext_niv_hier_funcnal.cdn_tip_grupo_cargo
    field cdn_empresa like funcionario.cdn_empresa
    field seq as int
    field principal as logical init true
    field row_table as rowid
    field num_pessoa_fisic like funcionario.num_pessoa_fisic
    field cdn_funcionario like funcionario.cdn_funcionario
    index seq as primary seq.

def temp-table tt-indiv-factor-lists no-undo
    field cdn_empresa like funcionario.cdn_empresa
    field cdn_estab like rh_estab.cdn_estab
    field log_corporativo like tip_indiv_factor.log_corporativo
    field inicial as char
    field num_linha_ini as int
    field num_linha_fim as int.

def temp-table tt-ocorrencias no-undo
    field cdn_empresa like funcionario.cdn_empresa
    field cdn_estab like funcionario.cdn_estab
    field cdn_funcionario like funcionario.cdn_funcionario
    field dat_inic_lotac_func like func_unid_lotac_plano.dat_inic_lotac_func
    field dat_fim_lotac_func like func_unid_lotac_plano.dat_fim_lotac_func
    field observacoes as char
    field tipo as int 
    field mes_processo as int
    index ocor as primary dat_inic_lotac_func tipo.

def temp-table tt-entradas no-undo
    field cdn_empresa like funcionario.cdn_empresa
    field cdn_estab like funcionario.cdn_estab
    field cdn_funcionario like funcionario.cdn_funcionario
    field dat_inic as date 
    field cdn_empres_orig like sit_afast_func.cdn_empres_orig
    field cdn_estab_orig like sit_afast_func.cdn_estab_orig 
    field cdn_func_orig like sit_afast_func.cdn_func_orig  
    field mes as int
    field ano as int
    field done as logical
    index data as primary dat_inic.

def temp-table tt-afastamentos no-undo
    field cdn_empresa like funcionario.cdn_empresa
    field cdn_estab like funcionario.cdn_estab
    field cdn_funcionario like funcionario.cdn_funcionario
    field mes as int
    field num_dias_afastado as int.

def temp-table tt-promocoes no-undo
    field cdn_empresa like funcionario.cdn_empresa
    field cdn_estab like funcionario.cdn_estab
    field cdn_funcionario like funcionario.cdn_funcionario
    field cdn_cargo_basic like cargo_basic.cdn_cargo_basic
    field dat_inic as date 
    field mes as int
    field ano as int
    FIELD tipo AS INT.

def buffer btt-tip-cash-awards for tt-tip-cash-awards.

def temp-table tt-funcionarios no-undo
    field cdn_empresa like funcionario.cdn_empresa
    field cdn_estab like funcionario.cdn_estab
    field cdn_funcionario like funcionario.cdn_funcionario.

def temp-table tt-situacoes no-undo
    field cdn_sit_afast_func like sit_afast.cdn_sit_afast_func
    field influi_plr as logical.

def temp-table tt-observacoes no-undo
    field mes_processo as int
    field observacoes as char
    field seq as int
    field row_table as rowid
    field mostra as logical init true
    index seq as primary seq.
&ENDIF
