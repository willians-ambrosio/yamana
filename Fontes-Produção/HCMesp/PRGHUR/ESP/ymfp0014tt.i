/******************************************************************************
** Programa       : ymfp0014tt.i
** Altera‡Æo      : 1789506  - 05/01/18 - FSW TOTVS Joinville HCM
** Objetivo       : Importa‡Æo Planilha PLR
*****************************************************************************/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field v_cdn_empres_usuar as char
    field imprime_parametros as logical

    field tt_ini_cdn_estab like funcionario.cdn_estab
    field tt_fim_cdn_estab like funcionario.cdn_estab
    field tt_ini_cdn_funcionario like funcionario.cdn_funcionario
    field tt_fim_cdn_funcionario like funcionario.cdn_funcionario

    field tt_cdn_param_calc_ppr like param_calc_ppr.cdn_param_calc_ppr
    field tt_num_ano_refer_calc_efetd like movto_ppr.num_ano_refer_calc_efetd
    field tt_num_mes_refer_calc_efetd like movto_ppr.num_mes_refer_calc_efetd

    field tt_ind_tip_importa as int
    field tt_des_ind_importa as char format "x(13)" 

    field tt_arquivo_importa as char format "x(100)"

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

&IF defined(YMFP0014RP) &THEN
def temp-table ttfaixa_param_calc_ppr_func no-undo like faixa_param_calc_ppr_func.
def temp-table ttmovto_ppr no-undo like movto_ppr.
def temp-table tttip_func_indiv_factor_prov no-undo like tip_func_indiv_factor_prov.
def temp-table tttip_func_indiv_factor_pag no-undo like tip_func_indiv_factor_pag.

def temp-table tt-tip-cash-awards no-undo
    field log_corporativo like tip_indiv_factor.log_corporativo
    field cod_id_feder like funcionario.cod_id_feder
    field dat_inicio as date
    field dat_termino as date
    field val_salario like funcionario.val_salario_atual
    field desempenho as char
    field fator_individual as dec
    field margem like tip_est_contrib_margin_prov.perc_margem
    field resultado like tip_est_targets_prov.result
    field fator_margem like tip_contrib_margin.margem
    field result_sal like tip_targets.result
    field proporcao as int
    field valor_tip as dec
    FIELD valor_tip_total AS DEC
    field seq as int
    index seq as primary seq.

def temp-table tt-erros no-undo
    field chave as char
    field mensagem as char
    field seq as int
    index erro as primary chave.

&ENDIF
