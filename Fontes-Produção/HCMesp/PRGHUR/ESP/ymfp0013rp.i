/******************************************************************************
** Programa       : ymfp0013rp.i
** Altera‡Æo      : 1789506  - 05/01/18 - FSW TOTVS Joinville HCM
** Objetivo       : Exporta‡Æo Planilha PLR
*****************************************************************************/

for last {&tip_est_contrib_margin} no-lock 
    where {&tip_est_contrib_margin}.cdn_estab       = tt-tip-cash-awards.cdn_estab
      and {&tip_est_contrib_margin}.cdn_empresa     = tt-tip-cash-awards.cdn_empresa
      and {&tip_est_contrib_margin}.log_corporativo = tt-tip-cash-awards.log_corporativo,
    first tip_contrib_margin no-lock
    where tip_contrib_margin.cdn_empresa        = tt-tip-cash-awards.cdn_empresa
      and tip_contrib_margin.cdn_estab          = tt-tip-cash-awards.cdn_estab
      and tip_contrib_margin.log_corporativo    = tt-tip-cash-awards.log_corporativo
      and tip_contrib_margin.dt_inicio          < v_data_termino
      and tip_contrib_margin.dt_termino         > v_data_inicio
      and {&tip_est_contrib_margin}.perc_margem >= tip_contrib_margin.perc_ini
      and {&tip_est_contrib_margin}.perc_margem <= tip_contrib_margin.perc_fim :

    for each btt-tip-cash-awards
        where btt-tip-cash-awards.cdn_empresa     = tt-tip-cash-awards.cdn_empresa    
          and btt-tip-cash-awards.cdn_estab       = tt-tip-cash-awards.cdn_estab      
          and btt-tip-cash-awards.log_corporativo = tt-tip-cash-awards.log_corporativo :

        assign btt-tip-cash-awards.fator_margem = tip_contrib_margin.margem 
               btt-tip-cash-awards.margem       = {&tip_est_contrib_margin}.perc_margem / 100.

    end.


end.

for last {&tip_est_targets} no-lock 
    where {&tip_est_targets}.cdn_estab       = tt-tip-cash-awards.cdn_estab
      and {&tip_est_targets}.cdn_empresa     = tt-tip-cash-awards.cdn_empresa
      and {&tip_est_targets}.log_corporativo = tt-tip-cash-awards.log_corporativo,
    first tip_targets no-lock
    where tip_targets.cdn_empresa        = tt-tip-cash-awards.cdn_empresa
      and tip_targets.cdn_estab          = tt-tip-cash-awards.cdn_estab
      and tip_targets.log_corporativo    = tt-tip-cash-awards.log_corporativo
      and tip_targets.dt_inicio          < v_data_termino
      and tip_targets.dt_termino         > v_data_inicio
      and tip_targets.result             = {&tip_est_targets}.result :

    for each btt-tip-cash-awards
        where btt-tip-cash-awards.cdn_empresa     = tt-tip-cash-awards.cdn_empresa    
          and btt-tip-cash-awards.cdn_estab       = tt-tip-cash-awards.cdn_estab      
          and btt-tip-cash-awards.log_corporativo = tt-tip-cash-awards.log_corporativo :

        case btt-tip-cash-awards.cdn_tip_grupo_cargo :
            when 1 then assign btt-tip-cash-awards.resultado  = tip_targets.result / 100
                               btt-tip-cash-awards.result_sal = tip_targets.ind_diretor.
            when 2 then assign btt-tip-cash-awards.resultado  = tip_targets.result / 100
                               btt-tip-cash-awards.result_sal = tip_targets.ind_gerente.
            when 3 then assign btt-tip-cash-awards.resultado  = tip_targets.result / 100
                               btt-tip-cash-awards.result_sal = tip_targets.ind_coordenador.
            when 4 then assign btt-tip-cash-awards.resultado  = tip_targets.result / 100
                               btt-tip-cash-awards.result_sal = tip_targets.ind_superv_sr.
            when 5 then assign btt-tip-cash-awards.resultado  = tip_targets.result / 100
                               btt-tip-cash-awards.result_sal = tip_targets.ind_demais.
        end case.


    end.


end.
