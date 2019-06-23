
{include/i_dbvers.i}
{prghur/fpp/fp9240.i8}
define {1} {2} temp-table tt-param
    {prghur/fpp/fp9240.i8}
    field parametro                 as logical /* Imprime parametros */
    field v_cdn_empres_usuar        like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini      like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim      like unid_lotac.cod_unid_lotac
    field i-es-ini                  like rh_estab.cdn_estab
    field i-es-fim                  like rh_estab.cdn_estab
    field i-fc-ini                  like funcionario.cdn_funcionario
    field i-fc-fim                  like funcionario.cdn_funcionario
    field v_num_tip_aces_usuar      as integer format "9" 
    field v_cod_grp_usuar           as char
    field v_num_opcao               as int  format "9"
    field v_des_opcao               as char format "x(10)"
    field v_dat_valid               as date format "99/99/9999"
    field v_log_expande_estrut      as log
    field v_num_salta_pg            as integer
    field v_num_quebra              as integer
    field v_num_faixa               as integer
    field destino                   as integer
    field arquivo                   as char
    field modelo-rtf                as char
    field l-habilitaRtf             as log
    field usuario                   as char
    field data-exec                 as date format "99/99/9999"
    field hora-exec                 as integer
    field classifica                as integer
    field desc-classifica           as char format "x(40)"
    field c-cc-ini                  LIKE funcionario.cod_rh_ccusto
    field c-cc-fim                  LIKE funcionario.cod_rh_ccusto
    field c-nm-ini                  as char 
    field c-nm-fim                  as char 
    field d-dt-ini                  as date format "99/99/9999"
    field d-dt-fim                  as date format "99/99/9999"
    field i-mt-ini                  as integer
    field i-mt-fim                  as integer
    field cdn_cargo_ini             like funcionario.cdn_cargo_basic
    field cdn_cargo_fim             like funcionario.cdn_cargo_basic
    field cdn_func_centrdor_ini     like funcionario.cdn_func_centrdor
    field cdn_func_centrdor_fim     like funcionario.cdn_func_centrdor
    field cdn_tip_contrat_ini       like funcionario.cdn_tip_contrat_func
    field cdn_tip_contrat_fim       like funcionario.cdn_tip_contrat_func
    field idi_tip_cla_motiv         as int
    field idi_imp_ult_mov_tip       as int
    field v_log_imp_ult_movto       as log
    field l-imp-desl                as logical
    FIELD num_casas_dec             AS INTEGER
    field idi_tip_cargo_funcao      like cargo.idi_tip_cargo_funcao
    field des_tip_cargo_funcao      as char format "x(10)"
    field cdn_turno_ini             as int
    field cdn_turno_fim             as int.

define {1} {2} temp-table tt-digita
    field v_cdn_empres_usuar      	like param_empres_rh.cdn_empresa
    field v_cdn_estab       	    like rh_estab.cdn_estab
    field v_cdn_funcionario 	    like funcionario.cdn_funcionario
    field v_nom_pessoa      	    like funcionario.nom_pessoa_fisic
    field cdn_plano_lotac   	    like plano_lotac.cdn_plano_lotac
    field v_cod_unid_lotac  	    like unid_lotac.cod_unid_lotac
    field cdn_motiv_liber_sal       like motiv_liber_sal.cdn_motiv_liber_sal
    field des_motiv_liber_sal       like motiv_liber_sal.des_motiv_liber_sal
    index id is primary unique
          cdn_motiv_liber_sal.

define temp-table tt-motivo-func
    FIELD cdn_empresa                   LIKE histor_sal_func.cdn_empresa
    FIELD cdn_estab                     LIKE histor_sal_func.cdn_estab
    field cdn_motivo                    like histor_sal_func.cdn_motiv_liber_sal
    field qtde-func                     as INT.

