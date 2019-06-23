define {1} {2} temp-table tt-param
    field v_cdn_empres_usuar    like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini  like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim  like unid_lotac.cod_unid_lotac
    field i-es-ini              like rh_estab.cdn_estab
    field i-es-fim              like rh_estab.cdn_estab
    field i-fc-ini              like funcionario.cdn_funcionario
    field i-fc-fim              like funcionario.cdn_funcionario
    field v_num_tip_aces_usuar  as integer format "9" 
    field v_cod_grp_usuar       as char
    field v_num_opcao           as int  format "9"
    field v_des_opcao           as char format "x(10)"
    field v_dat_valid           as date format "99/99/9999"
    field v_log_expande_estrut  as log
    field v_num_salta_pg        as integer
    field v_num_quebra          as integer
    field v_num_faixa           as integer
    field destino               as integer
    field arquivo               as char
    field usuario               as char
    field data-exec             as date format "99/99/9999"
    field hora-exec             as integer
    field classifica            as integer
    field d-dt-ini              as date format "99/99/9999"
    field d-dt-fim              as date format "99/99/9999"
    field i-tp-res              as integer 
    field desc-classifica       as char format "x(40)"
    field c-arq-lqd             as char format "x(40)"
    field i-bco-liq-ini         like funcionario.cdn_bco_liq
    field i-bco-liq-fim         like funcionario.cdn_bco_liq  
    field i-mes-ref             like param_empres_rh.num_mes_refer_calc_efetd 
    field i-ano-ref             like param_empres_rh.num_ano_refer_calc_efetd format "9999"
    field i-tp-fl               like movto_calcul_func.idi_tip_fp
    field i-parc                like movto_calcul_func.qti_parc_habilit_calc_fp
    field l-mensal              as logical
    field l-horista             as logical
    field l-semanal             as logical
    field l-quinzenal           as logical
    field l-tarefa              as logical 
    field l-diarista            as logical
    field cdn_estab_centr       like rh_estab.cdn_estab
    field cdn_banco             like rh_bco.cdn_banco
    field cdn_agencia           like rh_agenc_bcia.cdn_agenc_bcia
    field cod_dv_agencia        as char    format "x"
    field num_conta             as decimal format "999999999999"
    field cod_dv_conta          as char    format "x"
    field num_convenio          as int     format "99999"
    field cod_docto             as char    format "x(20)"
    field dat_lancto            as date    format "99/99/9999"
    field log_cred_conta        as logical  
    field log_doc               as logical
    field log_cheque            as logical
    field log_cartao            as logical
    field log_conta_poup        as logical
    field parametro             as logical /* Imprime parametros */
    FIELD log_gera_seg_b        AS LOGICAL
    field num_seq               as int format "999999".

define {1} {2} temp-table tt-digita
    field v_cdn_empres_usuar like param_empres_rh.cdn_empresa
    field v_cdn_estab        like rh_estab.cdn_estab
    field v_cdn_funcionario  like funcionario.cdn_funcionario
    field v_nom_pessoa       like funcionario.nom_pessoa_fisic
    field cdn_plano_lotac    like plano_lotac.cdn_plano_lotac
    field v_cod_unid_lotac   like unid_lotac.cod_unid_lotac
    index id is primary unique
          v_cdn_empres_usuar
          v_cdn_estab
          v_cdn_funcionario.

