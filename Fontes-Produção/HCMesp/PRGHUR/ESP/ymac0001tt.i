define {1} {2} temp-table tt-param no-undo
    field destino                 as integer
    field arquivo                 as char
    field usuario                 as char
    field data-exec               as date format "99/99/9999"
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    field v_cdn_empres_usuar    like param_empres_rh.cdn_empresa
    field v_num_tip_aces_usuar    as integer format "9" 
    field v_cod_grp_usuar         as char
    field i-es-ini              like rh_estab.cdn_estab
    field i-es-fim              like rh_estab.cdn_estab
    field i-bco-liq-ini         like funcionario.cdn_bco_liq
    field i-bco-liq-fim         like funcionario.cdn_bco_liq  
    field i-pe-ini              like bnfciar_palim.cdn_bnfciar_palim
    field i-pe-fim              like bnfciar_palim.cdn_bnfciar_palim
    field v_cdn_funcionario_ini like funcionario.cdn_funcionario
    field v_cdn_funcionario_fim like funcionario.cdn_funcionario
    field d-dt-ini                as date format "99/99/9999"
    field d-dt-fim                as date format "99/99/9999"
    field d-dt-sal                as date format "99/99/9999"
    field v_log_envia_reg_b       as logical
    field v_log_adto_normal       as logical
    field v_log_ferias            as logical
    field v_log_adto_13_sal       as logical
    field v_log_rescisao          as logical
    field v_log_13_sal            as logical
    field v_log_normal            as logical
    field v_log_plr               as logical
    field c-arq-lqd               as char format "x(40)"
    field cdn_banco             like rh_bco.cdn_banco
    field cdn_agencia           like rh_agenc_bcia.cdn_agenc_bcia
    field cod_dv_agencia          as char    format "x"
    field num_conta               as decimal format "999999999999"
    field cod_dv_conta            as char    format "x"
    field cod_docto               as char    format "x(20)"
    field dat_lancto              as date    format "99/99/9999"
    field cdn_estab_centr       like rh_estab.cdn_estab
    field num_convenio            as char    format "x(20)"
    field log_cred_conta          as logical  
    field log_doc                 as logical
    field log_cheque              as logical
    field log_cartao              as logical
    field log_conta_poup          as logical
    field num_cpfpis              as int     format "9"
    field tb-parametro            as logical
    field cdn_funcionario_ini     like funcionario.cdn_funcionario
    field cdn_funcionario_fim     like funcionario.cdn_funcionario
    field i-ind-selec             as int.

define {1} {2} temp-table tt-digita no-undo
    field v_cdn_empres_usuar      	  like param_empres_rh.cdn_empresa
    field es-codigo           like funcionario.cdn_estab
    field c-nome-es           as char  format "x(30)" 
    field fc-codigo           like funcionario.cdn_funcionario
    field fc-centr            like funcionario.cdn_func_centrdor
    field c-nome-fc           as char  format "x(30)"  
    field cs-codigo           like funcionario.cdn_categ_sal
    field orig-contr          as int
    field cdn-prest           as int.

