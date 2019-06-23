define {1} {2} temp-table tt-param
    field parametro               as logical /* Imprime parametros */
    field v_cdn_empres_usuar      like param_empres_rh.cdn_empresa
    field v_cdn_empresa_evento    like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini    like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim    like unid_lotac.cod_unid_lotac
    field i-es-ini                like rh_estab.cdn_estab
    field i-es-fim                like rh_estab.cdn_estab
    field i-fc-ini                like funcionario.cdn_funcionario
    field i-fc-fim                like funcionario.cdn_funcionario
    field i-turno-ini             like func_ptoelet.cdn_turno_trab
    field i-turno-fim             like func_ptoelet.cdn_turno_trab    
    field v_num_tip_aces_usuar    as integer format "9" 
    field v_cod_grp_usuar         as char
    field v_num_opcao             as int  format "9"
    field v_des_opcao             as char format "x(10)"
    field v_dat_valid             as date format "99/99/9999"
    field v_log_expande_estrut    as log
    field v_num_salta_pg          as integer
    field v_num_quebra            as integer
    field v_num_faixa             as integer
    field destino                 as integer
    field arquivo                 as char
    field usuario                 as char
    field data-exec               as date format "99/99/9999"
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    field ano-ref                 as integer format "99"
    field mes-ref                 as integer format "99"
    field i-ind-selec             as integer
    field v_log_mensal            as logical
    field v_log_horista           as logical
    field v_log_quinzenal         as logical
    field v_log_semanal           as logical
    field v_log_tarefa            as logical
    field v_log_diarista          as logical
    field log_fecha               as logical
    field v_log_ms                as logical
    field v_ind_integr            as integer
    field v_dat_ini               as date
    field v_dat_fim               as date
    field v_cdn_prestdor_ini      like prestdor_serv.cdn_prestdor_serv
    field v_cdn_prestdor_fim      like prestdor_serv.cdn_prestdor_serv
    field v_orig_func             as logical label "Funcionario" 
    field v_orig_temp             as logical label "Tempor rio"
    field v_orig_contratado       as logical label "Contratado"
    field v_orig_cooperado        as logical label "Cooperado"
    field v_orig_socio            as logical label "S¢cio"
    field v_orig_estag            as logical label "Estagi rio"
    field v_orig_terc             as logical label "Terceiro Ponto"
    field i-ctr-ini               as int format "99"
    field i-ctr-fim               as int format "99"
    field c-ccusto-ini            like func_ptoelet.cod_rh_ccusto
    field c-ccusto-fim            like func_ptoelet.cod_rh_ccusto
    field i-classe-ini            like func_ptoelet.cdn_clas_func
    field i-classe-fim            like func_ptoelet.cdn_clas_func.

define {1} {2} temp-table tt-digita
    field i-cdn_clas_func   as integer   
    field v_cdn_empres_usuar  like param_empres_rh.cdn_empresa
    field i-es-codigo  like rh_estab.cdn_estab
    field i-fc-codigo      LIKE funcionario.cdn_funcionario
    field i-dv-matric      as integer   format "9"
    field c-nome           as char      format "x(40)"
    field i-cs-codigo      as integer   format "99"
    field i-cdn-turno      as integer   format "9999"
    field i-cdn_turma_trab as integer   format ">9"
    field dat_ref_per      as date format "99/99/9999" initial ?
    field da-dat-ini       as date format "99/99/9999"
    field c-pais           like localidade.cod_pais
    field i-localid        like localidade.cdn_localidade
    index id      is primary 
          v_cdn_empres_usuar
          i-es-codigo 
          i-fc-codigo
    index calculo           
          i-cdn_clas_func 
          v_cdn_empres_usuar
          i-es-codigo 
          i-cs-codigo
    index turno 
       i-cdn-turno i-cdn_turma_trab c-pais i-localid.
