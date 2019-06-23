define {1} {2} temp-table tt-param
    field parametro               as logical /* Imprime parametros */
    field v_cdn_empres_usuar      like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini    like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim    like unid_lotac.cod_unid_lotac
    field i-es-ini                like rh_estab.cdn_estab
    field i-es-fim                like rh_estab.cdn_estab
    field i-fc-ini                like funcionario.cdn_funcionario
    field i-fc-fim                like funcionario.cdn_funcionario
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
    field modelo-rtf              as char
    field l-habilitaRtf           as log
    field usuario                 as char
    field data-exec               as date format "99/99/9999"
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    field d-dt-inicio             as date format "99/99/9999"
    field d-dt-final              as date format "99/99/9999".
    
        
define {1} {2} temp-table tt-digita
    field v_cdn_empres_usuar      	  like param_empres_rh.cdn_empresa
    field v_cdn_estab       	  like rh_estab.cdn_estab
    field v_cdn_funcionario 	  like funcionario.cdn_funcionario
    field v_nom_pessoa      	  like funcionario.nom_pessoa_fisic
    field cdn_plano_lotac   	  like plano_lotac.cdn_plano_lotac
    field v_cod_unid_lotac  	  like unid_lotac.cod_unid_lotac
    index id is primary unique
          v_cdn_empres_usuar
          v_cdn_estab
          v_cdn_funcionario.
