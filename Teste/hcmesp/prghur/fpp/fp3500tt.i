/*** Importante toda vez que for alterado este include deve ser tamb‚m
     buscado via roundtable todos os programas fp3500 e fp3501 *******/


{include/i_dbvers.i}
{prghur/fpp/fp9240.i8}
define {1} {2} temp-table tt-param
    {prghur/fpp/fp9240.i8}
    field v_cdn_empres_usuar    like param_empres_rh.cdn_empresa
    field v_cdn_empresa_evento  like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini  like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim  like unid_lotac.cod_unid_lotac
    field i-es-ini              like rh_estab.cdn_estab
    field i-es-fim              like rh_estab.cdn_estab
    field i-fc-ini              like funcionario.cdn_funcionario
    field i-fc-fim              like funcionario.cdn_funcionario
    field i-contr-ini           like funcionario.cdn_tip_contrat_func
    field i-contr-fim           like funcionario.cdn_tip_contrat_func
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
    field arquivo                 as CHAR FORMAT "x(60)"
    field usuario                 as char
    field data-exec               as date format "99/99/9999"
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    field i-cc-codigo-1         like funcionario.cod_rh_ccusto
    field i-cc-codigo-2         like funcionario.cod_rh_ccusto
    field i-bc-codigo-1         like rh_bco.cdn_banco
    field i-bc-codigo-2         like rh_bco.cdn_banco
    field i-ag-codigo-1         like rh_agenc_bcia.cdn_agenc_bcia
    field i-ag-codigo-2         like rh_agenc_bcia.cdn_agenc_bcia
    field v_nom_func_ini        like funcionario.nom_pessoa_fisic
    field v_nom_func_fim        like funcionario.nom_pessoa_fisic
    field cdn_local_pagto_ini   like funcionario.cdn_local_pagto
    field cdn_local_pagto_fim   like funcionario.cdn_local_pagto
    FIELD v_niv_unid_lotac        AS INTEGER FORMAT "z9"
    field l-emite-demi            as logical
    field i-ano-ref               as integer format "9999"
    field i-mes-ref               as integer format "99"
    field i-tipo-folha            as integer format "9"
    field i-parcela               as integer format "9"
    field i-tipo-formula          as integer format "9"
    field r-forma-pgto          like funcionario.idi_forma_pagto
    field l-mensal                as logical
    field l-horista               as logical
    field l-semanal               as logical
    field l-quinzenal             as logical
    field l-tarefa                as logical 
    field l-diarista              as logical
    field v_log_origem            as logical
/* v_log_origem = yes -> Coletiva
                = no  -> Individual */
    field l-emite-afast           as logical
    field l-emite-ferias          as logical
    field v_mes_ini               as int 
    field v_ano_ini               as int 
    field v_mes_fim               as int 
    field v_ano_fim               as int 
    field v_log_enviar_email      AS LOGICAL
    field l-parametro             AS LOGICAL
    FIELD l-imp-carga-horar-sem   AS LOGICAL
    field des-layout              as character
    field arquivo-pdf             as char format "x(40)"
    field arquivo-imagem          AS CHAR FORMAT "x(200)"
    field l-layout-detalhado      as LOG
    FIELD tipo-execucao           AS INTEGER
    FIELD l-email-pdf             AS LOGICAL
    FIELD texto-email             AS CHARACTER.
 

define {1} {2} temp-table tt-digita
    field v_cdn_empres_usuar like param_empres_rh.cdn_empresa
    field v_cdn_estab        like rh_estab.cdn_estab
    field v_cdn_funcionario  like funcionario.cdn_funcionario
    field v_cdn_func_centr   like funcionario.cdn_func_centrdor
    field i-contrato         like funcionario.cdn_tip_contrat_func
    field v_nom_pessoa       like funcionario.nom_pessoa_fisic
    field cdn_plano_lotac    like plano_lotac.cdn_plano_lotac
    field v_cod_unid_lotac   like unid_lotac.cod_unid_lotac
    field forma-pgto         like funcionario.idi_forma_pagto
    field cs-codigo          like funcionario.cdn_categ_sal
    field cc-codigo          like funcionario.cod_rh_ccusto
    field bco-liquido        like funcionario.cdn_bco_liq
    field age-liquido        like funcionario.cdn_agenc_bcia_liq
    field num_forma_pagto    like funcionario.idi_forma_pagto
    index id is primary /*unique*/
        v_cdn_empres_usuar
        v_cdn_estab
        v_cdn_funcionario.
