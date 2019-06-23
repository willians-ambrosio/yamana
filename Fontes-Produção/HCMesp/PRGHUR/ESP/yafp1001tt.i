define temp-table tt-param no-undo
    field destino               as integer
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    field classifica            as integer
    field modelo-rtf            as char format "x(35)"
    field l-habilitaRtf         as log
    field arquivo-gerado        as char format "x(256)"
    field v_cdn_empres_usuar    like funcionario.cdn_empresa                 
    field cdn_estab_ini         like funcionario.cdn_estab
    field cdn_estab_fim         like funcionario.cdn_estab
    field cod_rh_ccusto_ini     like funcionario.cod_rh_ccusto
    field cod_rh_ccusto_fim     like funcionario.cod_rh_ccusto
    field cod_unid_lotac_ini    like funcionario.cod_unid_lotac
    field cod_unid_lotac_fim    like funcionario.cod_unid_lotac
    field v_mes_refer           as int
    field v_ano_refer           as int.


define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.
