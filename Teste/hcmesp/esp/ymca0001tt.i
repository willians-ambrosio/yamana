define temp-table tt-param
    field destino           as integer
    field arquivo           as char
    field arq-destino       as char
    field arq-entrada       as char
    field todos             as integer
    field usuario           as char
    field data-exec         as date
    field hora-exec         as integer
    field cdn_empresa_ini   like funcionario.cdn_empresa
    field cdn_empresa_fim   like funcionario.cdn_empresa
    field cdn_estab_ini     like funcionario.cdn_estab
    field cdn_estab_fim     like funcionario.cdn_estab
    field cod_rh_ccusto_ini like funcionario.cod_rh_ccusto
    field cod_rh_ccusto_fim like funcionario.cod_rh_ccusto
    field dat_liber_sal     like histor_sal_func.dat_liber_sal.
