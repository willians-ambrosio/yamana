define temp-table tt-param no-undo
    field destino             as integer
    field arquivo             as char format "x(35)"
    field usuario             as char format "x(12)"
    field data-exec           as date
    field hora-exec           as integer
    field classifica          as integer
    field parametro           as log
    field desc-classifica     as char format "x(40)"
    field v_cdn_empres_usuar  as char
    field cdn_estab_central   as char
    field cdn_estab_ini      like rh_estab.cdn_estab
    field cdn_estab_fim      like rh_estab.cdn_estab
    field cdn_banco_ini      like rh_bco.cdn_banco
    field cdn_banco_fim      like rh_bco.cdn_banco
    field dat_pagto_ini       as date
    field dat_pagto_fim       as date
    field dat_outros_pagtos   as date
    field c-arquivo-pensao    as char
    field log_normal          as log
    field log_adiant_normal   as log
    field log_13              as log
    field log_adiant_13       as log
    field log_ferias          as log
    field log_pensao          as log
    field log_rescisao        as log
    field num_mes_refer       as int
    field num_ano_refer       as int
    field cdn_agenc_emp       as int
    field num_dig_agenc       as char format "xx"
    field num_conta_emp       as int
    field num_dig_conta       as char format "x"
    field cdn_banco_emp       as int
    field dat_lancto          as date
    field cod_convenio        as char
    field cod_docto           as char format "x(20)"
    field idi_layout          as int
    field log_credito_conta   as log
    field log_cartao_salario  as log
    field cod_dac             as char format "x(01)".
    

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.
