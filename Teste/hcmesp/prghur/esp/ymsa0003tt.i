define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    
    FIELD cdn_emp_ini      AS CHARACTER
    FIELD cdn_emp_fim      AS CHARACTER
    FIELD cdn_est_ini      AS CHARACTER
    FIELD cdn_est_fim      AS CHARACTER
    FIELD cdn_fun_ini      AS INTEGER
    FIELD cdn_fun_fim      AS INTEGER
    FIELD dat_bat_ini      AS DATE
    FIELD dat_bat_fim      AS DATE.

define temp-table tt-digita no-undo
    FIELD cdn_planta       AS INTEGER   format "zzzz9"
    field cdn_local        as integer   format "zzzzz9"
    field des_local        as character format "x(30)"
    index id cdn_planta cdn_local.
