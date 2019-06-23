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
    
    FIELD cdn_disp_ini     AS INTEGER
    FIELD cdn_disp_fim     AS INTEGER
    FIELD dat_ref_ini      AS DATE
    FIELD dat_ref_fim      AS DATE.

define temp-table tt-digita no-undo
    field cdn_tip_refeicao as integer   format ">>>>9"
    field des_tip_refeicao as character format "x(30)"
    index id cdn_tip_refeicao.
