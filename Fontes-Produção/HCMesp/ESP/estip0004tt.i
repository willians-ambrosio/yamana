define temp-table tt-param          no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    field classifica                as integer
    FIELD l-simula                  AS LOGICAL
    field desc-classifica           as char format "x(40)"
    field cdn_empresa               like funcionario.cdn_empresa
    field cdn_estab                 like funcionario.cdn_estab
    FIELD cdn_param_calc_ppr        LIKE PARAM_calc_ppr.cdn_param_calc_ppr
    field num_anotip                as integer
    FIELD idi_tip_relat             AS INTEGER.

define temp-table tt-digita         no-undo
    field ordem                     as integer   format ">>>>9"
    field exemplo                   as character format "x(30)"
    index id ordem.


def temp-table tt-raw-digita
   field raw-digita      as raw.
