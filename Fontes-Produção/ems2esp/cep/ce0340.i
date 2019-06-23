

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char    format "x(35)"
    field usuario          as char    format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field estabelec-ini    as char    format "x(3)"
    field estabelec-fim    as char    format "x(3)"
    field ge-ini           as int     format "99"
    field ge-fim           as int     format "99"
    field familia-ini      as char    format "x(8)"
    field familia-fim      as char    format "x(8)"
    field item-ini         as char    format "x(16)"
    field item-fim         as char    format "x(16)"
    field depos-ini        as char    format "x(3)"
    field depos-fim        as char    format "x(3)"
    field periodico        as logical format "Sim/NÆo"
    field pto-enc          as logical format "Sim/NÆo"
    field elimina          as integer format "9"
    field l-depositos      as logical format "Sim/NÆo"
    field l-solcomp        as logical format "Sim/NÆo"
    field l-reqest         as logical format "Sim/NÆo"
    field i-sldaloc        as integer format "9"
    field i-contacont      as integer format "9"
    field i-consprev       as integer format "9"
    field desc-elimina     as char    format "x(20)"
    field desc-sldaloc     as char    format "x(22)"
    field desc-contacont   as char    format "x(20)"
    field desc-consprev    as char    format "x(10)".
