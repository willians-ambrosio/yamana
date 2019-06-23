define temp-table tt-param no-undo
    field destino          as integer
    field c-destino        as char 
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD cod-identific-bem-ini LIKE ri-bem.cod-identific-bem
    FIELD cod-identific-bem-fim LIKE ri-bem.cod-identific-bem
    FIELD dat-entrada-ini       AS   DATE
    FIELD dat-entrada-fim       AS   DATE
    .
