/*****************************************************************************
**
**  PROGRAMA: apya500.i
**
**  Data....: 30/10/2017
**
**  Autor...: ECR
**
**  Vers∆o..: 
**
**  OBS.....: Temp-table tt-param
**
*******************************************************************************/

    def temp-table tt-param     no-undo
        field destino           as integer
        field arquivo           as char format "x(35)"
        field usuario           as char format "x(12)"
        field data-exec         as date
        field hora-exec         as integer
        field classifica        as integer
        field desc-class        as char format "x(40)"
        field empresa-ini       as char   
        field empresa-fim       as char
        field banco-ini         as char   
        field banco-fim         as char
        field prod-financ-ini   as char
        field prod-financ-fim   as char  
        field tip-produt-ini    as CHAR
        field tip-produt-fim    as CHAR
        field contrato-ini      as CHAR
        field contrato-fim      as CHAR
        field dat-operac-ini    as date
        field dat-operac-fim    as date
        field imp-periodo       as INTE
        field imp-moeda         as INTE
        field imp-tipo          as INTE
        field imp-total         as INTE.
                                    

/*  Fim de Programa ***********************************************************/
