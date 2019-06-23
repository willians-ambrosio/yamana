{include/i_dbvers.i}
/***************************************************************************
***
***  cdp/cd0727.i - Defini‡Æo Temp-Table
***
***************************************************************************/

define temp-table tt-param no-undo
    field destino             as integer
    field arquivo             as char format "x(35)"
    field usuario             as char format "x(12)"
    field data-exec           as date
    field hora-exec           as integer
&IF "{&mguni_version}" >= "2.071" &THEN
    field i-empresa           LIKE empresa.ep-codigo
&ELSE
    field i-empresa           as integer
&ENDIF
    field i-modulo            as integer
    field c-modulo            as char format "x(40)"    
    field i-empresa-ini       like hist-tit-eliminados.ep-codigo
    field i-empresa-fim       like hist-tit-eliminados.ep-codigo
    field c-estab-ini         like hist-tit-eliminados.cod-estabel
    field c-estab-fim         like hist-tit-eliminados.cod-estabel
    field c-esp-ini           like hist-tit-eliminados.cod-esp
    field c-esp-fim           like hist-tit-eliminados.cod-esp
    field c-serie-ini         like hist-tit-eliminados.serie
    field c-serie-fim         like hist-tit-eliminados.serie
    field c-nr-docto-ini      like hist-tit-eliminados.nr-docto
    field c-nr-docto-fim      like hist-tit-eliminados.nr-docto
    field i-emitente-ini      as integer format ">>>>>>>>9" 
    field i-emitente-fim      as integer format ">>>>>>>>9"
    field da-data-ini         like hist-tit-eliminados.data-elim-canc
    field da-data-fim         like hist-tit-eliminados.data-elim-canc
    field c-usuario-ini       like hist-tit-eliminados.usuario-elim-canc
    field c-usuario-fim       like hist-tit-eliminados.usuario-elim-canc
    field log-pag-parametros  as logical.

/* fim cdp/cd0727.i */
