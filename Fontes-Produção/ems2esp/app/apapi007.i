/**********************************************************************************
**
**  APAPI007.I
**
**  Defini‡Æo temp-tableïs de parƒmetros e de erro da api.
**
**********************************************************************************/

def temp-table tt-param-filtro no-undo
    field i-empresa        like tit-ap.ep-codigo
    field i-cod-fornec     like tit-ap.cod-fornec
    field c-cod-est        like tit-ap.cod-estabel
    field c-cod-est-ini    like tit-ap.cod-estabel
    field c-cod-est-fim    like tit-ap.cod-estabel
    field c-esp-ini        like tit-ap.cod-esp
    field c-esp-fim        like tit-ap.cod-esp
    field dt-trans-ini     like tit-ap.dt-transacao
    field dt-trans-fim     like tit-ap.dt-transacao
    field i-port-ini       like tit-ap.portador
    field i-port-fim       like tit-ap.portador
    field i-moeda          like tit-ap.moeda
    field da-conversao     as date format "99/99/9999"
    field saldo-anterior   as decimal format "->>>,>>>,>>9.99"
    field saldo-aberto     as decimal format "->>>,>>>,>>9.99"
    field l-ant-aber       as logical
    field l-matriz         as logical
    field l-saldo-aberto   as logical
    field l-faixa          as logical
    field cod-versao-integ as integer
    field l-acompanha      as logical
    field l-vid-rel        as logical.
    
def temp-table tt-tit-ap like tit-ap
    field row-tit-ap   as rowid
    field c-modalidade as char format "x(20)"
    field c-tipo-pagto as char format "x(20)"
    field i-atraso     as inte format "->>>9".
    
/* apapi007.i */
