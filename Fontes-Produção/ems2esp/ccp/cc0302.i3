/***************************************************************************************************
**
**  cc0302.i3 - Definicao da temp-table tt-param
**
***************************************************************************************************/

define temp-table tt-param
    field destino      as integer
    field arquivo      as char
    field usuario      as char
    field data-exec    as date
    field hora-exec    as integer
    field c-item-i     as char
    field c-item-f     as char
    field c-estab-i    as char
    field c-estab-f    as char
    field c-compr-i    as char
    field c-compr-f    as char
    field i-ordem-i    as integer
    field i-ordem-f    as integer
    field d-data-i     as date format 99/99/9999
    field d-data-f     as date format 99/99/9999
    field c-fami-i     as char
    field c-fami-f     as char
    field i-tipo       as integer
    field l-ficha      as logical
    field l-parcela    as logical
    field l-envio      as logical
    field c-envio      as char
    field c-tipo       as char
    field c-destino    as char
    field i-class      as integer
    field l-bus-to-bus as logical
    field l-eprocurement as logical
    field l-automatico   as logical.

def temp-table tt-b2b
    field numero-ordem       like cotacao-item.numero-ordem
    field cod-emitente       like cotacao-item.cod-emitente
    field nome-abrev         like emitente.nome-abrev
    field it-codigo          like cotacao-item.it-codigo
    index ordem-emitente is primary unique
          numero-ordem
          cod-emitente.

/* cc0302.i3 */
