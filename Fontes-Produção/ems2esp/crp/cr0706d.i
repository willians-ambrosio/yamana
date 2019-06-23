/*********************************************************************
***
***  crp/cr0706d.i - Defini‡Æo temp-table tt-param.
***
*********************************************************************/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field tipo-relat       as integer.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.
    
def temp-table tt-raw-digita
    field raw-digita as raw.    

def temp-table tt-documento no-undo like titulo
     field row-documento  as rowid
     field c-modalidade   like titulo.modalidade
     field c-mo-codigo    as char format "x(03)"
     field de-saldo       like titulo.vl-saldo
     field de-saldo-me    like titulo.vl-saldo-me
     field i-dias         as int init 0
     field de-juros       as dec 
     field de-total-juros as dec.


