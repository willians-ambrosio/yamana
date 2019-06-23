define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as character format "x(35)":U
    field usuario          as character format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field it-codigo        like item.it-codigo
    field data-ini         like param-estoq.mensal-ate 
    field data-fim         like param-estoq.mensal-ate 
    field mes-nome         as character
    field nome-empresa     like estabelec.nome.

define temp-table tt-estrutura no-undo
    field it-codigo    like item.it-codigo
    field inform-compl like item.inform-compl
    field codigo-refer like item.codigo-refer
    field cod-estabel  like item.cod-estabel
    field niv-mais-bai like item.niv-mais-bai
    field un           like item.un
    field num-sequen   as integer column-label 'Seq'
    
    field qtidade-ajust     like saldo-estoq.qtidade-ini column-label 'Qtde Ajust'

    field qtidade-ini       like saldo-estoq.qtidade-ini column-label 'Qtde Inic'
    field qtidade-from      like saldo-estoq.qtidade-ini column-label 'Qtde From'
    field qtidade-movto     like saldo-estoq.qtidade-ini column-label 'Qtde Movto'
    field qtidade-to        like saldo-estoq.qtidade-ini column-label 'Qtde To'
    field qtidade-fin       like saldo-estoq.qtidade-fin column-label 'Qtde Fin'
                           
    field sald-cash-ini     like movto-estoq.valor-mat-m column-label 'Vl Cash Inic' 
    field sald-cash-from    like movto-estoq.valor-mat-m column-label 'Vl Cash From' 
    field sald-cash-movto   like movto-estoq.valor-mat-m column-label 'Vl Cash Movto'
    field sald-cash-to      like movto-estoq.valor-mat-m column-label 'Vl Cash To'   
    field sald-cash-fin     like movto-estoq.valor-mat-m column-label 'Vl Cash Fin'  

    field sald-nocash-ini   like movto-estoq.valor-mat-m column-label 'Vl No Cash Inic' 
    field sald-nocash-from  like movto-estoq.valor-mat-m column-label 'Vl No Cash From' 
    field sald-nocash-movto like movto-estoq.valor-mat-m column-label 'Vl No Cash Movto'
    field sald-nocash-to    like movto-estoq.valor-mat-m column-label 'Vl No Cash To'   
    field sald-nocash-fin   like movto-estoq.valor-mat-m column-label 'Vl No Cash Fin'  

    index id is primary unique num-sequen it-codigo ascending.
