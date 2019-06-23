/*********************************************************************************
**
**  CR0706.I3
**
**  Calculo dos valores do titulo.
**
**********************************************************************************/

assign de-calc-vl-saldo   = titulo.vl-saldo.

if  i-moeda <> 0 then do:
    if  i-moeda = titulo.mo-codigo then
        assign de-calc-vl-saldo   = titulo.vl-saldo-me.
    else
        assign de-calc-vl-saldo   = round(titulo.vl-saldo / de-cotacao,2).
end.

