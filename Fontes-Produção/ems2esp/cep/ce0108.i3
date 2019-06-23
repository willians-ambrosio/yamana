/******************************************************************
**
**    CE0108.I3 - ACERTA SALDO EM PODER DE TERCEIROS
**
******************************************************************/

for each saldo-terc
   where saldo-terc.it-codigo      = item.it-codigo
     and saldo-terc.valor-mat-m[1] = 0
     and saldo-terc.valor-mob-m[1] = 0 
     and saldo-terc.valor-ggf-m[1] = 0 exclusive-lock:

   find    componente
     where componente.nro-docto = saldo-terc.nro-docto
       and componente.serie-docto = saldo-terc.serie-docto
       and componente.cod-emitente = saldo-terc.cod-emitente
       and componente.nat-operacao = saldo-terc.nat-operacao
       and componente.it-codigo = saldo-terc.it-codigo
       and componente.sequencia = saldo-terc.sequencia
       and componente.cod-refer = saldo-terc.cod-refer
     exclusive-lock no-error.
                    
   assign saldo-terc.valor-mat-m[1] = componente.preco-total[1] 
                                 / if componente.quantidade <> 0 then 
                                      componente.quantidade
                                   else 1.

   if  (componente.ct-codigo = ct-initial
   and  componente.sc-codigo = sc-initial) then 
       assign componente.ct-codigo = tt-param.ct-conta
              componente.sc-codigo = tt-param.sc-conta.

   if param-estoq.tem-moeda1 then do:
      run cdp/cd0813.p (0, 1, saldo-terc.valor-mat-m[1],
                        saldo-terc.dt-retorno, output de-aux).

      if de-aux <> ? then 
         assign saldo-terc.valor-mat-m[2] = de-aux.
   end.

   if param-estoq.tem-moeda2 then do:
      run cdp/cd0813.p (0, 2, saldo-terc.valor-mat-m[1],
                        saldo-terc.dt-retorno, output de-aux).
      if  de-aux <> ? then 
          assign saldo-terc.valor-mat-m[3] = de-aux.
   end.

   if i-mo-fasb <> 0 then 
      assign saldo-terc.valor-mat-m[i-mo-fasb] = componente.preco-total[2]
                                               / componente.quantidade.
   if i-mo-cmi <> 0 then 
      assign saldo-terc.valor-mat-m[i-mo-cmi] = componente.pr-total-cmi
                                              / componente.quantidade.

   for each movto-estoq 
      where movto-estoq.serie-docto = saldo-terc.serie-docto
        and movto-estoq.nro-docto   = saldo-terc.nro-docto
        and movto-estoq.cod-emitente = saldo-terc.cod-emitente
        and movto-estoq.nat-operacao = saldo-terc.nat-operacao 
        and movto-estoq.it-codigo  = saldo-terc.it-codigo
        exclusive-lock :
      if  movto-estoq.sequen-nf          = saldo-terc.sequencia 
      and movto-estoq.valor-mat-m[1]     = 0
      and movto-estoq.valor-mob-m[1]     = 0 
      and movto-estoq.valor-ggf-m[1]     = 0 then do i-moeda = 1 to 3:
          assign movto-estoq.valor-mat-m[i-moeda] = saldo-terc.valor-mat-m[i-moeda]
                                                * movto-estoq.quantidade
                 movto-estoq.valor-ggf-m[i-moeda] = saldo-terc.valor-ggf-m[i-moeda]
                                                * movto-estoq.quantidade                                                
                 movto-estoq.valor-mob-m[i-moeda] = saldo-terc.valor-mob-m[i-moeda]
                                                * movto-estoq.quantidade.
      end.
   end.
end.
