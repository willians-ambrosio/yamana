/********************************************************************************
**
**  CR0706.I
**  
**  Calcula saldo anterior.
**
********************************************************************************/  
 
if  titulo.tipo = 3 then
    assign de-saldo-anterior = de-saldo-anterior + de-calc-vl-saldo.
else do:
    if  titulo.tipo = 2 
    or  titulo.tipo = 7 then do:
        assign de-saldo-anterior = de-saldo-anterior - de-calc-vl-saldo.

        for each mov-tit 
            where mov-tit.ep-codigo    = titulo.ep-codigo
            and   mov-tit.cod-estabel  = titulo.cod-estabel
            and   mov-tit.esp-antecip  = titulo.cod-esp
            and   mov-tit.doc-antecip  = titulo.nr-docto
            and   mov-tit.parc-antecip = titulo.parcela
            and   mov-tit.serie        = titulo.serie
            and   mov-tit.transacao    = 2
            and   mov-tit.dt-credito  >= {1} no-lock:
            
            /*****{crp/cr0706.i5}*******/
            assign de-calc-vl-antecip = mov-tit.vl-antecip
                   de-calc-vl-baixa   = mov-tit.vl-baixa.
            
            assign de-saldo-anterior = de-saldo-anterior - de-calc-vl-antecip.
        end.
                
        for each mov-tit
            where mov-tit.ep-codigo   = titulo.ep-codigo
            and   mov-tit.cod-estabel = titulo.cod-estabel
            and   mov-tit.cod-esp     = titulo.cod-esp
            and   mov-tit.nr-docto    = titulo.nr-docto
            and   mov-tit.parcela     = titulo.parcela
            and   mov-tit.serie       = titulo.serie
            and   mov-tit.dt-credito >= d-dt-emissao-inicial
            and   mov-tit.transacao   = 13 no-lock:

            /*****{crp/cr0706.i5}*******/
            assign de-calc-vl-antecip = mov-tit.vl-antecip
                   de-calc-vl-baixa   = mov-tit.vl-baixa.

            assign de-saldo-anterior = if  mov-tit.lancamento = 1 then
                                           de-saldo-anterior + de-calc-vl-baixa
                                       else
                                           de-saldo-anterior - de-calc-vl-baixa.
        end.
        
    end.
    else do:
        assign de-saldo-anterior = de-saldo-anterior + de-calc-vl-saldo.
        
        for each mov-tit
            where mov-tit.ep-codigo   = titulo.ep-codigo
            and   mov-tit.cod-estabel = titulo.cod-estabel
            and   mov-tit.cod-esp     = titulo.cod-esp
            and   mov-tit.nr-docto    = titulo.nr-docto
            and   mov-tit.parcela     = titulo.parcela
            and   mov-tit.serie       = titulo.serie
            and   mov-tit.dt-credito >= {1}
            and  (mov-tit.transacao   = 2
            or    mov-tit.transacao   = 21
            or    mov-tit.transacao   = 22
            or    mov-tit.transacao   = 23
            or    mov-tit.transacao   = 3
            or    mov-tit.transacao   = 13) no-lock:
    
            /*****{crp/cr0706.i5}*******/
            assign de-calc-vl-antecip = mov-tit.vl-antecip
                   de-calc-vl-baixa   = mov-tit.vl-baixa.
    
            assign de-saldo-anterior = if  mov-tit.transacao = 2
                                       or  mov-tit.transacao = 21 
                                       or  mov-tit.transacao = 22 
                                       or  mov-tit.transacao = 23 
                                       or  mov-tit.transacao = 3 then
                                           de-saldo-anterior + de-calc-vl-baixa
                                        else
                                           if  mov-tit.lancamento = 1 then
                                               de-saldo-anterior + de-calc-vl-baixa
                                           else  
                                               de-saldo-anterior - de-calc-vl-baixa.
        end.
    end.
end.
