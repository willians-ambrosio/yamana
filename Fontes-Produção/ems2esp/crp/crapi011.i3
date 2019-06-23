/*******************************************************************************
**
**  CRAPI011.I3 - C lculo da estatistica do Grupo Centralizador 
**
********************************************************************************/

/* relacionamento Grupo com Matriz */
for each matriz-grupo-centraliz use-index grupo-matriz where
         matriz-grupo-centraliz.gr-centraliz = i-grupo-centraliz
         no-lock:    
        
    /* Clientes da Matriz */
    for each b-emitente no-lock use-index ch-matriz
        where b-emitente.nome-matriz = matriz-grupo-centraliz.matriz
        break by b-emitente.cod-emit
              by b-emitente.nome-matriz:
    
        find estatist
            where estatist.cod-emitente = b-emitente.cod-emitente
            no-lock no-error. 
    
        if  avail estatist then do:
            if  estatist.vl-maior-tit > tt-estatistica-a.vl-maior-tit then
                assign tt-estatistica-a.vl-maior-tit = estatist.vl-maior-tit
                       tt-estatistica-a.dt-maior-tit = estatist.dt-maior-tit.
        
            if  estatist.dt-ult-tit > tt-estatistica-a.dt-ult-tit 
            or  tt-estatistica-a.dt-ult-tit = ? then
                assign tt-estatistica-a.vl-ult-tit   = estatist.vl-ult-tit
                       tt-estatistica-a.dt-ult-tit   = estatist.dt-ult-tit.
        end.
    
        if  tt-estatistica-a.vl-maior-tit = ? then
            assign tt-estatistica-a.vl-maior-tit = 0.
     
        if  tt-estatistica-a.vl-ult-tit = ? then
            assign tt-estatistica-a.vl-ult-tit = 0.

        {crp/crapi011.i4 b-emitente}
            
        assign da-ini-est = date(month(da-fim-est),1,int(year(da-fim-est)) - 1).  
            
    end.
end.

do  i-cont = 1 to 12:
    if  i-atraso[i-cont] = ? then
        assign i-atraso[i-cont] = 0.
       
    if  i-recebimento[i-cont] = ? then
        assign i-recebimento[i-cont] = 0.    
end.

/* fim crp/crapi011.i3 */
