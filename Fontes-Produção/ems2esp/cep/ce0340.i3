/*******************************************************************************
**
**    CE0340.I3 - Verifica‡Æo do saldo dispon¡vel do item
**
******************************************************************************/

if  tt-param.l-depositos then do:
    for each tt-digita 
       where tt-digita.cod-estabel = b-estab-mat.cod-estabel
         and tt-digita.lista no-lock:
         
        for each  saldo-estoq fields (it-codigo cod-estabel cod-depos qtidade-atu 
                                      qt-alocada qt-aloc-prod qt-aloc-ped)
                              use-index estabel-item no-lock
            where saldo-estoq.it-codigo   = {1}.it-codigo
            and   saldo-estoq.cod-estabel = tt-digita.cod-estabel
            and   saldo-estoq.cod-depos   = tt-digita.cod-depos:
        
            assign de-qt-estoque = de-qt-estoque + saldo-estoq.qtidade-atu -
                                   if  tt-param.i-sldaloc = 1 then /*Considera Aloca‡äes*/
                                       ( saldo-estoq.qt-alocada
                                       + saldo-estoq.qt-aloc-prod
                                       + saldo-estoq.qt-aloc-ped)
                                   else 0.
        end.
    end.
end.
else 
for each  saldo-estoq fields (it-codigo cod-estabel cod-depos qtidade-atu
                              qt-alocada qt-aloc-prod qt-aloc-ped) 
                      use-index estabel-item no-lock
    where saldo-estoq.it-codigo   = {1}.it-codigo
    and   saldo-estoq.cod-estabel = b-estab-mat.cod-estabel:
    
    for first deposito fields (cod-depos cons-saldo) 
        where deposito.cod-depos = saldo-estoq.cod-depos no-lock: end.
    if  avail deposito 
    and deposito.cons-saldo = yes then
        assign de-qt-estoque = de-qt-estoque + saldo-estoq.qtidade-atu -
                               if  tt-param.i-sldaloc = 1 then /*Considera Aloca‡äes*/
                                   ( saldo-estoq.qt-alocada
                                   + saldo-estoq.qt-aloc-prod
                                   + saldo-estoq.qt-aloc-ped)
                               else 0.
end.

if  tt-param.l-reqest
and tt-param.l-solcomp then do:
    assign i-situacao[1] = 1 
           i-situacao[2] = 3 
           i-situacao[3] = 4 .
    
    do i-ind = 1 to 3:
    
        for each it-requisicao fields (it-codigo cod-estabel estado situacao qt-a-atender nr-requisicao)
                                 &if "{&bf_mat_versao_ems}" >= "2.042" &then use-index sit-requis &endif
           where it-requisicao.it-codigo    = item-uni-estab.it-codigo
             and it-requisicao.cod-estabel  = item-uni-estab.cod-estabel
             and it-requisicao.situacao     = i-situacao[i-ind]
             and it-requisicao.estado       = 1 /*aprovada*/ no-lock:

            if it-requisicao.qt-a-atender <= 0 then next.
    
            find first requisicao no-lock
                where requisicao.nr-requisicao = it-requisicao.nr-requisicao no-error.
            if  avail requisicao then do:
    
                if requisicao.tp-requis = 1 then /* Requisi‡Æo*/
                    assign de-qt-req = de-qt-req + it-requisicao.qt-a-atender.
                else 
                if  requisicao.tp-requis    = 2 /* Solicita‡Æo */
                and (it-requisicao.situacao = 1 
                or   it-requisicao.situacao = 3)  then
                    assign de-qt-sol = de-qt-sol + it-requisicao.qt-a-atender.
            end.
        end.
    end.
end.
else if  tt-param.l-reqest then do:
    assign i-situacao[1] = 1
           i-situacao[2] = 3
           i-situacao[3] = 4.
           
    do i-ind = 1 to 3:
        for each it-requisicao fields (it-codigo cod-estabel estado situacao qt-a-atender nr-requisicao)
                               &if "{&bf_mat_versao_ems}" >= "2.042" &then use-index sit-requis &endif
               where it-requisicao.it-codigo     = {1}.it-codigo
                 and it-requisicao.cod-estabel   = {2}
                 and it-requisicao.situacao      = i-situacao[i-ind]
                 and it-requisicao.estado        = 1 /* aprovada */ no-lock: 
        
                if it-requisicao.qt-a-atender <= 0 then next.
                
                find first requisicao no-lock
                    where requisicao.nr-requisicao = it-requisicao.nr-requisicao no-error.
                if  avail requisicao 
                and requisicao.tp-requis = 1 then /* Requisi‡Æo*/
                        assign de-qt-req = de-qt-req + it-requisicao.qt-a-atender.
        end.
    end.      
end.
else if tt-param.l-solcomp then do:
    assign i-situacao[1] = 1
           i-situacao[2] = 3.
           
    do i-ind = 1 to 2:
        for each it-requisicao fields (it-codigo cod-estabel estado situacao qt-a-atender nr-requisicao)
                               &if "{&bf_mat_versao_ems}" >= "2.042" &then use-index sit-requis &endif
           where it-requisicao.it-codigo    = {1}.it-codigo
             and it-requisicao.cod-estabel  = {2}
             and it-requisicao.situacao     = i-situacao[i-ind]
             and it-requisicao.estado       = 1 /*Aprovada*/ no-lock:
                            
            if it-requisicao.qt-a-atender <= 0 then next.
            
            find first requisicao no-lock
                where requisicao.nr-requisicao = it-requisicao.nr-requisicao no-error.
            if  avail requisicao 
            and requisicao.tp-requis = 2 /* Solicitacao Compras */ then 
                assign de-qt-sol = de-qt-sol + it-requisicao.qt-a-atender.
        end.    
    end.  
end.

for each  prazo-compra fields (it-codigo quant-saldo numero-ordem situacao) no-lock
     where prazo-compra.it-codigo   = {1}.it-codigo
       and prazo-compra.quant-saldo > 0:
    for first ordem-compra fields (cod-estabel numero-ordem dep-almoxar
                                   ct-codigo sc-codigo)
       where  ordem-compra.cod-estabel = b-estab-mat.cod-estabel
       and    ordem-compra.numero-ordem = prazo-compra.numero-ordem no-lock: end.
       
       if  not avail ordem-compra then next.
       
       if  tt-param.i-contacont = 1 and /*Sem Conta Cont bil*/
           ordem-compra.ct-codigo <> " " and
		   ordem-compra.sc-codigo <> " " and
           avail item and
           item.tipo-contr <> 1 then next. /* F¡sico */

    if  ordem-compra.dep-almoxar <> "" then do:
        if  tt-param.l-depositos then do:
            find first tt-digita
                 where  tt-digita.cod-estabel = ordem-compra.cod-estabel
                 and    tt-digita.cod-depos   = ordem-compra.dep-almoxar
                 no-lock no-error.
            if  not avail tt-digita or
                not tt-digita.lista then next.
        end.
        else do:    
            for first deposito fields (cod-depos cons-saldo) 
                where deposito.cod-depos = ordem-compra.dep-almoxar no-lock: end.
            if  avail deposito and deposito.cons-saldo = no then next.
        end.
    end.
    if  prazo-compra.situacao <> 6 and prazo-compra.situacao <> 4 then
        assign de-qt-cc      = de-qt-cc      + prazo-compra.quant-saldo.
end.
