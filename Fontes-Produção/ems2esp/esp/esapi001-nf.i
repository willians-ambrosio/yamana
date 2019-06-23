find first docum-est no-lock
     where rowid(docum-est) = {1} no-error.
if avail docum-est then do:

    for each  docum-est-cex no-lock
        where docum-est-cex.serie-docto       = docum-est.serie-docto
        and   docum-est-cex.nro-docto         = docum-est.nro-docto
        and   docum-est-cex.cod-emitente      = docum-est.cod-emitente
        and   docum-est-cex.nat-operacao      = docum-est.nat-operacao:

        for each  desp-embarque no-lock
            where desp-embarque.cod-desp          = docum-est-cex.cod-desp
            and   desp-embarque.embarque          = docum-est.embarque
            and   desp-embarque.cod-emitente-desp = docum-est-cex.cod-emitente:

            find first desp-embarque-ext no-lock
                 where desp-embarque-ext.cod-estabel       = desp-embarque.cod-estabel
                 and   desp-embarque-ext.embarque          = desp-embarque.embarque
                 and   desp-embarque-ext.cod-itiner        = desp-embarque.cod-itiner
                 and   desp-embarque-ext.cod-pto-contr     = desp-embarque.cod-pto-contr
                 and   desp-embarque-ext.cod-desp          = desp-embarque.cod-desp
                 and   desp-embarque-ext.cod-emitente-desp = desp-embarque.cod-emitente-desp no-error.

            if avail desp-embarque-ext then do:

                find first sub-div-ordem no-lock
                     where sub-div-ordem.ep-codigo      = desp-embarque-ext.ep-codigo
                     and   sub-div-ordem.cod-est-exec   = desp-embarque-ext.cod-estabel
                     and   sub-div-ordem.num-ord-magnus = desp-embarque-ext.num-ord-inv no-error.

                if avail sub-div-ordem then do:

                    find first param-inv no-lock 
                         where param-inv.ep-codigo = sub-div-ordem.ep-codigo no-error.

                    if {2} = 2 then do: /*Atualiza‡Æo da Nota Fiscal vai mandar 2 - Modifica‡Æo*/
    
                        {utp\ut-liter.i Despesa_de_Importa‡Æo}
            
                        create controle-inv-esp.
                        assign controle-inv-esp.cod-emitente      = docum-est-cex.cod-emitente
                               controle-inv-esp.cod-desp          = docum-est-cex.cod-desp
                               controle-inv-esp.cod-emitente-desp = docum-est-cex.cod-emitente-desp
                               controle-inv-esp.cod-est-exec      = desp-embarque.cod-estabel
                               controle-inv-esp.cod-itiner        = desp-embarque.cod-itiner
                               controle-inv-esp.cod-pto-contr     = desp-embarque.cod-pto-contr
                               controle-inv-esp.dt-trans          = docum-est.dt-atualiza
                               controle-inv-esp.embarque          = docum-est.embarque
                               controle-inv-esp.ent-real          = desp-embarque.val-desp
                               controle-inv-esp.ep-codigo         = desp-embarque-ext.ep-codigo
                               controle-inv-esp.nat-operacao      = docum-est-cex.nat-operacao
                               controle-inv-esp.nro-docto         = docum-est-cex.nro-docto
                               controle-inv-esp.num-ord-inv       = desp-embarque-ext.num-ord-inv
                               controle-inv-esp.num-ordem         = sub-div-ordem.num-ordem
                               controle-inv-esp.num-projeto       = sub-div-ordem.num-projeto
                               controle-inv-esp.sai-comp          = desp-embarque.val-desp
                               controle-inv-esp.serie-docto       = docum-est-cex.serie-docto
                               controle-inv-esp.tipo-doc          = return-value
                               controle-inv-esp.sequencia         = 0.

                        run pi-atualiza-verba in this-procedure (input  2                            ,
                                                                 input  controle-inv-esp.ep-codigo   ,
                                                                 input  controle-inv-esp.num-ord-inv ,
                                                                 input  controle-inv-esp.dt-trans    ,
                                                                 input  param-inv.moeda-inv          ,
                                                                 input  controle-inv-esp.sai-comp    ,
                                                                 input  controle-inv-esp.ent-real    ,
                                                                 output table tt-erro).

                    end.
                    else if {2} = 3 then do: /*Desatualiza‡Æo da Nota Fiscal vai mandar 3 - Elimina‡Æo*/
                        find first controle-inv-esp exclusive-lock 
                             where controle-inv-esp.cod-est-exec = desp-embarque.cod-estabel
                             and   controle-inv-esp.num-projeto  = sub-div-ordem.num-projeto
                             and   controle-inv-esp.num-ordem    = sub-div-ordem.num-ordem
                             and   controle-inv-esp.num-ord-inv  = desp-embarque-ext.num-ord-inv no-error.
                        if avail controle-inv-esp then do:
                            run pi-atualiza-verba in this-procedure (input  2                             ,
                                                                     input  controle-inv-esp.ep-codigo    ,
                                                                     input  controle-inv-esp.num-ord-inv  ,
                                                                     input  controle-inv-esp.dt-trans     ,
                                                                     input  param-inv.moeda-inv           ,
                                                                     input  controle-inv-esp.sai-comp * -1,
                                                                     input  controle-inv-esp.ent-real * -1,
                                                                     output table tt-erro).
                            delete controle-inv-esp.
                            if can-find (first tt-erro) then
                                return "NOK":U.
                        end.
                    end.
                end.
            end.
        end.
        return "OK":U.
    end.
end.
