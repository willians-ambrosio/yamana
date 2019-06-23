find first desp-embarque-ext no-lock
     where rowid(desp-embarque-ext) = {1} no-error.
if avail desp-embarque-ext then do:

    find first param-inv no-lock 
     where param-inv.ep-codigo = desp-embarque-ext.ep-codigo no-error.

    if {2} = 3 then do:
        find first controle-inv-esp exclusive-lock
             where controle-inv-esp.ep-codigo   = desp-embarque-ext.ep-codigo
             and   controle-inv-esp.num-ord-inv = desp-embarque-ext.num-ord-inv no-error.

        if avail controle-inv-esp then
            run pi-atualiza-verba in this-procedure (input  3                             , /* Valida e Atualiza Verba */
                                                     input  controle-inv-esp.ep-codigo    ,
                                                     input  controle-inv-esp.num-ord-inv  ,
                                                     input  today                         ,
                                                     input  param-inv.moeda-inv           ,
                                                     input  controle-inv-esp.ent-comp * -1, /* Compromissado */
                                                     input  0                             , /* Realizado */
                                                     output table tt-erro).
        if can-find (first tt-erro) then
            return "NOK":U.



    end.
    else do:

        find first controle-inv-esp exclusive-lock
             where controle-inv-esp.cod-est-exec      = desp-embarque-ext.cod-estabel
             and   controle-inv-esp.embarque          = desp-embarque-ext.embarque
             and   controle-inv-esp.cod-itiner        = desp-embarque-ext.cod-itiner
             and   controle-inv-esp.cod-pto-contr     = desp-embarque-ext.cod-pto-contr
             and   controle-inv-esp.cod-desp          = desp-embarque-ext.cod-desp
             and   controle-inv-esp.cod-emitente-desp = desp-embarque-ext.cod-emitente-desp
             and   controle-inv-esp.num-ord-inv       = desp-embarque-ext.num-ord-inv  no-error.
        if avail controle-inv-esp then
            assign controle-inv-esp.ent-comp    = desp-embarque-ext.val-desp.

        else do:
            find first sub-div-ordem no-lock
                 where sub-div-ordem.ep-codigo      = desp-embarque-ext.ep-codigo
                 and   sub-div-ordem.num-ord-magnus = desp-embarque-ext.num-ord-inv no-error.
    
            find last  bfcontrole-inv-esp no-lock
                 where bfcontrole-inv-esp.cod-est-exec = desp-embarque-ext.cod-estabel
                 and   bfcontrole-inv-esp.num-projeto  = sub-div-ordem.num-projeto
                 and   bfcontrole-inv-esp.num-ordem    = sub-div-ordem.num-ordem
                 and   bfcontrole-inv-esp.num-ord-inv  = desp-embarque-ext.num-ord-inv no-error.
            if avail bfcontrole-inv-esp then
                assign i-seq = bfcontrole-inv-esp.sequencia + 1.
            else
                assign i-seq = 1.
    
    
    
            {utp/ut-liter.i Despesa_de_Importa‡Æo}
            create controle-inv-esp.
            assign controle-inv-esp.cod-desp          = desp-embarque-ext.cod-desp
                   controle-inv-esp.cod-emitente-desp = desp-embarque-ext.cod-emitente-desp
                   controle-inv-esp.cod-est-exec      = desp-embarque-ext.cod-estabel
                   controle-inv-esp.cod-itiner        = desp-embarque-ext.cod-itiner
                   controle-inv-esp.cod-pto-contr     = desp-embarque-ext.cod-pto-contr
                   controle-inv-esp.embarque          = desp-embarque-ext.embarque
                   controle-inv-esp.ent-comp          = desp-embarque-ext.val-desp
                   controle-inv-esp.ep-codigo         = desp-embarque-ext.ep-codigo
                   controle-inv-esp.num-ord-inv       = desp-embarque-ext.num-ord-inv
                   controle-inv-esp.num-projeto       = sub-div-ordem.num-projeto
                   controle-inv-esp.num-ordem         = sub-div-ordem.num-ordem
                   controle-inv-esp.tipo-doc          = return-value
                   controle-inv-esp.sequencia         = i-seq.
        end.
    
        run pi-atualiza-verba in this-procedure (input  2                            , /* Valida e Atualiza Verba */
                                                 input  controle-inv-esp.ep-codigo   ,
                                                 input  controle-inv-esp.num-ord-inv ,
                                                 input  today                        ,
                                                 input  param-inv.moeda-inv          ,
                                                 input  controle-inv-esp.ent-comp    , /* Compromissado */
                                                 input  0                            , /* Realizado */
                                                 output table tt-erro).
    end.
    if can-find (first tt-erro) then
        return "NOK":U.

    return "OK":U.
end.
