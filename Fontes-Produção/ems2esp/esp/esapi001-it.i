find first item-contrat no-lock
     where rowid(item-contrat) = {1} no-error.
if avail item-contrat then do:

    find first contrato-for no-lock
         where contrato-for.nr-contrato = item-contrat.nr-contrato no-error.
    if avail contrato-for then do:
        find first estabelec no-lock
             where estabelec.cod-estabel = contrato-for.cod-estabel no-error.
        if avail estabelec then
            assign cod-empresa = estabelec.ep-codigo.
    end.
    else do:
        find first param-global no-lock no-error.
        assign cod-empresa = param-global.empresa-prin.
    end.

    find first item-contrat-ext exclusive-lock
         where item-contrat-ext.nr-contrato  = item-contrat.nr-contrato
         and   item-contrat-ext.num-seq-item = item-contrat.num-seq-item no-error.
    if avail item-contrat-ext then do:

        find first param-inv no-lock 
             where param-inv.ep-codigo = cod-empresa no-error.

        find first sub-div-ordem no-lock
             where sub-div-ordem.ep-codigo      = cod-empresa
             and   sub-div-ordem.num-ord-magnus = item-contrat-ext.num-ord-inv no-error.

        find first controle-inv-esp exclusive-lock
             where controle-inv-esp.nr-contrato  = item-contrat.nr-contrato
             and   controle-inv-esp.num-seq-item = item-contrat.num-seq-item no-error.

        if not avail controle-inv-esp then do:
            find last  bfcontrole-inv-esp no-lock
                 where bfcontrole-inv-esp.cod-est-exec = contrato-for.cod-estabel
                 and   bfcontrole-inv-esp.num-projeto  = sub-div-ordem.num-projeto
                 and   bfcontrole-inv-esp.num-ordem    = sub-div-ordem.num-ordem
                 and   bfcontrole-inv-esp.num-ord-inv  = item-contrat-ext.num-ord-inv no-error.

            if avail bfcontrole-inv-esp then
                assign i-seq = bfcontrole-inv-esp.sequencia + 1.
            else
                assign i-seq = 1.

            create controle-inv-esp.
            assign controle-inv-esp.ep-codigo    = cod-empresa
                   controle-inv-esp.cod-est-exec = contrato-for.cod-estabel
                   controle-inv-esp.num-ord-inv  = item-contrat-ext.num-ord-inv
                   controle-inv-esp.dt-trans     = contrato-for.dt-contrato
                   controle-inv-esp.tipo-doc     = "Item Contrato"
                   controle-inv-esp.ent-comp     = (item-contrat.val-total - item-contrat.sld-val-receb)
                   controle-inv-esp.nr-contrato  = item-contrat.nr-contrato
                   controle-inv-esp.num-seq-item = item-contrat.num-seq-item
                   controle-inv-esp.nro-docto    = string(item-contrat.nr-contrato) + "/" + string(item-contrat.num-seq-item)
                   controle-inv-esp.cod-emitente = contrato-for.cod-emitente
                   controle-inv-esp.sequencia    = i-seq
                   controle-inv-esp.num-projeto  = sub-div-ordem.num-projeto
                   controle-inv-esp.num-ordem    = sub-div-ordem.num-ordem.
            run pi-atualiza-verba in this-procedure (input 2,
                                                     input controle-inv-esp.ep-codigo,
                                                     input controle-inv-esp.num-ord-inv,
                                                     input controle-inv-esp.dt-trans,
                                                     input param-inv.moeda-inv,
                                                     input controle-inv-esp.ent-comp,
                                                     input controle-inv-esp.ent-real,
                                                     output table tt-erro).
        end.
        else do:
            if controle-inv-esp.num-ord-inv <> item-contrat-ext.num-ord-inv then do:

                assign c-tipo-alter    = "C".
                assign c-texto-orig    = ""
                       c-texto         = ""
                       c-alter-origem  = string(controle-inv-esp.num-ord-inv)
                       c-alterado      = string(item-contrat-ext.num-ord-inv)
                       c-motivo-alter  = "Num Ord Inv"
                       gr-contrato-for = rowid(contrato-for).

                run cnp/cnapi002.p ("item-contrat",
                                    rowid(item-contrat),
                                    "item-contrat-ext.num-ord-inv",
                                    l-relacionamento).

                /***find first hist-alter exclusive-lock
                     where hist-alter.nr-contrato    = item-contrat.nr-contrato 
                     and   hist-alter.num-seq-item   = item-contrat.num-seq-item 
                     and   hist-alter.des-tipo-alter = "item-contrat-ext.num-ord-inv"
                     and   hist-alter.num-seq-adit   = 0 no-error.
                if avail hist-alter THEN DO:
                    find last b-hist-alter where
                              b-hist-alter.nr-contrato      = item-contrat.nr-contrato    and
                              b-hist-alter.num-seq-item     = item-contrat.num-seq-item   and
                              b-hist-alter.num-seq-anexo    = 0 and
                              b-hist-alter.num-seq-clausula = 0 and
                              b-hist-alter.des-tipo-alter   = "item-contrat-ext.num-ord-inv" and
                              b-hist-alter.num-seq-adit     = 1 no-lock no-error.
                    if avail b-hist-alter then
                         assign hist-alter.num-seq-alter = b-hist-alter.num-seq-alter + 1.
                
                    assign hist-alter.num-seq-adit   = 1.
                END.***/
            end.

            run pi-atualiza-verba in this-procedure (input 2,
                                                     input controle-inv-esp.ep-codigo,
                                                     input controle-inv-esp.num-ord-inv,
                                                     input controle-inv-esp.dt-trans,
                                                     input param-inv.moeda-inv,
                                                     input controle-inv-esp.ent-comp * -1,
                                                     input 0,
                                                     output table tt-erro).

            assign controle-inv-esp.ent-comp    = (item-contrat.val-total - item-contrat.sld-val-receb).
            assign controle-inv-esp.num-ord-inv = item-contrat-ext.num-ord-inv
                   controle-inv-esp.num-projeto = sub-div-ordem.num-projeto
                   controle-inv-esp.num-ordem   = sub-div-ordem.num-ordem.

            run pi-atualiza-verba in this-procedure (input 2,
                                                     input controle-inv-esp.ep-codigo,
                                                     input controle-inv-esp.num-ord-inv,
                                                     input controle-inv-esp.dt-trans,
                                                     input param-inv.moeda-inv,
                                                     input controle-inv-esp.ent-comp,
                                                     input 0,
                                                     output table tt-erro).
        end.

        if can-find(first tt-erro) then
            return "NOK":U.
        return "OK":U.
    end.
end.
