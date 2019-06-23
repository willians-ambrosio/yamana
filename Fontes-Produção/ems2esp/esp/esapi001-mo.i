find first controle-inv-esp no-lock
     where rowid(controle-inv-esp) = {1} no-error.
if avail controle-inv-esp then do:
    find first param-inv no-lock 
         where param-inv.ep-codigo = controle-inv-esp.ep-codigo no-error.
    if {2} = 3 then do:
        run pi-atualiza-verba in this-procedure (input  3                            , /* Atualiza Verba */
                                                 input  controle-inv-esp.ep-codigo   ,
                                                 input  controle-inv-esp.num-ord-inv ,
                                                 input  controle-inv-esp.dt-trans    ,
                                                 input  param-inv.moeda-inv          ,
                                                 input  0                            , /* Compromissado */
                                                 input  controle-inv-esp.valor-origem * -1 , /* Realizado */
                                                 output table tt-erro).
        if can-find (first tt-erro) then
            return "NOK":U.
    end.
    else do:
        run pi-atualiza-verba in this-procedure (input  3                            , /* Atualiza Verba */
                                                 input  controle-inv-esp.ep-codigo   ,
                                                 input  controle-inv-esp.num-ord-inv ,
                                                 input  controle-inv-esp.dt-trans    ,
                                                 input  param-inv.moeda-inv          ,
                                                 input  0                            , /* Compromissado */
                                                 input  controle-inv-esp.valor-origem, /* Realizado */
                                                 output table tt-erro).
        if can-find (first tt-erro) then
            return "NOK":U.
    end.
    return "OK":U.
end.
