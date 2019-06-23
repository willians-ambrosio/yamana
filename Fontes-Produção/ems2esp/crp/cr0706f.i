for each titulo {1}
    where {2}
    and   titulo.ep-codigo    = i-ep-codigo-usuario
    and   titulo.cod-esp     >= c-cod-esp-inicial
    and   titulo.cod-esp     <= c-cod-esp-final
    and   titulo.cod-rep     >= i-cod-rep-inicial
    and   titulo.cod-rep     <= i-cod-rep-final
    and   titulo.cod-por     >= i-cod-por-inicial
    and   titulo.cod-por     <= i-cod-por-final
    and   titulo.dt-emissao  <= d-dt-emissao-final no-lock:
    
    if l-web = no then do:
        run pi-acompanhar in h-acomp (input "Docto/P " + string(titulo.nr-docto) + "/" + string(titulo.parcela)).
    end.
    
    if  titulo.dt-emissao < d-dt-emissao-inicial then do:
        run pi-saldo-anterior.
    end.

    assign de-calc-vl-saldo   = titulo.vl-saldo.

    /*---- Totaliza‡Æo --------*/       
    {crp/cr0706.i1 "d-dt-emissao-final"}
    
end.
