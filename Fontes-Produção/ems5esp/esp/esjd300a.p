/*
cadastrar no programa (esp/esfnd001.w)

Programa  : "ESJD300"
Parƒmetro : "Conecta banco HCM"
Valor     : -db hcm -ld hcm -N tcp -H ydmdsrpvm02 -S 23607
*/
find first es-param-prog no-lock
     where es-param-prog.cod_prog_dtsul = "ESJD300"
       and es-param-prog.cod-param      = "Conecta banco HCM" no-error.
if  avail es-param-prog then do:
    connect value(es-param-prog.txt-valor) no-error.
    run esp/esjd300.w.
    disconnect "hcm".
end.
else
    run utp/ut-msgs.p(input "show", input 17006,
                      input "ConexÆo com Banco do HCM inexistente").
