/* def var NOME-ARQ as char format "x(35)" label "Arquivo". */

/* assign nome-arq = session:temp-directory + "criterio_conta.txt".  
update NOME-ARQ with 5 down width 100 side-label 1 col. */

output to "criterio_conta.txt".

MESSAGE "Confira o arquivo CRITERIO_CONTA.txt gerado em sua pasta temp." VIEW-AS ALERT-BOX.

def temp-table tt-criterio
    field cod_cta_ctbl like criter_distrib_cta_ctbl.cod_cta_ctbl
    field JMC          like criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto column-label 'JMC'
    field MFB          like criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto column-label 'MFB'
    field MMR          like criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto column-label 'MMR'
    field SDM          like criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto column-label 'SDM'
    field YDM          like criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto column-label 'YDM'
    index ch-conta as primary unique cod_cta_ctbl.

 for each criter_distrib_cta_ctbl no-lock:
    find first tt-criterio
         where tt-criterio.cod_cta_ctbl = criter_distrib_cta_ctbl.cod_cta_ctbl no-error.
    if  not avail tt-criterio then do:
        create tt-criterio no-error.
        assign tt-criterio.cod_cta_ctbl = criter_distrib_cta_ctbl.cod_cta_ctbl.
    end.

    case criter_distrib_cta_ctbl.cod_empresa:
        when 'JMC' then
            case criter_distrib_cta_ctbl.ind_criter_distrib_ccusto:
                when 'definidos'     then assign tt-criterio.JMC = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto.
                when 'n∆o utiliza'   then assign tt-criterio.JMC = 'n∆o utiliza'.
                when 'utiliza todos' then assign tt-criterio.JMC = 'utiliza todos'.
            end case.
        when 'MFB' then
            case criter_distrib_cta_ctbl.ind_criter_distrib_ccusto:
                when 'definidos'     then assign tt-criterio.MFB = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto.
                when 'n∆o utiliza'   then assign tt-criterio.MFB = 'n∆o utiliza'.
                when 'utiliza todos' then assign tt-criterio.MFB = 'utiliza todos'.
            end case.
        when 'MMR' then
            case criter_distrib_cta_ctbl.ind_criter_distrib_ccusto:
                when 'definidos'     then assign tt-criterio.MMR = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto.
                when 'n∆o utiliza'   then assign tt-criterio.MMR = 'n∆o utiliza'.
                when 'utiliza todos' then assign tt-criterio.MMR = 'utiliza todos'.
            end case.
        when 'SDM' then
            case criter_distrib_cta_ctbl.ind_criter_distrib_ccusto:
                when 'definidos'     then assign tt-criterio.SDM = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto.
                when 'n∆o utiliza'   then assign tt-criterio.SDM = 'n∆o utiliza'.
                when 'utiliza todos' then assign tt-criterio.SDM = 'utiliza todos'.
            end case.
        when 'YDM' then
            case criter_distrib_cta_ctbl.ind_criter_distrib_ccusto:
                when 'definidos'     then assign tt-criterio.YDM = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto.
                when 'n∆o utiliza'   then assign tt-criterio.YDM = 'n∆o utiliza'.
                when 'utiliza todos' then assign tt-criterio.YDM = 'utiliza todos'.
            end case.
    end case.

/*
    disp criter_distrib_cta_ctbl.cod_cta_ctbl 
         criter_distrib_cta_ctbl.cod_empresa 
         criter_distrib_cta_ctbl.cod_estab 
         criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto 
         criter_distrib_cta_ctbl.cod_plano_cta_ctbl 
         criter_distrib_cta_ctbl.dat_fim_valid 
         criter_distrib_cta_ctbl.dat_inic_valid 
         criter_distrib_cta_ctbl.ind_criter_distrib_ccusto 
        criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl 
/*          criter_distrib_cta_ctbl.cod_livre_1 */
         criter_distrib_cta_ctbl.dat_livre_1 
/*          criter_distrib_cta_ctbl.log_livre_1 */
/*          criter_distrib_cta_ctbl.num_livre_1 */
/*          criter_distrib_cta_ctbl.val_livre_1 */
         ''
        with 1 col no-error.
*/
end.

for each tt-criterio:
    disp tt-criterio.cod_cta_ctbl
         tt-criterio.JMC
         tt-criterio.MFB
         tt-criterio.MMR
         tt-criterio.SDM
         tt-criterio.YDM
        with width 150.

end.
output close.
