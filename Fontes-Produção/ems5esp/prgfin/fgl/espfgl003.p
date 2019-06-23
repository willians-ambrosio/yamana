/* def var NOME-ARQ as char format "x(35)" label "Arquivo". */

/* assign nome-arq = session:temp-directory + "mapas_ativos.txt".  
update NOME-ARQ with 5 down width 100 side-label 1 col. */

output to 'mapas_ativos.txt'.

MESSAGE "Confira o arquivo MAPA_ATIVOS.txt gerado em sua pasta temp." VIEW-AS ALERT-BOX.

put 'empresa;estabelecimento;mapa;descricao mapa;ccusto;descricao ccusto' skip.

for each mapa_distrib_ccusto no-lock
   where mapa_distrib_ccusto.ind_tip_mapa_distrib_ccusto = 'Lista'
     and mapa_distrib_ccusto.dat_fim_valid              >= today,
    each item_lista_ccusto no-lock
   where item_lista_ccusto.cod_estab               = mapa_distrib_ccusto.cod_estab
     and item_lista_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
     and item_lista_ccusto.cod_empresa             = mapa_distrib_ccusto.cod_empresa
     and item_lista_ccusto.cod_plano_ccusto        = 'PLCCUNI',
   first emsuni.ccusto no-lock
   where emsuni.ccusto.cod_empresa      = item_lista_ccusto.cod_empresa
     and emsuni.ccusto.cod_plano_ccusto = item_lista_ccusto.cod_plano_ccusto
     and emsuni.ccusto.cod_ccusto       = item_lista_ccusto.cod_ccusto:
     put mapa_distrib_ccusto.cod_empresa             ';'
         mapa_distrib_ccusto.cod_estab               ';'
         mapa_distrib_ccusto.cod_mapa_distrib_ccusto ';'
         mapa_distrib_ccusto.des_mapa_distrib_ccusto ';'
         item_lista_ccusto.cod_ccusto                ';'
         emsuni.ccusto.des_tit_ctbl skip.
end.
output close.
