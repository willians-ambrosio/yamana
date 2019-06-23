/*------------------------------------------------------------------------
    Programa: TW-ITEM_DISTRIB_CCUSTO
    Objetivo: Manter a extensao da tabela para que seja replicada na ilha
    Autor...: Eugˆnio A. Marietti [EMA] - DSC
    Data....: Nov/2010
  ----------------------------------------------------------------------*/
{include/i-prgvrs.i tw-item_distrib_ccusto 5.06.00.000}

def param buffer p_table     for item_distrib_ccusto.
def param buffer p_old_table for item_distrib_ccusto.

if  avail p_table then do:
    find first ext_item_distrib_ccusto exclusive-lock
         where ext_item_distrib_ccusto.cod_estab               = p_table.cod_estab
           and ext_item_distrib_ccusto.cod_mapa_distrib_ccusto = p_table.cod_mapa_distrib_ccusto
           and ext_item_distrib_ccusto.cod_unid_negoc          = p_table.cod_unid_negoc
           and ext_item_distrib_ccusto.cod_empresa             = p_table.cod_empresa
           and ext_item_distrib_ccusto.cod_plano_ccusto        = p_table.cod_plano_ccusto
           and ext_item_distrib_ccusto.cod_ccusto              = p_table.cod_ccusto no-error.
    if  not avail ext_item_distrib_ccusto then do:
        create ext_item_distrib_ccusto.
        assign ext_item_distrib_ccusto.cod_estab               = p_table.cod_estab
               ext_item_distrib_ccusto.cod_mapa_distrib_ccusto = p_table.cod_mapa_distrib_ccusto
               ext_item_distrib_ccusto.cod_unid_negoc          = p_table.cod_unid_negoc
               ext_item_distrib_ccusto.cod_empresa             = p_table.cod_empresa
               ext_item_distrib_ccusto.cod_plano_ccusto        = p_table.cod_plano_ccusto
               ext_item_distrib_ccusto.cod_ccusto              = p_table.cod_ccusto no-error.
    end.
    assign ext_item_distrib_ccusto.cod_controle = 1 no-error.
end.

return 'OK'.
