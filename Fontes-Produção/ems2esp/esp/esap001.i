for each tit_ap no-lock
   where tit_ap.cod_empresa         = v_cod_empres_usuar
     and tit_ap.cod_estab          >= tt-param.c-cod-estabel-ini
     and tit_ap.cod_estab          <= tt-param.c-cod-estabel-fim
     and tit_ap.cod_espec_docto    >= tt-param.c-cod-esp-ini
     and tit_ap.cod_espec_docto    <= tt-param.c-cod-esp-fim
     and tit_ap.cod_portador       >= tt-param.i-portador-ini
     and tit_ap.cod_portador       <= tt-param.i-portador-fim
     and tit_ap.cdn_fornecedor     >= tt-param.i-cod-fornec-ini
     and tit_ap.cdn_fornecedor     <= tt-param.i-cod-fornec-fim
     and tit_ap.dat_vencto_tit_ap  >= tt-param.d-dt-vencimen-ini
     and tit_ap.dat_vencto_tit_ap  <= tt-param.d-dt-vencimen-fim
     and tit_ap.log_sdo_tit_ap
     and tit_ap.log_tit_ap_estordo  = no
     and(tit_ap.ind_tip_espec_docto = "Normal"
      or tit_ap.ind_tip_espec_docto = "Imposto Retido"),
   first ems5.fornecedor no-lock
   where ems5.fornecedor.cdn_fornecedor = tit_ap.cdn_fornecedor
    break by {1}
          by {2}:
    run pi-acompanhar in h-acomp(input "Documento: " + tit_ap.cod_tit_ap).

    /* se estiver no bordero entÆo nÆo deve aparecer */
    if  can-find(first item_bord_ap no-lock
                 where item_bord_ap.cod_estab            = tit_ap.cod_estab
                   and item_bord_ap.cod_espec_docto      = tit_ap.cod_espec_docto
                   and item_bord_ap.cod_ser_docto        = tit_ap.cod_ser_docto
                   and item_bord_ap.cdn_fornecedor       = tit_ap.cdn_fornecedor
                   and item_bord_ap.cod_tit_ap           = tit_ap.cod_tit_ap
                   and item_bord_ap.cod_parcela          = tit_ap.cod_parcela
                   and item_bord_ap.ind_sit_item_bord_ap = "em aberto") then next.

    create tt-digita.
    assign tt-digita.id          = lc-i-cont
           tt-digita.sel         = "NAO"
           tt-digita.cod-estabel = tit_ap.cod_estab            
           tt-digita.cod-fornec  = tit_ap.cdn_fornecedor       
           tt-digita.nome-abrev  = ems5.fornecedor.nom_abrev   
           tt-digita.cod-esp     = tit_ap.cod_espec_docto      
           tt-digita.serie       = tit_ap.cod_ser_docto        
           tt-digita.nr-docto    = tit_ap.cod_tit_ap           
           tt-digita.parcela     = tit_ap.cod_parcela          
           tt-digita.portador    = tit_ap.cod_portador         
           tt-digita.dt-vencimen = tit_ap.dat_vencto_tit_ap    
           tt-digita.dt-emissao  = tit_ap.dat_emis_docto       
           tt-digita.valor-saldo = tit_ap.val_sdo_tit_ap       
           tt-digita.vl-original = tit_ap.val_origin_tit_ap.

    assign lc-i-cont = lc-i-cont + 1.
end.
