
FOR EACH irf_rendto_mestre WHERE
         irf_rendto_mestre.num_ano_refer_fp    = 2018:

    
      FOR EACH irf_despes_medic WHERE 
               irf_despes_medic.cdn_empresa         = irf_rendto_mestre.cdn_empres_centrdor AND
               irf_despes_medic.cdn_estab           = irf_rendto_mestre.cdn_estab_centrdor  AND
               irf_despes_medic.cod_cpf_cgc         = irf_rendto_mestre.cod_cpf_cgc         AND
               irf_despes_medic.num_ano_refer       = irf_rendto_mestre.num_ano_refer_fp    AND
               irf_despes_medic.idi_tip_inform_dirf = 5                                     AND
               irf_despes_medic.cod_cgc_declar      = irf_rendto_mestre.cod_cgc_declar      AND
               irf_despes_medic.cdn_retenc_irf      = irf_rendto_mestre.cdn_retenc_irf      AND
               irf_despes_medic.cdn_event_fp        = '336'                                 :

          ASSIGN irf_despes_medic.idi_tip_operac = 2.
 
      END.
END.
