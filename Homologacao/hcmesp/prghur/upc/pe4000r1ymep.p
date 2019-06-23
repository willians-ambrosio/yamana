/************************************************************************
** UPC - prghur/upc/pe4000r1ymep.p                                     **
** Programa chamador - prghur/pep/pe4000r1                             **
** Fun‡Æo - Compensar qtde hrs negativas mˆs Bco de Hrs                **
** Cliente - YAMANA                                                    **
** Data - 09/2009                                                      **
*************************************************************************/

{include/i-epc200.i1}
def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

def shared buffer bfunc-ponto         for func_ptoelet.
def shared buffer bcontrole-catponto  for sit_calc_ptoelet_categ.

def buffer bf_bco_hrs_compens_esp  for bco_hrs_compens_func.
def buffer bf2_bco_hrs_compens_esp for bco_hrs_compens_func.

def var v_qtd_descto_hrs       like turno_trab_ext.qti_hrs_descto_bco  no-undo.
def var v_aux_idi_hrs_posit    like bco_hrs_compens_func.idi_hrs_posit no-undo.
def var v_qtd_hrs_aux_term_bco like bco_hrs_compens_func.num_horar_term_mpe no-undo.

find first tt-epc no-lock where
    tt-epc.cod-event = "BCO" no-error.
if avail tt-epc
     and p-ind-event = "BCO" then do:

   for each bco_hrs_compens_func_esp of bfunc-ponto exclusive-lock
      where bco_hrs_compens_func_esp.dat_atualiz_bco_hora < bcontrole-catponto.dat_inic_period_apurac_pto_mes:
       delete bco_hrs_compens_func_esp.
   end.

   for each bf_bco_hrs_compens_esp of bfunc-ponto exclusive-lock
      where bf_bco_hrs_compens_esp.dat_atualiz_bco_hora >= bcontrole-catponto.dat_inic_period_apurac_pto_mes
        and bf_bco_hrs_compens_esp.dat_atualiz_bco_hora <= bcontrole-catponto.dat_term_period_apurac_pto_mes:
       create bco_hrs_compens_func_esp.
       buffer-copy bf_bco_hrs_compens_esp to bco_hrs_compens_func_esp no-error.
   end.

   find first turno_trab_ext no-lock
        where turno_trab_ext.cdn_turno_trab = bfunc-ponto.cdn_turno_trab no-error.
   if avail turno_trab_ext
        and turno_trab_ext.log_descta_hrs_bco then do:

       assign v_qtd_descto_hrs = turno_trab_ext.qti_hrs_descto_bco.

        for each bf_bco_hrs_compens_esp of bfunc-ponto exclusive-lock
          where bf_bco_hrs_compens_esp.cod_mes_ano_refer_fp      = ""
            and bf_bco_hrs_compens_esp.idi_hrs_posit             = 1 /* Positivo */
            and bf_bco_hrs_compens_esp.dat_atualiz_bco_hora     >= bcontrole-catponto.dat_inic_period_apurac_pto_mes
            and bf_bco_hrs_compens_esp.dat_atualiz_bco_hora     <= bcontrole-catponto.dat_term_period_apurac_pto_mes:

           if bf_bco_hrs_compens_esp.qti_hrs_marcac_ptoelet <= v_qtd_descto_hrs then
              assign v_qtd_descto_hrs                        = v_qtd_descto_hrs - bf_bco_hrs_compens_esp.qti_hrs_marcac_ptoelet
                     bf_bco_hrs_compens_esp.idi_hrs_posit    = 3 /* Positivos compensado */.
           else do:

              assign v_qtd_hrs_aux_term_bco                              = bf_bco_hrs_compens_esp.num_horar_term_mpe
                     bf_bco_hrs_compens_esp.num_horar_term_mpe           = bf_bco_hrs_compens_esp.num_horar_inic_mpe + v_qtd_descto_hrs
                     bf_bco_hrs_compens_esp.qti_hrs_marcac_ptoelet       = bf_bco_hrs_compens_esp.num_horar_term_mpe - bf_bco_hrs_compens_esp.num_horar_inic_mpe
                     bf_bco_hrs_compens_esp.qti_hrs_marcac_ptoelet_orig  = bf_bco_hrs_compens_esp.num_horar_term_mpe - bf_bco_hrs_compens_esp.num_horar_inic_mpe
                     v_aux_idi_hrs_posit                                 = bf_bco_hrs_compens_esp.idi_hrs_posit
                     bf_bco_hrs_compens_esp.idi_hrs_posit                = 3 /* Positivos compensado */
                     v_qtd_descto_hrs                                    = 0.

              create bf2_bco_hrs_compens_esp.
              assign bf2_bco_hrs_compens_esp.cdn_empresa                    = bf_bco_hrs_compens_esp.cdn_empresa
                     bf2_bco_hrs_compens_esp.cdn_estab                      = bf_bco_hrs_compens_esp.cdn_estab
                     bf2_bco_hrs_compens_esp.cdn_funcionario                = bf_bco_hrs_compens_esp.cdn_funcionario
                     bf2_bco_hrs_compens_esp.cdn_tip_compcao_hrs            = bf_bco_hrs_compens_esp.cdn_tip_compcao_hrs
                     bf2_bco_hrs_compens_esp.dat_atualiz_bco_hora           = bf_bco_hrs_compens_esp.dat_atualiz_bco_hora
                     bf2_bco_hrs_compens_esp.num_horar_inic_mpe             = bf_bco_hrs_compens_esp.num_horar_term_mpe
                     bf2_bco_hrs_compens_esp.num_horar_term_mpe             = v_qtd_hrs_aux_term_bco
                     bf2_bco_hrs_compens_esp.idi_hrs_posit                  = v_aux_idi_hrs_posit
                     bf2_bco_hrs_compens_esp.qti_hrs_marcac_ptoelet_orig    = bf2_bco_hrs_compens_esp.num_horar_term_mpe - bf2_bco_hrs_compens_esp.num_horar_inic_mpe
                     bf2_bco_hrs_compens_esp.qti_hrs_marcac_ptoelet         = bf2_bco_hrs_compens_esp.num_horar_term_mpe - bf2_bco_hrs_compens_esp.num_horar_inic_mpe
                     bf2_bco_hrs_compens_esp.idi_tratam_lancto_bco_hrs      = bf_bco_hrs_compens_esp.idi_tratam_lancto_bco_hrs
                     bf2_bco_hrs_compens_esp.log_hrs_diurno                 = bf_bco_hrs_compens_esp.log_hrs_diurno
                     bf2_bco_hrs_compens_esp.log_valoriz_bco_hrs_adc_notur  = bf_bco_hrs_compens_esp.log_valoriz_bco_hrs_adc_notur
                     bf2_bco_hrs_compens_esp.log_valoriz_bco_hrs_adc_suplem = bf_bco_hrs_compens_esp.log_valoriz_bco_hrs_adc_suplem
                     bf2_bco_hrs_compens_esp.log_valoriz_bco_hrs_adc_hext   = bf_bco_hrs_compens_esp.log_valoriz_bco_hrs_adc_hext
                     bf2_bco_hrs_compens_esp.num_livre_2                    = bf_bco_hrs_compens_esp.num_livre_2
                     bf2_bco_hrs_compens_esp.cod_tip_dia                    = bf_bco_hrs_compens_esp.cod_tip_dia
                     substr(bf2_bco_hrs_compens_esp.cod_livre_1,1,2)        = substr(bf_bco_hrs_compens_esp.cod_livre_1,1,2).
              leave.
           end.
       end. /* bco */
   end.
end.

return "OK-Yamana-pe4000".



