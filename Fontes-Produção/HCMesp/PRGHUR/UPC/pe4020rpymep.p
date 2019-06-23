/**************************************************************************
**                                                                       **
** EPC - altera banco de horas para situa‡Æo anterior a integra‡Æo       **
**                                                                       **
**************************************************************************/
{utp/ut-glob.i}
{include/i-epc200.i1}

DEF INPUT PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

def var v_return            as char                             no-undo.    
def var v_cdn_empresa       like funcionario.cdn_empresa        no-undo.
def var v_cdn_estab         like funcionario.cdn_estab          no-undo.
def var v_cdn_funcionario   like funcionario.cdn_funcionario    no-undo.
def var v_mes_refer         as int                              no-undo.
def var v_ano_refer         as int                              no-undo.

FIND tt-epc NO-LOCK WHERE  
     tt-epc.cod-event = p-ind-event NO-ERROR.
IF AVAILABLE tt-epc THEN DO:
   CASE p-ind-event:
      WHEN "elim_dsr" THEN DO:
            
          assign v_cdn_empresa     = substr(tt-epc.val-parameter,01,03)
                 v_cdn_estab       = substr(tt-epc.val-parameter,04,03)
                 v_cdn_funcionario = int(substr(tt-epc.val-parameter,07,08))
                 v_mes_refer       = int(substr(tt-epc.cod-parameter,01,02))
                 v_ano_refer       = int(substr(tt-epc.cod-parameter,03,04)).

          find first sit_calc_ptoelet_func no-lock
               where sit_calc_ptoelet_func.cdn_empresa                 = v_cdn_empresa    
                 and sit_calc_ptoelet_func.cdn_estab                   = v_cdn_estab      
                 and sit_calc_ptoelet_func.cdn_funcionario             = v_cdn_funcionario
                 and sit_calc_ptoelet_func.num_mes_primei_calc_realzdo = v_mes_refer      
                 and sit_calc_ptoelet_func.num_ano_primei_calc_ptoelet = v_ano_refer no-error.
          if avail sit_calc_ptoelet_func then do:

              for each bco_hrs_compens_func exclusive-lock
                 where bco_hrs_compens_func.cdn_empresa           = sit_calc_ptoelet_func.cdn_empresa    
                   and bco_hrs_compens_func.cdn_estab             = sit_calc_ptoelet_func.cdn_estab      
                   and bco_hrs_compens_func.cdn_funcionario       = sit_calc_ptoelet_func.cdn_funcionario
                   and bco_hrs_compens_func.dat_atualiz_bco_hora >= sit_calc_ptoelet_func.dat_inic_period_apurac_pto_mes 
                   and bco_hrs_compens_func.dat_atualiz_bco_hora <= sit_calc_ptoelet_func.dat_term_period_apurac_pto_mes :

                  delete bco_hrs_compens_func.
              end.

              for each bco_hrs_compens_func_esp exclusive-lock
                 where bco_hrs_compens_func_esp.cdn_empresa           = sit_calc_ptoelet_func.cdn_empresa    
                   and bco_hrs_compens_func_esp.cdn_estab             = sit_calc_ptoelet_func.cdn_estab      
                   and bco_hrs_compens_func_esp.cdn_funcionario       = sit_calc_ptoelet_func.cdn_funcionario
                   and bco_hrs_compens_func_esp.dat_atualiz_bco_hora >= sit_calc_ptoelet_func.dat_inic_period_apurac_pto_mes 
                   and bco_hrs_compens_func_esp.dat_atualiz_bco_hora <= sit_calc_ptoelet_func.dat_term_period_apurac_pto_mes:

                  create bco_hrs_compens_func.
                  buffer-copy bco_hrs_compens_func_esp to bco_hrs_compens_func no-error.

                  delete bco_hrs_compens_func_esp.
              end.
              assign v_return = 'elim_dsr-OK'.
          end. /*avail sit_calc_ptoelet_func*/
      end. /* WHEN "DSR" THEN DO: */
   end case.
end.
