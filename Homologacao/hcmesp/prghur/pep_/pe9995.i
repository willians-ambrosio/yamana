/*******************************************************************************************
 **                                                                                       **
 **                                                                                       **
 **   INCLUDE:                                                                            **
 **                                                                                       **
 **   PROCEDIMENTO: leitura da lota‡Æo de turno/turma/intervalo de um dia determinado     **
 **                                                                                       **
 **   PAR¶METROS: {1} - Data da ocorrˆncia                                                **
 **                                                                                       **
 **                                                                                       **
 *******************************************************************************************/

assign v_dat_aux_proc = {1}.
&if "{2}" <> "no" &THEN
 IF NOT AVAIL bfunc-ponto THEN                           
    FIND bfunc-ponto OF funcionario NO-LOCK NO-ERROR. 
&endif
  


































IF CAN-FIND (det_calend_func NO-LOCK WHERE
             det_calend_func.cdn_empresa      = bfunc-ponto.cdn_empresa AND
             det_calend_func.cdn_estab        = bfunc-ponto.cdn_estab AND
             det_calend_func.cdn_funcionario  = bfunc-ponto.cdn_funcionario AND
        /*     det_calend_func.cdn_turno_trab   = v_cdn_turno and
             det_calend_func.cdn_turma_trab   = v_cdn_turma and
             det_calend_func.cod_pais         = v_cod_pais  and
             det_calend_func.cdn_localidade   = v_cdn_localid and */
             det_calend_func.dat_refer_calend = v_dat_aux_proc) THEN DO:
   FIND det_calend_func NO-LOCK WHERE
        det_calend_func.cdn_empresa      = bfunc-ponto.cdn_empresa AND
        det_calend_func.cdn_estab        = bfunc-ponto.cdn_estab AND
        det_calend_func.cdn_funcionario  = bfunc-ponto.cdn_funcionario AND
    /*    det_calend_func.cdn_turno_trab   = v_cdn_turno and
        det_calend_func.cdn_turma_trab   = v_cdn_turma and
        det_calend_func.cod_pais         = v_cod_pais  and
        det_calend_func.cdn_localidade   = v_cdn_localid and */
        det_calend_func.dat_refer_calend = v_dat_aux_proc NO-ERROR.
   ASSIGN v_cdn_jorn   = det_calend_func.cdn_jorn_trab
          v_cdn_interv = det_calend_func.num_livre_1
          v_log_achou  = YES.
   ASSIGN v_cdn_turno   = det_calend_func.cdn_turno_trab  
          v_cdn_turma   = det_calend_func.cdn_turma_trab  
          v_cod_pais    = det_calend_func.cod_pais        
          v_cdn_localid = det_calend_func.cdn_localidade.
   if can-find(last alter_jorn_trab of bfunc-ponto where
                    alter_jorn_trab.dat_inic_alter_jorn_trab <= v_dat_aux_proc and
                    alter_jorn_trab.dat_term_alter_jorn_trab >= v_dat_aux_proc) then do:
      find last alter_jorn_trab of bfunc-ponto NO-LOCK where
                alter_jorn_trab.dat_inic_alter_jorn_trab <= v_dat_aux_proc and
                alter_jorn_trab.dat_term_alter_jorn_trab >= v_dat_aux_proc no-error.
      assign v_cdn_jorn   = alter_jorn_trab.cdn_jorn_trab
             v_cdn_interv = alter_jorn_trab.cdn_interv_refei.
   end.
END.
ELSE DO:

   find last func_localid of bfunc-ponto no-lock where
             func_localid.dat_inic_lotac_func <= v_dat_aux_proc no-error.
   if avail func_localid then
      assign v_cod_pais    = func_localid.cod_pais
             v_cdn_localid = func_localid.cdn_localidade.
   else do:
      assign v_cod_pais    = ""
             v_cdn_localid = 0.
   end.

   if can-find(last emprest_turno_turma_trab of bfunc-ponto where
                    emprest_turno_turma_trab.dat_inic_alter_horar_turma <= v_dat_aux_proc and
                    emprest_turno_turma_trab.dat_fim_alter_horar_turma  >= v_dat_aux_proc) then do:
      find last emprest_turno_turma_trab of bfunc-ponto no-lock where
                emprest_turno_turma_trab.dat_inic_alter_horar_turma <= v_dat_aux_proc and
                emprest_turno_turma_trab.dat_fim_alter_horar_turma  >= v_dat_aux_proc no-error.
      assign v_cdn_turno       = emprest_turno_turma_trab.cdn_turno_trab
             v_cdn_turma       = emprest_turno_turma_trab.cdn_turma_trab
             v_log_emprest_1   = yes.
   end.
   else do:
      find last func_turno_trab of bfunc-ponto no-lock where
                func_turno_trab.dat_inic_lotac_func_turno_trab <= v_dat_aux_proc no-error.
      if avail func_turno_trab then
         assign v_cdn_turno     = func_turno_trab.cdn_turno_trab
                v_cdn_turma     = func_turno_trab.cdn_turma_trab
                v_log_emprest_1 = no.
      else do:
         assign v_cdn_turno     = 0
                v_cdn_turma     = 0
                v_log_emprest_1 = no.
      end.
   end.

   ASSIGN v_log_achou = NO.

   find det_calend_turma_localid where
        det_calend_turma_localid.cdn_turno_trab   = v_cdn_turno and
        det_calend_turma_localid.cdn_turma_trab   = v_cdn_turma and
        det_calend_turma_localid.dat_refer_calend = v_dat_aux_proc and
        det_calend_turma_localid.cod_pais         = v_cod_pais and  
        det_calend_turma_localid.cdn_localidade   = v_cdn_localid no-lock no-error.
   IF AVAIL det_calend_turma_localid THEN DO:
      assign v_cdn_jorn  = det_calend_turma_localid.cdn_jorn_trab
             v_log_achou = YES.
   END.


   if can-find(last alter_jorn_trab of bfunc-ponto where
                    alter_jorn_trab.dat_inic_alter_jorn_trab <= v_dat_aux_proc and
                    alter_jorn_trab.dat_term_alter_jorn_trab >= v_dat_aux_proc) then do:
      find last alter_jorn_trab of bfunc-ponto NO-LOCK where
                alter_jorn_trab.dat_inic_alter_jorn_trab <= v_dat_aux_proc and
                alter_jorn_trab.dat_term_alter_jorn_trab >= v_dat_aux_proc no-error.
      assign v_cdn_jorn   = alter_jorn_trab.cdn_jorn_trab
             v_cdn_interv = alter_jorn_trab.cdn_interv_refei.
   end.
   else do:
      if v_log_achou then do:

         if v_log_emprest_1 = yes then do:
            find last emprest_interv_jorn of bfunc-ponto no-lock where
                      emprest_interv_jorn.cdn_turno_trab = v_cdn_turno and
                      emprest_interv_jorn.cdn_turma_trab = v_cdn_turma and
                      emprest_interv_jorn.cdn_jorn_trab  = v_cdn_jorn and
                      emprest_interv_jorn.dat_inic_alter_interv <= v_dat_aux_proc no-error.
            if avail emprest_interv_jorn then
               assign v_cdn_interv = emprest_interv_jorn.cdn_interv_refei.
            else
               assign v_cdn_interv = 0.
         end.
         else do:









            find last func_lotac_interv_refei of bfunc-ponto no-lock where
                      func_lotac_interv_refei.cdn_turno_trab = v_cdn_turno and
                      func_lotac_interv_refei.cdn_turma_trab = v_cdn_turma and
                      func_lotac_interv_refei.cdn_jorn_trab  = v_cdn_jorn  and
                      func_lotac_interv_refei.dat_inic_interv_turno_trab <= v_dat_aux_proc no-error.
            if avail func_lotac_interv_refei then
               assign v_cdn_interv = func_lotac_interv_refei.cdn_interv_refei.
            else
               assign v_cdn_interv = 0.
         END.
      end.
   end.
end.
