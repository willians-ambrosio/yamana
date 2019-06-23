/**************************************** pe4000.i ******************************************************/
/*                                                                                                      */
/*                 - validar os funcion†rios que devem ou n∆o ser integrados                            */
/*                 - chamar o processo de c†lculo das horas                                             */
/*                                                                                                      */
/********************************************************************************************************/
if bfunc-ponto.cdn_estab <> {prghur/dop/eng012.i} then do:  
   find funcionario no-lock where
        funcionario.cdn_empresa     = bfunc-ponto.cdn_empresa and
        funcionario.cdn_estab       = bfunc-ponto.cdn_estab   and
        funcionario.cdn_funcionario = bfunc-ponto.cdn_funcionario no-error.
   if avail funcionario and funcionario.cdn_sit_calc_func <> 0 and funcionario.cdn_sit_calc_func < 9 then do:
      if can-find(first movto_calcul_func where
                        movto_calcul_func.cdn_empresa      = funcionario.cdn_empresa     and
                        movto_calcul_func.cdn_estab        = funcionario.cdn_estab       and
                        movto_calcul_func.cdn_funcionario  = funcionario.cdn_funcionario and
                        movto_calcul_func.num_ano_refer_fp = tt-param.ano-ref and
                        movto_calcul_func.num_mes_refer_fp = tt-param.mes-ref and
                        movto_calcul_func.idi_tip_fp               = 1 and
                        movto_calcul_func.qti_parc_habilit_calc_fp = 9) and
         tt-param.i-ind-selec <> 3 then do:
         assign v_num_erro = v_num_erro + 1.
         {utp/ut-liter.i C†lculo_normal_da_Folha_j†_efetuado_para_funcion†rio mpe L}
         create tt_erro.
         assign tt_erro.log_erro = yes
                tt_erro.num_erro = v_num_erro
                tt_erro.des_erro = trim(return-value) + " " + 
                string(funcionario.cdn_funcionario,"99999999").
         next.
      end.
   end.
end.

find first bcontrole-funcponto exclusive-lock where
     bcontrole-funcponto.cdn_empresa = bfunc-ponto.cdn_empresa  and
     bcontrole-funcponto.cdn_estab = bfunc-ponto.cdn_estab      and                      
     bcontrole-funcponto.cdn_funcionario = bfunc-ponto.cdn_funcionario  and
     bcontrole-funcponto.num_ano_primei_calc_ptoelet = tt-param.ano-ref and
     bcontrole-funcponto.num_mes_primei_calc_realzdo = tt-param.mes-ref no-error.
if avail bcontrole-funcponto then do:
   assign v_dat_ini_per = bcontrole-funcponto.dat_inic_period_apurac_pto_mes
          v_dat_fim_per = bcontrole-funcponto.dat_term_period_apurac_pto_mes.


   /* Toni EPC - Senac */
   for each tt-epc exclusive-lock
      where tt-epc.cod-event = "instrutor_A":U :
      delete tt-epc. 
   end.
   create tt-epc.
   assign tt-epc.cod-event     = "instrutor_A":U
          tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa     ) + ';' +   
                                 string(bfunc-ponto.cdn_estab       ) + ';' +   
                                 string(bfunc-ponto.cdn_funcionario ) + ';' +   
                                 string(v_dat_ini_per               ) + ';' +
                                 string(v_dat_fim_per               ).

   {include/i-epc201.i "instrutor_A"}
   /* Toni EPC - Senac */

   
   if bfunc-ponto.dat_admis_func > bcontrole-funcponto.dat_inic_period_apurac_pto_mes then
      assign v_dat_ini_per = bfunc-ponto.dat_admis_func.
   if bfunc-ponto.cdn_estab <> {prghur/dop/eng012.i} then do:
      find funcionario of bfunc-ponto no-lock no-error.
      if avail funcionario and funcionario.dat_admis_transf_func > bcontrole-funcponto.dat_inic_period_apurac_pto_mes then
         assign v_dat_ini_per = funcionario.dat_admis_transf_func.
   end.

   if bfunc-ponto.dat_desligto_func < bcontrole-funcponto.dat_term_period_apurac_pto_mes then do:
      find habilit_rescis of bfunc-ponto no-lock no-error.
      if available habilit_rescis and habilit_rescis.dat_inic_aviso_previo = bfunc-ponto.dat_desligto_func and
         habilit_rescis.idi_tip_aviso_previo <> 3 then
         assign v_dat_fim_per = bfunc-ponto.dat_desligto_func - 1.
      else do:
         assign v_log_transf = no.
         for each sit_afast_func of bfunc-ponto no-lock where
                  sit_afast_func.dat_inic_sit_afast = bfunc-ponto.dat_desligto_func:
            find sit_afast no-lock where
                 sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func no-error.
            if avail sit_afast and sit_afast.idi_signif_sit = 4 then do:
               assign v_dat_fim_per = bfunc-ponto.dat_desligto_func - 1
                      v_log_transf = yes.
               leave.
            end.
         end.
         if v_log_transf = no then
            assign v_dat_fim_per = bfunc-ponto.dat_desligto_func.
      end. /* else */
   end. /* dt-desligto */
   else do:
       if month(bfunc-ponto.dat_desligto_func) = tt-param.mes-ref and
          year(bfunc-ponto.dat_desligto_func)  = tt-param.ano-ref then do:
           if bfunc-ponto.dat_desligto_func <> ? and 
              bfunc-ponto.dat_desligto_func  > bcontrole-funcponto.dat_term_period_apurac_pto_mes then
               assign v_dat_fim_per = bfunc-ponto.dat_desligto_func.
       end.
   end.

   assign v_log_nao_calc = no.
   do v_dat_ref = v_dat_ini_per to v_dat_fim_per:

      /*FO 1700.858 - MRS - Verifica se existe cargo maquinista cadastrado **/
      IF CAN-FIND(FIRST cargo_basic NO-LOCK WHERE cargo_basic.log_livre_1 = YES) THEN    
         IF CAN-FIND( func_turno_trab OF bfunc-ponto WHERE 
                      func_turno_trab.dat_inic_lotac_func_turno_trab <= v_dat_ref AND
                      func_turno_trab.dat_term_lotac_func            >= v_dat_ref AND 
                      func_turno_trab.cdn_turno_trab                  = 9000 /* Maqunista */) THEN 
         NEXT.
      /*FO 1700.858 - MRS*/

      if not can-find(first par_marcac_ptoelet of bfunc-ponto where
                      par_marcac_ptoelet.dat_proces_mpe = v_dat_ref) then do:
         assign v_log_nao_calc = yes.
         leave.
      end.
   end.
   if v_log_nao_calc = yes then do:
       assign v_num_erro = v_num_erro + 1.
       {utp/ut-liter.i Per°odo_Ponto_do_funcion†rio mpe L}
       assign v_des_erro1 = trim(return-value) + " " + 
       string(bfunc-ponto.cdn_funcionario) + " ".
       {utp/ut-liter.i n∆o_calculado mpe L}
       assign v_des_erro1 = v_des_erro1 + " " + trim(return-value).
       create tt_erro.
       assign tt_erro.num_erro = v_num_erro
              tt_erro.des_erro = v_des_erro1
              tt_erro.log_erro = yes.
       next.
   end.

   /* aqui */
   if ((bfunc-ponto.dat_desligto_func = ? and
        can-find(first par_marcac_ptoelet of bfunc-ponto where
                     par_marcac_ptoelet.dat_proces_mpe >= bcontrole-funcponto.dat_inic_period_apurac_pto_mes and
                     par_marcac_ptoelet.dat_proces_mpe <= bcontrole-funcponto.dat_term_period_apurac_pto_mes and
                     par_marcac_ptoelet.idi_tip_ocor_mpe = 17) and v_log_verifica_ocor ) OR 
      (bfunc-ponto.dat_desligto_func <> ? AND
       can-find(first par_marcac_ptoelet of bfunc-ponto where
                      par_marcac_ptoelet.dat_proces_mpe >= bcontrole-funcponto.dat_inic_period_apurac_pto_mes and
                      par_marcac_ptoelet.dat_proces_mpe <= bfunc-ponto.dat_desligto_func and
                      par_marcac_ptoelet.idi_tip_ocor_mpe = 17) and v_log_verifica_ocor)) then do:
      assign v_num_erro = v_num_erro + 1.
      {utp/ut-liter.i Funcion†rio mpe L}
      assign v_des_erro1 = trim(return-value) + " " + 
      string(bfunc-ponto.cdn_funcionario) + " ".
      {utp/ut-liter.i possui_ocorrància_de_batida_sem_par_no_per°odo mpe L}
      assign v_des_erro1 = v_des_erro1 + " " + trim(return-value).
      IF bfunc-ponto.dat_desligto_func <> ? THEN
         ASSIGN v_des_erro1 = v_des_erro1 + " " + "calculado".
      create tt_erro.
      assign tt_erro.num_erro = v_num_erro
             tt_erro.des_erro = v_des_erro1
             tt_erro.log_erro = yes.
      next.
   end.

   if can-find(first movto_ptoelet of bfunc-ponto where
               movto_ptoelet.num_ano_refer_fp  = bcontrole-catponto.num_ano_primei_calc_ptoelet and
               movto_ptoelet.num_mes_refer_fp  = bcontrole-catponto.num_mes_primei_calc_realzdo and
               movto_ptoelet.idi_tip_fp_calcul = 1 and
               movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
               movto_ptoelet.idi_tip_movto_ptoelet = 1 AND
               movto_ptoelet.val_livre_1 = 0) then do:
       assign v_num_erro = v_num_erro + 1.
       {utp/ut-liter.i Per°odo_Ponto_do_funcion†rio mpe L}
       assign v_des_erro1 = trim(return-value) + " " + 
       string(bfunc-ponto.cdn_funcionario) + " ".
       {utp/ut-liter.i j†_integrado mpe L}
       assign v_des_erro1 = v_des_erro1 + " " + trim(return-value).
       create tt_erro.
       assign tt_erro.num_erro = v_num_erro
              tt_erro.des_erro = v_des_erro1
              tt_erro.log_erro = yes.
       next.
   end.

   &if "{&dthrtma_version}" >= "2.08" &then
      if can-find(first autoriz_hora_extra_compens where
                        autoriz_hora_extra_compens.cdn_empresa     = bfunc-ponto.cdn_empresa and
                        autoriz_hora_extra_compens.cdn_estab       = bfunc-ponto.cdn_estab   and
                        autoriz_hora_extra_compens.cdn_funcionario = bfunc-ponto.cdn_funcionario and
                        autoriz_hora_extra_compens.dat_proces_mpe >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
                        autoriz_hora_extra_compens.dat_proces_mpe <= bcontrole-catponto.dat_term_period_apurac_pto_mes and
                        autoriz_hora_extra_compens.log_autoriz_aprov = no) then do:
         assign v_num_erro  = v_num_erro + 1
                v_des_erro1 = "Func " + string(bfunc-ponto.cdn_funcionario) + " possui autorizaá‰es".
         create tt_erro.
         assign tt_erro.num_erro = v_num_erro
                tt_erro.des_erro = v_des_erro1 + " n∆o aprovadas p/ o per°odo"
                tt_erro.log_erro = yes.
      end.
   &endif
   
   /* tratamento para validar autorizacoes calculo ponto com base no MOAI e ajustar situacoes nao aprovadas pelo Medicina */

   ASSIGN v-dia  = (bcontrole-funcponto.dat_term_period_apurac_pto_mes - bcontrole-funcponto.dat_inic_period_apurac_pto_mes) + 1
          l-erro = NO.
   DO i-ind = 1 TO v-dia:
      IF SUBSTRING(bcontrole-funcponto.cod_livre_1,i-ind,1) = 'p' OR
         SUBSTRING(bcontrole-funcponto.cod_livre_1,i-ind,1) = 'n' THEN DO:
         assign v_num_erro = v_num_erro + 1.
         {utp/ut-liter.i Existem_dias_n∆o_aprovados_no_Per°odo_Ponto_do_funcion†rio mpe L}
         assign v_des_erro1 = trim(return-value) + " " + 
         string(bfunc-ponto.cdn_funcionario) + " ".
         assign v_des_erro1 = v_des_erro1 + " " + trim(return-value).
         create tt_erro.
         assign tt_erro.num_erro = v_num_erro
                tt_erro.des_erro = v_des_erro1
                tt_erro.log_erro = YES.
         ASSIGN i-ind = v-dia
                l-erro = YES.
      END.
   END.
   IF l-erro = YES THEN NEXT.

   FOR EACH sit_afast NO-LOCK WHERE
       substr(sit_afast.cod_livre_1,29,1) = 'S',
       EACH sit_afast_func of bfunc-ponto EXCLUSIVE-LOCK WHERE
            sit_afast_func.cdn_sit_afast_func       = sit_afast.cdn_sit_afast_func AND
            sit_afast_func.dat_inic_sit_afast      <= bcontrole-funcponto.dat_term_period_apurac_pto_mes AND
            sit_afast_func.dat_term_sit_afast      >= bcontrole-funcponto.dat_inic_period_apurac_pto_mes AND
            substr(sit_afast_func.cod_livre_1,39,1) = 'e':
       FOR EACH par_marcac_ptoelet of bfunc-ponto EXCLUSIVE-LOCK where
           par_marcac_ptoelet.dat_proces_mpe    >= sit_afast_func.dat_inic_sit_afast and
           par_marcac_ptoelet.dat_proces_mpe    <= sit_afast_func.dat_term_sit_afas and
           par_marcac_ptoelet.dat_proces_mpe    >= bcontrole-funcponto.dat_inic_period_apurac_pto_mes AND  
           par_marcac_ptoelet.dat_proces_mpe    <= bcontrole-funcponto.dat_term_period_apurac_pto_mes AND   
           par_marcac_ptoelet.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func:
           FIND bsit_afast WHERE
                bsit_afast.cdn_sit_afast_func = int(substr(sit_afast.cod_livre_1,30,3))
                NO-LOCK NO-ERROR.
           FOR EACH efp_par_marcac_ptoelet OF bfunc-ponto EXCLUSIVE-LOCK WHERE
               efp_par_marcac_ptoelet.dat_proces_mpe             = par_marcac_ptoelet.dat_proces_mpe            AND
               efp_par_marcac_ptoelet.num_horar_inic_proces_mpe >= par_marcac_ptoelet.num_horar_inic_proces_mpe AND
               efp_par_marcac_ptoelet.num_horar_inic_proces_mpe <= par_marcac_ptoelet.num_horar_fim_proces_mpe:
               ASSIGN efp_par_marcac_ptoelet.cdn_sit_afast_func  = bsit_afast.cdn_sit_afast_func.
               IF bfunc-ponto.cdn_categ_sal = 2 THEN DO: /* horista */
                  IF efp_par_marcac_ptoelet.log_hrs_diurno = YES THEN
                     ASSIGN efp_par_marcac_ptoelet.cdn_efp = bsit_afast.cdn_event_diurno_horist.  
                  ELSE
                     ASSIGN efp_par_marcac_ptoelet.cdn_efp = bsit_afast.cdn_event_notur_horist.  
               END.
               ELSE DO: /* mensalista e outros */
                  IF efp_par_marcac_ptoelet.log_hrs_diurno = YES THEN
                     ASSIGN efp_par_marcac_ptoelet.cdn_efp = bsit_afast.cdn_event_afast_diurno.
                  ELSE
                     ASSIGN efp_par_marcac_ptoelet.cdn_efp = bsit_afast.cdn_event_afast_notur.
               END.
           END.
           ASSIGN par_marcac_ptoelet.cdn_sit_afast_func = bsit_afast.cdn_sit_afast_func.
       END.
       ASSIGN substr(sit_afast_func.cod_livre_1,40,3) = string(sit_afast_func.cdn_sit_afast_func)
              sit_afast_func.cdn_sit_afast_func       = bsit_afast.cdn_sit_afast_func.
   END.

   /* fim tratamento - aprovacao MOAI - schossland - 04/2008 */


   run pi-acompanhar in prh_acomp (input string(bfunc-ponto.cdn_funcionario)).

   for each tt-epc exclusive-lock where tt-epc.cod-event = "estagiario":
       delete tt-epc. 
   end.
   create tt-epc.
   &if "{&cd_rel_hr}" >= "2.11" &then  
   assign tt-epc.cod-event = "estagiario"
          tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) +
                                 STRING(bfunc-ponto.cdn_estab) +
                                 STRING(bfunc-ponto.cdn_funcionario).   
   &else  
   assign tt-epc.cod-event = "estagiario"
          tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                 STRING(bfunc-ponto.cdn_estab,"999") +
                                 STRING(bfunc-ponto.cdn_funcionario,"99999999").   
   &endif 

   {include/i-epc201.i "estagiario"}

   /* se for estagiario e entrar na epc, nao chamar o pe4000r1 */
   IF RETURN-VALUE = "ok-estag" THEN.
   ELSE 
      run prghur/pep/pe4000r1.p. 

      FOR EACH movto_ptoelet WHERE
               movto_ptoelet.cdn_empresa                 = "800"           AND 
               movto_ptoelet.cdn_estab                   = "801"           AND 
               movto_ptoelet.cdn_funcionario             = 11915           AND 
               movto_ptoelet.num_ano_refer_fp            = 2019            AND 
               movto_ptoelet.num_mes_refer_fp           >= 01              AND 
               movto_ptoelet.num_mes_refer_fp           <= 02              AND 
               movto_ptoelet.cdn_efp                     = "003"           NO-LOCK:

/*                                                                                               */
/*                 find func_ptoelet no-lock where                                               */
/*                      func_ptoelet.cdn_empresa     = movto_ptoelet.cdn_empresa       and       */
/*                      func_ptoelet.cdn_estab       = movto_ptoelet.cdn_estab         and       */
/*                      func_ptoelet.cdn_funcionario = movto_ptoelet.cdn_funcionario   no-error. */
/*                 IF AVAIL func_ptoelet THEN                                                    */
/*                    MESSAGE "func_ptoelet.dat_desligto_func " func_ptoelet.dat_desligto_func   */
/*                        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                     */

         MESSAGE 
              "saiu do  pe4000r1.p"  SKIP
              "movto_ptoelet.cdn_empresa                          "  movto_ptoelet.cdn_empresa                         skip
              "movto_ptoelet.cdn_estab                            "  movto_ptoelet.cdn_estab                           skip
              "movto_ptoelet.cdn_funcionario                      "  movto_ptoelet.cdn_funcionario                     skip
              "movto_ptoelet.num_ano_refer_fp                     "  movto_ptoelet.num_ano_refer_fp                    skip
              "movto_ptoelet.num_mes_refer_fp                     "  movto_ptoelet.num_mes_refer_fp                    skip
              "movto_ptoelet.idi_tip_fp_calcul                    "  movto_ptoelet.idi_tip_fp_calcul                   skip
              "movto_ptoelet.num_parc_calc_movto_ptoelet          "  movto_ptoelet.num_parc_calc_movto_ptoelet         skip
              "movto_ptoelet.cdn_efp                              "  movto_ptoelet.cdn_efp                             skip
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.


   if bfunc-ponto.dat_desligto_func <> ? then do:
      if not can-find(first movto_ptoelet of bfunc-ponto where
                            movto_ptoelet.num_ano_refer_fp  = tt-param.ano-ref and
                            movto_ptoelet.num_mes_refer_fp  = tt-param.mes-ref and
                            movto_ptoelet.idi_tip_fp_calcul = 1 and
                            movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
                            movto_ptoelet.idi_tip_movto_ptoelet = 1) then do:
          
             assign i-ev-codigo = i-ev-trbdiu
                    i-qtd-hrs   = 0.
             run pi-cria-movto.
          end.
   end.

   assign bcontrole-funcponto.idi_sit_ptoelet_mes_func = 3.


   /** FO 1392.772 - Engenharia Transferencia x Ponto Eletronico **/
   if substring(bfunc-ponto.cod_livre_1,47,1) = "S" and 
      avail b-funciona then do:
      find first bcontrole-funcponto-orig exclusive-lock
           where bcontrole-funcponto-orig.cdn_empresa                 = b-funciona.cdn_empresa
             and bcontrole-funcponto-orig.cdn_estab                   = b-funciona.cdn_estab
             and bcontrole-funcponto-orig.cdn_funcionario             = b-funciona.cdn_funcionario
             and bcontrole-funcponto-orig.num_ano_primei_calc_ptoelet = tt-param.ano-ref
             and bcontrole-funcponto-orig.num_mes_primei_calc_realzdo = tt-param.mes-ref no-error.

      if avail bcontrole-funcponto-orig then do:
         assign bcontrole-funcponto-orig.idi_sit_ptoelet_mes_func = 3.

         find bfunc-ponto-orig of b-funciona no-lock no-error.
         
         find bcatponto-orig no-lock where
              bcatponto-orig.cdn_clas_func = bfunc-ponto-orig.cdn_clas_func and
              bcatponto-orig.cdn_empresa   = b-funciona.cdn_empresa         and
              bcatponto-orig.cdn_estab     = b-funciona.cdn_estab           and
              bcatponto-orig.cdn_categ_sal = bfunc-ponto-orig.cdn_categ_sal no-error.
         
         find b-sit_calc_ptoelet_categ of bcatponto-orig exclusive-lock where
              b-sit_calc_ptoelet_categ.num_mes_primei_calc_realzdo = tt-param.mes-ref and
              b-sit_calc_ptoelet_categ.num_ano_primei_calc_ptoelet = tt-param.ano-ref no-error.
         
         if avail b-sit_calc_ptoelet_categ then
            assign b-sit_calc_ptoelet_categ.idi_sit_ptoelet_categ = bcontrole-funcponto-orig.idi_sit_ptoelet_mes_func.
      end.
   end.
end. /* bcontrole */
