/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i FM000001 1.00.00.000}  /*** 010000 ***/
/*******************************************************************************
**       Programa: prghur/fmp/fm000001.p
**       Data....: setembro/2009
**       Autor...: DATASUL S.A.
**       Objetivo: Calculo Evento - Adicional de turno FO 1847422
*******************************************************************************/ 
def shared var i-vl-retorno       as dec                  no-undo.
def shared var i-bs-retorno       as dec                  no-undo.
def shared var i-un-retorno       as dec                  no-undo.
def shared var d-sal-hrcalc       as dec                  no-undo.
def shared var i-ev-aux           as char                 no-undo.
def shared var i-bs-aux           as dec extent 5         no-undo.

def shared buffer bfunciona        for funcionario.
def shared buffer bparam_empres_rh for param_empres_rh.
def shared buffer bturno_trab      for turno_trab.

def        buffer bfunc-ponto                   for func_ptoelet.
def        buffer bcateg_sal_esp                for categ_sal.
def        buffer bfunc_turno_trab_esp          for func_turno_trab.
def        buffer bevent_fp_esp                 for event_fp.
def        buffer bemprest_turno_turma_trab_esp for emprest_turno_turma_trab.

/********************* Definicao de Buffer's Locais ***************************/

def var v_cont_dia_mes      as int                                        no-undo.
def var v_cont_dia_turno    as int                                        no-undo.

def var v_ano_refer         like param_empres_rh.num_ano_refer_calc_efetd no-undo.
def var v_mes_refer         like param_empres_rh.num_mes_refer_calc_efetd no-undo.
def var dat_inic_ponto      as  date                          no-undo.
def var dat_fim_ponto       as  date                          no-undo.
def var dt-dia-cal          as  date                          no-undo.


assign i-un-retorno      = 0
       i-vl-retorno      = 0
       i-bs-retorno      = 0. 

find bevent_fp_esp no-lock where
     bevent_fp_esp.cdn_event_fp = i-ev-aux no-error.

find bcateg_sal_esp no-lock where
     bcateg_sal_esp.cdn_empresa   = bfunciona.cdn_empresa and
     bcateg_sal_esp.cdn_estab     = bfunciona.cdn_estab   and
     bcateg_sal_esp.cdn_categ_sal = bfunciona.cdn_categ_sal no-error. 

if bcateg_sal_esp.num_dia_inic_period_pto = 1 then 
   assign dat_inic_ponto = date(bparam_empres_rh.num_mes_refer_calc_efetd,
                           bcateg_sal_esp.num_dia_inic_period_pto,
                           bparam_empres_rh.num_ano_refer_calc_efetd).
else
   if bparam_empres_rh.num_mes_refer_calc_efetd = 1 then
      assign dat_inic_ponto = date(12,
                              bcateg_sal_esp.num_dia_inic_period_pto,
                              bparam_empres_rh.num_ano_refer_calc_efetd - 1).
   else
      assign dat_inic_ponto = date(bparam_empres_rh.num_mes_refer_calc_efetd - 1,
                              bcateg_sal_esp.num_dia_inic_period_pto,
                              bparam_empres_rh.num_ano_refer_calc_efetd).

assign dat_fim_ponto = date(bparam_empres_rh.num_mes_refer_calc_efetd,
                            bcateg_sal_esp.num_dia_fim_period_pto,
                            bparam_empres_rh.num_ano_refer_calc_efetd).
       

/* Verifica se funcinario teve alteracao de turno */
find bfunc-ponto of bfunciona no-error.
if avail bfunc-ponto then do:

   assign v_cont_dia_mes   = 0
          v_cont_dia_turno = 0.

   do dt-dia-cal = dat_inic_ponto to dat_fim_ponto:
         assign v_cont_dia_mes = v_cont_dia_mes + 1.

         find first bemprest_turno_turma_trab_esp of bfunc-ponto 
              where bemprest_turno_turma_trab_esp.dat_inic_alter_horar_turma <= dt-dia-cal 
                and bemprest_turno_turma_trab_esp.dat_fim_alter_horar_turma  >= dt-dia-cal no-lock no-error.
         if avail bemprest_turno_turma_trab_esp then do:
            find first turno_trab_ext no-lock
                 where turno_trab_ext.cdn_turno_trab = bemprest_turno_turma_trab_esp.cdn_turno_trab no-error.
            if avail turno_trab_ext 
                 and turno_trab_ext.log_adc_turno then 
               assign v_cont_dia_turno = v_cont_dia_turno + 1.
         end.
         else do:
            find first bfunc_turno_trab_esp of bfunc-ponto 
                 where bfunc_turno_trab_esp.dat_inic_lotac_func_turno_trab <= dt-dia-cal
                   and bfunc_turno_trab_esp.dat_term_lotac_func >= dt-dia-cal no-lock no-error. 
            if avail bfunc_turno_trab_esp then do:

               find first turno_trab_ext no-lock
                    where turno_trab_ext.cdn_turno_trab = bfunc_turno_trab_esp.cdn_turno_trab no-error.
               if avail turno_trab_ext 
                    and turno_trab_ext.log_adc_turno then 
                  assign v_cont_dia_turno = v_cont_dia_turno + 1.
            end.
         end.
   end.
end.

if v_cont_dia_turno > 0 then do:
   assign i-bs-retorno = i-bs-aux[5].
   if i-bs-retorno > 0 then
      assign i-vl-retorno = (((i-bs-retorno * bevent_fp_esp.val_tax_multcao_val_unit) / v_cont_dia_mes) * v_cont_dia_turno).
end.

return "OK".















