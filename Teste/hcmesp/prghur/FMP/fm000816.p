/**************************************************************************************
**         Sistema : Folha de Pagamento                                              ** 
**         Programa: fm000816                                                        ** 
**           Versao: HR.2.08.000                                                     ** 
**         CLIENTE : Minera‡Æo Fazenda Brasileiro S/A                                ** 
**         Data....: Abr/05  - Alterado em 04.08.05                                  ** 
**         Autor...: Flavio Capitanio - AFVIEW (11) 9756-8761                        ** 
**         Objetivo: Gera o Pagamento do D‚cimo Terceiro(ADTO) no Retorno das f‚rias **                                                   **
**         Banco   : dthrpyc                                                         ** 
**         Tabela  : funcionario                                                     ** 
** Ultima Alteracao:                                                                 ** 
**      Observacao : Sequˆncia de Calculo do evento 816 deve ser maior               ** 
**                   que evento 221 e menor que envento 416 ou seja 5286             **
***************************************************************************************/
/***----------- variaveis de Saida -----------------***/
def shared var i-un-retorno as de                                      no-undo.
def shared var i-vl-retorno as de                                      no-undo.
def shared var i-bs-retorno as de                                      no-undo.

def shared temp-table tt-evt   NO-UNDO
   FIELD cdn_event_fp       LIKE event_fp.cdn_event_fp
   FIELD i-val-evt          AS   DEC
   FIELD i-un-evt           AS   DEC
   INDEX tt-val-evt  AS UNIQUE 
     cdn_event_fp.     

/*------------- variaveis de selecao --------------***/

/*** Buffer na formula de Calculo          ***/
def shared buffer bfunciona       for funcionario.
def buffer bhabilit_ferias for habilit_ferias.
/*DEF SHARED VAR i-val-evt AS DEC EXTENT 999 NO-UNDO.*/

/*-------------- Variaveis Locais   -----------*/
def var dd-afas as i.
{utp/ut-glob.i}  /**  Seg Usu rio ***/
/*------------------ Verifica a Situa‡Æo de Ferias ----------------------*/
/*
find last bhabilit_ferias of bfunciona no-error.
If Not Avail bhabilit_ferias Then Return.
    /*
     message  "Funcionario " bfunciona.cdn_funcionario skip
              "Dias de Afastamento " dd-afas   skip
              bhabilit_ferias.dat_term_period_aqst_ferias skip 
              "Periodo Aquis " bhabilit_ferias.dat_inic_period_aqst_ferias skip
              "Periodo Aquis Fim " bhabilit_ferias.dat_term_period_aqst_ferias skip
              view-as alert-box information. 
*/
*/
FIND FIRST param_empres_rh Where 
           param_empres_rh.cdn_empresa = bfunciona.cdn_empresa NO-LOCK NO-ERROR. 
       
Find Last sit_afast_func of bfunciona WHERE 
          sit_afast_func.cdn_sit_afast_func = 90 and 
          year(dat_term_sit_afast)  =  param_empres_rh.num_ano_refer_calc_efetd And
          Month(dat_term_sit_afast) =  param_empres_rh.num_mes_refer_calc_efetd No-lock No-error.

If Not Avail sit_afast_func Then Return.

message  "Funcionario " bfunciona.cdn_funcionario skip
         "Dias de Afastamento " dd-afas   skip
         "Codigo de Situa‡Æo "  cdn_sit_afast_func skip
         "Data Termino F‚rias " dat_term_sit_afast skip
         view-as alert-box information.
/************************** Calculo do evento ******************************/ 

Find First movto_fp_control_parc Where 
     movto_fp_control_parc.cdn_empresa     = bfunciona.cdn_empresa           And
     movto_fp_control_parc.cdn_estab       = bfunciona.cdn_estab             And
     movto_fp_control_parc.cdn_funcionario = bfunciona.cdn_funcionario       And
     movto_fp_control_parc.cdn_event_fp    = "416"                             And
     movto_fp_control_parc.num_seq_movto_fp_control_parc = 1                 And
     movto_fp_control_parc.num_ano_inic_parc_pagto_fp    = param_empres_rh.num_ano_refer_calc_efetd And
     movto_fp_control_parc.num_mes_inic_movto_parcdo     = param_empres_rh.num_mes_refer_calc_efetd
     Exclusive-lock No-error.
  
find tt-evt where
    tt-evt.cdn_event_fp = '221' no-error.
if avail tt-evt AND
    tt-evt.i-val-evt > 0 And Not Avail movto_fp_control_parc THEN DO:

/*If i-val-evt[221] > 0 And Not Avail movto_fp_control_parc Then
Do:*/
  Find Last histor_sal_func Of bfunciona No-lock No-error.
  Assign 
  i-bs-retorno = histor_sal_func.val_salario_mensal
  i-vl-retorno = Trunc(Round(i-bs-retorno / 2,2),2) 
  i-un-retorno = 0.
  Create movto_fp_control_parc.
  ASSIGN
  movto_fp_control_parc.idi_sit_lancto_movto_par      = 1
  movto_fp_control_parc.cdn_empresa                   = bfunciona.cdn_empresa
  movto_fp_control_parc.cdn_estab                     = bfunciona.cdn_estab
  movto_fp_control_parc.cdn_funcionario               = bfunciona.cdn_funcionario
  movto_fp_control_parc.cdn_event_fp                  = "416"
  movto_fp_control_parc.num_seq_movto_fp_control_parc = 1
  movto_fp_control_parc.num_ano_inic_parc_pagto_fp    = param_empres_rh.num_ano_refer_calc_efetd
  movto_fp_control_parc.num_mes_inic_movto_parcdo     = param_empres_rh.num_mes_refer_calc_efetd
  movto_fp_control_parc.qtd_unid_event_fp             = 0
  movto_fp_control_parc.val_calcul_efp                = i-vl-retorno
  movto_fp_control_parc.cdn_val_unit_fp               = 0
  movto_fp_control_parc.qti_parc_lancto_movto_parcdo  = 1
  movto_fp_control_parc.qti_parc_consdo_movto_parcdo  = 0
  movto_fp_control_parc.num_gerac_movto_vale_transp   = 0
  movto_fp_control_parc.idi_orig_movto_parcdo         = 1
  movto_fp_control_parc.idi_sit_lancto_movto_parc     = 1
  movto_fp_control_parc.cdn_ult_tip_folha_calcul      = 0
  movto_fp_control_parc.num_ult_parc_movto_parcdo     = 0
  movto_fp_control_parc.num_ano_ult_calc_fp           = 0
  movto_fp_control_parc.num_mes_ult_calc_movto_parcdo = 0
  movto_fp_control_parc.idi_tip_atualiz_movto_parcdo  = 1
  movto_fp_control_parc.dat_pagto_efet_efp            = ?
  movto_fp_control_parc.val_base_calc_fp              = 0
  movto_fp_control_parc.num_parc_movto_benefic        = 0
  movto_fp_control_parc.cod_usuar_ult_atualiz         = c-seg-usuario
  movto_fp_control_parc.dat_ult_atualiz               = TODAY
/*movto_fp_control_parc.hra_ult_atualiz               = */
  movto_fp_control_parc.cod_livre_1                   = "               N"
  movto_fp_control_parc.cod_livre_2                   = ""
  movto_fp_control_parc.dat_livre_1                   = ?
  movto_fp_control_parc.dat_livre_2                   = ?
  movto_fp_control_parc.log_livre_1                   = NO
  movto_fp_control_parc.log_livre_2                   = NO
  movto_fp_control_parc.num_livre_1                   = 0
  movto_fp_control_parc.num_livre_2                   = 0
  movto_fp_control_parc.val_livre_1                   = 0
  movto_fp_control_parc.val_livre_2                   = 0
  movto_fp_control_parc.cod_rh_ccusto                 = ""
  movto_fp_control_parc.log_consid_calc_folha_compl   = NO
  movto_fp_control_parc.cod_tip_mdo                   = "".
 /*  20 201 835504 416 1 2004 3 0 1500 0 1 0 0 1 1 0 0 0 0 1 ? 0 0 "super" 08/08/05 "114847" "               N" "" ? ? no no 0 0 0 0 "" no ""
  */                                                     
       message 
              "Gerado Movimento de Controle de Parcela " skip 
              "Funcionario " bfunciona.cdn_funcionario skip
              "Dias de Afastamento " dd-afas   skip
              "Codigo de Situa‡Æo "  cdn_sit_afast_func skip
              /*"Periodo Aquis " bhabilit_ferias.dat_inic_period_aqst_ferias skip
              "Periodo Aquis Fim " bhabilit_ferias.dat_term_period_aqst_ferias skip */
              "Data Termino F‚rias " dat_term_sit_afast skip
              "Valor Gerado      " i-vl-retorno
              view-as alert-box information.

End.

/*-- fm000816.p ---*/
