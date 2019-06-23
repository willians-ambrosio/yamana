/*******************************************************************************
* Programa: es0002.p
* Autor   : Fl vio Capitanio
* Data    : Ma/05 - AFVIEW SERVI€OS EMPRESARIAIS
                     fcapitanio@uol.com,br
                     (11) 9756-8761 (11) 8341-1361
*******************************************************************************/
DEFINE PARAM BUFFER p-table     FOR func_turno_trab.
DEFINE PARAM BUFFER p-old-table FOR func_turno_trab.

DEFINE VARIABLE c-objeto AS CHARACTER      NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(PROGRAM-NAME(3),"~/"),PROGRAM-NAME(3),"~/").
  /*
{utp/ut-glob.i}  /**  Seg Usu rio ***/
/*{utp\utapi019.i} /**** envio de email ******/ */
Find usuar_mestre Where usuar_mestre.cod_usuario = c-seg-usuario No-lock No-error.
*/
If Avail p-table And p-table.cdn_turno_trab = 17
                 And p-table.cdn_turma_trab <= 4 Then
Do:
   Run pi-grava-movto.
 /*  Message "Passei " Skip
        "p-table    "  p-table.cdn_turno_trab     Skip      
        "p-old-table"  p-old-tabLE.cdn_turno_trab Skip
        View-as Alert-box Information.
   */
End.
Else 
   If Avail p-table Then
  Do:
     Find Last func_turno_trab Where
               func_turno_trab.cdn_empresa     = p-table.cdn_empresa And 
               func_turno_trab.cdn_estab       = p-table.cdn_estab    And
               func_turno_trab.cdn_funcionario = p-table.cdn_funcionario And
               func_turno_trab.cdn_turno_trab  = 17 And 
               func_turno_trab.cdn_turma_trab  <= 4 Exclusive-lock No-error.
     If Avail func_turno_trab Then
        Run pi-altera-movto.
  End.

Procedure pi-altera-movto:
    Find event_fp Where  event_fp.cdn_event_fp = "146" No-lock No-error.
    If Not Avail event_fp Then
       Message  "Evento 146  nÆo cadastrado "  View-as Alert-box Information.

    Find Last movto_fp_control_parc Where
              movto_fp_control_parc.cdn_empresa     = p-table.cdn_empresa And
              movto_fp_control_parc.cdn_estab       = p-table.cdn_estab  And
              movto_fp_control_parc.cdn_funcionario = p-table.cdn_funcionario And
              movto_fp_control_parc.cdn_event_fp    = "146" Exclusive-lock No-error.
             /* movto_fp_control_parc.num_seq_movto_fp_control_parc = 1  */
 If Avail movto_fp_control_parc And movto_fp_control_parc.idi_sit_lancto_movto_parc = 1 Then
    Assign movto_fp_control_parc.idi_sit_lancto_movto_par = 2.

End Procedure.
Procedure pi-grava-movto:
    Find event_fp Where  event_fp.cdn_event_fp = "146" No-lock No-error.
    If Not Avail event_fp Then
       Message  "Evento 146  nÆo cadastrado "  View-as Alert-box Information.
   
   Find Last movto_fp_control_parc Where
             movto_fp_control_parc.cdn_empresa     = p-table.cdn_empresa And
             movto_fp_control_parc.cdn_estab       = p-table.cdn_estab  And
             movto_fp_control_parc.cdn_funcionario = p-table.cdn_funcionario And
             movto_fp_control_parc.cdn_event_fp    = "146" Exclusive-lock No-error.
               /* movto_fp_control_parc.num_seq_movto_fp_control_parc = 1  */
   If Avail movto_fp_control_parc And movto_fp_control_parc.idi_sit_lancto_movto_parc = 2 Then
      Assign movto_fp_control_parc.idi_sit_lancto_movto_par = 1.
   Else
      If Not Avail movto_fp_control_parc Then
      Do:
          FIND FIRST param_empres_rh Where 
                     param_empres_rh.cdn_empresa = p-table.cdn_empresa NO-LOCK NO-ERROR. 
          Create movto_fp_control_parc.
          ASSIGN
            movto_fp_control_parc.idi_sit_lancto_movto_par      = 1
            movto_fp_control_parc.cdn_empresa                   = p-table.cdn_empresa
            movto_fp_control_parc.cdn_estab                     = p-table.cdn_estab
            movto_fp_control_parc.cdn_funcionario               = p-table.cdn_funcionario
            movto_fp_control_parc.cdn_event_fp                  = "146"
            movto_fp_control_parc.num_seq_movto_fp_control_parc = 1
            movto_fp_control_parc.num_ano_inic_parc_pagto_fp    = param_empres_rh.num_ano_refer_calc_efetd
            movto_fp_control_parc.num_mes_inic_movto_parcdo     = param_empres_rh.num_mes_refer_calc_efetd
            movto_fp_control_parc.qtd_unid_event_fp             = 0
            movto_fp_control_parc.val_calcul_efp                = 0
            movto_fp_control_parc.cdn_val_unit_fp               = 0
            movto_fp_control_parc.qti_parc_lancto_movto_parcdo  = 99
            movto_fp_control_parc.qti_parc_consdo_movto_parcdo  = 0
            /*    movto_fp_control_parc.idi_tip_atualiz_movto_parcdo  = 2 */
            movto_fp_control_parc.dat_pagto_efet_efp            = ?
            movto_fp_control_parc.val_base_calc_fp              = 0
            /*movto_fp_control_parc.cod_usuar_ult_atualiz         = c-seg-usuario */
            movto_fp_control_parc.dat_ult_atualiz               = Today.


      End.

End Procedure.
