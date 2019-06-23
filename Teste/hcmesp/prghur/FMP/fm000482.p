/********** C lculo Evento 482 - Despesas Assistˆncia M‚dica **************/

def shared var i-vl-retorno   as dec            no-undo.
def shared var i-un-retorno   as dec            no-undo.
/*def shared var i-val-evt      as dec extent 999 no-undo.
def shared var i-un-evt       as dec extent 999 no-undo.*/

DEF SHARED BUFFER bfunciona   FOR funcionario.
DEF SHARED BUFFER bturno_trab FOR turno_trab.
DEF SHARED BUFFER bcontrole   FOR habilit_calc_fp.

DEF VAR d_vl_salario LIKE funcionario.val_salario_atual      NO-UNDO.
DEF VAR d_vl_eventos LIKE funcionario.val_salario_atual      NO-UNDO.
DEF VAR d_vl_difer   LIKE funcionario.val_salario_atual      NO-UNDO.
DEF VAR i-mes-fol    LIKE bcontrole.num_mes_refer_fp_calcula NO-UNDO.
DEF VAR i-ano-fol    LIKE bcontrole.num_ano_refer_fp_calcula NO-UNDO.

def var i-val-evt-483   as dec no-undo.
def var i-val-evt-484   as dec no-undo.

def shared temp-table tt-evt   NO-UNDO
   FIELD cdn_event_fp       LIKE event_fp.cdn_event_fp
   FIELD i-val-evt          AS   DEC
   FIELD i-un-evt           AS   DEC
   INDEX tt-val-evt  AS UNIQUE 
     cdn_event_fp.     

find tt-evt where
    tt-evt.cdn_event_fp = '483' no-error.
if avail tt-evt then
    assign i-val-evt-483 = tt-evt.i-val-evt.

find tt-evt where
    tt-evt.cdn_event_fp = '484' no-error.
if avail tt-evt then
    assign i-val-evt-484 = tt-evt.i-val-evt.

ASSIGN d_vl_salario = IF bfunciona.cdn_categ_sal = 1 /* M */
                      THEN TRUNC(ROUND(bfunciona.val_salario_atual * 0.06,2),2)
                      ELSE TRUNC(ROUND((bfunciona.val_salario_atual * bturno_trab.qtd_hrs_padr_mes_rh) * 0.06,2),2)
       /*d_vl_eventos = i-val-evt[483] + i-val-evt[484]*/
       d_vl_eventos = i-val-evt-483 + i-val-evt-484
       d_vl_difer   = IF d_vl_eventos > d_vl_salario
                      THEN d_vl_eventos - d_vl_salario
                      ELSE 0
       i-vl-retorno = IF d_vl_eventos > d_vl_salario
                      THEN d_vl_salario
                      ELSE d_vl_eventos.

IF d_vl_difer > 0 THEN DO:
    if bcontrole.num_mes_refer_fp_calcula = 12 then
       assign i-mes-fol = 1 
              i-ano-fol = bcontrole.num_ano_refer_fp_calcula + 1.
    else
       assign i-mes-fol = bcontrole.num_mes_refer_fp_calcula + 1
              i-ano-fol = bcontrole.num_ano_refer_fp_calcula.

    FIND movto_fp_control_parc WHERE
         movto_fp_control_parc.cdn_empresa                   = bfunciona.cdn_empresa     AND
         movto_fp_control_parc.cdn_estab                     = bfunciona.cdn_estab       AND
         movto_fp_control_parc.cdn_funcionario               = bfunciona.cdn_funcionario AND
         movto_fp_control_parc.cdn_event_fp                  = "484"                     AND
         movto_fp_control_parc.num_seq_movto_fp_control_parc = 1     EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL movto_fp_control_parc THEN 
        CREATE movto_fp_control_parc.

    assign movto_fp_control_parc.cdn_empresa                   = bfunciona.cdn_empresa
           movto_fp_control_parc.cdn_estab                     = bfunciona.cdn_estab
           movto_fp_control_parc.num_ano_inic_parc_pagto_fp    = i-ano-fol
           movto_fp_control_parc.num_mes_inic_movto_parcdo     = i-mes-fol
           movto_fp_control_parc.cdn_funcionario               = bfunciona.cdn_funcionario
           movto_fp_control_parc.cdn_event_fp                  = "484"
           movto_fp_control_parc.val_calcul_efp                = d_vl_difer
           movto_fp_control_parc.qtd_unid_event_fp             = 0
           movto_fp_control_parc.qti_parc_lancto_movto_parcdo  = 1
           movto_fp_control_parc.qti_parc_consdo_movto_parcdo  = 0
           movto_fp_control_parc.idi_orig_movto_parcdo         = 2 /*"C"*/
           movto_fp_control_parc.cdn_ult_tip_folha_calcul      = 1
           movto_fp_control_parc.num_ult_parc_movto_parcdo     = 9
           movto_fp_control_parc.num_ano_ult_calc_fp           = i-ano-fol
           movto_fp_control_parc.num_mes_ult_calc_movto_parcdo = i-mes-fol
           movto_fp_control_parc.idi_tip_atualiz_movto_parcdo  = 2 /*"I"*/
           movto_fp_control_parc.log_livre_1                   = NO
           movto_fp_control_parc.log_consid_calc_folha_compl   = NO
           movto_fp_control_parc.num_seq_movto_fp_control_parc = 1.
END.
/**********************************************************************************/


