/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i PE4000R4 1.02.14.048 } /*** 0101448 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i pe4000r4 MPE}
&ENDIF

/******************************************************************************
**       Programa: PE4000R4.P
******************************************************************************/
{include/i_cdrel_hr.i}

{prghur/pep/pe4000tt.i SHARED}
FIND FIRST tt-param NO-LOCK.

def new shared var dt-aux-sitini      as date no-undo.
def new shared var dt-aux-iniss       as date no-undo.
def new shared var dt-aux-sitfim      as date no-undo. 
def new shared var dat-inic-afames    as date no-undo.
def new shared var v_dat_transf       as date format "99/99/9999"     no-undo.
def new shared var v_cdn_sit_transf like sit_afast.cdn_sit_afast_func no-undo.

def shared var l-desligado         as log no-undo. /* cris func demit */
def shared var d-qtd-hrs-diu       as dec no-undo.
def shared var d-qtd-hrs-not       as dec no-undo.
def shared var d-qtd-hrs-diu-dsr   as dec no-undo.
def shared var d-qtd-hrs-not-dsr   as dec no-undo.
def shared var d-qtd-hrs-diu-fer   as dec no-undo.
def shared var d-qtd-hrs-not-fer   as dec no-undo.
def shared var d-qtd-trb-sem       as dec no-undo.
def shared var d-qtd-dsr-sem       as dec no-undo.
def shared var d-qtd-trb-mes       as dec no-undo.
def shared var d-qtd-dsr-mes       as dec no-undo.
def shared var i-qtd-dsr-per       as int no-undo.
def shared var d-qtd-fer-mes       as dec no-undo.
def shared var i-qtd-fer-per       as int no-undo.
def shared var d-hrs-diu-dsr-per   as dec no-undo.
def shared var d-hrs-not-dsr-per   as dec no-undo.         
def shared var d-hrs-diu-fer-per   as dec no-undo.
def shared var d-hrs-not-fer-per   as dec no-undo.         
def shared var d-hrs-diu-dsr-sit   as dec no-undo.
def shared var d-hrs-not-dsr-sit   as dec no-undo.         
def shared var d-hrs-diu-fer-sit   as dec no-undo.
def shared var d-hrs-not-fer-sit   as dec no-undo.         
def shared var d-hrs-diu-trb-sit   as dec no-undo.
def shared var d-hrs-not-trb-sit   as dec no-undo.
def shared var d-hrs-diu-trb-adm   as dec no-undo.
def shared var d-hrs-not-trb-adm   as dec no-undo.
def shared var d-hrs-diu-dsr-adm   as dec no-undo.
def shared var d-hrs-not-dsr-adm   as dec no-undo.
def shared var d-hrs-diu-fer-adm   as dec no-undo.
def shared var d-hrs-not-fer-adm   as dec no-undo.
def shared var d-hrs-diu-trb-dem   as dec no-undo.
def shared var d-hrs-not-trb-dem   as dec no-undo.
def shared var d-hrs-diu-dsr-dem   as dec no-undo.
def shared var d-hrs-not-dsr-dem   as dec no-undo.
def shared var d-hrs-diu-fer-dem   as dec no-undo.
def shared var d-hrs-not-fer-dem   as dec no-undo.
def shared var d-tot-hrs-trb       as dec no-undo.
def shared var d-tot-hrs-dsr       as dec no-undo.
def shared var d-tot-hrs-fer       as dec no-undo.
def shared var d-tot-hrs-sit       as dec no-undo.
def shared var d-tot-hrs-trb-diu   as dec no-undo.
def shared var d-tot-hrs-trb-not   as dec no-undo.
def shared var d-tot-hrs-sup-not   as dec no-undo.
def shared var d-tot-hrs-dsr-diu   as dec no-undo.
def shared var d-tot-hrs-dsr-not   as dec no-undo.
def shared var d-tot-hrs-fer-diu   as dec no-undo.
def shared var d-tot-hrs-fer-not   as dec no-undo.
def shared var d-hrs-fer-mes-diu   as dec no-undo.
def shared var d-hrs-fer-mes-not   as dec no-undo.     
def shared var d-hrs-sup-sit-not   as dec no-undo.
def shared var d-tot-hrsmes        as dec no-undo.
def shared var d-hrs-afast-diu     as dec no-undo. /* cris afast */
def shared var d-hrs-afast-not     as dec no-undo. /* cris afast */
def shared var d-hrs-afast         as dec no-undo. /* cris afast */
def shared var d-hrs-dsr-afast-diu as dec no-undo. /* cris afast */
def shared var d-hrs-dsr-afast-not as dec no-undo. /* cris afast */
def shared var d-hrs-fer-afast-diu as dec no-undo. /* cris afast */
def shared var d-hrs-fer-afast-not as dec no-undo. /* cris afast */
def shared var i-aux-min-atraso    as int no-undo.
def shared var i-aux-qtd-atraso    as int no-undo.
def shared var i-aux-qtd-atrasos   as int no-undo.
def shared var i-tot-hrs-neg       as int no-undo.
def shared var i-aux-hrs-neg       as int no-undo.
def shared var i-tot-hrs-pos       as int no-undo.
def shared var v_num_dif_1         as int no-undo.
def shared var v_num_dif_2         as int no-undo.
def shared var i-ev-supnot         like event_fp.cdn_event_fp no-undo.
def shared var l-sup-evnot         as log no-undo.
def shared var i-ev-trbdiu         like event_fp.cdn_event_fp no-undo.
def shared var i-ev-trbnot         like event_fp.cdn_event_fp no-undo.
def shared var i-ev-dsrdiu         like event_fp.cdn_event_fp no-undo.
def shared var i-ev-dsrnot         like event_fp.cdn_event_fp no-undo.
def shared var i-ev-dsrdiuper      like event_fp.cdn_event_fp no-undo.
def shared var i-ev-dsrnotper      like event_fp.cdn_event_fp no-undo.
def shared var i-ev-ferdiuper      like event_fp.cdn_event_fp no-undo.
def shared var i-ev-fernotper      like event_fp.cdn_event_fp no-undo.
def shared var i-ev-desc-hrs-diu   like event_fp.cdn_event_fp no-undo.
def shared var i-ev-desc-hrs-not   like event_fp.cdn_event_fp no-undo.
DEF SHARED VAR i-ev-hrs-dif-diu    like event_fp.cdn_event_fp NO-UNDO.
DEF SHARED VAR i-ev-hrs-dif-not    like event_fp.cdn_event_fp NO-UNDO.
def shared var c-mmaa-folha        like bco_hrs_compens_func.cod_mes_ano_refer_fp no-undo.
def shared var i-ult-diames        as int no-undo.
def shared var dat-ini-mes         as date no-undo.
def shared var dat-fim-mes         as date no-undo.
def shared var v_num_erro          as int  no-undo.
def shared var v_log_fecha         as log  no-undo.
def shared var l-afast-mes         as log  no-undo.
def shared var v_hrs_sit_not_per   as dec  no-undo.
DEF SHARED VAR v_log_mes_seg       AS LOG  NO-UNDO.
DEF SHARED VAR v_log_calend_func   AS LOG  NO-UNDO.
def shared var l-afast-periodo     as log  no-undo.

def shared var v_log_param_modul_agric as log no-undo.
def shared var v_log_sind_suplem       as log no-undo.

def var dt-dia-cal                 as date no-undo.
def var dt-aux-adm                 as date no-undo.
def var v_row_pri_evt              as rowid no-undo.
def var v_qti_extra                as int no-undo.
def var i-ev-codigo                like event_fp.cdn_event_fp no-undo.
def var i-ev-percdsr               like event_fp.cdn_event_fp no-undo.
def var i-qtd-hrs                  like movto_ptoelet.qtd_movto_ptoelet no-undo.
def var i-qtd-hrs-aux              as dec no-undo.
def var i-qtd-sup-hrs-not          like movto_ptoelet.qtd_movto_ptoelet no-undo.
def var v_cdn_evto_sup_not         like event_fp.cdn_event_fp no-undo.
def var i-qtd-dia                  as int no-undo.
def var i-semana                   as int no-undo. 
def var dt-aux-ini                 as date no-undo.
def var dt-aux-fim                 as date no-undo.
def var i-qtd-hrs-hpc-seg          as int no-undo.
def var d-per-calc-diu             as dec no-undo.
def var d-per-calc-not             as dec no-undo.
def var l-tratada-situa            as log no-undo.
def var i-tot-hra-diu              as int no-undo.
def var i-tot-hra-not              as int no-undo.
def var i-tot-hra-sit              as int no-undo.
def var i-ini-interv               as int no-undo.
def var i-ini-intclc               as int no-undo.
def var i-fim-interv               as int no-undo.
def var i-fim-intclc               as int no-undo.
def var i-interv-diu               as int no-undo.
def var i-interv-not               as int no-undo.
def var i-tot-hra-extra            as int no-undo.
def var i-dif-hrs-pag              as int no-undo.
def var i-dif-qtd-hrs              as int no-undo.
def var i-dia-sema                 as int no-undo.
def var i-hr-diurna                as dec no-undo.
def var i-hr-noturna               as dec no-undo.
def var i-tot-hr-dia               as dec no-undo.
def var i-tot-aux                  as dec no-undo.
def var c-tp-dia                   as char format "x(02)"     no-undo.
def var v_num_min_1                as int format "999999"     no-undo.
def var v_num_min_2                as int format "999999"     no-undo.
def var v_num_hora_1               as int format "9999999999" no-undo.
def var v_num_hora_2               as int format "9999999999" no-undo.
def var v_num_hrs_neg              as int format "9999999999" no-undo.
def var v_num_hrs_pos              as int format "9999999999" no-undo.
def var i-hextra-aux               as int format "9999999"    no-undo.
def var i-val-aux                  as dec  no-undo.
def var i-tot-rat                  as dec  no-undo.
def var d-aux-rat                  as dec  no-undo.
def var d-aux-tot-rat              as dec  no-undo.
def var d-dif-aux                  as dec  no-undo.
def var i-int-hra-diu              as int  no-undo.
def var i-int-hra-not              as int  no-undo.
def var i-prilan-diu               as int  no-undo.
def var i-prilan-not               as int  no-undo.
def var i-seglan-diu               as int  no-undo.
def var i-seglan-not               as int  no-undo.
def var hra-aux-ini-sit            as int  no-undo.
def var hra-aux-fim-sit            as int  no-undo.
def var hra-ini-seglan             as int  no-undo.
def var hra-fim-seglan             as int  no-undo.
def var hra-ini-prilan             as int  no-undo.
def var hra-fim-prilan             as int  no-undo.
def var hra-aux-ini-int            as int  no-undo.
def var hra-aux-fim-int            as int  no-undo.
def var dt-ini-sit3                as date no-undo.
def var dt-fim-sit3                as date no-undo.
def var i-hra-ini                  as int  no-undo.
def var i-hra-fim                  as int  no-undo.
def var d-tot-hrs-not              as dec  no-undo.
def var i-ult-semant               as int  no-undo.
def var v_dat_fim_sem              as date format "99/99/9999"       no-undo. /* cris */
def var i-qtd-movto              like efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet no-undo.
DEF VAR i-qtd-min                  AS INT                            NO-UNDO.
def var v_log_justif               as log                            no-undo. /* cris */
def var v_des_erro                 as char format "x(70)"            no-undo.
def var v_qti_tot_dia              as int                            no-undo.
def var v_qtd_diurna_dia           as dec                            no-undo.
def var v_log_func_ant             as log initial no                 no-undo.
def var v_cdn_emp_func             like func_ptoelet.cdn_empresa     no-undo.
def var v_cdn_estab_func           like func_ptoelet.cdn_estab       no-undo.
def var v_cdn_func_func            like func_ptoelet.cdn_funcionario no-undo.
def var v_qtd_hrs_sit_sup          as dec                            no-undo.

def var v_qtd_hrs_diu_desc_dsr     as dec                            no-undo.
def var v_qtd_hrs_not_desc_dsr     as dec                            no-undo.
def var v_cdn_event_desc_diu       like event_fp.cdn_event_fp        no-undo.
def var v_cdn_event_desc_not       like event_fp.cdn_event_fp        no-undo.
def var v_log_perde_dsr            as log                            no-undo.
def var v_log_perde_dsr_falta      as log                            no-undo.
def var v_log_influi               as log                            no-undo.
DEF VAR v_dat_final_semana         AS DATE format "99/99/9999"       no-undo.
DEF VAR v_num_seq_sit              AS INT                            NO-UNDO.
def var v_log_movto_extra          as log                            no-undo.
def var v_log_ferias               as logical                        no-undo.
def var v_log_dsr_re_fe            as log init no                    no-undo.

DEF VAR v_cdn_empresa_dsr     LIKE funcionario.cdn_empresa     NO-UNDO.
DEF VAR v_cdn_estab_dsr       LIKE funcionario.cdn_estab       NO-UNDO.
DEF VAR v_cdn_funcionario_dsr LIKE funcionario.cdn_funcionario NO-UNDO.

/***** cris - sit dia **********************************************************/
def var v_dat_aux_proc    as date format "99/99/9999"                  no-undo.
def var v_dat_fim_localid as date format "99/99/9999"                  no-undo.
def var v_dat_fim_emprest as date format "99/99/9999"                  no-undo.
def var v_dat_fim_lotac   as date format "99/99/9999"                  no-undo.
def var v_cod_pais        like localidade.cod_pais                     no-undo.
def var v_cdn_localid     like localidade.cdn_localidade               no-undo.
def var v_cdn_turno       like turno_trab.cdn_turno_trab               no-undo.
def var v_cdn_turma       like turma_trab.cdn_turma_trab               no-undo.
def var v_cdn_jorn        like jorn_trab.cdn_jorn_trab                 no-undo.
def var v_cdn_interv      like interv_refei_jorn_trab.cdn_interv_refei no-undo.
def var v_log_emprest     as logical                                   no-undo.
def var v_log_emprest_1   as logical                                   no-undo.
def var v_dat_aux_lotac   as date format "99/99/9999"                  no-undo.
DEF VAR v_dat_process_ant AS DATE FORMAT "99/99/9999"                  NO-UNDO.
DEF VAR v_num_horar_term_ant AS INT                                    NO-UNDO.
DEF VAR i-ev-cod-ant      like event_fp.cdn_event_fp                   NO-UNDO.
DEF VAR v_log_achou       AS LOG                                       NO-UNDO.
def var v_log_desctou_dsr as log                                       no-undo.
def var dt-aux            as date                                      no-undo.
DEF VAR v_qtd_lim_minut_atraso AS DEC                                  NO-UNDO.

/* variaveis para o especifico da Protege */
DEF VAR v_cdn_evt_490   LIKE event_fp.cdn_event_fp                     NO-UNDO.
DEF VAR v_cdn_evt_495   LIKE event_fp.cdn_event_fp                     NO-UNDO.
DEF VAR v_cdn_evt_995   LIKE event_fp.cdn_event_fp                     NO-UNDO.
DEF VAR v_cdn_evt_996   LIKE event_fp.cdn_event_fp                     NO-UNDO.

def var v_num_hra_ini_not as int format "999999" init 79200 no-undo.
def var v_num_hra_fim_not as int format "999999" init 18000 no-undo.

def shared temp-table tt_erro
    field num_erro as int  format ">>>>>>>>>9"
    field des_erro as char format "x(70)"
    field log_erro as log.

def shared temp-table tt-aux-semana
    field mes-refer       as char format "x(03)"
    field num-semana      as int      
    field dat-fim         as date
    field qtd-hrs-diu     as int 
    field qtd-hrs-not     as int   
    field qtd-hrs-diu-dsr as int 
    field qtd-hrs-not-dsr as int   
    field qtd-hrs-diu-fer as int 
    field qtd-hrs-not-fer as int   
    field qtd-hco         as int 
    field qtd-hpc         as int 
    field qtd-trb-sem     as int 
    field qtd-dsr-sem     as int 
    field qtd-fer-sem     as int 
    field percentual-dsr  as dec
    field qtd-atrasos     as int           
    field min-atrasos     as int 
    field qtd-hrs-hpc-seg as int
    index id is primary unique
          mes-refer
          num-semana 
          dat-fim. 

def temp-table tt-supnot-jorinc no-undo
    FIELD v_num_seq  AS INT
    field v_cdn_efp  like event_fp.cdn_event_fp
    field v_qtd_hrs  as dec
    index id is primary unique
          v_num_seq v_cdn_efp.

def temp-table tt-sit-descta no-undo
    FIELD v_num_seq     AS INT
    field v_cdn_efp     like event_fp.cdn_event_fp
    field v_qtd_hrs     as dec
    field v_qtd_hrs_aux as dec
    field v_log_diu     as log
    index id is primary unique
          v_num_seq v_cdn_efp.

DEF SHARED TEMP-TABLE tt-calendar LIKE det_calend_turma_localid.

def new shared buffer bsit_afast for sit_afast.
def new shared buffer bsit_afast_func for sit_afast_func.
def shared buffer bfunc-ponto for func_ptoelet. 
def shared buffer bcontrole-funcponto for sit_calc_ptoelet_func.
def shared buffer bfunc-bcohrs for bco_hrs_compens_func. 
def shared buffer cfunc-bcohrs for bco_hrs_compens_func. 
def shared buffer bcontrole-catponto for sit_calc_ptoelet_categ. 
def shared buffer bcalentur FOR det_calend_turma_localid.   
def shared buffer bturnotrb for turno_trab.  
def shared buffer bturmatrb for turma_trab.
def shared buffer bcatponto for categ_ptoelet.
def shared buffer bparam-pe for param_empres_tma.
def buffer bcalendia for tt-calendar.
def buffer btt-calendar for tt-calendar.
def buffer befp_par_marcac_ptoelet for efp_par_marcac_ptoelet.
def buffer btt-aux-semana for tt-aux-semana.
def buffer bsit_func for sit_afast_func.

{include/i-epc200.i pe4000r4}

ASSIGN v_cdn_evt_490          = ""
       v_cdn_evt_495          = "" 
       v_cdn_evt_995          = "" 
       v_cdn_evt_996          = ""
       v_qtd_lim_minut_atraso = truncate(round(bcatponto.qtd_lim_minut_atraso / 3600,3),3)
       v_qtd_lim_minut_atraso = truncate(round(v_qtd_lim_minut_atraso * 3600,3),0).

IF v_qtd_lim_minut_atraso < bcatponto.qtd_lim_minut_atraso THEN 
   ASSIGN v_qtd_lim_minut_atraso = bcatponto.qtd_lim_minut_atraso.

/*** UPC PROTEGE eventos 490, 495, 995 e 996 ***/
FOR EACH tt-epc WHERE tt-epc.cod-event = "eventos":U:
    DELETE tt-epc. 
END.

CREATE tt-epc.
ASSIGN tt-epc.cod-event = "eventos":U
       &if '{&cd_rel_hr}' < '2.11' &then
            tt-epc.val-parameter = STRING(bfunc-ponto.cdn_empresa,"999") +
                                   STRING(bfunc-ponto.cdn_estab,"999") +
                                   STRING(bfunc-ponto.cdn_funcionario,"99999999").
       &else
            tt-epc.val-parameter = STRING(bfunc-ponto.cdn_empresa) + ';' +
                                   STRING(bfunc-ponto.cdn_estab) + ';' +
                                   STRING(bfunc-ponto.cdn_funcionario).
       &endif       

{include/i-epc201.i "eventos"}

if return-value = "OK-eventos":U THEN DO:
   find tt-epc no-lock where
        tt-epc.cod-event = "eventos":U no-error.
   if avail tt-epc then 
      ASSIGN v_cdn_evt_490 = SUBSTRING(tt-epc.val-parameter,20,3)
             v_cdn_evt_495 = SUBSTRING(tt-epc.val-parameter,23,3)
             v_cdn_evt_995 = SUBSTRING(tt-epc.val-parameter,26,3)
             v_cdn_evt_996 = SUBSTRING(tt-epc.val-parameter,29,3).
END.

/*** Fim UPC Protege eventos***/

assign l-afast-mes           = yes
       dat-inic-afames       = ?
/*        i-ult-semant       = tt-calendar.num_sema_dia_calend  */
       v_dat_fim_sem         = ?
       v_dat_transf          = ?
       v_log_func_ant        = no
       v_log_perde_dsr       = no
       v_log_perde_dsr_falta = NO
       v_log_movto_extra     = no
       v_log_desctou_dsr     = no.

find first turno_trab of bfunc-ponto no-lock no-error.
if avail turno_trab and  
        (turno_trab.num_livre_1 > 0 or  
         turno_trab.num_livre_2 > 0) then 
    assign v_num_hra_ini_not = turno_trab.num_livre_1 
           v_num_hra_fim_not = turno_trab.num_livre_2.
else 
    assign v_num_hra_ini_not = 79200 
           v_num_hra_fim_not = 18000.

/******** Chamada utilizada para gravar horario 
          noturno <> do padrao e do sindicato ********/

for each tt-epc where 
         tt-epc.cod-event = "hra_notur":U:
    delete tt-epc. 
end.
create tt-epc.
assign tt-epc.cod-event = "hra_notur":U
       &if '{&cd_rel_hr}' < '2.11' &then
            tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                   string(bfunc-ponto.cdn_estab,"999") +
                                   string(bfunc-ponto.cdn_funcionario,"99999999").
       &else
            tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + ';' +
                                   string(bfunc-ponto.cdn_estab) + ';' +
                                   string(bfunc-ponto.cdn_funcionario).
       &endif

{include/i-epc201.i "hra_notur"}

if return-value = "ALT-HORAR-NOTUR" then do:
    find first tt-epc
        where tt-epc.cod-event     = "hra_notur":U
        and   tt-epc.cod-parameter = 'altera_periodo_noturno_OK' no-error.

    if avail tt-epc then
        ASSIGN v_num_hra_ini_not = int(entry(1, tt-epc.val-parameter))
               v_num_hra_fim_not = int(entry(2, tt-epc.val-parameter)).

end.
/*nova EPC - Ch SDMZK2 - Protege*/
/*Verifica se o funcion rio esteve afastado por "Falta Injustificada, Afastado ou Contrato desativado" durante o mˆs todo */
do dt-aux = bcontrole-catponto.dat_inic_period_apurac_pto_mes to bcontrole-catponto.dat_term_period_apurac_pto_mes:
   find first bsit_afast_func of bfunc-ponto no-lock where
         bsit_afast_func.dat_term_proces_sit_afast >= dt-aux and
         bsit_afast_func.dat_inic_proces_sit_afast <= dt-aux and
         bsit_afast_func.dat_integr_sit_afast_func = ? no-error.
   if not avail bsit_afast_func then do:
       assign l-afast-periodo = no.
       leave.
   end.
   else do:
       find first bsit_afast no-lock of bsit_afast_func no-error.
       if bsit_afast.idi_signif_sit = 2 or
          bsit_afast.idi_signif_sit = 7 then
           assign l-afast-periodo = yes.
       else do:
           assign l-afast-periodo = no.
           leave.           
       end.
   end.
end.

for each bsit_afast_func of bfunc-ponto exclusive-lock where 
         bsit_afast_func.dat_term_proces_sit_afast >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
         bsit_afast_func.dat_inic_proces_sit_afast <= dat-fim-mes,
   first bsit_afast no-lock where
         bsit_afast.cdn_sit_afast_func = bsit_afast_func.cdn_sit_afast_func and
         bsit_afast.idi_signif_sit = 3:
   assign v_dat_transf     = bsit_afast_func.dat_inic_sit_afast
          v_cdn_sit_transf = bsit_afast_func.cdn_sit_afast_func.
end.

/* lˆ situa‡äes de f‚rias - cris */
for each bsit_afast_func of bfunc-ponto exclusive-lock where 
         bsit_afast_func.dat_term_proces_sit_afast >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
         bsit_afast_func.dat_inic_proces_sit_afast <= dat-fim-mes,
   first bsit_afast no-lock where
         bsit_afast.cdn_sit_afast_func = bsit_afast_func.cdn_sit_afast_func and
         bsit_afast.idi_signif_sit = 5:
   run PI-INIC-PERIOD.
     /***** marca situacao como tratada **************/
   if bsit_afast_func.dat_integr_sit_afast_func <> ? and
      bsit_afast_func.dat_integr_sit_afast_func >= dt-aux-sitfim
      then next.
   if bsit_afast_func.dat_integr_sit_afast_func >= dt-aux-sitini then
      assign dt-aux-sitini = bsit_afast_func.dat_integr_sit_afast_func + 1.                       
   run prghur/pep/pe4000r5.p.
   run pi-trata-situac.
end. /* f‚rias */

/* lˆ situa‡äes de afastamento - cris */
for each bsit_afast_func of bfunc-ponto exclusive-lock where 
         bsit_afast_func.dat_term_proces_sit_afast >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
         bsit_afast_func.dat_inic_proces_sit_afast <= dat-fim-mes,
   first bsit_afast no-lock where
         bsit_afast.cdn_sit_afast_func = bsit_afast_func.cdn_sit_afast_func and
         bsit_afast.idi_signif_sit = 2:
   if d-tot-hrs-sit >= d-tot-hrsmes then do:

      IF i-ev-hrs-dif-diu = "" or
         i-ev-hrs-dif-not = "" THEN DO:
         assign v_num_erro = v_num_erro + 1.
         {utp/ut-liter.i Situa‡Æo mpe L}
         assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
         {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
         assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
         {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_trab mpe L}
         assign v_des_erro = v_des_erro + trim(return-value).
         create tt_erro.
         assign tt_erro.num_erro = v_num_erro
                tt_erro.des_erro = v_des_erro
                tt_erro.log_erro = yes.
      END.
   end.
   run PI-INIC-PERIOD.
     /***** marca situacao como tratada **************/
   if bsit_afast_func.dat_integr_sit_afast_func <> ? and
      bsit_afast_func.dat_integr_sit_afast_func >= dt-aux-sitfim
      then next.
   if bsit_afast_func.dat_integr_sit_afast_func >= dt-aux-sitini then
      assign dt-aux-sitini = bsit_afast_func.dat_integr_sit_afast_func + 1.                       
   run prghur/pep/pe4000r5.p.
   run pi-trata-situac.
end. /* afastamento */

run pi-contrato-desativado.

assign v_qtd_hrs_diu_desc_dsr = 0
       v_qtd_hrs_not_desc_dsr = 0
       v_num_seq_sit          = 0
       v_dat_final_semana     = DATE(12,31,9999)
       v_num_horar_term_ant   = 0 
       v_dat_process_ant      = ?.

RUN pi-elimina-tt.
for each bsit_afast_func of bfunc-ponto EXCLUSIVE-LOCK where 
         bsit_afast_func.dat_term_proces_sit_afast >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
         bsit_afast_func.dat_inic_proces_sit_afast <= dat-fim-mes:
   assign v_log_justif = no.   
   find bsit_afast of bsit_afast_func no-lock no-error.

   if bsit_afast.idi_signif_sit = 2  or /** afastado            **/
      bsit_afast.idi_signif_sit = 10 or /** contrato desativado **/
      bsit_afast.idi_signif_sit = 5 /** ferias **/ then    /* cris */
      next.

   /** incluido log v_log_mes_seg **/      
   if bsit_afast.idi_signif_sit = 9 and l-desligado then do:
      find categ_sal of bfunc-ponto no-lock no-error.
      if month(bfunc-ponto.dat_desligto_func) <> month(dat-ini-mes) and
         year(bfunc-ponto.dat_desligto_func) <> year(dat-ini-mes) and 
         bsit_afast_func.dat_term_sit_afast > date(month(dat-ini-mes),categ_sal.num_dia_fim_period_pto,year(dat-ini-mes))  and
         v_log_mes_seg = no then
         next.
      if bsit_afast_func.dat_inic_sit_afast <  dat-ini-mes and
         bsit_afast_func.dat_term_sit_afast >= dat-ini-mes then
         assign v_log_justif = yes.
   end.

   /*** desconsidera situa‡äes com origem no ponto com data inicio maior data termino periodo ponto *******/
   if bsit_afast_func.idi_sit_afast_func = 3 and
      bsit_afast_func.dat_inic_proces_sit_afast > bcontrole-catponto.dat_term_period_apurac_pto_mes and
      not l-desligado then /* cris func demit */
      next. 

   if bsit_afast.idi_signif_sit = 3 or /** entrada transferencia **/
      bsit_afast.idi_signif_sit = 4    /** saida transferencia **/
      then next.

   if bsit_afast.idi_signif_sit = 6 then /** funcionario demitido **/
      next.
   if d-tot-hrs-sit >= d-tot-hrsmes then do:
      IF i-ev-hrs-dif-diu = "" or
         i-ev-hrs-dif-diu = "" THEN DO:
         assign v_num_erro = v_num_erro + 1.
         {utp/ut-liter.i Situa‡Æo mpe L}
         assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
         {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
         assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
         {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_trab mpe L}
         assign v_des_erro = v_des_erro + trim(return-value).
         create tt_erro.
         assign tt_erro.num_erro = v_num_erro
                tt_erro.des_erro = v_des_erro
                tt_erro.log_erro = yes.
      END.
   end.

   run PI-INIC-PERIOD. /* cris */
     /***** marca situacao como tratada **************/
   if bsit_afast_func.dat_integr_sit_afast_func <> ? and
      bsit_afast_func.dat_integr_sit_afast_func >= dt-aux-sitfim
      then next.
   if bsit_afast_func.dat_integr_sit_afast_func >= dt-aux-sitini then
      assign dt-aux-sitini = bsit_afast_func.dat_integr_sit_afast_func + 1.                       

   if bsit_afast.idi_signif_sit = 6 then 
      assign l-afast-mes = no.

   ASSIGN v_num_seq_sit = v_num_seq_sit + 1.

   /**** situacoes tipo hora - marca situacao como tratada **************/
   assign v_log_func_ant        = no
          v_log_perde_dsr_falta = NO.  
   if bsit_afast.idi_signif_sit = 8 then do:
      assign v_cdn_event_desc_diu = ""
             v_cdn_event_desc_not = ""
             v_log_perde_dsr      = no
             v_log_influi         = no
             v_log_desctou_dsr    = no.

      if bsit_afast.log_influi_para_fer = yes or bsit_afast.log_influi_repous = yes then
         assign v_log_influi = yes.
      if bsit_afast_func.dat_integr_sit_afast_func <> ? then 
         next.
      IF bsit_afast_func.dat_inic_proces_sit_afast > bcontrole-catponto.dat_term_period_apurac_pto_mes and
         not l-desligado THEN 
         NEXT.
      assign bsit_afast_func.dat_integr_sit_afast_func = bsit_afast_func.dat_inic_proces_sit_afast
             bsit_afast_func.num_mes_ano_refer_fp = c-mmaa-folha.

      /* gabriela */       
      if (bsit_afast_func.dat_inic_proces_sit_afast <= bcontrole-catponto.dat_term_period_apurac_pto_mes) 
         or (l-desligado = yes and bsit_afast_func.dat_inic_proces_sit_afast <= dat-fim-mes) then do:
         if v_dat_transf <> ? and bsit_afast_func.dat_inic_proces_sit_afast < v_dat_transf then do:
            find first bsit_func of bfunc-ponto no-lock where
                       bsit_func.dat_term_proces_sit_afast >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
                       bsit_func.dat_inic_proces_sit_afast <= dat-fim-mes and
                       bsit_func.cdn_sit_afast_func         = v_cdn_sit_transf no-error.
            if avail bsit_func then do:
               assign v_log_func_ant = yes.
               find bfunc-ponto no-lock where
                    bfunc-ponto.cdn_empresa     = bsit_func.cdn_empres_orig and
                    bfunc-ponto.cdn_estab       = bsit_func.cdn_estab_orig  and
                    bfunc-ponto.cdn_funcionario = bsit_func.cdn_func_orig   no-error.
               if not avail bfunc-ponto then
                  find bfunc-ponto no-lock where
                       bfunc-ponto.cdn_empresa     = bsit_func.cdn_empresa and
                       bfunc-ponto.cdn_estab       = bsit_func.cdn_estab   and
                       bfunc-ponto.cdn_funcionario = bsit_func.cdn_funcionario no-error.
            end.
         end.
         for each efp_par_marcac_ptoelet of bfunc-ponto EXCLUSIVE-LOCK where
                  efp_par_marcac_ptoelet.dat_proces_mpe             = bsit_afast_func.dat_inic_proces_sit_afast and
                  efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet       = 4 and
                  efp_par_marcac_ptoelet.num_horar_inic_proces_mpe >= bsit_afast_func.num_hora_inic_proces_sit_afast and
                  efp_par_marcac_ptoelet.num_horar_fim_proces_mpe  <= bsit_afast_func.num_horar_fim_proces_sit_afast:
             if efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet = 0 
                then next.

             assign efp_par_marcac_ptoelet.num_mes_ano_refer_fp = c-mmaa-folha
                    i-ev-codigo   = efp_par_marcac_ptoelet.cdn_efp
                    i-qtd-hrs-aux = efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet / 3600
                    i-qtd-hrs     = truncate(round(efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet / 3600,3),3)
                    i-qtd-movto = 0.

             find first tt-aux-semana where
                        tt-aux-semana.dat-fim >= efp_par_marcac_ptoelet.dat_proces_mpe no-error.
             if not available tt-aux-semana then 
                find last tt-aux-semana no-error.
             if tt-aux-semana.qtd-atrasos = 0 and tt-aux-semana.min-atrasos = 0 then
                assign v_qtd_hrs_diu_desc_dsr  = 0
                       v_qtd_hrs_not_desc_dsr  = 0.

             IF tt-aux-semana.dat-fim <> v_dat_final_semana AND bcatponto.idi_lim_minut_atraso = 2 THEN DO:
                FOR EACH tt-sit-descta EXCLUSIVE-LOCK:
                   DELETE tt-sit-descta.
                END.
                FOR EACH tt-supnot-jorinc EXCLUSIVE-LOCK:
                   DELETE tt-supnot-jorinc.
                END.
             END.
             ASSIGN v_dat_final_semana = tt-aux-semana.dat-fim.
             if efp_par_marcac_ptoelet.log_hrs_diurno = yes then do:
                assign v_cdn_event_desc_diu = i-ev-codigo.
                if (d-tot-hrs-sit + i-qtd-hrs) > d-tot-hrsmes then do:
                    find first event_fp no-lock where
                        event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                        event_fp.cdn_event_fp = i-ev-codigo no-error.
                    if avail event_fp then do:
                        if event_fp.idi_ident_efp = 1 then do:
                            assign v_num_erro = v_num_erro + 1.
                            {utp/ut-liter.i Situa‡Æo mpe L}
                            assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
                            {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
                            assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
                            {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_normais mpe L}
                            assign v_des_erro = v_des_erro + trim(return-value).
                            create tt_erro.
                            assign tt_erro.num_erro = v_num_erro
                                   tt_erro.des_erro = v_des_erro
                                   tt_erro.log_erro = yes.
                            next.
                        end.
                    end.

                   assign i-ev-cod-ant = i-ev-codigo.
                   if d-tot-hrs-sit < d-tot-hrsmes then do:
                      IF i-ev-hrs-dif-diu <> "" THEN DO:
                          find first event_fp no-lock where
                                     event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                     event_fp.cdn_event_fp = i-ev-codigo no-error.

                          assign i-ev-cod-ant = i-ev-codigo
                                 i-qtd-hrs    = i-qtd-hrs - (d-tot-hrsmes - d-tot-hrs-sit)
                                 i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                                   event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                                   event_fp.cdn_event_fp <> v_cdn_evt_495 
                                                then i-ev-hrs-dif-diu 
                                                else i-ev-codigo.

                          if i-ev-codigo = v_cdn_evt_490 or 
                             i-ev-codigo = v_cdn_evt_495 then
                             assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                                  then v_cdn_evt_995 
                                                  else v_cdn_evt_996.                          
                          if v_log_influi = no then do:
                             run pi-movto-jorn-incompleta.
                             ASSIGN i-ev-codigo = i-ev-cod-ant.
                             find first event_fp no-lock where
                                 event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                 event_fp.cdn_event_fp = i-ev-codigo no-error.
                             if avail event_fp and
                                event_fp.idi_ident_efp <> 2 then
                                assign /*d-tot-hrs-sit     = d-tot-hrs-sit + i-qtd-hrs*/
                                       d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs.
                          end.
                          ELSE DO:
                             assign v_qtd_hrs_diu_desc_dsr = v_qtd_hrs_diu_desc_dsr + i-qtd-hrs.
                             find first tt-sit-descta exclusive-lock where
                                        tt-sit-descta.v_num_seq = v_num_seq_sit AND
                                        tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
                             if not avail tt-sit-descta then do:
                                create tt-sit-descta.
                                assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                                       tt-sit-descta.v_cdn_efp     = i-ev-codigo
                                       tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                                       tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs
                                       tt-sit-descta.v_log_diu     = yes.
                             end.
                             else
                                assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                                       tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs.
                          END.
                      END.
                      assign i-qtd-hrs   = d-tot-hrsmes - d-tot-hrs-sit
                             i-ev-codigo = i-ev-cod-ant.

                      if v_log_influi = no then do:
                         run pi-movto-jorn-incompleta.
                         find first event_fp no-lock where
                                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                    event_fp.cdn_event_fp = i-ev-codigo no-error.
                         if avail event_fp and
                             event_fp.idi_ident_efp <> 2 then
                             assign d-tot-hrs-sit     = d-tot-hrs-sit + i-qtd-hrs
                                    d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs.
                      end.
                      else DO:
                         assign i-qtd-movto            = i-qtd-movto + (i-qtd-hrs * 3600) 
                                v_qtd_hrs_diu_desc_dsr = v_qtd_hrs_diu_desc_dsr + i-qtd-hrs.

                         find first tt-sit-descta exclusive-lock where
                                    tt-sit-descta.v_num_seq = v_num_seq_sit AND
                                    tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
                         if not avail tt-sit-descta then do:
                            create tt-sit-descta.
                            assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                                   tt-sit-descta.v_cdn_efp     = i-ev-codigo
                                   tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                                   tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs
                                   tt-sit-descta.v_log_diu     = yes.
                         end.
                         else
                            assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                                   tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs.
                      END.
                   end.
                   ELSE DO:
                      IF i-ev-hrs-dif-diu <> "" THEN DO:

                         find first event_fp no-lock where
                                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                    event_fp.cdn_event_fp = i-ev-codigo no-error.

                         assign i-ev-cod-ant = i-ev-codigo
                                i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                                  event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                                  event_fp.cdn_event_fp <> v_cdn_evt_495
                                               then i-ev-hrs-dif-diu 
                                               else i-ev-codigo.

                         if i-ev-codigo = v_cdn_evt_490 or 
                            i-ev-codigo = v_cdn_evt_495 then
                            assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                                 then v_cdn_evt_995 
                                                 else v_cdn_evt_996.                                

                         IF NOT v_log_influi THEN DO:
                            run pi-movto-jorn-incompleta.
                            ASSIGN i-ev-codigo = i-ev-cod-ant.
                            find first event_fp no-lock where
                                       event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                       event_fp.cdn_event_fp = i-ev-codigo no-error.
                            if avail event_fp and
                               event_fp.idi_ident_efp <> 2 then
                               assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs
                                      d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs-aux.
                         END.
                         ELSE DO:
                            assign v_qtd_hrs_diu_desc_dsr = v_qtd_hrs_diu_desc_dsr + i-qtd-hrs.
                            find first tt-sit-descta exclusive-lock where
                                       tt-sit-descta.v_num_seq = v_num_seq_sit AND
                                       tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
                            if not avail tt-sit-descta then do:
                               create tt-sit-descta.
                               assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                                      tt-sit-descta.v_cdn_efp     = i-ev-codigo
                                      tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                                      tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs
                                      tt-sit-descta.v_log_diu     = yes.
                            end.
                            else
                               assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                                      tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs.
                         END.
                      END.
                      assign i-qtd-hrs   = 0.
                   END.
                end.
                else do:
                   if v_log_influi = no then do:
                      run pi-movto-jorn-incompleta.

                      find first event_fp no-lock where
                                 event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                 event_fp.cdn_event_fp = i-ev-codigo no-error.
                      if avail event_fp and
                          event_fp.idi_ident_efp <> 2 then
                          assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs
                                 d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs-aux.
                   end.
                   else do:
                      assign v_qtd_hrs_diu_desc_dsr = v_qtd_hrs_diu_desc_dsr + i-qtd-hrs
                             i-qtd-movto            = i-qtd-movto + (i-qtd-hrs * 3600).
 
                      find first tt-sit-descta exclusive-lock where
                                 tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
                      if not avail tt-sit-descta then do:
                         create tt-sit-descta.
                         assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                                tt-sit-descta.v_cdn_efp     = i-ev-codigo
                                tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                                tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs-aux
                                tt-sit-descta.v_log_diu     = yes.
                      end.
                      else
                         assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                                tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs-aux.
                   end.
                end.  
             end.
             else do:
                assign v_cdn_event_desc_not = i-ev-codigo.
                if (d-tot-hrs-sit + i-qtd-hrs) > d-tot-hrsmes then do:
                   find first event_fp no-lock where
                       event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                       event_fp.cdn_event_fp = i-ev-codigo no-error.
                   if avail event_fp then do:
                       if event_fp.idi_ident_efp = 1 then do:
                           assign v_num_erro = v_num_erro + 1.
                           {utp/ut-liter.i Situa‡Æo mpe L}
                           assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
                           {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
                           assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
                           {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_normais mpe L}
                           assign v_des_erro = v_des_erro + trim(return-value).
                           create tt_erro.
                           assign tt_erro.num_erro = v_num_erro
                                  tt_erro.des_erro = v_des_erro
                                  tt_erro.log_erro = yes.
                           next.
                       end.
                   end.
                   assign i-ev-cod-ant = i-ev-codigo.
                   if d-tot-hrs-sit < d-tot-hrsmes then do:
                      IF i-ev-hrs-dif-not <> "" THEN DO:
                          find first event_fp no-lock where
                                     event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                     event_fp.cdn_event_fp = i-ev-codigo no-error.
                          assign i-ev-cod-ant = i-ev-codigo
                                 i-qtd-hrs    = i-qtd-hrs - (d-tot-hrsmes - d-tot-hrs-sit)
                                 i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                                   event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                                   event_fp.cdn_event_fp <> v_cdn_evt_495 
                                                then i-ev-hrs-dif-not 
                                                else i-ev-codigo.

                          if i-ev-codigo = v_cdn_evt_490 or 
                             i-ev-codigo = v_cdn_evt_495 then
                             assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                                  then v_cdn_evt_995 
                                                  else v_cdn_evt_996.                                 

                          if v_log_influi = no then do:
                             run pi-movto-jorn-incompleta.
                             ASSIGN i-ev-cod-ant = i-ev-codigo.
                             /*Alteracao Ref. Eventos Integracao*/
                             find first event_fp no-lock where
                                        event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                        event_fp.cdn_event_fp = i-ev-codigo no-error.
                             if avail event_fp and
                                event_fp.idi_ident_efp <> 2 then
                                assign d-tot-hrs-sit     = d-tot-hrs-sit + i-qtd-hrs
                                       d-hrs-not-trb-sit = d-hrs-not-trb-sit + i-qtd-hrs.
                                                                   
                             /* gravar a suplementacao das horas noturnas no evento de desconto */
                             IF bsit_afast.cod_event_suplem_notur > "" THEN DO:

                                if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                                   v_log_sind_suplem = no then
                                   assign i-qtd-hrs   = 0.
                                else
                                   assign i-qtd-hrs   = (i-qtd-hrs * 0.1428572).

                                run pi-movto-jorn-incompleta.
                             END.
                          END.   
                          ELSE DO:
                             assign v_qtd_hrs_not_desc_dsr = v_qtd_hrs_not_desc_dsr + i-qtd-hrs.

                             find first tt-sit-descta exclusive-lock where
                                        tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
                             if not avail tt-sit-descta then do:
                                create tt-sit-descta.
                                assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                                       tt-sit-descta.v_cdn_efp     = i-ev-codigo
                                       tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                                       tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs
                                       tt-sit-descta.v_log_diu     = no.
                             end.
                             else
                                assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                                       tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs.

                             /* gravar a suplementacao das horas noturnas no evento de desconto */
                             IF bsit_afast.cod_event_suplem_notur <> "" THEN DO:

                                if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                                   v_log_sind_suplem = no then
                                   assign i-qtd-hrs   = 0.
                                else
                                   assign i-qtd-hrs   = (i-qtd-hrs * 0.1428572).

                                find first tt-supnot-jorinc exclusive-lock where
                                           tt-supnot-jorinc.v_cdn_efp = i-ev-codigo no-error.
                                if not avail tt-supnot-jorinc then do:
                                   create tt-supnot-jorinc.
                                   assign tt-supnot-jorinc.v_num_seq = v_num_seq_sit
                                          tt-supnot-jorinc.v_cdn_efp = i-ev-codigo
                                          tt-supnot-jorinc.v_qtd_hrs = i-qtd-hrs.
                                end.
                                else
                                   assign tt-supnot-jorinc.v_qtd_hrs = tt-supnot-jorinc.v_qtd_hrs + i-qtd-hrs.
                             END.
                          END.
                      END.

                      assign i-qtd-hrs   = d-tot-hrsmes - d-tot-hrs-sit
                             i-ev-codigo = i-ev-cod-ant.
                      if v_log_influi = no then do:
                         run pi-movto-jorn-incompleta.
                         /*Alteracao Ref. Eventos Integracao*/
                         find first event_fp no-lock where
                                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                    event_fp.cdn_event_fp = i-ev-codigo no-error.
                         if avail event_fp and
                             event_fp.idi_ident_efp <> 2 then
                             assign d-tot-hrs-sit     = d-tot-hrs-sit + i-qtd-hrs
                                    d-hrs-not-trb-sit = d-hrs-not-trb-sit + i-qtd-hrs.

                         assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs.
                      end.
                      else DO:
                         assign i-qtd-movto            = i-qtd-movto + (i-qtd-hrs * 3600)
                                v_qtd_hrs_not_desc_dsr = v_qtd_hrs_not_desc_dsr + i-qtd-hrs.

                         find first tt-sit-descta exclusive-lock where
                                    tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
                         if not avail tt-sit-descta then do:
                            create tt-sit-descta.
                            assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                                   tt-sit-descta.v_cdn_efp     = i-ev-codigo
                                   tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                                   tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs
                                   tt-sit-descta.v_log_diu     = no.
                         end.
                         else
                            assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                                   tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs.
                      END.
                   end.
                   else DO:
                      IF i-ev-hrs-dif-not <> "" THEN DO:
                         find first event_fp no-lock where
                                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                    event_fp.cdn_event_fp = i-ev-codigo no-error.
                         assign i-ev-cod-ant = i-ev-codigo
                                i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                                  event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                                  event_fp.cdn_event_fp <> v_cdn_evt_495 
                                               then i-ev-hrs-dif-not 
                                               else i-ev-codigo.

                         if i-ev-codigo = v_cdn_evt_490 or 
                            i-ev-codigo = v_cdn_evt_495 then
                            assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                                 then v_cdn_evt_995 
                                                 else v_cdn_evt_996.

                         if v_log_influi = no then DO:
                            run pi-movto-jorn-incompleta.

                            /* gravar a suplementacao das horas noturnas no evento de desconto */
                            IF bsit_afast.cod_event_suplem_notur <> "" THEN DO:

                               if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                                  v_log_sind_suplem = no then
                                  assign i-qtd-hrs   = 0.
                               else
                                  assign i-qtd-hrs   = (i-qtd-hrs * 0.1428572).

                               run pi-movto-jorn-incompleta.
                            END.
                         END.
                         ELSE DO:
                            assign v_qtd_hrs_not_desc_dsr = v_qtd_hrs_not_desc_dsr + i-qtd-hrs.

                            find first tt-sit-descta exclusive-lock where
                                       tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
                            if not avail tt-sit-descta then do:
                               create tt-sit-descta.
                               assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                                      tt-sit-descta.v_cdn_efp     = i-ev-codigo
                                      tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                                      tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs
                                      tt-sit-descta.v_log_diu     = no.
                            end.
                            else
                               assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                                      tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs.

                            /* gravar a suplementacao das horas noturnas no evento de desconto */
                            IF bsit_afast.cod_event_suplem_notur <> "" THEN DO:

                               if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                                  v_log_sind_suplem = no then
                                  assign i-qtd-hrs   = 0.
                               else
                                  assign i-qtd-hrs   = (i-qtd-hrs * 0.1428572).

                               find first tt-supnot-jorinc exclusive-lock where
                                          tt-supnot-jorinc.v_cdn_efp = i-ev-codigo no-error.
                               if not avail tt-supnot-jorinc then do:
                                  create tt-supnot-jorinc.
                                  assign tt-supnot-jorinc.v_num_seq = v_num_seq_sit
                                         tt-supnot-jorinc.v_cdn_efp = i-ev-codigo
                                         tt-supnot-jorinc.v_qtd_hrs = i-qtd-hrs.
                               end.
                               else
                                  assign tt-supnot-jorinc.v_qtd_hrs = tt-supnot-jorinc.v_qtd_hrs + i-qtd-hrs.
                            END.
                         END.
                         ASSIGN i-ev-codigo = i-ev-cod-ant.
                      END.
                      assign i-qtd-hrs   = 0.
                   END.
                end.
                else do:           
                   if v_log_influi = no then do:
                      run pi-movto-jorn-incompleta.
                      /*Alteracao Ref. Eventos Integracao*/
                      find first event_fp no-lock where
                                 event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                 event_fp.cdn_event_fp = i-ev-codigo no-error.
                      if avail event_fp and
                         event_fp.idi_ident_efp <> 2 then
                         assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs
                                d-hrs-not-trb-sit = d-hrs-not-trb-sit + i-qtd-hrs-aux.

                      assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux.
                   end.
                   else do:
                      assign v_qtd_hrs_not_desc_dsr = v_qtd_hrs_not_desc_dsr + i-qtd-hrs
                             i-qtd-movto            = i-qtd-movto + (i-qtd-hrs * 3600).

                      find first tt-sit-descta exclusive-lock where
                                 tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
                      if not avail tt-sit-descta then do:
                         create tt-sit-descta.
                         assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                                tt-sit-descta.v_cdn_efp     = i-ev-codigo
                                tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                                tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs-aux
                                tt-sit-descta.v_log_diu     = no.
                      end.
                      else
                         assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                                tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs-aux.
                   end.
                end.
                /* tt-suplement */
                if bsit_afast.cod_event_suplem_notur <> "" then do:

                   if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                      v_log_sind_suplem = no then
                      assign i-ev-codigo   = bsit_afast.cod_event_suplem_notur
                             i-qtd-hrs-aux = 0
                             i-qtd-hrs     = 0.
                   else
                      assign i-ev-codigo   = bsit_afast.cod_event_suplem_notur
                             i-qtd-hrs-aux = i-qtd-hrs * 0.1428572
                             i-qtd-hrs     = truncate(round(i-qtd-hrs * 0.1428572,3),3).

                   if (d-tot-hrs-sit + i-qtd-hrs) > d-tot-hrsmes then do:
                       find first event_fp no-lock where
                           event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                           event_fp.cdn_event_fp = i-ev-codigo no-error.
                       if avail event_fp then do:
                           if event_fp.idi_ident_efp = 1 then do:
                               assign v_num_erro = v_num_erro + 1.
                               {utp/ut-liter.i Situa‡Æo mpe L}
                               assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
                               {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
                               assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
                               {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_normais mpe L}
                               assign v_des_erro = v_des_erro + trim(return-value).
                               create tt_erro.
                               assign tt_erro.num_erro = v_num_erro
                                      tt_erro.des_erro = v_des_erro
                                      tt_erro.log_erro = yes.
                               next.
                           end.
                       end.

                       if d-tot-hrs-sit < d-tot-hrsmes then do:
                          assign i-qtd-hrs = d-tot-hrsmes - d-tot-hrs-sit.
                         if v_log_influi = no then do:
                            find first event_fp no-lock where
                                       event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                       event_fp.cdn_event_fp = i-ev-codigo no-error.
                            ASSIGN i-ev-cod-ant = i-ev-codigo
                                   i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                                     event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                                     event_fp.cdn_event_fp <> v_cdn_evt_495 
                                                  then i-ev-hrs-dif-not 
                                                  else i-ev-codigo.

                            if i-ev-codigo = v_cdn_evt_490 or 
                               i-ev-codigo = v_cdn_evt_495 then
                               assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                                    then v_cdn_evt_995 
                                                    else v_cdn_evt_996.                                   

                            run pi-movto-jorn-incompleta.
                            find first event_fp no-lock where
                                       event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                       event_fp.cdn_event_fp = i-ev-codigo no-error.

                            assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                                   d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                       then d-tot-hrs-sit + i-qtd-hrs 
                                                       else d-tot-hrs-sit
                                   d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs
                                   i-ev-codigo       = i-ev-cod-ant.
                         end.
                         else do:
                            find first event_fp no-lock where
                                       event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                       event_fp.cdn_event_fp = i-ev-codigo no-error.
                            assign i-qtd-movto  = i-qtd-movto + (i-qtd-hrs * 3600)
                                   i-ev-cod-ant = i-ev-codigo
                                   i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                                     event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                                     event_fp.cdn_event_fp <> v_cdn_evt_495 
                                                  then i-ev-hrs-dif-not 
                                                  else i-ev-codigo.

                            if i-ev-codigo = v_cdn_evt_490 or 
                               i-ev-codigo = v_cdn_evt_495 then
                               assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                                    then v_cdn_evt_995 
                                                    else v_cdn_evt_996.                                   

                            find first tt-supnot-jorinc exclusive-lock where
                                       tt-supnot-jorinc.v_cdn_efp = i-ev-codigo no-error.
                            if not avail tt-supnot-jorinc then do:
                               create tt-supnot-jorinc.
                               assign tt-supnot-jorinc.v_num_seq = v_num_seq_sit
                                      tt-supnot-jorinc.v_cdn_efp = i-ev-codigo
                                      tt-supnot-jorinc.v_qtd_hrs = i-qtd-hrs.
                            end.
                            else
                               assign tt-supnot-jorinc.v_qtd_hrs = tt-supnot-jorinc.v_qtd_hrs + i-qtd-hrs.

                            ASSIGN i-ev-codigo = i-ev-cod-ant.
                         end.
                      end.          
                   end.
                   else do:
                      if v_log_influi = no then do:
                         run pi-movto-jorn-incompleta.
                         find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.

                         assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                                d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                    then d-tot-hrs-sit + i-qtd-hrs 
                                                    else d-tot-hrs-sit
                                d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs-aux.
                      end.
                      else do:
                         assign i-qtd-movto = i-qtd-movto + (i-qtd-hrs * 3600).
                         find first tt-supnot-jorinc exclusive-lock where
                                    tt-supnot-jorinc.v_cdn_efp = i-ev-codigo no-error.
                         if not avail tt-supnot-jorinc then do:
                            create tt-supnot-jorinc.
                            assign tt-supnot-jorinc.v_num_seq = v_num_seq_sit
                                   tt-supnot-jorinc.v_cdn_efp = i-ev-codigo
                                   tt-supnot-jorinc.v_qtd_hrs = i-qtd-hrs.
                         end.
                         else
                            assign tt-supnot-jorinc.v_qtd_hrs = tt-supnot-jorinc.v_qtd_hrs + i-qtd-hrs.
                      end.
                   end.
                end.
             end.    

             /**** cris - turno m¢vel (desconto dsr) ***********/
             if v_log_influi = yes THEN DO:
                /* gabriela */
                IF v_dat_process_ant = efp_par_marcac_ptoelet.dat_proces_mpe AND
                   v_num_horar_term_ant = efp_par_marcac_ptoelet.num_horar_inic_proces_mpe THEN .
                ELSE DO:
                   assign tt-aux-semana.qtd-atrasos = if i-qtd-movto <> 0 then
                                                         tt-aux-semana.qtd-atrasos + 1
                                                      else
                                                         tt-aux-semana.qtd-atrasos.
                END.
                ASSIGN i-qtd-min                 = ROUND( i-qtd-movto / 60,0)
                       i-qtd-movto               = i-qtd-min * 60
                       tt-aux-semana.min-atrasos = tt-aux-semana.min-atrasos + i-qtd-movto.
             END.

             ASSIGN v_dat_process_ant    = efp_par_marcac_ptoelet.dat_proces_mpe
                    v_num_horar_term_ant = efp_par_marcac_ptoelet.num_horar_fim_proces_mpe.
         end. /* efp */

         if v_log_func_ant = yes then do:
            find bfunc-ponto no-lock where
                 bfunc-ponto.cdn_empresa     = bsit_func.cdn_empresa and
                 bfunc-ponto.cdn_estab       = bsit_func.cdn_estab   and
                 bfunc-ponto.cdn_funcionario = bsit_func.cdn_funcionario no-error.
         end.
      end.  
      else do:
          RUN pi_desligado.
      end. /* else */
      run pi-trata-situac. /* cris */
      run pi-movto-sit.
   end.               

   /**** falta injustificada ************************/  
   if bsit_afast.idi_signif_sit = 7 OR
      bsit_afast.idi_signif_sit = 9 then do:
      assign i-qtd-sup-hrs-not = 0   /*** cris ****/
             v_cdn_evto_sup_not = "".
      find first tt-aux-semana where
           tt-aux-semana.dat-fim >= dt-aux-sitini no-lock no-error.
      if not available tt-aux-semana then
         find last tt-aux-semana no-lock no-error.
      assign v_dat_fim_localid = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
             v_dat_fim_emprest = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
             v_dat_fim_lotac   = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
             v_log_emprest     = no
             v_log_func_ant    = no
             v_cdn_emp_func    = bfunc-ponto.cdn_empresa
             v_cdn_estab_func  = bfunc-ponto.cdn_estab
             v_cdn_func_func   = bfunc-ponto.cdn_funcionario
             v_qtd_hrs_sit_sup = 0.
      do dt-dia-cal = dt-aux-sitini to dt-aux-sitfim:
         if dt-dia-cal <= tt-aux-semana.dat-fim then do:
            find first tt-aux-semana where
                 tt-aux-semana.dat-fim >= dt-dia-cal no-lock no-error.
            if not available tt-aux-semana then
               find last tt-aux-semana no-lock no-error. 
         end.

         if v_dat_transf <> ? and dt-dia-cal < v_dat_transf then do:
            if v_log_func_ant = no then do:
               find first bsit_func of bfunc-ponto no-lock where
                          bsit_func.dat_term_proces_sit_afast >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
                          bsit_func.dat_inic_proces_sit_afast <= dat-fim-mes and
                          bsit_func.cdn_sit_afast_func         = v_cdn_sit_transf no-error.
               if avail bsit_func then do:
                  assign v_log_func_ant = yes.
                  find bfunc-ponto no-lock where
                       bfunc-ponto.cdn_empresa     = bsit_func.cdn_empres_orig and
                       bfunc-ponto.cdn_estab       = bsit_func.cdn_estab_orig  and
                       bfunc-ponto.cdn_funcionario = bsit_func.cdn_func_orig   no-error.
                  if not avail bfunc-ponto then
                     find bfunc-ponto no-lock where
                          bfunc-ponto.cdn_empresa     = bsit_func.cdn_empresa and
                          bfunc-ponto.cdn_estab       = bsit_func.cdn_estab   and
                          bfunc-ponto.cdn_funcionario = bsit_func.cdn_funcionario no-error.
               end.
            end.
         end.
         else do:
            if v_log_func_ant = yes then
               find bfunc-ponto no-lock where
                    bfunc-ponto.cdn_empresa     = bsit_func.cdn_empresa and
                    bfunc-ponto.cdn_estab       = bsit_func.cdn_estab   and
                    bfunc-ponto.cdn_funcionario = bsit_func.cdn_funcionario no-error.
            assign v_log_func_ant = no.
         end.

         {prghur/pep/pe9994.i dt-dia-cal}
         find tt-calendar where
              tt-calendar.cdn_turno_trab   = v_cdn_turno and
              tt-calendar.cdn_turma_trab   = v_cdn_turma and
              tt-calendar.dat_refer_calend = dt-dia-cal  and
              tt-calendar.cod_pais         = v_cod_pais  and  
              tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
         IF NOT AVAIL tt-calendar THEN
            FIND FIRST tt-calendar where
                       tt-calendar.dat_refer_calend = dt-dia-cal no-lock no-error.
         if bsit_afast.idi_hrs_desc_dia = 1 then do:  /* real */
            assign l-tratada-situa = no.
            for each efp_par_marcac_ptoelet EXCLUSIVE-LOCK where 
                     efp_par_marcac_ptoelet.cdn_empresa           = bfunc-ponto.cdn_empresa     and
                     efp_par_marcac_ptoelet.cdn_estab             = bfunc-ponto.cdn_estab       and
                     efp_par_marcac_ptoelet.cdn_funcionario       = bfunc-ponto.cdn_funcionario and
                     efp_par_marcac_ptoelet.dat_proces_mpe        = dt-dia-cal                  and
                     /*efp_par_marcac_ptoelet.cdn_sit_afast_func    = bsit_afast_func.cdn_sit_afast_func*/
                     efp_par_marcac_ptoelet.cdn_sit_afast_func    <> 0:
               assign efp_par_marcac_ptoelet.num_mes_ano_refer_fp = c-mmaa-folha
                      l-tratada-situa = yes
                      i-ev-codigo   = efp_par_marcac_ptoelet.cdn_efp
                      i-qtd-hrs-aux = efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet / 3600
                      i-qtd-hrs     = truncate(round(efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet / 3600,3),3).
               /*Alteracao Ref. Eventos Integracao*/
               if efp_par_marcac_ptoelet.log_hrs_diurno = yes then do:
                  run pi-hrs-diu. /* calcula a quantidade do movto de situa‡Æo */
                  if tt-calendar.idi_sit_dia_trab = 3 then do: /*** repouso ***/
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign d-hrs-diu-dsr-sit = d-hrs-diu-dsr-sit + i-qtd-hrs.
                     else
                        assign d-hrs-diu-dsr-sit = d-hrs-diu-dsr-sit + i-qtd-hrs-aux.
                  end.
                  else do:
                     if tt-calendar.idi_sit_dia_trab = 4 then do: /*** feriado ***/
                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-diu-fer-sit = d-hrs-diu-fer-sit + i-qtd-hrs.
                        else
                           assign d-hrs-diu-fer-sit = d-hrs-diu-fer-sit + i-qtd-hrs-aux.
                     end.
                     else do:
                         find first event_fp no-lock where
                                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                    event_fp.cdn_event_fp = i-ev-codigo no-error.
                         if avail event_fp and
                             event_fp.idi_ident_efp <> 2 then do:
                             if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                                 assign d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs.
                             else
                                 assign d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs-aux.
                         end.
                     end.
                  end.
                  find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.
                  if avail event_fp and
                     event_fp.idi_ident_efp <> 2 then
                      assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs.
               end.
               else do:
                  run pi-hrs-not. /* calcula a quantidade do movto de situa‡Æo */
                  if tt-calendar.idi_sit_dia_trab = 3 then do: /*** repouso ***/
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign d-hrs-not-dsr-sit = d-hrs-not-dsr-sit + i-qtd-hrs.
                     else
                        assign d-hrs-not-dsr-sit = d-hrs-not-dsr-sit + i-qtd-hrs-aux.
                  end.
                  else do:
                     if tt-calendar.idi_sit_dia_trab = 4 then do: /*** feriado ***/
                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-not-fer-sit = d-hrs-not-fer-sit + i-qtd-hrs.
                        else
                           assign d-hrs-not-fer-sit = d-hrs-not-fer-sit + i-qtd-hrs-aux.
                     end.
                     else do:
                        find first event_fp no-lock where
                            event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                            event_fp.cdn_event_fp = i-ev-codigo no-error.

                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                      then d-hrs-not-trb-sit + i-qtd-hrs 
                                                      else d-hrs-not-trb-sit
                                  v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                                  i-qtd-sup-hrs-not = i-qtd-sup-hrs-not + i-qtd-hrs.
                        else
                           assign d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                      then d-hrs-not-trb-sit + i-qtd-hrs-aux 
                                                      else d-hrs-not-trb-sit
                                  v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                                  i-qtd-sup-hrs-not = i-qtd-sup-hrs-not + i-qtd-hrs-aux.
                     end.
                  end.
                  find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.

                  assign d-tot-hrs-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                         then d-tot-hrs-sit + i-qtd-hrs 
                                         else d-tot-hrs-sit.
                  if bsit_afast.cod_event_suplem_notur <> "" then do:  /* prim sup */
                     if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                        v_log_sind_suplem = no then.
                     else
                        assign v_qtd_hrs_sit_sup = v_qtd_hrs_sit_sup + (i-qtd-hrs * 0.1428572).
                     /*assign i-ev-codigo   = bsit_afast.num_livre_1
                            i-qtd-hrs-aux = i-qtd-hrs * 0.1428572
                            i-qtd-hrs     = truncate(round(i-qtd-hrs * 0.1428572,2),2).
                     run pi-hrs-not. /* calcula a quantidade do movto de situa‡Æo */
                     assign d-tot-hrs-sit     = d-tot-hrs-sit + i-qtd-hrs.
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                               d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs.
                     else
                        assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                               d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs-aux.*/
                  end.
                  else do: /*** cris ***/
                     if bsit_afast.idi_hrs_desc_dia = 1 and l-sup-evnot = yes then do:
                        if v_cdn_evto_sup_not = "" then
                           assign v_cdn_evto_sup_not = i-ev-codigo.
                     end.
                  end.
               end.   
               if v_log_func_ant = yes then
                  find bfunc-ponto no-lock where
                       bfunc-ponto.cdn_empresa     = bsit_func.cdn_empres_orig and
                       bfunc-ponto.cdn_estab       = bsit_func.cdn_estab_orig  and
                       bfunc-ponto.cdn_funcionario = bsit_func.cdn_func_orig   no-error.
            end. /* efp */
            if l-tratada-situa = no then do:
               if tt-calendar.qti_padr_hrs_diurno > 0 then do:
                  assign i-ev-codigo   = bsit_afast.cdn_event_afast_diurno
                         i-qtd-hrs-aux = tt-calendar.qti_padr_hrs_diurno / 3600
                         i-qtd-hrs     = truncate(round(tt-calendar.qti_padr_hrs_diurno / 3600,3),3).
                  run pi-hrs-diu. /* calcula a quantidade do movto de situa‡Æo */
                  if tt-calendar.idi_sit_dia_trab = 3 then do: /*** repouso ***/
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign d-hrs-diu-dsr-sit = d-hrs-diu-dsr-sit + i-qtd-hrs.
                     else
                        assign d-hrs-diu-dsr-sit = d-hrs-diu-dsr-sit + i-qtd-hrs-aux.
                  end.
                  else do:
                     if tt-calendar.idi_sit_dia_trab = 4 then do: /*** feriado ***/
                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-diu-fer-sit = d-hrs-diu-fer-sit + i-qtd-hrs.
                        else
                           assign d-hrs-diu-fer-sit = d-hrs-diu-fer-sit + i-qtd-hrs-aux.
                     end.
                     else do:
                         find first event_fp no-lock where
                                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                    event_fp.cdn_event_fp = i-ev-codigo no-error.
                          if avail event_fp and
                              event_fp.idi_ident_efp <> 2 then do:
                             if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                                 assign d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs.
                             else
                                 assign d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs-aux.
                         end.

                     end.
                  end.
                  find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.

                  if avail event_fp and event_fp.idi_ident_efp <> 2 then
                     assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs.

               end.
               if tt-calendar.qti_padr_hrs_notur > 0 then do:
                  assign i-ev-codigo   = bsit_afast.cdn_event_afast_notur
                         i-qtd-hrs-aux = tt-calendar.qti_padr_hrs_notur / 3600
                         i-qtd-hrs     = truncate(round(tt-calendar.qti_padr_hrs_notur / 3600,3),3).

                  find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.

                  run pi-hrs-not. /* calcula a quantidade do movto de situa‡Æo */
                  if tt-calendar.idi_sit_dia_trab = 3 then do: /*** repouso ***/
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign d-hrs-not-dsr-sit = d-hrs-not-dsr-sit + i-qtd-hrs.
                     else
                        assign d-hrs-not-dsr-sit = d-hrs-not-dsr-sit + i-qtd-hrs-aux.
                  end.
                  else do:
                     if tt-calendar.idi_sit_dia_trab = 4 then do: /*** feriado ***/
                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-not-fer-sit = d-hrs-not-fer-sit + i-qtd-hrs.
                        else
                           assign d-hrs-not-fer-sit = d-hrs-not-fer-sit + i-qtd-hrs-aux.
                     end.
                     else do:
                        find first event_fp no-lock where
                                   event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                   event_fp.cdn_event_fp = i-ev-codigo no-error.

                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                      then d-hrs-not-trb-sit + i-qtd-hrs 
                                                      else d-hrs-not-trb-sit
                                  v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                                  i-qtd-sup-hrs-not = i-qtd-sup-hrs-not + i-qtd-hrs.
                        else
                           assign d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                      then d-hrs-not-trb-sit + i-qtd-hrs-aux 
                                                      else d-hrs-not-trb-sit
                                  v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                                  i-qtd-sup-hrs-not = i-qtd-sup-hrs-not + i-qtd-hrs-aux.
                     end.
                  end.

                  find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.

                  assign d-tot-hrs-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                         then d-tot-hrs-sit + i-qtd-hrs 
                                         else d-tot-hrs-sit.

                  if bsit_afast.cod_event_suplem_notur <> "" then do:
                     if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                        v_log_sind_suplem = no then.
                     else 
                        assign v_qtd_hrs_sit_sup = v_qtd_hrs_sit_sup + (i-qtd-hrs * 0.1428572).
                    /* assign i-ev-codigo   = bsit_afast.num_livre_1
                            i-qtd-hrs-aux = i-qtd-hrs * 0.1428572
                            i-qtd-hrs     = truncate(round(i-qtd-hrs * 0.1428572,2),2).
                     run pi-hrs-not. /* calcula a quantidade do movto de situa‡Æo */
                     assign d-tot-hrs-sit     = d-tot-hrs-sit + i-qtd-hrs.
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                               d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs.
                     else
                        assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                               d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs-aux.*/
                  end.
                  else do: /*** cris ****/
                     if bsit_afast.idi_hrs_desc_dia = 1 and l-sup-evnot = yes then do:
                        if v_cdn_evto_sup_not = "" then
                           assign v_cdn_evto_sup_not = i-ev-codigo.
                     end.
                  end.
               end.          
               if v_log_func_ant = yes then
                  find bfunc-ponto no-lock where
                       bfunc-ponto.cdn_empresa     = bsit_func.cdn_empres_orig and
                       bfunc-ponto.cdn_estab       = bsit_func.cdn_estab_orig  and
                       bfunc-ponto.cdn_funcionario = bsit_func.cdn_func_orig   no-error.
            end. 
         end. /* real */
         else do:  /* padrÆo */
            if tt-calendar.qti_padr_hrs_diurno > 0 and
               tt-calendar.qti_padr_hrs_notur > 0 then do:
               /* diurna */
               assign v_qti_tot_dia    = tt-calendar.qti_padr_hrs_diurno + tt-calendar.qti_padr_hrs_notur
                      v_qtd_diurna_dia = tt-calendar.qti_padr_hrs_diurno / v_qti_tot_dia
                      i-ev-codigo      = IF bfunc-ponto.cdn_categ_sal = 2
                                            THEN bsit_afast.cdn_event_diurno_horist
                                            ELSE bsit_afast.cdn_event_afast_diurno
                      i-qtd-hrs-aux    = bturnotrb.qtd_hrs_padr_dia_rh * v_qtd_diurna_dia
                      i-qtd-hrs        = truncate(round((bturnotrb.qtd_hrs_padr_dia_rh * v_qtd_diurna_dia),3),3)
                      v_qtd_diurna_dia = i-qtd-hrs.
               run pi-hrs-diu. /* calcula a quantidade do movto de situa‡Æo */
               if tt-calendar.idi_sit_dia_trab = 3 then do: /*** repouso ***/
                  if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                     assign d-hrs-diu-dsr-sit = d-hrs-diu-dsr-sit + i-qtd-hrs.
                  else
                     assign d-hrs-diu-dsr-sit = d-hrs-diu-dsr-sit + i-qtd-hrs-aux.
               end.
               else do:
                  if tt-calendar.idi_sit_dia_trab = 4 then do: /*** feriado ***/
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign d-hrs-diu-fer-sit = d-hrs-diu-fer-sit + i-qtd-hrs.
                     else
                        assign d-hrs-diu-fer-sit = d-hrs-diu-fer-sit + i-qtd-hrs-aux.
                  end.
                  else do:
                      find first event_fp no-lock where
                                 event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                 event_fp.cdn_event_fp = i-ev-codigo no-error.
                      if avail event_fp and
                          event_fp.idi_ident_efp <> 2 then do:
                          if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                              assign d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs.
                          else
                              assign d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs-aux.
                      end.
                  end.
               end.
               find first event_fp no-lock where
                          event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                          event_fp.cdn_event_fp = i-ev-codigo no-error.
               if avail event_fp and
                  event_fp.idi_ident_efp <> 2 then
                   assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs.

               /* noturna */
               assign i-ev-codigo   = IF bfunc-ponto.cdn_categ_sal = 2
                                         THEN bsit_afast.cdn_event_notur_horist
                                         ELSE bsit_afast.cdn_event_afast_notur
                      i-qtd-hrs-aux = bturnotrb.qtd_hrs_padr_dia_rh - v_qtd_diurna_dia
                      i-qtd-hrs     = truncate(round((bturnotrb.qtd_hrs_padr_dia_rh - v_qtd_diurna_dia),3),3).

               run pi-hrs-not. /* calcula a quantidade do movto de situa‡Æo */
               if tt-calendar.idi_sit_dia_trab = 3 then do: /*** repouso ***/
                  if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                     assign d-hrs-not-dsr-sit = d-hrs-not-dsr-sit + i-qtd-hrs.
                  else
                     assign d-hrs-not-dsr-sit = d-hrs-not-dsr-sit + i-qtd-hrs-aux.
               end.
               else do:
                  if tt-calendar.idi_sit_dia_trab = 4 then do: /*** feriado ***/
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign d-hrs-not-fer-sit = d-hrs-not-fer-sit + i-qtd-hrs.
                     else
                        assign d-hrs-not-fer-sit = d-hrs-not-fer-sit + i-qtd-hrs-aux.
                  end.
                  else do:
                      find first event_fp no-lock where
                                 event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                 event_fp.cdn_event_fp = i-ev-codigo no-error.

                      if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                         assign d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                    then d-hrs-not-trb-sit + i-qtd-hrs 
                                                    else d-hrs-not-trb-sit
                                v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                                i-qtd-sup-hrs-not = i-qtd-sup-hrs-not + i-qtd-hrs.
                      else
                         assign d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                    then d-hrs-not-trb-sit + i-qtd-hrs-aux 
                                                    else d-hrs-not-trb-sit
                                v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                                i-qtd-sup-hrs-not = i-qtd-sup-hrs-not + i-qtd-hrs-aux.
                  end.
               end. 
               find first event_fp no-lock where
                          event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                          event_fp.cdn_event_fp = i-ev-codigo no-error.

               assign d-tot-hrs-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                      then d-tot-hrs-sit + i-qtd-hrs 
                                      else d-tot-hrs-sit.
            end.
            else do:
               if tt-calendar.qti_padr_hrs_diurno > 0 then do:
                  assign i-ev-codigo      = bsit_afast.cdn_event_afast_diurno
                         i-qtd-hrs-aux    = bturnotrb.qtd_hrs_padr_dia_rh
                         i-qtd-hrs        = truncate(round(bturnotrb.qtd_hrs_padr_dia_rh,3),3).

                  run pi-hrs-diu. /* calcula a quantidade do movto de situa‡Æo */
                  if tt-calendar.idi_sit_dia_trab = 3 then do: /*** repouso ***/
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign d-hrs-diu-dsr-sit = d-hrs-diu-dsr-sit + i-qtd-hrs.
                     else
                        assign d-hrs-diu-dsr-sit = d-hrs-diu-dsr-sit + i-qtd-hrs-aux.
                  end.
                  else do:
                     if tt-calendar.idi_sit_dia_trab = 4 then do: /*** feriado ***/
                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-diu-fer-sit = d-hrs-diu-fer-sit + i-qtd-hrs.
                        else
                           assign d-hrs-diu-fer-sit = d-hrs-diu-fer-sit + i-qtd-hrs-aux.
                     end.
                     else do:
                         find first event_fp no-lock where
                            event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                            event_fp.cdn_event_fp = i-ev-codigo no-error.
                         if avail event_fp and
                             event_fp.idi_ident_efp <> 2 then do:
                             if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                                 assign d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs.
                             else
                                 assign d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs-aux.
                         end.
                     end.
                  end.
                  find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.
                  if avail event_fp and
                     event_fp.idi_ident_efp <> 2 then
                      assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs.

               end.
               if tt-calendar.qti_padr_hrs_notur > 0 then do:
                  assign i-ev-codigo   = bsit_afast.cdn_event_afast_notur
                         i-qtd-hrs-aux = bturnotrb.qtd_hrs_padr_dia_rh
                         i-qtd-hrs     = truncate(round(bturnotrb.qtd_hrs_padr_dia_rh,3),3).
                  run pi-hrs-not. /* calcula a quantidade do movto de situa‡Æo */
                  if tt-calendar.idi_sit_dia_trab = 3 then do: /*** repouso ***/
                     if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                        assign d-hrs-not-dsr-sit = d-hrs-not-dsr-sit + i-qtd-hrs.
                     else
                        assign d-hrs-not-dsr-sit = d-hrs-not-dsr-sit + i-qtd-hrs-aux.
                  end.
                  else do:
                     if tt-calendar.idi_sit_dia_trab = 4 then do: /*** feriado ***/
                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-not-fer-sit = d-hrs-not-fer-sit + i-qtd-hrs.
                        else
                           assign d-hrs-not-fer-sit = d-hrs-not-fer-sit + i-qtd-hrs-aux.
                     end.
                     else do:
                        find first event_fp no-lock where
                            event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                            event_fp.cdn_event_fp = i-ev-codigo no-error.

                        if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
                           assign d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                      then d-hrs-not-trb-sit + i-qtd-hrs 
                                                      else d-hrs-not-trb-sit
                                  v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                                  i-qtd-sup-hrs-not = i-qtd-sup-hrs-not + i-qtd-hrs.
                        else
                           assign d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                      then d-hrs-not-trb-sit + i-qtd-hrs-aux 
                                                      else d-hrs-not-trb-sit
                                  v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                                  i-qtd-sup-hrs-not = i-qtd-sup-hrs-not + i-qtd-hrs-aux.
                     end.
                  end.
                  find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.

                  assign d-tot-hrs-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                         then d-tot-hrs-sit + i-qtd-hrs 
                                         else d-tot-hrs-sit.
               end.
               /*********************** AQUI AQUI AQUI - LINHA 1742 ********************************/
               if search ("prghur~\fpp~\fppl25rp.p") <> ? /*and (NOT l-afast-periodo OR l-afast-mes )*/ then do: /* Ser  executado somente na MRS (por enquanto) */
                  IF tt-calendar.qti_padr_hrs_notur = 0 AND
                     tt-calendar.qti_padr_hrs_diurno = 0 THEN DO:
                     assign i-ev-codigo      = bsit_afast.cdn_event_afast_diurno
                            i-qtd-hrs-aux    = bturnotrb.qtd_hrs_padr_dia_rh
                            i-qtd-hrs        = truncate(round(bturnotrb.qtd_hrs_padr_dia_rh,3),3).

                     run pi-hrs-diu. /* calcula a quantidade do movto de situa‡Æo */
                     find first event_fp no-lock where
                                event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                event_fp.cdn_event_fp = i-ev-codigo no-error.

                     if avail event_fp and event_fp.idi_ident_efp <> 2 THEN DO:
                         if (d-tot-hrs-sit + bturnotrb.qtd_hrs_padr_dia_rh) < bturnotrb.qtd_hrs_padr_mes_rh  then
                            ASSIGN  d-tot-hrs-sit = d-tot-hrs-sit + bturnotrb.qtd_hrs_padr_dia_rh.
    
                         if d-tot-hrs-sit > bturnotrb.qtd_hrs_padr_mes_rh then
                            assign d-tot-hrs-sit = bturnotrb.qtd_hrs_padr_mes_rh.
                     END.
                  END.
               end.
               /************************************************************************************/
            end.
            if v_log_func_ant = yes then
               find bfunc-ponto no-lock where
                    bfunc-ponto.cdn_empresa     = bsit_func.cdn_empres_orig and
                    bfunc-ponto.cdn_estab       = bsit_func.cdn_estab_orig  and
                    bfunc-ponto.cdn_funcionario = bsit_func.cdn_func_orig   no-error.
            for each efp_par_marcac_ptoelet EXCLUSIVE-LOCK where 
                     efp_par_marcac_ptoelet.cdn_empresa           = bfunc-ponto.cdn_empresa     and
                     efp_par_marcac_ptoelet.cdn_estab             = bfunc-ponto.cdn_estab       and
                     efp_par_marcac_ptoelet.cdn_funcionario       = bfunc-ponto.cdn_funcionario and
                     efp_par_marcac_ptoelet.dat_proces_mpe        = dt-dia-cal                  and
                     efp_par_marcac_ptoelet.cdn_sit_afast_func    = bsit_afast_func.cdn_sit_afast_func:
               assign efp_par_marcac_ptoelet.num_mes_ano_refer_fp = c-mmaa-folha.
            end. 
         end.
      end.  /* do */   /* at‚ aqui */
    

      if bsit_afast.cod_event_suplem_notur <> "" and v_qtd_hrs_sit_sup > 0 then do:
         assign i-ev-codigo   = bsit_afast.cod_event_suplem_notur
                i-qtd-hrs-aux = v_qtd_hrs_sit_sup
                i-qtd-hrs     = truncate(round(v_qtd_hrs_sit_sup,3),3).

         run pi-hrs-not. /* calcula a quantidade do movto de situa‡Æo */

         find first event_fp no-lock where
                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                    event_fp.cdn_event_fp = i-ev-codigo no-error.
         if avail event_fp and
            event_fp.idi_ident_efp <> 2 then
             assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs.

         if (d-tot-hrs-sit + i-qtd-hrs-aux) > d-tot-hrsmes then
            assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                   d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs.
         else
            assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                   d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs-aux.
      end.
      if v_log_func_ant = yes then do:
         assign v_log_func_ant = no.
         find bfunc-ponto no-lock where
              bfunc-ponto.cdn_empresa     = v_cdn_emp_func and
              bfunc-ponto.cdn_estab       = v_cdn_estab_func and
              bfunc-ponto.cdn_funcionario = v_cdn_func_func no-error.
      end.
      /*if bsit_afast.num_livre_1 = 0 and bsit_afast.idi_hrs_desc_dia = 1 and 
         l-sup-evnot = yes and d-hrs-not-trb-sit > 0 then do:
         assign i-qtd-hrs = truncate(round(i-qtd-sup-hrs-not * 0.1428571,2),2)
                i-ev-codigo = v_cdn_evto_sup_not.
         run pi-hrs-not. /* calcula a quantidade do movto de situa‡Æo */
         if bfunc-ponto.cdn_categ_sal = 1 then
            assign d-tot-hrs-sit = d-tot-hrs-sit + i-qtd-hrs.
      end.*/
      run pi-trata-situac. /* cris */
      run pi-movto-sit.
   end.
end. 

if l-afast-mes = yes and 
   dat-inic-afames <= dat-fim-mes then
   assign l-afast-mes = no.
if dat-inic-afames = ? then
   assign l-afast-mes = no.

/*
assign d-hrs-diu-dsr-sit = truncate(round(d-hrs-diu-dsr-sit,2),2)
       d-hrs-not-dsr-sit = truncate(round(d-hrs-not-dsr-sit,2),2)
       d-hrs-diu-fer-sit = truncate(round(d-hrs-diu-fer-sit,2),2)
       d-hrs-not-fer-sit = truncate(round(d-hrs-not-fer-sit,2),2)
       d-hrs-diu-trb-sit = truncate(round(d-hrs-diu-trb-sit,2),2)
       d-hrs-not-trb-sit = truncate(round(d-hrs-not-trb-sit,2),2)
       i-qtd-sup-hrs-not = truncate(round(i-qtd-sup-hrs-not,2),2)
       d-tot-hrs-sit     = truncate(round(d-tot-hrs-sit,2),2).
*/

return.

PROCEDURE PI-CALCULO-HORA:
   if i-hra-fim <= v_num_hra_fim_not then do:
      assign i-tot-hra-not = i-tot-hra-not + i-hra-fim - i-hra-ini.
   end.   
   else do:
      if i-hra-fim <= v_num_hra_ini_not then do:
         if i-hra-ini < v_num_hra_fim_not then do:
            assign i-tot-hra-not = i-tot-hra-not + v_num_hra_fim_not - i-hra-ini
                   i-tot-hra-diu = i-tot-hra-diu + i-hra-fim - v_num_hra_fim_not.
         end.
         else do:
            assign i-tot-hra-diu = i-tot-hra-diu + i-hra-fim - i-hra-ini.                     
         end. 
      end.
      else do:
         if i-hra-ini < v_num_hra_ini_not then do:
            assign i-tot-hra-diu = i-tot-hra-diu + v_num_hra_ini_not - i-hra-ini
                   i-tot-hra-not = i-tot-hra-not + i-hra-fim - v_num_hra_ini_not.
         end.
         else do:
            assign i-tot-hra-not = i-tot-hra-not + i-hra-fim - i-hra-ini.                     
         end.                  
      end.             
   end.
END PROCEDURE.

PROCEDURE PI-INIC-PERIOD:
   /***** trata varialvel funcionario afastado durante todo o mes - sim/nao *************/

   if dat-inic-afames = ? and
      bsit_afast_func.dat_inic_sit_afast > dat-ini-mes then 
      assign l-afast-mes = no.   
   if bsit_afast.idi_sit_espcif_ptoelet = 1 then 
      assign l-afast-mes = no.
   if bsit_afast_func.dat_inic_sit_afast > dat-inic-afames THEN
      assign l-afast-mes = no.

   if l-afast-mes = yes and dat-inic-afames = ? then
      assign dat-inic-afames = if bsit_afast_func.dat_term_sit_afast >= dat-ini-mes then
                                  IF bfunc-ponto.cdn_categ_sal = 1 THEN
                                     bsit_afast_func.dat_term_sit_afast + 1
                                  ELSE bsit_afast_func.dat_term_sit_afast
                               else ?.

   assign dt-aux-sitini = if bsit_afast_func.dat_inic_proces_sit_afast > bcontrole-catponto.dat_inic_period_apurac_pto_mes
                          then bsit_afast_func.dat_inic_proces_sit_afast
                          else bcontrole-catponto.dat_inic_period_apurac_pto_mes
          dt-aux-iniss  = if bsit_afast_func.dat_inic_pagto_inss < bsit_afast_func.dat_integr_sit_afast_func 
                          then bsit_afast_func.dat_integr_sit_afast_func
                          else bsit_afast_func.dat_inic_pagto_inss.
   if bsit_afast.idi_signif_sit = 7 or
      bsit_afast.idi_signif_sit = 9 then do:
      assign dt-aux-sitfim = if bsit_afast_func.dat_term_proces_sit_afast > bcontrole-catponto.dat_term_period_apurac_pto_mes and 
                                not l-desligado then bcontrole-catponto.dat_term_period_apurac_pto_mes  /* cris func demit */
                             else bsit_afast_func.dat_term_proces_sit_afast.
      if bsit_afast.idi_signif_sit = 9 and l-desligado = yes and v_log_justif = yes then do:
         if dt-aux-sitini < dat-ini-mes then
            assign dt-aux-sitini = dat-ini-mes.
      end.
   end.
   else do:
      assign dt-aux-sitfim = if bsit_afast_func.dat_term_proces_sit_afast > dat-fim-mes
                             then dat-fim-mes
                             else bsit_afast_func.dat_term_proces_sit_afast.
   end.


END PROCEDURE.

PROCEDURE PI-TRATA-SITUAC:
   /******** Tratando DSR ******************/
   /**** trata quantidade de minutos atrasos - mensal/semanal ************/
   find first tt-aux-semana where
        tt-aux-semana.dat-fim >= dt-aux-sitini no-lock no-error.
   if not available tt-aux-semana then
      find last tt-aux-semana no-lock no-error.

   do dt-dia-cal = dt-aux-sitini to dt-aux-sitfim:
      if dt-dia-cal > tt-aux-semana.dat-fim then do:
         find first tt-aux-semana where
              tt-aux-semana.dat-fim >= dt-dia-cal no-lock no-error.
         if not available tt-aux-semana then
           find last tt-aux-semana no-lock no-error. 
      end.
      /*** UPC PROTEGE ***/
      FOR EACH tt-epc WHERE tt-epc.cod-event = "valida_dt_protege":
         DELETE tt-epc. 
      END.

      CREATE tt-epc.
      ASSIGN tt-epc.cod-event = "valida_dt_protege"
             &if '{&cd_rel_hr}' < '2.11' &then
                tt-epc.val-parameter = STRING(bfunc-ponto.cdn_empresa,"999") +
                                       STRING(bfunc-ponto.cdn_estab,"999") +
                                       STRING(bfunc-ponto.cdn_funcionario,"99999999")
             &else
                tt-epc.val-parameter = STRING(bfunc-ponto.cdn_empresa) + ';' +
                                       STRING(bfunc-ponto.cdn_estab) + ';' +
                                       STRING(bfunc-ponto.cdn_funcionario)
             &endif
             tt-epc.cod-parameter = STRING(dt-dia-cal,"99/99/9999") /*Dia do C lculo*/.

      {include/i-epc201.i "valida_dt_protege"}

       IF RETURN-VALUE = "NOK":U THEN
          NEXT.
      /******/

      if bsit_afast.log_influi_repous or bsit_afast.log_influi_para_fer then do:      
         if bsit_afast.idi_signif_sit = 8 then do:

            /* gabriela */   
            if tt-aux-semana.dat-fim = v_dat_fim_sem then do:
               for each efp_par_marcac_ptoelet of bfunc-ponto EXCLUSIVE-LOCK where
                        efp_par_marcac_ptoelet.dat_proces_mpe   = bsit_afast_func.dat_inic_proces_sit_afast and
                        efp_par_marcac_ptoelet.num_horar_inic_proces_mpe >= bsit_afast_func.num_hora_inic_proces_sit_afast and
                        efp_par_marcac_ptoelet.num_horar_fim_proces_mpe <= bsit_afast_func.num_horar_fim_proces_sit_afast:
                    assign i-aux-min-atraso = i-aux-min-atraso + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
               end.

               assign i-aux-qtd-atrasos = i-aux-qtd-atrasos + 1.
            end.
            else
               assign i-aux-min-atraso  = i-aux-min-atraso + tt-aux-semana.min-atraso
                      i-aux-qtd-atrasos = i-aux-qtd-atrasos + tt-aux-semana.qtd-atrasos.
            if v_qtd_lim_minut_atraso > 0 or bcatponto.qtd_lim_atraso > 0 then do: /*PE0180 - DSR*/

               if tt-aux-semana.dat-fim <= bcontrole-catponto.dat_term_period_apurac_pto_mes or
                  l-desligado then do:  /* cris func demit */

                  if v_qtd_lim_minut_atraso > 0 then 
                     run PI-PERDE-DSR-LIM-MIN.

                  /**** trata quantidade de atrasos - mensal/semanal */
                  if bcatponto.qtd_lim_atraso > 0 then do:
                     run PI-PERDE-DSR-QTD.
                  end. /* quant limite atraso > 0 */
               end. /* fim < term per */
               else do: /* desconto no mˆs seguinte */
                  if substring(bcatponto.cod_livre_1,3,1) = "1" then do:
                     if v_qtd_lim_minut_atraso > 0 then do:
                        if bcatponto.idi_lim_minut_atraso = 1 then do: /** mensal **/ 
                           if i-aux-min-atraso > v_qtd_lim_minut_atraso and tt-aux-semana.min-atraso > 0 then do:
                              if bfunc-ponto.log_func_descta_dsr = yes or
                                 bfunc-ponto.log_func_descta_fer = yes then
                                 assign substring(bfunc-ponto.cod_livre_2,03,01) = "s"
                                        v_log_perde_dsr                          = yes.
                           end.          
                        end.
                        else do: /*** semanal ****/
                           if tt-aux-semana.min-atraso > v_qtd_lim_minut_atraso then do:
                              if bfunc-ponto.log_func_descta_dsr = yes or
                                 bfunc-ponto.log_func_descta_fer = yes then
                                 assign substring(bfunc-ponto.cod_livre_2,03,01) = "s"
                                        v_log_perde_dsr                          = yes.
                           end.             
                        end.
                     end.
                     if bcatponto.qtd_lim_atraso > 0 then do:
                        if bcatponto.idi_lim_quant_atraso = 1 then do: /** mensal **/ 
                           assign i-aux-qtd-atrasos = i-aux-qtd-atrasos + tt-aux-semana.qtd-atrasos.
                           if i-aux-qtd-atrasos > bcatponto.qtd_lim_atraso and tt-aux-semana.qtd-atrasos > 0 then do:
                              if bfunc-ponto.log_func_descta_dsr = yes or
                                 bfunc-ponto.log_func_descta_fer = yes then
                                 assign substring(bfunc-ponto.cod_livre_2,03,01) = "s".
                           end.          
                        end. /* quant atraso = 1 */
                        else do:
                           if tt-aux-semana.qtd-atrasos > bcatponto.qtd_lim_atraso then do:
                              if bfunc-ponto.log_func_descta_dsr = yes or
                                 bfunc-ponto.log_func_descta_fer = yes then
                                 assign substring(bfunc-ponto.cod_livre_2,03,01) = "s".
                           end.
                        end. /* else */
                     end.
                  end.
               end. /* desconta no mˆs seguinte */
            end. /* parametriza perda dsr */
            else do: /* nÆo parametrizou perda de dsr - perde sempre */
               if tt-aux-semana.dat-fim <= bcontrole-catponto.dat_term_period_apurac_pto_mes or l-desligado then do:
                   run PI-PERDE-DSR-QTD.

                  /* gabriela */
                  find last btt-aux-semana no-lock no-error.
                  if tt-aux-semana.qtd-atrasos > 0 and
                     tt-aux-semana.qtd-dsr-sem = 0 and
                     btt-aux-semana.dat-fim = tt-aux-semana.dat-fim and
                     bcatponto.num_livre_1 = 2 and /*Periodo de Ponto*/
                     not l-desligado then 
                     assign substring(bfunc-ponto.cod_livre_2,03,01) = "s".
                  else do:
                      if tt-aux-semana.qtd-atrasos > 0 and
                         btt-aux-semana.dat-fim >= tt-aux-semana.dat-fim - int(weekday(btt-aux-semana.dat-fim) - 1) and
                         bcatponto.num_livre_1 = 1 and /*Mes Referencia*/
                         not l-desligado then 
                          assign substring(bfunc-ponto.cod_livre_2,03,01) = "s".
                  end.
               end.
               else do: /* perde dsr do mˆs seguinte */
                   if substring(bcatponto.cod_livre_1,3,1) = "1" then do:
                     if bcatponto.idi_lim_quant_atraso = 1 then do: /** mensal **/ 
                        /*gabriela */
                       /* assign i-aux-qtd-atrasos = i-aux-qtd-atrasos + tt-aux-semana.qtd-atrasos.*/
                        if i-aux-qtd-atrasos > bcatponto.qtd_lim_atraso and tt-aux-semana.qtd-atrasos > 0 then do: 
                           if bfunc-ponto.log_func_descta_dsr or
                              bfunc-ponto.log_func_descta_fer then
                              assign substring(bfunc-ponto.cod_livre_2,03,01) = "s".
                        end.          
                     end. /* quant atraso = 1 */
                     else do:
                        if tt-aux-semana.qtd-atrasos > bcatponto.qtd_lim_atraso then do:
                           if bfunc-ponto.log_func_descta_dsr or
                              bfunc-ponto.log_func_descta_fer then
                              assign substring(bfunc-ponto.cod_livre_2,03,01) = "s".
                        end.
                     end. /* else */
                  end.
               end.
            end. /* else */
         end. /* jornada incompleta */
         else do:
                if tt-aux-semana.dat-fim > bcontrole-catponto.dat_term_period_apurac_pto_mes and
                   l-desligado = no THEN DO:
                   if bfunc-ponto.log_func_descta_dsr or
                      bfunc-ponto.log_func_descta_fer THEN  
                      assign substring(bfunc-ponto.cod_livre_2,03,01) = "s".
                END.
            else do:
               /*** perde feriado dentro do mes *****/
               if bsit_afast.log_influi_para_fer and tt-aux-semana.qtd-fer-sem > 0 then do:
                  assign v_log_ferias = no
                         v_log_dsr_re_fe = no.
                  if bfunc-ponto.log_func_descta_fer = yes then do:
                     find last btt-aux-semana where
                         btt-aux-semana.dat-fim < tt-aux-semana.dat-fim no-error.
                     if avail btt-aux-semana then do:
                         do dt-aux = btt-aux-semana.dat-fim + 1 to tt-aux-semana.dat-fim:
                             {prghur/pep/pe9994.i dt-aux}
                             find tt-calendar where
                                  tt-calendar.cdn_turno_trab   = v_cdn_turno and
                                  tt-calendar.cdn_turma_trab   = v_cdn_turma and
                                  tt-calendar.dat_refer_calend = dt-aux  and
                                  tt-calendar.cod_pais         = v_cod_pais  and  
                                  tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
                             if avail tt-calendar and tt-calendar.idi_sit_dia_trab = 4 then do: /*Feriado*/
                                if can-find (first jorn_trab no-lock where     
                                                    jorn_trab.cdn_jorn_trab = tt-calendar.cdn_jorn_trab and 
                                                    jorn_trab.idi_sit_dia_trab = 3) then   /*** Evandro  - Desconta 2 DSR quando FE cai em um RE ***/
                                   assign v_log_dsr_re_fe = yes.
                                find last sit_afast_func of bfunc-ponto no-lock where
                                          sit_afast_func.dat_inic_sit_afast <= dt-aux and
                                          sit_afast_func.dat_term_sit_afast >= dt-aux no-error.
                                if avail sit_afast_func then
                                    find sit_afast of sit_afast_func no-lock no-error.
                                if avail sit_afast_func and sit_afast.idi_signif_sit = 5 then
                                    assign v_log_ferias = yes.
                                leave.
                             end.
                         end.
                     end.
                     if not v_log_ferias and
                        not v_log_dsr_re_fe then
                         assign i-qtd-fer-per     = i-qtd-fer-per + tt-aux-semana.qtd-fer-sem 
                                d-hrs-diu-fer-per = d-hrs-diu-fer-per + tt-aux-semana.qtd-hrs-diu-fer
                                d-hrs-not-fer-per = d-hrs-not-fer-per + tt-aux-semana.qtd-hrs-not-fer
                                tt-aux-semana.qtd-fer-sem     = 0
                                tt-aux-semana.qtd-hrs-diu-fer = 0
                                tt-aux-semana.qtd-hrs-not-fer = 0
                                v_log_perde_dsr_falta         = yes.
                  end.
               end.
               if bsit_afast.log_influi_repous then do:
                  if tt-aux-semana.qtd-dsr-sem > 0 then do:
                     if bfunc-ponto.log_func_descta_dsr = yes then do:
                         assign i-qtd-dsr-per     = i-qtd-dsr-per + tt-aux-semana.qtd-dsr-sem 
                                d-hrs-diu-dsr-per = d-hrs-diu-dsr-per + tt-aux-semana.qtd-hrs-diu-dsr
                                d-hrs-not-dsr-per = d-hrs-not-dsr-per + tt-aux-semana.qtd-hrs-not-dsr
                                tt-aux-semana.qtd-dsr-sem     = 0
                                tt-aux-semana.qtd-hrs-diu-dsr = 0
                                tt-aux-semana.qtd-hrs-not-dsr = 0
                                v_log_desctou_dsr             = yes
                                v_log_perde_dsr_falta         = yes.
                     end.
                  end.
                  else do:
                     if tt-aux-semana.dat-fim = (dat-ini-mes - 1) and 
                        not v_log_desctou_dsr then do:
                        find first tt-aux-semana where
                             tt-aux-semana.dat-fim >= dt-dia-cal and
                             tt-aux-semana.dat-fim >  (dat-ini-mes - 1) no-lock no-error.
                        if avail tt-aux-semana and tt-aux-semana.qtd-dsr-sem > 0 then do:
                           if bfunc-ponto.log_func_descta_dsr = yes then do:
                              assign i-qtd-dsr-per     = i-qtd-dsr-per + tt-aux-semana.qtd-dsr-sem 
                                     d-hrs-diu-dsr-per = d-hrs-diu-dsr-per + tt-aux-semana.qtd-hrs-diu-dsr
                                     d-hrs-not-dsr-per = d-hrs-not-dsr-per + tt-aux-semana.qtd-hrs-not-dsr
                                     tt-aux-semana.qtd-dsr-sem     = 0
                                     tt-aux-semana.qtd-hrs-diu-dsr = 0
                                     tt-aux-semana.qtd-hrs-not-dsr = 0
                                     v_log_perde_dsr_falta         = yes.
                           end.
                        end.
                     end.
                  end.
               end. 
            end.
         end. /* else */
         if v_log_perde_dsr or
            v_log_perde_dsr_falta then do:
            /* EPC para tratar gravacao dos domingos de dsr perdidos */
             for each tt-epc where tt-epc.cod-event = "grava_dsr":
                delete tt-epc. 
             end.

             create tt-epc.
             assign tt-epc.cod-event = "grava_dsr"
                    &if '{&cd_rel_hr}' < '2.11' &then
                        tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                               string(bfunc-ponto.cdn_estab,"999") +
                                               string(bfunc-ponto.cdn_funcionario,"99999999")
                    &else
                        tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + ';' +
                                               string(bfunc-ponto.cdn_estab) + ';' + 
                                               string(bfunc-ponto.cdn_funcionario)
                    &endif
                    tt-epc.cod-parameter = string(tt-aux-semana.dat-fim) /*Domingo perdido*/.
             {include/i-epc201.i "grava_dsr"}
             /********************************************************************/
         end.
      end. /* influi repouso */
      assign v_dat_fim_sem = tt-aux-semana.dat-fim.
   end. /* do */
    /***********************************************/

   if bsit_afast_func.dat_integr_sit_afast_func = ? then do:
      assign bsit_afast_func.dat_integr_sit_afast_func     = dt-aux-sitfim
             bsit_afast_func.num_mes_ano_refer_fp          = c-mmaa-folha
             bsit_afast_func.dat_ant_integr_sit_afast_func = bsit_afast_func.dat_integr_sit_afast_func.
   end.       
   else do:
      if bsit_afast_func.dat_integr_sit_afast_func = bsit_afast_func.dat_ant_integr_sit_afast_func then
         assign bsit_afast_func.dat_integr_sit_afast_func     = dt-aux-sitfim
                bsit_afast_func.num_mes_ano_refer_fp          = c-mmaa-folha.
      else
         assign bsit_afast_func.dat_ant_integr_sit_afast_func = bsit_afast_func.dat_integr_sit_afast_func
                bsit_afast_func.dat_integr_sit_afast_func     = dt-aux-sitfim
                bsit_afast_func.num_mes_ano_refer_fp          = c-mmaa-folha.
   end. 
END PROCEDURE.

PROCEDURE PI-HRS-DIU:  /****** cris 17/12/1999 *******/
   if v_log_func_ant = yes then do:
      assign v_log_func_ant = no.
      find bfunc-ponto no-lock where
           bfunc-ponto.cdn_empresa     = bsit_afast_func.cdn_empresa and
           bfunc-ponto.cdn_estab       = bsit_afast_func.cdn_estab   and
           bfunc-ponto.cdn_funcionario = bsit_afast_func.cdn_funcionario no-error.
   end.

   if (d-tot-hrs-sit + i-qtd-hrs) > d-tot-hrsmes then do:
      find first event_fp no-lock where
          event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
          event_fp.cdn_event_fp = i-ev-codigo no-error.            
      if avail event_fp then do:
          if event_fp.idi_ident_efp = 1 then do:
              assign v_num_erro = v_num_erro + 1.
              {utp/ut-liter.i Situa‡Æo mpe L}
              assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
              {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
              assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
              {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_normais mpe L}
              assign v_des_erro = v_des_erro + trim(return-value).
              create tt_erro.
              assign tt_erro.num_erro = v_num_erro
                     tt_erro.des_erro = v_des_erro
                     tt_erro.log_erro = yes.
/*               next. comentado para atender fo 2218328 - protege*/
          end.
      end.

      assign i-ev-cod-ant = i-ev-codigo.
      if d-tot-hrs-sit < d-tot-hrsmes then do:
         IF i-ev-hrs-dif-diu <> "" THEN DO:
            find first event_fp no-lock where
                       event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                       event_fp.cdn_event_fp = i-ev-codigo no-error.

            assign i-ev-cod-ant = i-ev-codigo
                   i-qtd-hrs    = i-qtd-hrs - (d-tot-hrsmes - d-tot-hrs-sit)
                   i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                     event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                     event_fp.cdn_event_fp <> v_cdn_evt_495 
                                  then i-ev-hrs-dif-diu 
                                  else i-ev-codigo.

            if i-ev-codigo = v_cdn_evt_490 or 
               i-ev-codigo = v_cdn_evt_495 then
               assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                    then v_cdn_evt_995 
                                    else v_cdn_evt_996.                   

            {prghur/pep/pe4000.i1}.
         END.
         assign i-qtd-hrs   = d-tot-hrsmes - d-tot-hrs-sit 
                i-ev-codigo = i-ev-cod-ant 
             /* d-hrs-diu-trb-sit = d-qtd-hrs-diu  cris */.

         {prghur/pep/pe4000.i1}.
      end. 
      ELSE DO:
          IF i-ev-hrs-dif-diu <> "" THEN DO:
              find first event_fp no-lock where
                         event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                         event_fp.cdn_event_fp = i-ev-codigo no-error.
              assign i-ev-cod-ant = i-ev-codigo
                     i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                       event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                       event_fp.cdn_event_fp <> v_cdn_evt_495 
                                    then i-ev-hrs-dif-diu 
                                    else i-ev-codigo.

              if i-ev-codigo = v_cdn_evt_490 or 
                 i-ev-codigo = v_cdn_evt_495 then
                 assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                      then v_cdn_evt_995 
                                      else v_cdn_evt_996.                     

             {prghur/pep/pe4000.i1}.

             ASSIGN i-ev-codigo = i-ev-cod-ant.
          END.

          assign i-qtd-hrs   = 0. 
      end.        
   end.
   else do: 
      {prghur/pep/pe4000.i1}.
      if search ("prghur~\fpp~\fppl25rp.p") <> ? /*and (NOT l-afast-periodo OR l-afast-mes ) MRS*/ then 
         IF movto_ptoelet.qtd_movto_ptoelet > d-tot-hrsmes THEN 
            assign movto_ptoelet.qtd_movto_ptoelet = d-tot-hrsmes.
   end.
END PROCEDURE.

PROCEDURE PI-HRS-NOT: /****** cris 17/12/1999 *********/
   if v_log_func_ant = yes then do:
      assign v_log_func_ant = no.
      find bfunc-ponto no-lock where
           bfunc-ponto.cdn_empresa     = bsit_afast_func.cdn_empresa and
           bfunc-ponto.cdn_estab       = bsit_afast_func.cdn_estab   and
           bfunc-ponto.cdn_funcionario = bsit_afast_func.cdn_funcionario no-error.
   end.
   if (d-tot-hrs-sit + i-qtd-hrs) > d-tot-hrsmes then do:
      find first event_fp no-lock where
          event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
          event_fp.cdn_event_fp = i-ev-codigo no-error.
      if avail event_fp then do:
          if event_fp.idi_ident_efp = 1 then do:
              assign v_num_erro = v_num_erro + 1.
              {utp/ut-liter.i Situa‡Æo mpe L}
              assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
              {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
              assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
              {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_normais mpe L}
              assign v_des_erro = v_des_erro + trim(return-value).
              create tt_erro.
              assign tt_erro.num_erro = v_num_erro
                     tt_erro.des_erro = v_des_erro
                     tt_erro.log_erro = yes.
              /*next.*/
          end.
      end.
      assign i-ev-cod-ant = i-ev-codigo.
      if d-tot-hrs-sit < d-tot-hrsmes then do:
         IF i-ev-hrs-dif-not <> "" THEN DO:
              find first event_fp no-lock where
                         event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                         event_fp.cdn_event_fp = i-ev-codigo no-error.

              assign i-ev-cod-ant = i-ev-codigo
                     i-qtd-hrs    = i-qtd-hrs - (d-tot-hrsmes - d-tot-hrs-sit)
                     i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 and 
                                       event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                       event_fp.cdn_event_fp <> v_cdn_evt_495 
                                    then i-ev-hrs-dif-not 
                                    else i-ev-codigo.

              if i-ev-codigo = v_cdn_evt_490 or 
                 i-ev-codigo = v_cdn_evt_495 then
                 assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                      then v_cdn_evt_995 
                                      else v_cdn_evt_996.

              {prghur/pep/pe4000.i1}.

              /* gravar a suplementacao das horas noturnas no evento de desconto */
              IF bsit_afast.cod_event_suplem_notur <> "" AND
                 bsit_afast.cod_event_suplem_notur <> i-ev-cod-ant THEN DO:

                 if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                    v_log_sind_suplem = no then
                    assign i-qtd-hrs   = 0.
                 else
                    assign i-qtd-hrs   = (i-qtd-hrs * 0.1428572).

                 {prghur/pep/pe4000.i1}.
              END.
         END.

         assign i-qtd-hrs   = d-tot-hrsmes - d-tot-hrs-sit 
                i-ev-codigo = i-ev-cod-ant 
               /* d-hrs-not-trb-sit = d-qtd-hrs-not cris */.

         {prghur/pep/pe4000.i1}.
      end. 
      ELSE DO:
         IF i-ev-hrs-dif-not <> "" THEN DO:
              find first event_fp no-lock where
                         event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                         event_fp.cdn_event_fp = i-ev-codigo no-error.
              assign i-ev-cod-ant = i-ev-codigo
                     i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                       event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                       event_fp.cdn_event_fp <> v_cdn_evt_495 
                                    then i-ev-hrs-dif-not 
                                    else i-ev-codigo.

              if i-ev-codigo = v_cdn_evt_490 or 
                 i-ev-codigo = v_cdn_evt_495 then
                 assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                      then v_cdn_evt_995 
                                      else v_cdn_evt_996.

              {prghur/pep/pe4000.i1}.

              /* gravar a suplementacao das horas noturnas no evento de desconto */
              IF bsit_afast.cod_event_suplem_notur <> "" AND
                 bsit_afast.cod_event_suplem_notur <> i-ev-cod-ant THEN DO:

                 if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                    v_log_sind_suplem = no then
                    assign i-qtd-hrs   = 0.
                 else
                    assign i-qtd-hrs   = (i-qtd-hrs * 0.1428572).

                 {prghur/pep/pe4000.i1}.
              END.

              ASSIGN i-ev-codigo = i-ev-cod-ant.
         END.

         assign i-qtd-hrs = 0.
      END.
   end.
   else do:          
       /* assign d-hrs-not-trb-sit = d-hrs-not-trb-sit + i-qtd-hrs. cris */
        {prghur/pep/pe4000.i1}.
        if search ("prghur~\fpp~\fppl25rp.p") <> ? /*and (NOT l-afast-periodo OR l-afast-mes )MRS */ then 
           IF movto_ptoelet.qtd_movto_ptoelet > d-tot-hrsmes THEN 
              assign movto_ptoelet.qtd_movto_ptoelet = d-tot-hrsmes.
   end.
END PROCEDURE.

PROCEDURE PI-PERDE-DSR-LIM-MIN:
   if bcatponto.idi_lim_minut_atraso = 1 then do: /** mensal **/
      if i-aux-min-atraso > v_qtd_lim_minut_atraso and tt-aux-semana.min-atraso > 0 then do:
         if bfunc-ponto.log_func_descta_dsr then do:
            if substring(bcatponto.cod_livre_1,3,1) = "1" then
               assign d-hrs-diu-dsr-per = d-hrs-diu-dsr-per + tt-aux-semana.qtd-hrs-diu-dsr
                      d-hrs-not-dsr-per = d-hrs-not-dsr-per + tt-aux-semana.qtd-hrs-not-dsr
                      i-qtd-dsr-per     = i-qtd-dsr-per + tt-aux-semana.qtd-dsr-sem
                      /*tt-aux-semana.qtd-fer-sem     = 0*/
                      tt-aux-semana.qtd-hrs-diu-dsr = 0
                      tt-aux-semana.qtd-hrs-not-dsr = 0
                      i-aux-min-atraso              = 0.
            assign v_log_perde_dsr = yes.
         end.
         if bsit_afast.log_influi_para_fer and tt-aux-semana.qtd-fer-sem > 0 then do:
            if bfunc-ponto.log_func_descta_fer then do:
               if substring(bcatponto.cod_livre_1,3,1) = "1" then
                  assign d-hrs-diu-fer-per = d-hrs-diu-fer-per + tt-aux-semana.qtd-hrs-diu-fer
                         d-hrs-not-fer-per = d-hrs-not-fer-per + tt-aux-semana.qtd-hrs-not-fer
                         i-qtd-fer-per     = i-qtd-fer-per + tt-aux-semana.qtd-fer-sem
                         tt-aux-semana.qtd-fer-sem     = 0
                         tt-aux-semana.qtd-hrs-diu-fer = 0
                         tt-aux-semana.qtd-hrs-not-fer = 0.
               assign v_log_perde_dsr = yes.
            end.
         end.
      end.          
   end.
   else do: /*** semanal ****/
      if tt-aux-semana.min-atrasos > v_qtd_lim_minut_atraso  then do:
     /* if round(((tt-aux-semana.min-atrasos / 3600) * 60),0) > round(((bcatponto.qtd_lim_minut_atraso / 3600) * 60),0) then do: /** Evandro - Problema de desconto de DSR por diferen‡a de 1 min **/ */
         if bfunc-ponto.log_func_descta_dsr then do:
            if substring(bcatponto.cod_livre_1,3,1) = "1" then
               assign d-hrs-diu-dsr-per = d-hrs-diu-dsr-per + tt-aux-semana.qtd-hrs-diu-dsr
                      d-hrs-not-dsr-per = d-hrs-not-dsr-per + tt-aux-semana.qtd-hrs-not-dsr
                      i-qtd-dsr-per     = i-qtd-dsr-per + tt-aux-semana.qtd-dsr-sem
                      /*tt-aux-semana.qtd-fer-sem     = 0*/
                      tt-aux-semana.qtd-hrs-diu-dsr = 0
                      tt-aux-semana.qtd-hrs-not-dsr = 0.
            assign v_log_perde_dsr   = yes
                   v_log_desctou_dsr = yes.
         end.
         if bsit_afast.log_influi_para_fer and tt-aux-semana.qtd-fer-sem > 0 then do:
            if bfunc-ponto.log_func_descta_fer then do:
               if substring(bcatponto.cod_livre_1,3,1) = "1" then
                  assign d-hrs-diu-fer-per = d-hrs-diu-fer-per + tt-aux-semana.qtd-hrs-diu-fer
                         d-hrs-not-fer-per = d-hrs-not-fer-per + tt-aux-semana.qtd-hrs-not-fer
                         i-qtd-fer-per     = i-qtd-fer-per + tt-aux-semana.qtd-fer-sem
                         tt-aux-semana.qtd-fer-sem     = 0
                         tt-aux-semana.qtd-hrs-diu-fer = 0
                         tt-aux-semana.qtd-hrs-not-fer = 0.
                  assign v_log_perde_dsr = yes.
            end.
         end.
      end.             
   end.
END PROCEDURE.

PROCEDURE PI-PERDE-DSR-QTD:
   if bcatponto.idi_lim_quant_atraso = 1 then do: /** mensal **/ 
      /*gabriela */
      /*assign i-aux-qtd-atrasos = i-aux-qtd-atrasos + tt-aux-semana.qtd-atrasos. */
      if i-aux-qtd-atrasos > bcatponto.qtd_lim_atraso and tt-aux-semana.qtd-atrasos > 0 then do:
         if bfunc-ponto.log_func_descta_dsr = yes then do:
            if substring(bcatponto.cod_livre_1,3,1) = "1" then
               assign d-hrs-diu-dsr-per = d-hrs-diu-dsr-per + tt-aux-semana.qtd-hrs-diu-dsr
                      d-hrs-not-dsr-per = d-hrs-not-dsr-per + tt-aux-semana.qtd-hrs-not-dsr
                      i-qtd-dsr-per     = i-qtd-dsr-per + tt-aux-semana.qtd-dsr-sem
                      /*tt-aux-semana.qtd-fer-sem     = 0*/
                      tt-aux-semana.qtd-hrs-diu-dsr = 0
                      tt-aux-semana.qtd-hrs-not-dsr = 0.
            assign v_log_perde_dsr = yes.
         end.
         if bsit_afast.log_influi_para_fer and tt-aux-semana.qtd-fer-sem > 0 then do:
            if bfunc-ponto.log_func_descta_fer = yes then do:
               if substring(bcatponto.cod_livre_1,3,1) = "1" then
                  assign d-hrs-diu-fer-per = d-hrs-diu-fer-per + tt-aux-semana.qtd-hrs-diu-fer
                         d-hrs-not-fer-per = d-hrs-not-fer-per + tt-aux-semana.qtd-hrs-not-fer
                         i-qtd-fer-per     = i-qtd-fer-per + tt-aux-semana.qtd-fer-sem
                         tt-aux-semana.qtd-fer-sem     = 0
                         tt-aux-semana.qtd-hrs-diu-fer = 0
                         tt-aux-semana.qtd-hrs-not-fer = 0.
               assign v_log_perde_dsr = yes.
            end.
         end.
      end.          
   end. /* quant atraso = 1 */
   else do:
      if tt-aux-semana.qtd-atrasos > bcatponto.qtd_lim_atraso then do:
         if bfunc-ponto.log_func_descta_dsr = yes then do:
            if substring(bcatponto.cod_livre_1,3,1) = "1" then
               assign d-hrs-diu-dsr-per = d-hrs-diu-dsr-per + tt-aux-semana.qtd-hrs-diu-dsr
                      d-hrs-not-dsr-per = d-hrs-not-dsr-per + tt-aux-semana.qtd-hrs-not-dsr
                      i-qtd-dsr-per     = i-qtd-dsr-per + tt-aux-semana.qtd-dsr-sem
                      /*tt-aux-semana.qtd-fer-sem     = 0*/
                      tt-aux-semana.qtd-hrs-diu-dsr = 0
                      tt-aux-semana.qtd-hrs-not-dsr = 0.
            assign v_log_perde_dsr = yes.
         end.
         if bsit_afast.log_influi_para_fer and tt-aux-semana.qtd-fer-sem > 0 then do:
            if bfunc-ponto.log_func_descta_fer = yes then do:
               if substring(bcatponto.cod_livre_1,3,1) = "1" then
                  assign d-hrs-diu-fer-per = d-hrs-diu-fer-per + tt-aux-semana.qtd-hrs-diu-fer
                         d-hrs-not-fer-per = d-hrs-not-fer-per + tt-aux-semana.qtd-hrs-not-fer
                         i-qtd-fer-per     = i-qtd-fer-per + tt-aux-semana.qtd-fer-sem
                         tt-aux-semana.qtd-fer-sem     = 0
                         tt-aux-semana.qtd-hrs-diu-fer = 0
                         tt-aux-semana.qtd-hrs-not-fer = 0.
               assign v_log_perde_dsr = yes.
            end.
         end.
      end.
   end. /* else */
END PROCEDURE.

PROCEDURE pi-movto-jorn-incompleta:
   if v_log_func_ant = yes then
      find bfunc-ponto no-lock where
           bfunc-ponto.cdn_empresa     = bsit_func.cdn_empresa and
           bfunc-ponto.cdn_estab       = bsit_func.cdn_estab   and
           bfunc-ponto.cdn_funcionario = bsit_func.cdn_funcionario no-error.
   {prghur/pep/pe4000.i1}.
   if v_log_func_ant = yes then
      find bfunc-ponto no-lock where
           bfunc-ponto.cdn_empresa     = bsit_func.cdn_empres_orig and
           bfunc-ponto.cdn_estab       = bsit_func.cdn_estab_orig  and
           bfunc-ponto.cdn_funcionario = bsit_func.cdn_func_orig   no-error.
end.

PROCEDURE pi-movto-sit:

/*     find first event_fp no-lock where                                              */
/*                event_fp.cdn_event_fp = bsit_afast.cdn_event_afast_diurno no-error. */
/*     if avail event_fp then do:                                                     */
/*        if event_fp.idi_ident_efp       = 1 and                                     */
/*           event_fp.idi_tip_inciden_liq = 1 then next.                              */
/*     end.                                                                           */
    
    if v_qtd_lim_minut_atraso > 0 or bcatponto.qtd_lim_atraso > 0 then do:

        /* desconta situacao conforme regra de perda de dsr */
        if substring(bcatponto.cod_livre_1,4,1) = "1" then do:
           if (bsit_afast.log_influi_repous or bsit_afast.log_influi_para_fer ) and 
              (bfunc-ponto.log_func_descta_dsr or bfunc-ponto.log_func_descta_fer) then do:
             if v_log_perde_dsr then do:
                /*** DIURNO ***/
                if v_qtd_hrs_diu_desc_dsr > 0 then do:
                   for each tt-sit-descta no-lock where
                            tt-sit-descta.v_log_diu = yes:
                      assign i-ev-codigo = tt-sit-descta.v_cdn_efp
                             i-qtd-hrs   = truncate(round(tt-sit-descta.v_qtd_hrs,3),3).
                      run pi-movto-jorn-incompleta.
                      find first event_fp no-lock where
                            event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                            event_fp.cdn_event_fp = i-ev-codigo no-error.
                      if avail event_fp and
                          event_fp.idi_ident_efp <> 2 then
                          assign d-tot-hrs-sit     = d-tot-hrs-sit + tt-sit-descta.v_qtd_hrs
                                 d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + tt-sit-descta.v_qtd_hrs_aux.
                   end.
                end.

                /*** NOTURNO ***/
                if v_qtd_hrs_not_desc_dsr > 0 then do:
                   for each tt-sit-descta no-lock where
                            tt-sit-descta.v_log_diu = no:
                      assign i-ev-codigo = tt-sit-descta.v_cdn_efp
                             i-qtd-hrs   = truncate(round(tt-sit-descta.v_qtd_hrs,3),3).
                      find first event_fp no-lock where
                          event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                          event_fp.cdn_event_fp = i-ev-codigo no-error.
                      run pi-movto-jorn-incompleta.
                      assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                 then d-tot-hrs-sit + tt-sit-descta.v_qtd_hrs 
                                                 else d-tot-hrs-sit
                             d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                 then d-hrs-not-trb-sit + tt-sit-descta.v_qtd_hrs_aux 
                                                 else d-hrs-not-trb-sit
                             v_hrs_sit_not_per = v_hrs_sit_not_per + tt-sit-descta.v_qtd_hrs_aux.
                   end.
                   for each tt-supnot-jorinc no-lock:
                      assign i-ev-codigo = tt-supnot-jorinc.v_cdn_efp
                             i-qtd-hrs = truncate(round(tt-supnot-jorinc.v_qtd_hrs,3),3).
                      run pi-movto-jorn-incompleta.
                      find first event_fp no-lock where
                          event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                          event_fp.cdn_event_fp = i-ev-codigo no-error.

                      assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                 then d-tot-hrs-sit + i-qtd-hrs 
                                                 else d-tot-hrs-sit
                             v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                             d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs.
                   end.
                end.

                /* a partir do momento em que a jornada incompleta ultrapassou o limite semanal ou mensal 
                   as temp tables abaixo devem ser eliminadas para nÆo gravar em dobro o que foi acumulado
                   at‚ este momento */
                FOR EACH tt-sit-descta EXCLUSIVE-LOCK :
                    DELETE tt-sit-descta.
                END.
                FOR EACH tt-supnot-jorinc EXCLUSIVE-LOCK :
                    DELETE tt-supnot-jorinc.
                END.

                assign v_qtd_hrs_diu_desc_dsr = 0
                       v_qtd_hrs_not_desc_dsr = 0.
             end.
          end.
          else do:
             /*** DIURNO ***/
             if v_log_perde_dsr then do:
                if v_qtd_hrs_diu_desc_dsr > 0 then do:
                   for each tt-sit-descta no-lock where
                            tt-sit-descta.v_log_diu = yes:
                      assign i-ev-codigo = tt-sit-descta.v_cdn_efp
                              i-qtd-hrs   = truncate(round(tt-sit-descta.v_qtd_hrs,3),3).
                       run pi-movto-jorn-incompleta.
                       find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error.
                       if avail event_fp and
                           event_fp.idi_ident_efp <> 2 then
                           assign d-tot-hrs-sit     = d-tot-hrs-sit + tt-sit-descta.v_qtd_hrs
                                  d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + tt-sit-descta.v_qtd_hrs_aux.
                    end.
                 end.

                 /*** NOTURNO ***/
                 if v_qtd_hrs_not_desc_dsr > 0 then do:
                    for each tt-sit-descta no-lock where
                             tt-sit-descta.v_log_diu = no:
                       assign i-ev-codigo = tt-sit-descta.v_cdn_efp
                              i-qtd-hrs   = truncate(round(tt-sit-descta.v_qtd_hrs,3),3).
                       find first event_fp no-lock where
                           event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                           event_fp.cdn_event_fp = i-ev-codigo no-error.

                       run pi-movto-jorn-incompleta.
                       assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                  then d-tot-hrs-sit + tt-sit-descta.v_qtd_hrs 
                                                  else d-tot-hrs-sit
                              d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                  then d-hrs-not-trb-sit + tt-sit-descta.v_qtd_hrs_aux 
                                                  else d-hrs-not-trb-sit
                              v_hrs_sit_not_per = v_hrs_sit_not_per + tt-sit-descta.v_qtd_hrs_aux.
                    end.
                    for each tt-supnot-jorinc no-lock:
                       assign i-ev-codigo = tt-supnot-jorinc.v_cdn_efp
                              i-qtd-hrs = truncate(round(tt-supnot-jorinc.v_qtd_hrs,3),3).
                       run pi-movto-jorn-incompleta.
                       find first event_fp no-lock where
                           event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                           event_fp.cdn_event_fp = i-ev-codigo no-error.

                       assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                                  then d-tot-hrs-sit + i-qtd-hrs 
                                                  else d-tot-hrs-sit
                              v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                              d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs.
                    end.
                 end.

                 FOR EACH tt-sit-descta EXCLUSIVE-LOCK WHERE
                          tt-sit-descta.v_num_seq = v_num_seq_sit:
                    DELETE tt-sit-descta.
                 END.
                 FOR EACH tt-supnot-jorinc EXCLUSIVE-LOCK WHERE
                          tt-supnot-jorinc.v_num_seq = v_num_seq_sit:
                    DELETE tt-supnot-jorinc.
                 END.

                 assign v_qtd_hrs_diu_desc_dsr = 0
                        v_qtd_hrs_not_desc_dsr = 0.
              end.
           end. 
       end.
       else do:
          /*** DIURNO ***/
          if v_qtd_hrs_diu_desc_dsr > 0 then do:
             for each tt-sit-descta no-lock where
                      tt-sit-descta.v_log_diu = YES AND
                      tt-sit-descta.v_num_seq = v_num_seq_sit:
                assign i-ev-codigo = tt-sit-descta.v_cdn_efp
                       i-qtd-hrs   = truncate(round(tt-sit-descta.v_qtd_hrs,3),3).
                run pi-movto-jorn-incompleta.
                find first event_fp no-lock where
                      event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                      event_fp.cdn_event_fp = i-ev-codigo no-error.
                if avail event_fp and
                    event_fp.idi_ident_efp <> 2 then
                    assign d-tot-hrs-sit     = d-tot-hrs-sit + tt-sit-descta.v_qtd_hrs
                           d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + tt-sit-descta.v_qtd_hrs_aux.
             end.
          end.

          /*** NOTURNO ***/
          if v_qtd_hrs_not_desc_dsr > 0 then do:
             for each tt-sit-descta no-lock where
                      tt-sit-descta.v_log_diu = NO AND
                      tt-sit-descta.v_num_seq = v_num_seq_sit:
                assign i-ev-codigo = tt-sit-descta.v_cdn_efp
                       i-qtd-hrs   = truncate(round(tt-sit-descta.v_qtd_hrs,3),3).
                find first event_fp no-lock where
                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                    event_fp.cdn_event_fp = i-ev-codigo no-error.

                run pi-movto-jorn-incompleta.
                assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                           then d-tot-hrs-sit + tt-sit-descta.v_qtd_hrs 
                                           else d-tot-hrs-sit
                       d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                           then d-hrs-not-trb-sit + tt-sit-descta.v_qtd_hrs_aux 
                                           else d-hrs-not-trb-sit
                       v_hrs_sit_not_per = v_hrs_sit_not_per + tt-sit-descta.v_qtd_hrs_aux.
             end.
             for each tt-supnot-jorinc no-lock:
                assign i-ev-codigo = tt-supnot-jorinc.v_cdn_efp
                       i-qtd-hrs = truncate(round(tt-supnot-jorinc.v_qtd_hrs,3),3).
                run pi-movto-jorn-incompleta.
                find first event_fp no-lock where
                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                    event_fp.cdn_event_fp = i-ev-codigo no-error.

                assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                           then d-tot-hrs-sit + i-qtd-hrs 
                                           else d-tot-hrs-sit
                       v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                       d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs.
             end.
          end.
          FOR EACH tt-sit-descta EXCLUSIVE-LOCK WHERE
                   tt-sit-descta.v_num_seq = v_num_seq_sit:
             DELETE tt-sit-descta.
          END.
          FOR EACH tt-supnot-jorinc EXCLUSIVE-LOCK WHERE
                   tt-supnot-jorinc.v_num_seq = v_num_seq_sit:
             DELETE tt-supnot-jorinc.
          END.
          assign v_qtd_hrs_diu_desc_dsr = 0
                 v_qtd_hrs_not_desc_dsr = 0.
       end.
    end.
    else do:
       /*** DIURNO ***/
       if v_qtd_hrs_diu_desc_dsr > 0 then do:
          for each tt-sit-descta no-lock where
                   tt-sit-descta.v_log_diu = YES AND
                   tt-sit-descta.v_num_seq = v_num_seq_sit:
             assign i-ev-codigo = tt-sit-descta.v_cdn_efp
                    i-qtd-hrs   = truncate(round(tt-sit-descta.v_qtd_hrs,3),3).
             run pi-movto-jorn-incompleta.
             find first event_fp no-lock where
                   event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                   event_fp.cdn_event_fp = i-ev-codigo no-error.
             if avail event_fp and
                 event_fp.idi_ident_efp <> 2 then
                 assign d-tot-hrs-sit     = d-tot-hrs-sit + tt-sit-descta.v_qtd_hrs
                        d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + tt-sit-descta.v_qtd_hrs_aux.
          end.
       end.

       /*** NOTURNO ***/
       if v_qtd_hrs_not_desc_dsr > 0 then do:
          for each tt-sit-descta no-lock where
                   tt-sit-descta.v_log_diu = NO AND
                   tt-sit-descta.v_num_seq = v_num_seq_sit:
             assign i-ev-codigo = tt-sit-descta.v_cdn_efp
                    i-qtd-hrs   = truncate(round(tt-sit-descta.v_qtd_hrs,3),3).
             find first event_fp no-lock where
                 event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                 event_fp.cdn_event_fp = i-ev-codigo no-error.

             run pi-movto-jorn-incompleta.
             assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                        then d-tot-hrs-sit + tt-sit-descta.v_qtd_hrs 
                                        else d-tot-hrs-sit
                    d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 
                                        then d-hrs-not-trb-sit + tt-sit-descta.v_qtd_hrs_aux 
                                        else d-hrs-not-trb-sit
                    v_hrs_sit_not_per = v_hrs_sit_not_per + tt-sit-descta.v_qtd_hrs_aux.
          end.
          for each tt-supnot-jorinc no-lock:
             assign i-ev-codigo = tt-supnot-jorinc.v_cdn_efp
                    i-qtd-hrs = truncate(round(tt-supnot-jorinc.v_qtd_hrs,3),3).
             run pi-movto-jorn-incompleta.
             find first event_fp no-lock where
                 event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                 event_fp.cdn_event_fp = i-ev-codigo no-error.
             assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 then d-tot-hrs-sit + i-qtd-hrs else d-tot-hrs-sit
                    v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                    d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs.
          end.
       end.
       FOR EACH tt-sit-descta EXCLUSIVE-LOCK WHERE
                   tt-sit-descta.v_num_seq = v_num_seq_sit:
          DELETE tt-sit-descta.
       END.
       FOR EACH tt-supnot-jorinc EXCLUSIVE-LOCK WHERE
                   tt-supnot-jorinc.v_num_seq = v_num_seq_sit:
          DELETE tt-supnot-jorinc.
       END.
       assign v_qtd_hrs_diu_desc_dsr = 0
              v_qtd_hrs_not_desc_dsr = 0.
    end.

    if v_log_func_ant = yes then 
       find bfunc-ponto no-lock where
            bfunc-ponto.cdn_empresa     = bsit_func.cdn_empresa and
            bfunc-ponto.cdn_estab       = bsit_func.cdn_estab   and
            bfunc-ponto.cdn_funcionario = bsit_func.cdn_funcionario no-error.
END PROCEDURE.

PROCEDURE pi-elimina-tt:
   FOR EACH tt-sit-descta EXCLUSIVE-LOCK:
      DELETE tt-sit-descta.
   END.
   FOR EACH tt-supnot-jorinc EXCLUSIVE-LOCK:
      DELETE tt-supnot-jorinc.
   END.
END PROCEDURE.

PROCEDURE pi-contrato-desativado.

    /* Contrato Desativado  */
    for each bsit_afast_func of bfunc-ponto exclusive-lock where 
             bsit_afast_func.dat_term_proces_sit_afast >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
             bsit_afast_func.dat_inic_proces_sit_afast <= dat-fim-mes,
       first bsit_afast no-lock where
             bsit_afast.cdn_sit_afast_func = bsit_afast_func.cdn_sit_afast_func and
             bsit_afast.idi_signif_sit = 10 :
       if d-tot-hrs-sit >= d-tot-hrsmes then do:
          assign v_num_erro = v_num_erro + 1.
          {utp/ut-liter.i Situacao mpe L}
          assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
          {utp/ut-liter.i nao_considerada_para_func_ mpe L}
          assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
          {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_trab mpe L}
          assign v_des_erro = v_des_erro + trim(return-value).
          create tt_erro.
          assign tt_erro.num_erro = v_num_erro
                 tt_erro.des_erro = v_des_erro
                 tt_erro.log_erro = yes.
       end.
       run PI-INIC-PERIOD.
         /***** marca situacao como tratada **************/
       if bsit_afast_func.dat_integr_sit_afast_func <> ? and
          bsit_afast_func.dat_integr_sit_afast_func >= dt-aux-sitfim
          then next.
       if bsit_afast_func.dat_integr_sit_afast_func >= dt-aux-sitini then
          assign dt-aux-sitini = bsit_afast_func.dat_integr_sit_afast_func + 1.                       
       run prghur/pep/pe4000r5.p.
       run pi-trata-situac.
    end. /* afastamento */

END PROCEDURE.


PROCEDURE pi_horar_interv.
if tt-calendar.num_livre_1 <> 0 then
   assign v_cdn_interv = tt-calendar.num_livre_1.
if v_cdn_interv > 0 then do:
   if can-find(first alter_jorn_trab no-lock where
      alter_jorn_trab.cdn_empresa               = bfunc-ponto.cdn_empresa         and
      alter_jorn_trab.cdn_estab                 = bfunc-ponto.cdn_estab           and
      alter_jorn_trab.cdn_funcionario           = bfunc-ponto.cdn_funcionario     and
      alter_jorn_trab.cdn_jorn_trab             = v_cdn_jorn                      and
      alter_jorn_trab.dat_inic_alter_jorn_trab <= bsit_afast_func.dat_inic_proces_sit_afast and
      alter_jorn_trab.dat_term_alter_jorn_trab >= bsit_afast_func.dat_inic_proces_sit_afast) then do:
      find last alter_jorn_trab no-lock where
                alter_jorn_trab.cdn_empresa               = bfunc-ponto.cdn_empresa         and
                alter_jorn_trab.cdn_estab                 = bfunc-ponto.cdn_estab           and
                alter_jorn_trab.cdn_funcionario           = bfunc-ponto.cdn_funcionario     and
                alter_jorn_trab.cdn_jorn_trab             = v_cdn_jorn                      and
                alter_jorn_trab.dat_inic_alter_jorn_trab <= bsit_afast_func.dat_inic_proces_sit_afast and
                alter_jorn_trab.dat_term_alter_jorn_trab >= bsit_afast_func.dat_inic_proces_sit_afast no-error.
      if bfunc-ponto.cdn_estab <> '0' then
         find horar_jorn_trab_ptoelet no-lock where
              horar_jorn_trab_ptoelet.cdn_clas_func    = bfunc-ponto.cdn_clas_func             and
              horar_jorn_trab_ptoelet.cdn_empresa      = bfunc-ponto.cdn_empresa               and
              horar_jorn_trab_ptoelet.cdn_estab        = bfunc-ponto.cdn_estab                 and        
              horar_jorn_trab_ptoelet.cdn_categ_sal    = bfunc-ponto.cdn_categ_sal             and 
              horar_jorn_trab_ptoelet.cdn_turno_trab   = v_cdn_turno                           and
              horar_jorn_trab_ptoelet.cdn_jorn_trab    = v_cdn_jorn                            and
              horar_jorn_trab_ptoelet.cdn_interv_refei = alter_jorn_trab.cdn_interv_refei      and
              horar_jorn_trab_ptoelet.cdn_funcionario  = bfunc-ponto.cdn_funcionario           and
              horar_jorn_trab_ptoelet.dat_alter_jorn_trab = alter_jorn_trab.dat_inic_alter_jorn_trab no-error.
      else
         find horar_jorn_trab_ptoelet no-lock where
              horar_jorn_trab_ptoelet.cdn_clas_func       = bfunc-ponto.cdn_clas_func                and
              horar_jorn_trab_ptoelet.cdn_empresa         = bfunc-ponto.cdn_empresa                  and
              horar_jorn_trab_ptoelet.cdn_estab           = bfunc-ponto.cdn_estab_lotac_func_ptoelet and        
              horar_jorn_trab_ptoelet.cdn_categ_sal       = bfunc-ponto.cdn_categ_sal                and 
              horar_jorn_trab_ptoelet.cdn_turno_trab      = v_cdn_turno                              and
              horar_jorn_trab_ptoelet.cdn_jorn_trab       = v_cdn_jorn                               and
              horar_jorn_trab_ptoelet.cdn_interv_refei    = alter_jorn_trab.cdn_interv_refei         and
              horar_jorn_trab_ptoelet.cdn_funcionario     = bfunc-ponto.cdn_funcionario              and
              horar_jorn_trab_ptoelet.dat_alter_jorn_trab = alter_jorn_trab.dat_inic_alter_jorn_trab no-error.
   end. 
   else do:
      if bfunc-ponto.cdn_estab <> '0' then
         find horar_jorn_trab_ptoelet no-lock where
              horar_jorn_trab_ptoelet.cdn_clas_func    = bfunc-ponto.cdn_clas_func and
              horar_jorn_trab_ptoelet.cdn_empresa      = bfunc-ponto.cdn_empresa   and
              horar_jorn_trab_ptoelet.cdn_estab        = bfunc-ponto.cdn_estab     and        
              horar_jorn_trab_ptoelet.cdn_categ_sal    = bfunc-ponto.cdn_categ_sal and 
              horar_jorn_trab_ptoelet.cdn_turno_trab   = v_cdn_turno               and
              horar_jorn_trab_ptoelet.cdn_jorn_trab    = v_cdn_jorn                and
              horar_jorn_trab_ptoelet.cdn_interv_refei = v_cdn_interv              and
              horar_jorn_trab_ptoelet.cdn_funcionario  = {prghur/dop/eng002.i}                         and
              horar_jorn_trab_ptoelet.dat_alter_jorn_trab = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF  no-error.
      else
         find horar_jorn_trab_ptoelet no-lock where
              horar_jorn_trab_ptoelet.cdn_clas_func    = bfunc-ponto.cdn_clas_func and
              horar_jorn_trab_ptoelet.cdn_empresa      = bfunc-ponto.cdn_empresa   and
              horar_jorn_trab_ptoelet.cdn_estab        = bfunc-ponto.cdn_estab_lotac_func_ptoelet and        
              horar_jorn_trab_ptoelet.cdn_categ_sal    = bfunc-ponto.cdn_categ_sal and 
              horar_jorn_trab_ptoelet.cdn_turno_trab   = v_cdn_turno               and
              horar_jorn_trab_ptoelet.cdn_jorn_trab    = v_cdn_jorn                and
              horar_jorn_trab_ptoelet.cdn_interv_refei = v_cdn_interv              and
              horar_jorn_trab_ptoelet.cdn_funcionario  = {prghur/dop/eng002.i}                         and
              horar_jorn_trab_ptoelet.dat_alter_jorn_trab = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF  no-error.
   end. 
   /********* tratamento intervalo **********************/
   if (horar_jorn_trab_ptoelet.num_horar_fim_normal_priper  > 0  or   
       horar_jorn_trab_ptoelet.num_horar_inic_normal_segper > 0) and
      (bsit_afast_func.num_hora_inic_proces_sit_afast <= 
       horar_jorn_trab_ptoelet.num_horar_fim_normal_priper and
       bsit_afast_func.num_horar_fim_proces_sit_afast  >= 
       horar_jorn_trab_ptoelet.num_horar_fim_normal_priper) or 
      (bsit_afast_func.num_hora_inic_proces_sit_afast <= 
       horar_jorn_trab_ptoelet.num_horar_inic_normal_segper and
       bsit_afast_func.num_horar_fim_proces_sit_afast >= 
       horar_jorn_trab_ptoelet.num_horar_inic_normal_segper) then do:

      if bsit_afast_func.num_hora_inic_proces_sit_afast > 
         horar_jorn_trab_ptoelet.num_horar_fim_normal_priper then
         assign hra-aux-ini-int = bsit_afast_func.num_horar_inic_sit_afast.
      else
         assign hra-aux-ini-int = horar_jorn_trab_ptoelet.num_horar_term_origin_priper.
      IF bsit_afast_func.num_horar_fim_proces_sit_afast < 
         horar_jorn_trab_ptoelet.num_horar_inic_normal_segper then
         assign hra-aux-fim-int = bsit_afast_func.num_horar_term_sit_afast.
      else
         assign hra-aux-fim-int = horar_jorn_trab_ptoelet.num_horar_inic_origin_segper.
      if hra-aux-ini-int < v_num_hra_fim_not then do:
         if hra-aux-fim-int <= v_num_hra_fim_not then 
            assign i-int-hra-not = hra-aux-fim-int - hra-aux-ini-int. 
         else          
            assign i-int-hra-not = v_num_hra_fim_not - hra-aux-ini-int
                   i-int-hra-diu = hra-aux-fim-int - v_num_hra_fim_not.
      end.
      if hra-aux-ini-int >= v_num_hra_fim_not and
         hra-aux-fim-int <= v_num_hra_ini_not then do:
         assign i-int-hra-diu = hra-aux-fim-int - hra-aux-ini-int
                i-int-hra-not = 0.
      end.        
      if bsit_afast_func.num_hora_inic_proces_sit_afast <  v_num_hra_ini_not and
         bsit_afast_func.num_horar_fim_proces_sit_afast  >= v_num_hra_ini_not then do:
         if hra-aux-fim-int < 104460 then do:
            assign i-int-hra-diu = v_num_hra_ini_not - hra-aux-ini-int
                   i-int-hra-not = hra-aux-fim-int - v_num_hra_ini_not.
         end.
         else do:
            assign i-int-hra-diu = v_num_hra_ini_not - hra-aux-ini-int
                   i-int-hra-not = 25200
                   i-int-hra-diu = i-int-hra-diu + (hra-aux-fim-int - v_num_hra_fim_not).
         end.          
      end.
   end.

   /**** tratamento lanche no primeiro periodo *******************************/
   if horar_jorn_trab_ptoelet.num_horar_inic_lanche_priper > 0 or  
      horar_jorn_trab_ptoelet.num_horar_term_lanche_priper > 0 and 
     (bsit_afast_func.num_hora_inic_proces_sit_afast <= 
      horar_jorn_trab_ptoelet.num_horar_inic_lanche_priper and
      bsit_afast_func.num_horar_fim_proces_sit_afast >= 
      horar_jorn_trab_ptoelet.num_horar_inic_lanche_priper) or 
     (bsit_afast_func.num_hora_inic_proces_sit_afast <= 
      horar_jorn_trab_ptoelet.num_horar_term_lanche_priper and
      bsit_afast_func.num_horar_fim_proces_sit_afast >= 
      horar_jorn_trab_ptoelet.num_horar_term_lanche_priper) then do:

      if bsit_afast_func.num_hora_inic_proces_sit_afast > 
         horar_jorn_trab_ptoelet.num_horar_inic_lanche_priper then
         assign hra-ini-prilan = bsit_afast_func.num_horar_inic_sit_afast.
      else
         assign hra-ini-prilan = horar_jorn_trab_ptoelet.num_horar_inic_lanche_priper.
      if bsit_afast_func.num_horar_fim_proces_sit_afast < 
         horar_jorn_trab_ptoelet.num_horar_term_lanche_priper then
         assign hra-fim-prilan = bsit_afast_func.num_horar_term_sit_afast.
      else
         assign hra-fim-prilan = horar_jorn_trab_ptoelet.num_horar_term_lanche_priper.

      if hra-ini-prilan < v_num_hra_fim_not then do:
         if hra-fim-prilan <= v_num_hra_fim_not then 
            assign i-prilan-not = hra-fim-prilan - hra-ini-prilan. 
         else          
            assign i-prilan-not = v_num_hra_fim_not - hra-ini-prilan
                   i-prilan-diu = hra-fim-prilan - v_num_hra_fim_not.
      end.
      if hra-ini-prilan >= v_num_hra_fim_not and
         hra-fim-prilan <= v_num_hra_ini_not then do:
         assign i-prilan-diu = hra-fim-prilan - hra-ini-prilan
                i-prilan-not = 0.
      end.        
      if hra-ini-prilan <  v_num_hra_ini_not and
         hra-fim-prilan >= v_num_hra_ini_not then do:
         if hra-fim-prilan < 104460 then do:
            assign i-prilan-diu = v_num_hra_ini_not - hra-ini-prilan
                   i-prilan-not = hra-fim-prilan - v_num_hra_ini_not.
         end.
         else do:
            assign i-prilan-diu = v_num_hra_ini_not - hra-ini-prilan
                   i-prilan-not = 25200
                   i-prilan-diu = hra-ini-prilan + (hra-fim-prilan - v_num_hra_fim_not).
         end.          
      end.
   end.

   /****** tratamento lanche no segundo perido *********************/
   if horar_jorn_trab_ptoelet.num_horar_inic_lanche_segper > 0 or  
      horar_jorn_trab_ptoelet.num_horar_term_lanche_segper > 0 and 
     (bsit_afast_func.num_hora_inic_proces_sit_afast <= 
      horar_jorn_trab_ptoelet.num_horar_inic_lanche_segper and
      bsit_afast_func.num_horar_fim_proces_sit_afast >= 
      horar_jorn_trab_ptoelet.num_horar_inic_lanche_segper) or 
     (bsit_afast_func.num_hora_inic_proces_sit_afast <= 
      horar_jorn_trab_ptoelet.num_horar_term_lanche_segper and
      bsit_afast_func.num_horar_fim_proces_sit_afast >= 
      horar_jorn_trab_ptoelet.num_horar_term_lanche_segper) then do:

      if bsit_afast_func.num_hora_inic_proces_sit_afast > 
         horar_jorn_trab_ptoelet.num_horar_inic_lanche_segper then
         assign hra-ini-seglan = bsit_afast_func.num_horar_inic_sit_afast.
      else
         assign hra-ini-seglan = horar_jorn_trab_ptoelet.num_horar_inic_lanche_segper.
      if bsit_afast_func.num_horar_fim_proces_sit_afast < 
         horar_jorn_trab_ptoelet.num_horar_term_lanche_segper then
         assign hra-fim-seglan = bsit_afast_func.num_horar_term_sit_afast.
      else
         assign hra-fim-seglan = horar_jorn_trab_ptoelet.num_horar_term_lanche_segper.

      if hra-ini-seglan < v_num_hra_fim_not then do:
         if hra-fim-seglan <= v_num_hra_fim_not then 
            assign i-seglan-not = hra-fim-seglan - hra-ini-seglan. 
         else          
            assign i-seglan-not = v_num_hra_fim_not - hra-ini-seglan
                   i-seglan-diu = hra-fim-seglan - v_num_hra_fim_not.
      end.
      if hra-ini-seglan >= v_num_hra_fim_not and
         hra-fim-seglan <= v_num_hra_ini_not then do:
         assign i-seglan-diu = hra-fim-seglan - hra-ini-seglan
                i-seglan-not = 0.
      end.        
      if hra-ini-seglan <  v_num_hra_ini_not and
         hra-fim-seglan >= v_num_hra_ini_not then do:
         if hra-fim-seglan < 104460 then do:
            assign i-seglan-diu = v_num_hra_ini_not - hra-ini-seglan
                   i-seglan-not = hra-fim-seglan - v_num_hra_ini_not.
         end.
         else do:
            assign i-seglan-diu = v_num_hra_ini_not - hra-ini-seglan
                   i-seglan-not = 25200
                   i-seglan-diu = i-seglan-diu + (hra-fim-seglan - v_num_hra_fim_not).
         end.          
      end.
   end.
end.
END.

PROCEDURE pi_desligado.
if l-desligado = yes then do: /* cris func demit */
   assign v_log_emprest_1 = no.
   {prghur/pep/pe9995.i bsit_afast_func.dat_inic_proces_sit_afast}

   find tt-calendar where
        tt-calendar.cdn_turno_trab   = v_cdn_turno                               and
        tt-calendar.cdn_turma_trab   = v_cdn_turma                               and
        tt-calendar.dat_refer_calend = bsit_afast_func.dat_inic_proces_sit_afast and
        tt-calendar.cod_pais         = v_cod_pais                                and  
        tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.

   assign i-tot-hra-diu = 0
          i-tot-hra-not = 0.

   find first tt-aux-semana where 
              tt-aux-semana.dat-fim >= bsit_afast_func.dat_inic_proces_sit_afast no-error.
   if not available tt-aux-semana then 
      find last tt-aux-semana no-error.
   if tt-aux-semana.qtd-atrasos = 0 and tt-aux-semana.min-atrasos = 0 then
      assign v_qtd_hrs_diu_desc_dsr = 0
             v_qtd_hrs_not_desc_dsr = 0.

   IF tt-aux-semana.dat-fim <> v_dat_final_semana AND bcatponto.idi_lim_minut_atraso = 2 THEN DO:
      FOR EACH tt-sit-descta EXCLUSIVE-LOCK:
          DELETE tt-sit-descta.
      END.
      FOR EACH tt-supnot-jorinc EXCLUSIVE-LOCK:
          DELETE tt-supnot-jorinc.
      END.
   END.
   ASSIGN v_dat_final_semana = tt-aux-semana.dat-fim.

   if bsit_afast_func.num_hora_inic_proces_sit_afast < 0 then do:
      if bsit_afast_func.num_horar_fim_proces_sit_afast < 0 then do:
         assign i-hra-ini = bsit_afast_func.num_horar_inic_sit_afast
                i-hra-fim = bsit_afast_func.num_horar_term_sit_afast.
         run pi-calculo-hora.      
      end.
      else do:
         assign i-hra-ini = bsit_afast_func.num_horar_inic_sit_afast
                i-hra-fim = 86400.
         run pi-calculo-hora. 
         assign i-hra-ini = 0
                i-hra-fim = bsit_afast_func.num_horar_term_sit_afast.
         run pi-calculo-hora. 
      end.
   end.            
   else do:
      if bsit_afast_func.num_horar_fim_proces_sit_afast <= 86400 or
         bsit_afast_func.num_hora_inic_proces_sit_afast > 86400 then do:
         assign i-hra-ini = bsit_afast_func.num_horar_inic_sit_afast
                i-hra-fim = bsit_afast_func.num_horar_term_sit_afast.
         run pi-calculo-hora.      
      end.
      else do:
         assign i-hra-ini = bsit_afast_func.num_horar_inic_sit_afast
                i-hra-fim = 86400.
         run pi-calculo-hora. 
         assign i-hra-ini = 0
                i-hra-fim = bsit_afast_func.num_horar_term_sit_afast.
         run pi-calculo-hora. 
      end.
   end.    

   /*** tratamento horario de intervalo e lanche *****************************************/ 
   RUN pi_horar_interv.

   if i-tot-hra-diu > 0 then do:
      assign i-tot-hra-diu = i-tot-hra-diu - (i-int-hra-diu + i-prilan-diu + i-seglan-diu) 
             i-ev-codigo   = bsit_afast.cdn_event_afast_diurno
             i-qtd-hrs-aux = i-tot-hra-diu / 3600
             i-qtd-hrs     = truncate(round(i-tot-hra-diu / 3600,3),3)
             v_cdn_event_desc_diu = i-ev-codigo.
      if (d-tot-hrs-sit + i-qtd-hrs) > d-tot-hrsmes then do:
         find first event_fp no-lock where
             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
             event_fp.cdn_event_fp = i-ev-codigo no-error.
         if avail event_fp then do:
             if event_fp.idi_ident_efp = 1 then do:
                 assign v_num_erro = v_num_erro + 1.
                 {utp/ut-liter.i Situa‡Æo mpe L}
                 assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
                 {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
                 assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
                 {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_normais mpe L}
                 assign v_des_erro = v_des_erro + trim(return-value).
                 create tt_erro.
                 assign tt_erro.num_erro = v_num_erro
                        tt_erro.des_erro = v_des_erro
                        tt_erro.log_erro = yes.
                 next.
             end.
         end.
         assign i-ev-cod-ant = i-ev-codigo.
         if d-tot-hrs-sit < d-tot-hrsmes then do:
            IF i-ev-hrs-dif-diu <> "" THEN DO:
                find first event_fp no-lock where
                           event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                           event_fp.cdn_event_fp = i-ev-codigo no-error.
                assign i-ev-cod-ant = i-ev-codigo
                       i-qtd-hrs    = i-qtd-hrs - (d-tot-hrsmes - d-tot-hrs-sit)
                       i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND 
                                         event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                         event_fp.cdn_event_fp <> v_cdn_evt_495 
                                      then i-ev-hrs-dif-diu 
                                      else i-ev-codigo.

                if i-ev-codigo = v_cdn_evt_490 or 
                   i-ev-codigo = v_cdn_evt_495 then
                   assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                        then v_cdn_evt_995 
                                        else v_cdn_evt_996.

                {prghur/pep/pe4000.i1}.
            END.

            ASSIGN i-qtd-hrs   = d-tot-hrsmes - d-tot-hrs-sit
                   i-ev-codigo = i-ev-cod-ant.
            if v_log_influi = no then do:
               {prghur/pep/pe4000.i1}.
               find first event_fp no-lock where
                     event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                     event_fp.cdn_event_fp = i-ev-codigo no-error.
               if avail event_fp and
                   event_fp.idi_ident_efp <> 2 then
                   assign d-tot-hrs-sit     = d-tot-hrs-sit + i-qtd-hrs
                          d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs.
            end.
         end.
         else DO:
            IF i-ev-hrs-dif-diu <> "" THEN DO:

                find first event_fp no-lock where
                           event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                           event_fp.cdn_event_fp = i-ev-codigo no-error.

                assign i-ev-cod-ant = i-ev-codigo
                       i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                         event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                         event_fp.cdn_event_fp <> v_cdn_evt_495 
                                      then i-ev-hrs-dif-diu 
                                      else i-ev-codigo.

                if i-ev-codigo = v_cdn_evt_490 or 
                   i-ev-codigo = v_cdn_evt_495 then
                   assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                        then v_cdn_evt_995 
                                        else v_cdn_evt_996.               

               {prghur/pep/pe4000.i1}.

               ASSIGN i-ev-codigo = i-ev-cod-ant.
            END.
            assign i-qtd-hrs   = 0.
         END.

         IF v_log_influi = yes then do:
            assign v_qtd_hrs_diu_desc_dsr = v_qtd_hrs_diu_desc_dsr + i-qtd-hrs.
            find first tt-sit-descta exclusive-lock where
                       tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
            if not avail tt-sit-descta then do:
               create tt-sit-descta.
               assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                      tt-sit-descta.v_cdn_efp     = i-ev-codigo
                      tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                      tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs
                      tt-sit-descta.v_log_diu     = yes.
            end.
            else
               assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                      tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs.
         end.
      end.
      else do:           
         if v_log_influi = no then do:
            {prghur/pep/pe4000.i1}.
            find first event_fp no-lock where
                  event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                  event_fp.cdn_event_fp = i-ev-codigo no-error.
            if avail event_fp and
                event_fp.idi_ident_efp <> 2 then
                assign d-tot-hrs-sit     = d-tot-hrs-sit + i-qtd-hrs
                       d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs-aux.
         end.
         else do:
            assign v_qtd_hrs_diu_desc_dsr = v_qtd_hrs_diu_desc_dsr + i-qtd-hrs.
            find first tt-sit-descta exclusive-lock where
                       tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
            if not avail tt-sit-descta then do:
               create tt-sit-descta.
               assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                      tt-sit-descta.v_cdn_efp     = i-ev-codigo
                      tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                      tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs-aux
                      tt-sit-descta.v_log_diu     = yes.
            end.
            else
               assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                      tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs-aux.
         end.
      end.        

      if v_log_influi = yes then
         assign tt-aux-semana.qtd-atrasos = if i-qtd-hrs > 0 then  /* cris */
                                               tt-aux-semana.qtd-atrasos + 1
                                            else
                                               tt-aux-semana.qtd-atrasos
                tt-aux-semana.min-atrasos = tt-aux-semana.min-atrasos + (i-qtd-hrs * 3600). /* cris func demit */
   end.

   if i-tot-hra-not > 0 then do:
      assign i-tot-hra-not = i-tot-hra-not - (i-int-hra-not + i-prilan-not + i-seglan-not) 
             i-ev-codigo   = bsit_afast.cdn_event_afast_notur
             i-qtd-hrs-aux = i-tot-hra-not / 3600
             i-qtd-hrs     = truncate(round(i-tot-hra-not / 3600,3),3)
             v_cdn_event_desc_not = i-ev-codigo.


      if (d-tot-hrs-sit + i-qtd-hrs) > d-tot-hrsmes then do:
         find first event_fp no-lock where
             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
             event_fp.cdn_event_fp = i-ev-codigo no-error.
         if avail event_fp then do:
             if event_fp.idi_ident_efp = 1 then do:
                 assign v_num_erro = v_num_erro + 1.
                 {utp/ut-liter.i Situa‡Æo mpe L}
                 assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
                 {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
                 assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
                 {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_normais mpe L}
                 assign v_des_erro = v_des_erro + trim(return-value).
                 create tt_erro.
                 assign tt_erro.num_erro = v_num_erro
                        tt_erro.des_erro = v_des_erro
                        tt_erro.log_erro = yes.
                 next.
             end.
         end.
         assign i-ev-cod-ant = i-ev-codigo.
         if d-tot-hrs-sit < d-tot-hrsmes then do:
            IF i-ev-hrs-dif-not <> "" THEN DO:
                find first event_fp no-lock where
                           event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                           event_fp.cdn_event_fp = i-ev-codigo no-error.
                assign i-ev-cod-ant = i-ev-codigo
                       i-qtd-hrs    = i-qtd-hrs - (d-tot-hrsmes - d-tot-hrs-sit)
                       i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                         event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                         event_fp.cdn_event_fp <> v_cdn_evt_495 
                                      then i-ev-hrs-dif-not 
                                      else i-ev-codigo.

                if i-ev-codigo = v_cdn_evt_490 or 
                   i-ev-codigo = v_cdn_evt_495 then
                   assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                        then v_cdn_evt_995 
                                        else v_cdn_evt_996.                

                {prghur/pep/pe4000.i1}.
            END.

            assign i-qtd-hrs   = d-tot-hrsmes - d-tot-hrs-sit
                   i-ev-codigo = i-ev-cod-ant.
            find first event_fp no-lock where
                event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                event_fp.cdn_event_fp = i-ev-codigo no-error.
            if v_log_influi = no then do:
               {prghur/pep/pe4000.i1}.
               assign d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 then d-tot-hrs-sit + i-qtd-hrs else d-tot-hrs-sit
                      d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 then d-hrs-not-trb-sit + i-qtd-hrs else d-hrs-not-trb-sit
                      v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs.
            end.
         end.
         ELSE DO:
            IF i-ev-hrs-dif-not <> "" THEN DO:
                find first event_fp no-lock where
                           event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                           event_fp.cdn_event_fp = i-ev-codigo no-error.

                assign i-ev-cod-ant = i-ev-codigo
                       i-ev-codigo  = if avail event_fp and event_fp.idi_ident_efp <> 2 AND
                                         event_fp.cdn_event_fp <> v_cdn_evt_490 and 
                                         event_fp.cdn_event_fp <> v_cdn_evt_495 
                                      then i-ev-hrs-dif-not 
                                      else i-ev-codigo.

                if i-ev-codigo = v_cdn_evt_490 or 
                   i-ev-codigo = v_cdn_evt_495 then
                   assign i-ev-codigo = if i-ev-codigo = v_cdn_evt_490 
                                        then v_cdn_evt_995 
                                        else v_cdn_evt_996.               

                {prghur/pep/pe4000.i1}.

                ASSIGN i-ev-codigo = i-ev-cod-ant.
            END.
            assign i-qtd-hrs   = 0.
         END.

         if v_log_influi = yes then do:
            assign v_qtd_hrs_not_desc_dsr = v_qtd_hrs_not_desc_dsr + i-qtd-hrs.
            find first tt-sit-descta exclusive-lock where
                       tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
            if not avail tt-sit-descta then do:
               create tt-sit-descta.
               assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                      tt-sit-descta.v_cdn_efp     = i-ev-codigo
                      tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                      tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs
                      tt-sit-descta.v_log_diu     = no.
            end.
            else
               assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                      tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs.
         end.
      end.
      else do:
         if v_log_influi = no then do:
            {prghur/pep/pe4000.i1}.
            find first event_fp no-lock where
                event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                event_fp.cdn_event_fp = i-ev-codigo no-error.

            assign d-tot-hrs-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 then d-tot-hrs-sit + i-qtd-hrs else d-tot-hrs-sit
                   d-hrs-not-trb-sit = if avail event_fp and event_fp.idi_ident_efp <> 2 then d-hrs-not-trb-sit + i-qtd-hrs-aux else d-hrs-not-trb-sit
                   v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux.
         end.
         else do:
            assign v_qtd_hrs_not_desc_dsr = v_qtd_hrs_not_desc_dsr + i-qtd-hrs.
            find first tt-sit-descta exclusive-lock where
                       tt-sit-descta.v_cdn_efp = i-ev-codigo no-error.
            if not avail tt-sit-descta then do:
               create tt-sit-descta.
               assign tt-sit-descta.v_num_seq     = v_num_seq_sit
                      tt-sit-descta.v_cdn_efp     = i-ev-codigo
                      tt-sit-descta.v_qtd_hrs     = i-qtd-hrs
                      tt-sit-descta.v_qtd_hrs_aux = i-qtd-hrs-aux
                      tt-sit-descta.v_log_diu     = no.
            end.
            else
               assign tt-sit-descta.v_qtd_hrs     = tt-sit-descta.v_qtd_hrs + i-qtd-hrs
                      tt-sit-descta.v_qtd_hrs_aux = tt-sit-descta.v_qtd_hrs_aux + i-qtd-hrs-aux.
         end.
      end.        

      if v_log_influi = yes then
         assign tt-aux-semana.qtd-atrasos = if i-qtd-hrs > 0 then  /* cris */
                                               tt-aux-semana.qtd-atrasos + 1
                                            else
                                               tt-aux-semana.qtd-atrasos
                tt-aux-semana.min-atrasos = tt-aux-semana.min-atrasos + (i-qtd-hrs * 3600). /* cris func demit */
      /* tt-suplement */
      if bsit_afast.cod_event_suplem_notur <> "" then do:

         if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
            v_log_sind_suplem = no then
            assign i-ev-codigo   = bsit_afast.cod_event_suplem_notur
                   i-qtd-hrs-aux = 0
                   i-qtd-hrs     = 0.
         else
            assign i-ev-codigo   = bsit_afast.cod_event_suplem_notur
                   i-qtd-hrs-aux = i-qtd-hrs-aux * 0.1428572
                   i-qtd-hrs     = truncate(round(i-qtd-hrs * 0.1428572,3),3).

         if (d-tot-hrs-sit + i-qtd-hrs) > d-tot-hrsmes then do:
            find first event_fp no-lock where
                       event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                       event_fp.cdn_event_fp = i-ev-codigo no-error.
            if avail event_fp then do:
                if event_fp.idi_ident_efp = 1 then do:
                    assign v_num_erro = v_num_erro + 1.
                    {utp/ut-liter.i Situa‡Æo mpe L}
                    assign v_des_erro = trim(return-value) + " " + string(bsit_afast_func.cdn_sit_afast_func,"99") + " ".
                    {utp/ut-liter.i nÆo_considerada_para_func_ mpe L}
                    assign v_des_erro = v_des_erro + trim(return-value) + " " + string(bsit_afast_func.cdn_funcionario).
                    {utp/ut-liter.i ,_qtd_hrs_sit_>_que_qtd_hrs_normais mpe L}
                    assign v_des_erro = v_des_erro + trim(return-value).
                    create tt_erro.
                    assign tt_erro.num_erro = v_num_erro
                           tt_erro.des_erro = v_des_erro
                           tt_erro.log_erro = yes.
                    next.
                end.
            end.
            if d-tot-hrs-sit < d-tot-hrsmes then do:
               assign i-qtd-hrs = d-tot-hrsmes - d-tot-hrs-sit.
               if v_log_influi = no then do:
                  {prghur/pep/pe4000.i1}.
                  find first event_fp no-lock where
                             event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                             event_fp.cdn_event_fp = i-ev-codigo no-error. 

                  assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs
                         d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 then d-tot-hrs-sit + i-qtd-hrs else d-tot-hrs-sit
                         d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs.
               end.
               else do:
                  find first tt-supnot-jorinc exclusive-lock where
                             tt-supnot-jorinc.v_cdn_efp = i-ev-codigo no-error.
                  if not avail tt-supnot-jorinc then do:
                     create tt-supnot-jorinc.
                     assign tt-supnot-jorinc.v_num_seq = v_num_seq_sit
                            tt-supnot-jorinc.v_cdn_efp = i-ev-codigo
                            tt-supnot-jorinc.v_qtd_hrs = i-qtd-hrs.
                  end.
                  else
                     assign tt-supnot-jorinc.v_qtd_hrs = tt-supnot-jorinc.v_qtd_hrs + i-qtd-hrs.
               end.
            end.          
         END.
         else do:           
            if v_log_influi = no then do:
               {prghur/pep/pe4000.i1}.
               find first event_fp no-lock where
                   event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                   event_fp.cdn_event_fp = i-ev-codigo no-error.
               assign v_hrs_sit_not_per = v_hrs_sit_not_per + i-qtd-hrs-aux
                      d-tot-hrs-sit     = if avail event_fp and event_fp.idi_ident_efp <> 2 then d-tot-hrs-sit + i-qtd-hrs else d-tot-hrs-sit
                      d-hrs-sup-sit-not = d-hrs-sup-sit-not + i-qtd-hrs-aux.
            end.
            else do:
               find first tt-supnot-jorinc exclusive-lock where
                          tt-supnot-jorinc.v_cdn_efp = i-ev-codigo no-error.
               if not avail tt-supnot-jorinc then do:
                  create tt-supnot-jorinc.
                  assign tt-supnot-jorinc.v_num_seq = v_num_seq_sit
                         tt-supnot-jorinc.v_cdn_efp = i-ev-codigo
                         tt-supnot-jorinc.v_qtd_hrs = i-qtd-hrs.
               end.
               else
                  assign tt-supnot-jorinc.v_qtd_hrs = tt-supnot-jorinc.v_qtd_hrs + i-qtd-hrs.
            end.
         end.
      end. 
   end.         
end. /* func demitido */
END.
