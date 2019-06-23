/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i PE4000R1 1.02.17.088 } /*** 0101788 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i pe4000r1 MPE}
&ENDIF

/******************************************************************************
**       Programa: PE4000R1.P
******************************************************************************/
{include/i-epc200.i pe4000r1} /*** Jezi ***/

{include/i_dbvers.i}

{prghur/pep/pe4000tt.i SHARED}
FIND FIRST tt-param NO-LOCK NO-ERROR.

DEF SHARED VAR v_log_calend_func       AS LOG NO-UNDO.
def shared var i-ev-supnot             /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var l-sup-evnot             as log no-undo.
def shared var l-adc-evnot             as log no-undo. /* cris turno m¢vel */
def shared var i-ev-trbdiu             /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-trbnot             /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-dsrdiu             /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-dsrnot             /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-dsrdiuper          /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-dsrnotper          /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-ferdiuper          /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-fernotper          /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-desc-hrs-diu       /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var i-ev-desc-hrs-not       /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
DEF SHARED VAR i-ev-hrs-dif-diu        /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
DEF SHARED VAR i-ev-hrs-dif-not        /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def shared var c-mmaa-folha like bco_hrs_compens_func.cod_mes_ano_refer_fp no-undo.
def shared var i-ult-diames            as int  no-undo.
def shared var dat-ini-mes             as date no-undo.
def shared var dat-fim-mes             as date no-undo.
def shared var v_num_erro              as int  no-undo.
def shared var v_log_fecha             as log  no-undo.
def shared var d-ini-comp as date no-undo.
def shared var d-fim-comp as date no-undo.
DEF SHARED VAR v_log_mes_seg       AS LOG  NO-UNDO.

def shared var v_num_movtos             as int  format ">>>>>>>>>9"                no-undo.
def new shared var dt-ini-out          as date no-undo.
def new shared var dt-fim-out          as date no-undo. 
def new shared var d-qtd-hrs-sup-not   as dec no-undo. /* cris */
def new shared var l-desligado         as log no-undo.   
def new shared var l-func-normal       as log no-undo.
def new shared var d-qtd-hrs-diu       as dec no-undo.
def new shared var d-qtd-hrs-not       as dec no-undo.
def new shared var d-qtd-hrs-diu-dsr   as dec no-undo.
def new shared var d-qtd-hrs-not-dsr   as dec no-undo.
def new shared var d-qtd-hrs-diu-fer   as dec no-undo.
def new shared var d-qtd-hrs-not-fer   as dec no-undo.
def new shared var d-qtd-trb-sem       as dec no-undo.
def new shared var d-qtd-dsr-sem       as dec no-undo.
/* def new shared var d-qtd-trb-mes    as dec no-undo. 
def new shared var d-qtd-fer-mes       as dec no-undo.
def new shared var d-qtd-dsr-mes       as dec no-undo. */
def new shared var i-qtd-dsr-per       as int no-undo.
def new shared var i-qtd-fer-per       as int no-undo.
def new shared var d-hrs-diu-dsr-per   as dec no-undo.
def new shared var d-hrs-not-dsr-per   as dec no-undo.         
def new shared var d-hrs-diu-fer-per   as dec no-undo.
def new shared var d-hrs-not-fer-per   as dec no-undo.         
def new shared var d-hrs-diu-dsr-sit   as dec no-undo.
def new shared var d-hrs-not-dsr-sit   as dec no-undo.         
def new shared var d-hrs-diu-fer-sit   as dec no-undo.
def new shared var d-hrs-not-fer-sit   as dec no-undo.         
def new shared var d-hrs-diu-trb-sit   as dec no-undo.
def new shared var d-hrs-not-trb-sit   as dec no-undo.
def new shared var d-tot-hrs-trb       as dec no-undo.
def new shared var d-tot-hrs-dsr       as dec no-undo.
def new shared var d-tot-hrs-fer       as dec no-undo.
def new shared var d-tot-hrs-sit       as dec no-undo.
def new shared var d-tot-hrs-trb-diu   as dec no-undo.
def new shared var d-tot-hrs-trb-not   as dec no-undo.
def new shared var d-tot-hrs-sup-not   as dec no-undo.
def new shared var d-tot-hrs-dsr-diu   as dec no-undo.
def new shared var d-tot-hrs-dsr-not   as dec no-undo.
def new shared var d-tot-hrs-fer-diu   as dec no-undo.
def new shared var d-tot-hrs-fer-not   as dec no-undo.
def new shared var d-hrs-fer-mes-diu   as dec no-undo.
def new shared var d-hrs-fer-mes-not   as dec no-undo.
def new shared var d-hrs-sup-sit-not   as dec no-undo. 
def new shared var d-hrs-afast-diu     as dec no-undo. /* cris afast */
def new shared var d-hrs-afast-not     as dec no-undo. /* cris afast */
def new shared var d-hrs-afast         as dec no-undo. /* cris afast */
def new shared var d-hrs-dsr-afast-diu as dec no-undo. /* cris afast */
def new shared var d-hrs-dsr-afast-not as dec no-undo. /* cris afast */
def new shared var d-hrs-fer-afast-diu as dec no-undo. /* cris afast */
def new shared var d-hrs-fer-afast-not as dec no-undo. /* cris afast */
def new shared var l-afast-mes         as log no-undo.
def new shared var d-tot-hrsmes        as dec no-undo.
def new shared var i-aux-min-atraso    as int no-undo.
def new shared var i-aux-qtd-atrasos   as int no-undo.
def new shared var i-tot-hrs-neg       as int no-undo.
def new shared var i-aux-hrs-neg       as int no-undo.
def new shared var i-tot-hrs-pos       as int no-undo.
def new shared var v_num_dif_pos       as int no-undo.
def new shared var v_num_dif_neg       as int no-undo.
def new shared var l-troca-turno       as log no-undo.
def new shared var l-afast-periodo     as log no-undo.

def new shared var v_dat_inic_comp_transf as date format "99/99/9999" no-undo.
def new shared var v_dat_term_comp_transf as date format "99/99/9999" no-undo.
def new shared var v_qtd_trab_not_per     as dec no-undo.
def new shared var v_qtd_dsr_not_per      as dec no-undo.
def new shared var v_qtd_fer_not_per      as dec no-undo.
def new shared var v_qtd_sup_not_per      as dec no-undo.
def new shared var v_qtd_hrs_not_adc      as dec no-undo.
def new shared var v_hrs_sit_not_per      as dec no-undo.

def new shared var v_log_param_modul_agric as log no-undo.
def new shared var v_log_sind_suplem       as log no-undo.

def new shared var v_programa    as char format "x(08)" no-undo.

def new shared var v_qtd_hrs_padr_mes_rh like turno_trab.qtd_hrs_padr_mes_rh no-undo.
def new shared var v_log_upc_mrs         as log init no                      no-undo.
def            var v_qtd_hrs_mes_rh_1    like turno_trab.qtd_hrs_padr_mes_rh no-undo.
def var v_cod_tip_dia_ant like efp_par_marcac_ptoelet.cod_tip_dia no-undo.
def var v_cod_tip_dia_aux like efp_par_marcac_ptoelet.cod_tip_dia no-undo.
def var dt-dia-cal                     as date no-undo.
def var dt-dia-cal-aux                 as date no-undo.
def var dt-aux-sitini                  as date no-undo.
def var dt-aux-iniss                   as date no-undo.
def var dt-aux-sitfim                  as date no-undo. 
def var i-qtd-hrs-hpc-seg              as int no-undo.
def var d-per-calc-diu                 as dec no-undo.
def var d-per-calc-not                 as dec no-undo.
def var l-tratada-situa                as log no-undo.
def var i-tot-hra-diu                  as int no-undo.
def var i-tot-hra-not                  as int no-undo.
def var i-tot-hra-sit                  as int no-undo.
def var i-ini-interv                   as int no-undo.
def var i-ini-intclc                   as int no-undo.
def var i-fim-interv                   as int no-undo.
def var i-fim-intclc                   as int no-undo.
def var i-interv-diu                   as int no-undo.
def var i-interv-not                   as int no-undo.
def var i-tot-hra-extra                as int no-undo.
def var i-dif-hrs-pag                  as int no-undo.
def var i-dif-qtd-hrs                  as int no-undo.
def var i-dia-sema                     as int no-undo.
def var c-tp-dia                       as char format "x(02)"     no-undo.
def var v_num_min_pos                  as int format "999999"     no-undo.
def var v_num_min_neg                  as int format "999999"     no-undo.
def var v_num_hora_pos                 as int format "9999999999" no-undo.
def var v_num_hora_neg                 as int format "9999999999" no-undo.
def var v_num_hrs_neg                  as int format "9999999999" no-undo.
def var v_num_hrs_pos                  as int format "9999999999" no-undo.
def var i-hextra-aux                   as int format "9999999"    no-undo.
def var v_row_pri_evt                  as rowid no-undo.
def var v_qti_extra                    as int no-undo.
def var i-ev-codigo                    /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def var i-ev-percdsr                   /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def var i-qtd-hrs                      like movto_ptoelet.qtd_movto_ptoelet no-undo.
def var i-qtd-dia                      as int no-undo.
def var i-semana                       as int no-undo.     
def var d-tot-hrs-mes                  as dec no-undo. 
def var i-val-aux                      as dec  no-undo.
def var i-tot-rat                      as dec  no-undo.
def var d-aux-rat                      as dec  no-undo.
def var d-aux-tot-rat                  as dec  no-undo.
def var d-dif-aux                      as dec  no-undo.
def var l-feriado                      as log  no-undo.
def var v_ind_int                      as int  no-undo. /*cris*/
def var v_dat_term_compens             as date format "99/99/9999" no-undo.
def var v_qti_hrs_diu_fer              as int                      no-undo.
def var v_qti_hrs_not_fer              as int                      no-undo.
def var v_qti_fer_sem                  as int                      no-undo.
def var v_tot_trb_not                  as dec                      no-undo. /* cris */
def var d_qtd_hrs_adic_not   as dec no-undo.
def var i_efp_adic_not       /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def var i-tratam as int no-undo.
def var c-mmaa-adm-func     like bco_hrs_compens_func.cod_mes_ano_refer_fp no-undo.
def var c-mmaa-dem-func     like bco_hrs_compens_func.cod_mes_ano_refer_fp no-undo.
def var v_dat_inic_compens  as date format "99/99/9999"                    no-undo.
def var d_aux_fim                      as date format "99/99/9999" no-undo.
def var d_aux_inic                     as date format "99/99/9999" no-undo.
def var i                              as int                      no-undo.
def var i1                             as int                      no-undo.
def var l_semanal                      as log                      no-undo.
def var i-dias-fev                     as int                      no-undo.
def var i-dias-func                    as int                      no-undo.
def var i-mm-corr                      as int                      no-undo.
def var i-aa-corr                      as int                      no-undo.
def var i-mm-prox                      as int                      no-undo.
def var i-aa-prox                      as int                      no-undo.
def var i-nulo                         as char                     no-undo.
def var d-qtd-hrs-diu-aux              as dec                      no-undo. /* turno m¢vel */
def var d-qtd-hrs-not-aux              as dec                      no-undo. /* turno m¢vel */
def var v_num_mes_ocor                 as int                      no-undo.
def var v_num_ano_ocor                 as int                      no-undo.
def var v_num_mes                      as int                      no-undo.
def var v_num_ano                      as int                      no-undo.
def var v_dat_prox                     as date                     no-undo.
def var v_cdn_turno_ant                as int                      no-undo.
def var v_cdn_turma_ant                as int                      no-undo.
def var v_cdn_jorn_ant                 as int                      no-undo.
def var v_cdn_interv_ant               as int                      no-undo.
def var v_cod_pais_ant                 as char                     no-undo.
def var v_cdn_localid_ant              as int                      no-undo.
def var v_log_emprest_ant              as log                      no-undo.
def var v_dat_fim_localid_ant          as date format "99/99/9999" no-undo.
def var v_dat_fim_emprest_ant          as date format "99/99/9999" no-undo.
def var v_dat_fim_lotac_ant            as date format "99/99/9999" no-undo.
def var v_dat_ini_extra                as date format "99/99/9999" no-undo.
def var v_log_afast_func               as log  initial no          no-undo.
def var v_num_sem                      as int                      no-undo.
def var v_num_dif_sem                  as int                      no-undo.
def var v_dat_ini_sem                  as date format "99/99/9999" no-undo.
def var v_qtd_trab_not_per_aux         as dec                      no-undo.
def var v_num_tratam                   as int                      no-undo.
def var v_log_movto_extra              as log                      no-undo.
def var v_log_pag_dsr_dem              as log                      no-undo.
DEF VAR v-qtd-adic-notur-per           AS DEC                      NO-UNDO.
def var v_qti_hra_dia_padr             as int                      no-undo.
def var v_qti_dif_real_sit             as int                      no-undo.
def var v_qti_hra_dia_real             as int                      no-undo.
def var v_log_pri_sit                  as log                      no-undo.
/* cris - Fechamento das horas por per°odo de ponto */
def var v_dat_ini_per     as date format "99/99/9999" no-undo.
def var v_dat_fim_per     as date format "99/99/9999" no-undo.
def var v_qti_hrs_diu     as int                      no-undo.
def var v_qti_hrs_not     as int                      no-undo.
def var v_qti_hrs_diu_dsr as int                      no-undo.
def var v_qti_hrs_not_dsr as int                      no-undo.
def var v_qti_hrs_fer_diu as int                      no-undo.
def var v_qti_hrs_fer_not as int                      no-undo.
def var v_qti_dias_trab   as int                      no-undo.
def var v_qti_dias_dsr    as int                      no-undo.
def var v_qti_dias_fer    as int                      no-undo.
def var v_log_atualiz_tt  as log                      no-undo.
def var v_ind_lim_bco     as int                      no-undo.
/* def var v_cdn_evento      as int  extent 999          no-undo. */
/* def var v_qti_hrs_evento  as dec  extent 999          no-undo. */
def var v_log_admit       as log                      no-undo.
def var v_log_admit2      as log                      no-undo.

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
DEF VAR v_log_exc         AS LOG INITIAL NO                            NO-UNDO.
def var v_log_alter       as logical                                   no-undo.
DEF VAR v_dat_aux_term_compens AS DATE FORMAT "99/99/9999"             NO-UNDO.
DEF VAR v_log_sem_5       AS LOGICAL                                   NO-UNDO.
DEF VAR v_log_achou       AS LOG                                       NO-UNDO.
DEF VAR v_dat_fim_clc     AS DATE FORMAT "99/99/9999"                  NO-UNDO.
def var dt-aux            as date                                      no-undo.
/*Variaveis anderson*/
DEF VAR v_qtd_horas_padrao   AS INT INITIAL 0 NO-UNDO.
DEF VAR i_dias_trab_apos_dem AS INT INITIAL 0 NO-UNDO.
DEF VAR v_dat_desligto_func            as date format "99/99/9999" NO-UNDO.

def new shared temp-table tt-aux-semana
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


DEFINE BUFFER bf-tt-aux-semana FOR tt-aux-semana.

def new shared temp-table tt_hora_extra    /* cris */
    field cod_tip_dia   like efp_hora_extra_tip_dia_sind.cod_tip_dia
    field qtd_hrs_extra as int
    field num_mes_extra as int
    index tip_dia is unique primary
          cod_tip_dia ascending.

def shared temp-table tt-val-semana
    field cod_pais        like func_ptoelet.cod_pais
    field cdn_localidade  like func_ptoelet.cdn_localidade
    field mes-refer       as char format "x(03)" /* (ant) - anterior --- (ref) - referencia ou processo */
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
          cod_pais
          cdn_localidade
          mes-refer
          num-semana 
          dat-fim. 

def shared temp-table tt-hextra
    field tp-dia         as char
    field i-tot-hrs      as int
    field num_mes_extra  as int
    index tt-extra       is unique primary
          tp-dia         ascending.

def shared temp-table tt_erro
    field num_erro as int  format ">>>>>>>>>9"
    field des_erro as char format "x(70)"
    field log_erro as log.

def temp-table tt-efp-par-marcac no-undo like efp_par_marcac_ptoelet 
    INDEX od-cresc IS PRIMARY cdn_empresa
                              cdn_estab
                              cdn_funcionario
                              dat_proces_mpe
                              num_horar_inic_proces_mpe ASCENDING.

def temp-table tt-extra-semana no-undo
    field num_sem as int
    field dat_ini as date format "99/99/9999"
    field dat_fim as date format "99/99/9999"
    index tt-extra-sem is unique primary
          num_sem dat_ini ascending.

DEF TEMP-TABLE tt-qti-hrs-evento no-undo
    FIELD v_cdn_event_fp LIKE event_fp.cdn_event_fp
    FIELD v_qti_hrs_evento AS DEC
    INDEX id
          v_cdn_event_fp. 

DEF TEMP-TABLE tt_hex_ja_lida no-undo
    FIELD num_dia    AS DATE FORMAT "99/99/9999" 
    FIELD num_hora LIKE bco_hrs_compens_func.num_horar_inic_mpe
    index tt_hex_lida is unique primary
          num_dia num_hora ascending.
          
DEF SHARED TEMP-TABLE tt-calendar LIKE det_calend_turma_localid.

def shared buffer bfunc-ponto         for func_ptoelet. 
def shared buffer bcontrole-funcponto for sit_calc_ptoelet_func.
def shared buffer bfunc-bcohrs        for bco_hrs_compens_func. 
def shared buffer cfunc-bcohrs        for bco_hrs_compens_func. 
def shared buffer bcontrole-catponto  for sit_calc_ptoelet_categ. 
def shared buffer bcalentur           FOR det_calend_turma_localid.   
def shared buffer bturnotrb           for turno_trab.  
def shared buffer bturmatrb           for turma_trab.
def shared buffer bcatponto           for categ_ptoelet.
def shared buffer bparam-pe           for param_empres_tma.
def shared buffer bsit_afast          for sit_afast.                                            
def shared buffer bsit_afast_func     for sit_afast_func.
DEF BUFFER btt-calendar               FOR tt-calendar.

def buffer bcalendia               for tt-calendar.
def buffer btt-efp-par-marcac      for tt-efp-par-marcac.
def buffer bdet_calend_localid     for tt-calendar.
def buffer bdet_calend             for tt-calendar.
def buffer b2tt-calendar           for tt-calendar.
def buffer b3tt-calendar           for tt-calendar.
def buffer bmovto_ptoelet          for movto_ptoelet.
def buffer bffuncionario           for funcionario.
def buffer bfpar_marcac_ptoelet    for par_marcac_ptoelet.

def var v_cdn_estab_mens  like func_ptoelet.cdn_estab        no-undo.
def var v_cdn_func_mens   like func_ptoelet.cdn_funcionario no-undo.
def var v_mes_men         as int format "99"                no-undo.
def var v_ano_men         as int format "9999"              no-undo.
def var v_log_influi_rep  as log                            no-undo.
def var v_log_influi_fer  as log                            no-undo.
def var v_dat_ini_ult_sem_per_ant as date format "99/99/9999" no-undo.

form "ERRO CALENDARIO FUNCIONARIO"          at 18 skip
     "N∆o existem calend†rios gerados para" at 18 skip
     v_cdn_estab_mens                       at 15 skip
     v_cdn_func_mens                        at 16 skip
     "Mes Refer: "                          at 18 
     v_mes_men no-label 
     "/" 
     v_ano_men no-label skip(2)
     with stream-io side-labels no-attr-space no-box width 132 frame f-erro-calen-func.
run utp/ut-trfrrp.p (input frame f-erro-calen-func:handle).


assign v_log_param_modul_agric = no
       v_log_sind_suplem       = no.

find first sindicato where sindicato.cdn_sindicato = bfunc-ponto.cdn_sindicato no-lock no-error.

find param_modul_agric no-lock where   /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
     param_modul_agric.cdn_empresa = bfunc-ponto.cdn_empresa no-error.
if avail param_modul_agric then do:  
   assign v_log_param_modul_agric = yes.
   if (sindicato.log_suplem_notur_horist = no and  /** Horista **/
       bfunc-ponto.cdn_categ_sal = 2) or 
      (sindicato.log_suplem_notur_mensalis = no and  /** Mensalista **/
       bfunc-ponto.cdn_categ_sal = 1) OR 
      (sindicato.LOG_suplem_notur_taref = NO AND  
       bfunc-ponto.cdn_categ_sal = 5) then DO:
      assign l-sup-evnot = no
             v_log_sind_suplem = no.
   END.
   ELSE DO:
       IF (sindicato.LOG_suplem_notur_taref AND 
           bfunc-ponto.cdn_categ_sal = 5) OR
          (sindicato.log_suplem_notur_horist and  /** Horista **/
           bfunc-ponto.cdn_categ_sal = 2) or 
          (sindicato.log_suplem_notur_mensalis and  /** Mensalista **/
           bfunc-ponto.cdn_categ_sal = 1) THEN
          ASSIGN v_log_sind_suplem = YES.
   END.
end.

assign i-qtd-dsr-per     = 0
       d-hrs-diu-dsr-per = 0
       d-hrs-not-dsr-per = 0
       i-qtd-fer-per     = 0
       d-hrs-diu-fer-per = 0
       d-hrs-not-fer-per = 0
       d-tot-hrs-trb     = 0
       d-tot-hrs-dsr     = 0
       d-tot-hrs-fer     = 0
       d-tot-hrs-trb-diu = 0
       d-tot-hrs-trb-not = 0
       d-tot-hrs-dsr-diu = 0
       d-tot-hrs-dsr-not = 0
       d-tot-hrs-fer-diu = 0
       d-tot-hrs-fer-not = 0
       d-tot-hrs-sit     = 0
       d-qtd-hrs-diu     = 0
       d-qtd-hrs-not     = 0
       d-qtd-hrs-diu-dsr = 0 
       d-qtd-hrs-not-dsr = 0
       d-qtd-hrs-diu-fer = 0 
       d-qtd-hrs-not-fer = 0 
       d-hrs-diu-dsr-sit = 0
       d-hrs-not-dsr-sit = 0
       d-hrs-diu-fer-sit = 0
       d-hrs-not-fer-sit = 0
       d-hrs-diu-trb-sit = 0
       d-hrs-not-trb-sit = 0
       d-hrs-sup-sit-not = 0
       d-tot-hrsmes      = 0
       v_log_admit       = no
       v_log_admit2      = no
       v_qtd_trab_not_per_aux = 0
       v_log_movto_extra = no
       v_log_pag_dsr_dem = no
       i-dias-func = 0
       v_qtd_hrs_mes_rh_1 = 0.
assign v_num_mes_ocor = month(dat-fim-mes)
       v_num_ano_ocor = year(dat-fim-mes)
       l-func-normal  = yes
       l-desligado    = no
       dt-ini-out     = dat-ini-mes
       dt-fim-out     = dat-fim-mes.      

/******** Chamada EPC - Jeziel **********/
for each tt-epc EXCLUSIVE-LOCK where tt-epc.cod-event = "INI_INT":
    delete tt-epc. 
end.
create tt-epc.
&if "{&cd_rel_hr}" >= "2.11" &then
    assign tt-epc.cod-event = "INI_INT"
           tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                  string(bfunc-ponto.cdn_estab) + "|" +
                                  string(bfunc-ponto.cdn_funcionario).
&else
    assign tt-epc.cod-event = "INI_INT"
           tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                  string(bfunc-ponto.cdn_estab,"999") +
                                  string(bfunc-ponto.cdn_funcionario,"99999999").
&endif
{include/i-epc201.i "INI_INT"}

/** Dr. Jeziel **/
if bfunc-ponto.dat_admis_func > dat-ini-mes then do:
   assign dt-ini-out    = bfunc-ponto.dat_admis_func
          l-func-normal = no
          v_log_admit   = yes.
end.

if bfunc-ponto.dat_admis_func = dat-ini-mes then
   assign v_log_admit2 = yes.
if bfunc-ponto.dat_desligto_func <= dat-fim-mes then do:
   assign l-desligado = yes
          dt-fim-out  = bfunc-ponto.dat_desligto_func.
   /** le programaá∆o rescis∆o buscamdo data aviso ***********/
   find habilit_rescis of bfunc-ponto no-lock no-error.
   if available habilit_rescis then do:
      if habilit_rescis.dat_inic_aviso_previo = bfunc-ponto.dat_desligto_func and
         habilit_rescis.idi_tip_aviso_previo <> 3 then do:
         if month(habilit_rescis.dat_inic_aviso_previo - 1) <> month(habilit_rescis.dat_inic_aviso_previo)
            then do:
            assign dt-ini-out = ? 
                   dt-fim-out = ?
                   l-func-normal = no.
         end.
         else do: 
            assign dt-fim-out = habilit_rescis.dat_inic_aviso_previo - 1.        
            if day(dt-fim-out) < 30 then
               assign l-func-normal = no.
         end.
      end.
      else do: 
         if day(bfunc-ponto.dat_desligto_func) < 30 then
            assign dt-fim-out = bfunc-ponto.dat_desligto_func
                   l-func-normal = no.   
      end.
   end.
   /**********************************************************/
   else do:
      if day(bfunc-ponto.dat_desligto_func) < 30 then
         assign dt-fim-out = bfunc-ponto.dat_desligto_func
                l-func-normal = no.
   end.   
end.              

/** Jeziel **/
IF CAN-FIND(FIRST categ_sal where
                  categ_sal.cdn_empresa   = bfunc-ponto.cdn_empresa   and
                  categ_sal.cdn_estab     = bfunc-ponto.cdn_estab     and
                  categ_sal.cdn_categ_sal = bfunc-ponto.cdn_categ_sal and
                  substring(categ_sal.cod_livre_1,7,1) = "S") THEN 
   ASSIGN v_log_exc = YES.
ELSE 
   ASSIGN v_log_exc = NO.


IF NOT v_log_exc THEN DO:
    /** cria tt-aux-semana igual a tt-val-semana *****************************/ 
    for each tt-aux-semana EXCLUSIVE-LOCK:
        delete tt-aux-semana.
    end.

    /********* fechamento per°odo ponto - cris *********************/
    if (bcatponto.num_livre_1 = 1 or
        bcatponto.num_livre_1 = 3 OR
       not l-func-normal or
       l-desligado = yes) and
       bcatponto.idi_tratam_horar <> 3 /*and
       bcatponto.idi_tratam_horar <> 4*/ then
       assign v_dat_ini_per = dat-ini-mes
              v_dat_fim_per = dat-fim-mes.
    else
       assign v_dat_ini_per = bcontrole-funcponto.dat_inic_period_apurac_pto_mes
              v_dat_fim_per = dat-fim-mes.

    /*** logica verifica se teve troca/emprestimo de turno/turma/localidade no mes ***************/
    if can-find(first func_turno_trab of bfunc-ponto no-lock where
                      func_turno_trab.dat_inic_lotac_func_turno_trab >= v_dat_ini_per) then
       assign l-troca-turno = yes.
    else
       if can-find(first emprest_turno_turma_trab of bfunc-ponto no-lock where
                         emprest_turno_turma_trab.dat_inic_alter_horar_turma <= v_dat_fim_per and
                         emprest_turno_turma_trab.dat_fim_alter_horar_turma >= v_dat_ini_per) then
          assign l-troca-turno = yes.
       else
          if can-find(first alter_jorn_trab of bfunc-ponto no-lock where
                            alter_jorn_trab.dat_inic_alter_jorn_trab <= v_dat_fim_per and
                            alter_jorn_trab.dat_term_alter_jorn_trab >= v_dat_ini_per) then
             assign l-troca-turno = yes.
          else
             if can-find(first func_localid of bfunc-ponto no-lock where
                         func_localid.dat_inic_lotac_func >= v_dat_ini_per) then
                assign l-troca-turno = yes.

    if bcatponto.num_livre_1 = 2 or
       bcatponto.idi_tratam_horar = 3 or
       bcatponto.idi_tratam_horar = 4 then
       assign l-troca-turno = yes.

    if (bcatponto.num_livre_1 = 1 or 
        bcatponto.num_livre_1 = 3 or
       not l-func-normal or
       l-desligado = yes) and
       bcatponto.idi_tratam_horar <> 3 /*and
       bcatponto.idi_tratam_horar <> 4*/ then
       assign v_dat_fim_per = dat-fim-mes.
    else
       assign v_dat_fim_per = bcontrole-funcponto.dat_term_period_apurac_pto_mes.

    /* gabriela */
    IF v_log_calend_func THEN
       ASSIGN l-troca-turno = YES.

    /********** EPC CLIENTE GBARBOSA *****************/
    for each tt-epc EXCLUSIVE-LOCK where 
             tt-epc.cod-event = "calcula_limite_gbarbosa_4000r1_1":
        delete tt-epc.
    end.

    IF bfunc-ponto.dat_desligto_func = ? THEN
        ASSIGN v_dat_desligto_func = date(12,31,9999).
    ELSE
        ASSIGN v_dat_desligto_func = bfunc-ponto.dat_desligto_func.

    create tt-epc.
    assign tt-epc.cod-event     = "calcula_limite_gbarbosa_4000r1_1"
           tt-epc.cod-parameter = string(bfunc-ponto.cdn_empresa)                           + "|" +
                                  string(bfunc-ponto.cdn_estab)                             + "|" +
                                  string(bfunc-ponto.cdn_funcionario)                       + "|" +
                                  string(bfunc-ponto.cdn_sindicato)                         + "|" +
                                  STRING(v_dat_desligto_func)                               + "|" +
                                  STRING(bcontrole-catponto.dat_inic_period_apurac_pto_mes) + "|" +
                                  STRING(bcontrole-catponto.dat_term_period_apurac_pto_mes) + "|" +
                                  STRING(v_log_fecha)                                       + "|" +
                                  STRING(l-desligado)
           tt-epc.val-parameter = ?.
        
    {include/i-epc201.i "calcula_limite_gbarbosa_4000r1_1"}
    /*************************************************/

    /******************** N«O TROCAM DE TURNO ********************/
    if not l-troca-turno and
       not l-desligado and
       l-func-normal then do:
       for each tt-val-semana where
           tt-val-semana.cod_pais = bfunc-ponto.cod_pais and
           tt-val-semana.cdn_localidade = bfunc-ponto.cdn_localidade no-lock:
           create tt-aux-semana.
           assign tt-aux-semana.mes-refer       = tt-val-semana.mes-refer
                  tt-aux-semana.num-semana      = tt-val-semana.num-semana
                  tt-aux-semana.dat-fim         = tt-val-semana.dat-fim
                  tt-aux-semana.qtd-hrs-diu     = tt-val-semana.qtd-hrs-diu 
                  tt-aux-semana.qtd-hrs-not     = tt-val-semana.qtd-hrs-not  
                  tt-aux-semana.qtd-hrs-diu-dsr = tt-val-semana.qtd-hrs-diu-dsr
                  tt-aux-semana.qtd-hrs-not-dsr = tt-val-semana.qtd-hrs-not-dsr  
                  tt-aux-semana.qtd-hrs-diu-fer = tt-val-semana.qtd-hrs-diu-fer
                  tt-aux-semana.qtd-hrs-not-fer = tt-val-semana.qtd-hrs-not-fer  
                  tt-aux-semana.qtd-hrs-hpc-seg = tt-val-semana.qtd-hrs-hpc-seg   
                  tt-aux-semana.qtd-hco         = tt-val-semana.qtd-hco
                  tt-aux-semana.qtd-hpc         = tt-val-semana.qtd-hpc
                  tt-aux-semana.qtd-trb-sem     = tt-val-semana.qtd-trb-sem
                  tt-aux-semana.qtd-dsr-sem     = tt-val-semana.qtd-dsr-sem
                  tt-aux-semana.qtd-fer-sem     = tt-val-semana.qtd-fer-sem.
       end.

       if bcontrole-funcponto.dat_inic_period_apurac_pto_mes < dat-ini-mes and
          dt-ini-out <= dat-ini-mes then do:
          assign v_dat_fim_localid      = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                 v_dat_fim_emprest      = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                 v_dat_fim_lotac        = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                 v_log_emprest          = no
                 v_qtd_trab_not_per_aux = 0.
          do dt-dia-cal = bcontrole-funcponto.dat_inic_period_apurac_pto_mes to (dat-ini-mes - 1):

             {prghur/pep/pe9994.i dt-dia-cal}

             find tt-calendar where
                  tt-calendar.cdn_turno_trab   = v_cdn_turno and
                  tt-calendar.cdn_turma_trab   = v_cdn_turma and
                  tt-calendar.dat_refer_calend = dt-dia-cal  and
                  tt-calendar.cod_pais         = v_cod_pais  and
                  tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
             if not avail tt-calendar then
                next.
             if tt-calendar.idi_sit_dia_trab = 1 then do:
                if can-find(first efp_par_marcac_ptoelet of bfunc-ponto where
                                  efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                                  efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3) then do:
                   for each efp_par_marcac_ptoelet of bfunc-ponto no-lock where
                            efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                            (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3 or
                            efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 4):
                      if efp_par_marcac_ptoelet.log_hrs_diurno = no then
                         assign v_qtd_trab_not_per_aux = v_qtd_trab_not_per_aux + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                   end.
                end.
             end.
          end. /* do */
       END.
    end.
                /**************** TROCAM DE TURNO ************************/
    else do:
       if 
          bcatponto.num_livre_1 = 1 or 
          bcatponto.num_livre_1 = 3 or
          l-desligado or
          (v_log_admit2 and ( bcatponto.num_livre_1 = 1 OR bcatponto.num_livre_1 = 3)
           and bcatponto.idi_tratam_horar < 3) then do: /* apuraá∆o pelo màs de referància ou */
          run pi-cria-tt-mes-ref.                                           /* funcion†rios desligados  - cris    */
          if l-desligado and bcatponto.num_livre_1 = 2 then do:
             if bcontrole-funcponto.dat_inic_period_apurac_pto_mes < dat-ini-mes and
                dt-ini-out <= dat-ini-mes and not v_log_admit then do:
                assign v_dat_fim_localid  = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                       v_dat_fim_emprest  = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                       v_dat_fim_lotac    = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                       v_log_emprest      = no
                       v_qtd_trab_not_per = 0
                       v_qtd_dsr_not_per  = 0
                       v_qtd_fer_not_per  = 0
                       v_qtd_sup_not_per  = 0.
                do dt-dia-cal = bcontrole-funcponto.dat_inic_period_apurac_pto_mes to (dat-ini-mes - 1):

                   {prghur/pep/pe9994.i dt-dia-cal}

                   find tt-calendar where
                        tt-calendar.cdn_turno_trab   = v_cdn_turno and
                        tt-calendar.cdn_turma_trab   = v_cdn_turma and
                        tt-calendar.dat_refer_calend = dt-dia-cal  and
                        tt-calendar.cod_pais         = v_cod_pais  and
                        tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
                   if not avail tt-calendar then
                      next.
                   if tt-calendar.idi_sit_dia_trab = 4 then
                      assign v_qtd_fer_not_per = v_qtd_fer_not_per + tt-calendar.qti_padr_hrs_notur.

                   if tt-calendar.idi_sit_dia_trab = 3 then
                      assign v_qtd_dsr_not_per = v_qtd_dsr_not_per + tt-calendar.qti_padr_hrs_notur.

                   if tt-calendar.idi_sit_dia_trab = 1 then do:
                      if can-find(first efp_par_marcac_ptoelet of bfunc-ponto where
                                        efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                                       (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3 or
                                        efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 4)) then do:
                         for each efp_par_marcac_ptoelet of bfunc-ponto no-lock where
                                  efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                                  (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3 or
                                  efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 4):
                            IF efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 4 THEN /* Estava pagando adicional para afastado*/
                               NEXT.
                            if efp_par_marcac_ptoelet.log_hrs_diurno = no then
                               assign v_qtd_trab_not_per = v_qtd_trab_not_per + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                         end.
                      end.
                      else
                         assign v_qtd_trab_not_per = v_qtd_trab_not_per + tt-calendar.qti_padr_hrs_notur.
                   end.
                end. /* do */

                assign v_qtd_fer_not_per  = truncate(round(v_qtd_fer_not_per / 3600,3),3)
                       v_qtd_dsr_not_per  = truncate(round(v_qtd_dsr_not_per / 3600,3),3)
                       v_qtd_trab_not_per = truncate(round(v_qtd_trab_not_per / 3600,3),3)
                       v_qtd_sup_not_per  = if l-sup-evnot = yes then
                                               truncate(((v_qtd_trab_not_per + v_qtd_fer_not_per) * 0.1428571),3)
                                            else 0.
             end.
          end.
          ELSE DO:
             IF not l-desligado AND l-func-normal THEN DO:
                if bcontrole-funcponto.dat_inic_period_apurac_pto_mes < dat-ini-mes and
                   dt-ini-out <= dat-ini-mes then do:
                   assign v_dat_fim_localid      = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                          v_dat_fim_emprest      = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                          v_dat_fim_lotac        = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                          v_log_emprest          = no
                          v_qtd_trab_not_per_aux = 0.
                   do dt-dia-cal = bcontrole-funcponto.dat_inic_period_apurac_pto_mes to (dat-ini-mes - 1):

                      {prghur/pep/pe9994.i dt-dia-cal}

                      find tt-calendar where
                           tt-calendar.cdn_turno_trab   = v_cdn_turno and
                           tt-calendar.cdn_turma_trab   = v_cdn_turma and
                           tt-calendar.dat_refer_calend = dt-dia-cal  and
                           tt-calendar.cod_pais         = v_cod_pais  and
                           tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
                      if not avail tt-calendar then
                         next.
                      if tt-calendar.idi_sit_dia_trab = 1 then do:
                         if can-find(first efp_par_marcac_ptoelet of bfunc-ponto where
                                           efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                                           efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3) then do:
                            for each efp_par_marcac_ptoelet of bfunc-ponto no-lock where
                                     efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                                     (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3 or
                                     efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 4):
                               if efp_par_marcac_ptoelet.log_hrs_diurno = no then
                                  assign v_qtd_trab_not_per_aux = v_qtd_trab_not_per_aux + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                            end.
                         end.
                      end.
                   end. /* do */
                END. /* ini per < ini mes */
             END. /* func normal */
          END. /* else */
       end. /* màs refer */
       else do:
          if not l-func-normal or 
             (v_log_admit2 = yes and (bcatponto.num_livre_1 = 2 or bcatponto.idi_tratam_horar = 3 or bcatponto.idi_tratam_horar = 4)) then do: /* funcion†rios admitidos - cris */
             run pi-cria-tt-per-func.
          end.
          else do: /*  fechamento per°odo ponto - cris */

              MESSAGE "ponto 3 else 2"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
             run pi-cria-tt-mes-per.
          end.
       end.
    end.
end.


ASSIGN d-qtd-hrs-not     = 0 .

for each tt-aux-semana NO-LOCK where
    tt-aux-semana.mes-refer = "ref":
    MESSAGE "tt-aux-semana.qtd-hrs-not " tt-aux-semana.qtd-hrs-not
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN d-qtd-hrs-not     = d-qtd-hrs-not     + tt-aux-semana.qtd-hrs-not.
END.
MESSAGE "d-qtd-hrs-not " d-qtd-hrs-not
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

ASSIGN d-qtd-hrs-not = 0.


/**** fechamento horas trabalhas e dsr mes **********************************/
assign d-qtd-hrs-diu     = 0
       d-qtd-hrs-not     = 0 
       d-qtd-hrs-diu-dsr = 0 
       d-qtd-hrs-not-dsr = 0
       d-qtd-hrs-diu-fer = 0 
       d-qtd-hrs-not-fer = 0
       d-qtd-hrs-sup-not = 0.

IF substr(bcatponto.cod_livre_2,23,1) = "S" THEN
   ASSIGN l-sup-evnot = NO
          i-ev-supnot = ""/*0*/.

IF NOT v_log_exc THEN DO:

    MESSAGE "pe4000r1 ponto 4.7 d-qtd-hrs-not = " d-qtd-hrs-not SKIP
            "v_qtd_trab_not_per_aux " v_qtd_trab_not_per_aux
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    /**** fecha horas trb e dsr -  tt-aux-semana ******************************/ 
    for each tt-aux-semana NO-LOCK where
        tt-aux-semana.mes-refer = "ref": 

        assign d-qtd-hrs-diu     = d-qtd-hrs-diu     + tt-aux-semana.qtd-hrs-diu 
               d-qtd-hrs-not     = d-qtd-hrs-not     + tt-aux-semana.qtd-hrs-not  
               d-qtd-hrs-diu-dsr = d-qtd-hrs-diu-dsr + tt-aux-semana.qtd-hrs-diu-dsr
               d-qtd-hrs-not-dsr = d-qtd-hrs-not-dsr + tt-aux-semana.qtd-hrs-not-dsr  
               d-qtd-hrs-diu-fer = d-qtd-hrs-diu-fer + tt-aux-semana.qtd-hrs-diu-fer
               d-qtd-hrs-not-fer = d-qtd-hrs-not-fer + tt-aux-semana.qtd-hrs-not-fer.
    end.

    MESSAGE "pe4000r1 ponto 4.8 d-qtd-hrs-not = " d-qtd-hrs-not SKIP
            "v_qtd_trab_not_per_aux " v_qtd_trab_not_per_aux
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF v_qtd_trab_not_per_aux > 0 THEN DO:
       IF d-qtd-hrs-diu > 0 AND d-qtd-hrs-diu > v_qtd_trab_not_per_aux THEN
       DO:      

        MESSAGE "pe4000r1 ponto 3 atualizou d-qtd-hrs-not = " d-qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN d-qtd-hrs-diu = d-qtd-hrs-diu - v_qtd_trab_not_per_aux
                 d-qtd-hrs-not = d-qtd-hrs-not + v_qtd_trab_not_per_aux.

        MESSAGE "pe4000r1 ponto 4 atualizou d-qtd-hrs-not = " d-qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
       ELSE
          IF d-qtd-hrs-diu > 0 THEN
          DO:
        MESSAGE "pe4000r1 ponto 4 atualizou d-qtd-hrs-not = " d-qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
             ASSIGN d-qtd-hrs-not = d-qtd-hrs-not + d-qtd-hrs-diu
                    d-qtd-hrs-diu = 0.
        MESSAGE "pe4000r1 ponto 5 atualizou d-qtd-hrs-not = " d-qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
          END.
          ELSE
             ASSIGN v_qtd_trab_not_per_aux = 0.
    END.

    MESSAGE "pe4000r1 ponto 4.9 d-qtd-hrs-not = " d-qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    /*** verifica se dia seguinte a dt-fim-out Ç compensado ou dsr ****/
    /*** se for paga mais um dsr para o funcion†rio *******************/
    if l-desligado and dt-fim-out <> ? then do:

       /** Verifica se calculo de rescisao jˇ paga esta verba **/
       FIND FIRST idx_efp_espcif_rescis NO-LOCK WHERE
                  idx_efp_espcif_rescis.cdn_idx_efp_espcif_rescis = 547 NO-ERROR.
       IF NOT AVAIL idx_efp_espcif_rescis THEN DO:

           assign v_log_emprest_1 = no
                  v_dat_aux_lotac = dt-fim-out + 1.
           {prghur/pep/pe9995.i v_dat_aux_lotac}
           find tt-calendar where
                tt-calendar.cdn_turno_trab   = v_cdn_turno and
                tt-calendar.cdn_turma_trab   = v_cdn_turma and
                tt-calendar.dat_refer_calend = (dt-fim-out + 1) and
                tt-calendar.cod_pais         = v_cod_pais and
                tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
           IF NOT AVAIL tt-calendar THEN DO:
              assign v_num_erro = v_num_erro + 1.
              {utp/ut-liter.i Calendario_Funcionario_Inexistente mpe L}
              create tt_erro.
              assign tt_erro.num_erro = v_num_erro
                     tt_erro.des_erro = trim(return-value).
                     tt_erro.log_erro = YES.
              NEXT.
           END.
           if tt-calendar.idi_sit_dia_trab = 3 then do:  /*** repouso ***/

              assign d-qtd-hrs-diu-dsr = d-qtd-hrs-diu-dsr + tt-calendar.qti_padr_hrs_diurno
                     d-qtd-hrs-not-dsr = d-qtd-hrs-not-dsr + tt-calendar.qti_padr_hrs_notur.
              find last tt-aux-semana no-error.
              assign tt-aux-semana.qtd-dsr-sem     = 1
                     tt-aux-semana.qtd-hrs-diu-dsr = tt-calendar.qti_padr_hrs_diurno
                     tt-aux-semana.qtd-hrs-not-dsr = tt-calendar.qti_padr_hrs_notur
                     i-dias-func = 1.   

           end.
           if tt-calendar.idi_sit_dia_trab = 5 then do: /*** compensado ***/
              find first tt-calendar where
                         tt-calendar.cdn_turno_trab   = v_cdn_turno      and
                         tt-calendar.cdn_turma_trab   = v_cdn_turma      and
                         tt-calendar.dat_refer_calend > (dt-fim-out + 1) and
                         tt-calendar.idi_sit_dia_trab = 3                and 
                         tt-calendar.cod_pais         = v_cod_pais       and
                         tt-calendar.cdn_localidade   = v_cdn_localid
                         no-lock no-error.
              if avail tt-calendar then do:

                 assign d-qtd-hrs-diu-dsr = d-qtd-hrs-diu-dsr + tt-calendar.qti_padr_hrs_diurno
                        d-qtd-hrs-not-dsr = d-qtd-hrs-not-dsr + tt-calendar.qti_padr_hrs_notur.
                 find last tt-aux-semana no-error.
                 assign tt-aux-semana.qtd-dsr-sem     = 1
                        tt-aux-semana.qtd-hrs-diu-dsr = tt-calendar.qti_padr_hrs_diurno
                        tt-aux-semana.qtd-hrs-not-dsr = tt-calendar.qti_padr_hrs_notur
                        i-dias-func = 2.
              end.
           end.
       END.
    end.

    if d-qtd-hrs-not > 0 and l-sup-evnot = yes then  /* cris */
    DO:    
       if bparam-pe.log_livre_2 = no then
       DO:
          assign d-qtd-hrs-sup-not = (d-qtd-hrs-not + d-qtd-hrs-not-fer) * 0.1428571
                 d-qtd-hrs-sup-not = truncate(round(d-qtd-hrs-sup-not / 3600,3),3).
       END.
       else
       DO:    
          assign d-qtd-hrs-sup-not = d-qtd-hrs-not * 0.1428571
                 d-qtd-hrs-sup-not = truncate(round(d-qtd-hrs-sup-not / 3600,3),3).
       END.
    END.

    assign d-qtd-hrs-diu     = d-qtd-hrs-diu / 3600
           d-qtd-hrs-not     = d-qtd-hrs-not / 3600
           d-qtd-hrs-diu-dsr = d-qtd-hrs-diu-dsr / 3600
           d-qtd-hrs-not-dsr = d-qtd-hrs-not-dsr / 3600
           d-qtd-hrs-diu-fer = d-qtd-hrs-diu-fer / 3600
           d-qtd-hrs-not-fer = d-qtd-hrs-not-fer / 3600
           d-qtd-hrs-diu-aux = d-qtd-hrs-diu-aux / 3600
           d-qtd-hrs-not-aux = d-qtd-hrs-not-aux / 3600
           d-tot-hrsmes      = d-qtd-hrs-diu + d-qtd-hrs-not + d-qtd-hrs-diu-dsr + d-qtd-hrs-not-dsr
                               + d-qtd-hrs-diu-fer + d-qtd-hrs-not-fer + d-qtd-hrs-sup-not.

    if bfunc-ponto.cdn_categ_sal = 1 then do:                /* Horas Màs para Mensalistas */
       /************************CHAMADA EPC MRS**********************************/
       for each tt-epc EXCLUSIVE-LOCK where
                tt-epc.cod-event = "calc_mrs":
           delete tt-epc.
       end.
       create tt-epc.
       assign tt-epc.cod-event     = "calc_mrs"
              tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                     string(bfunc-ponto.cdn_estab,"999") +
                                     string(bfunc-ponto.cdn_funcionario,"99999999") +
                                     string(bcontrole-funcponto.dat_term_period_apurac_pto_mes,"99/99/9999").
       {include/i-epc201.i "calc_mrs"}
       /************************CHAMADA EPC MRS**********************************/
       if l-func-normal = yes then
          assign d-tot-hrsmes = if v_log_upc_mrs then v_qtd_hrs_padr_mes_rh else 
                                   bturnotrb.qtd_hrs_padr_mes_rh. /************************CHAMADA EPC MRS**********************************/

       if not l-func-normal and bparam-pe.idi_hrs_admit_demit = 2 then do: /* Selecionou Horas Padr∆o no PE0200 */
         assign i-nulo          = " "
                c-mmaa-adm-func = string(((month(bfunc-ponto.dat_admis_func) * 10000) + year(bfunc-ponto.dat_admis_func)),"999999")
                c-mmaa-dem-func = if bfunc-ponto.dat_desligto_func <> ? 
                                    then string(((month(bfunc-ponto.dat_desligto_func) * 10000) + year(bfunc-ponto.dat_desligto_func)),"999999") 
                                    else i-nulo
                i-mm-corr       = int(substr(c-mmaa-folha,01,02))
                i-aa-corr       = int(substr(c-mmaa-folha,03,04))
                i-mm-prox       = if i-mm-corr = 12 
                                     then 1
                                     else (i-mm-corr + 1)
                i-aa-prox       = if i-mm-corr = 12
                                     then (i-aa-corr + 1)
                                     else i-aa-corr.

         if c-mmaa-adm-func <> c-mmaa-dem-func then do: /* Funcion†rio com Admiss∆o OU Demiss∆o no Màs */
             if c-mmaa-folha = c-mmaa-adm-func then do: /* Funcion†rio com Admiss∆o no Màs */
                if day(bfunc-ponto.dat_admis_func) = 1 then
                   assign i-dias-func = 30.
                else
                   assign i-dias-func = 30 - day(bfunc-ponto.dat_admis_func - 1).
                if i-dias-func = 0 then 
                   assign i-dias-func = 1.
                assign d-tot-hrsmes = if v_log_upc_mrs then ((v_qtd_hrs_padr_mes_rh / 30) * i-dias-func) else 
                                         ((bturnotrb.qtd_hrs_padr_mes_rh / 30) * i-dias-func). /************************CHAMADA EPC MRS**********************************/
             end.
             if c-mmaa-folha = c-mmaa-dem-func then /* Funcion†rio com Demiss∆o no Màs */
                assign i-dias-func = i-dias-func + ((bfunc-ponto.dat_desligto_func - date(i-mm-corr,01,i-aa-corr)) + 1)
                       d-tot-hrsmes = if v_log_upc_mrs then ((v_qtd_hrs_padr_mes_rh / 30) * i-dias-func) else 
                                        ((bturnotrb.qtd_hrs_padr_mes_rh / 30) * i-dias-func). /************************CHAMADA EPC MRS**********************************/
             ELSE
                 if (STRING((MONTH(bcontrole-funcponto.dat_inic_period_apurac_pto_mes) * 10000) +
                            YEAR(bcontrole-funcponto.dat_inic_period_apurac_pto_mes), "999999") = c-mmaa-adm-func or
                     (STRING((MONTH(bcontrole-funcponto.dat_term_period_apurac_pto_mes) * 10000) +
                             YEAR(bcontrole-funcponto.dat_term_period_apurac_pto_mes), "999999") = c-mmaa-adm-func)) then do: /* Funcion†rio com Admiss∆o no Màs */
                     if day(bfunc-ponto.dat_admis_func) = 1 then
                         assign i-dias-func = 30.
                     else
                         assign i-dias-func = 30 - day(bfunc-ponto.dat_admis_func - 1).
                     if i-dias-func = 0 then 
                         assign i-dias-func = 1.
                     assign d-tot-hrsmes = if v_log_upc_mrs 
                                           then ((v_qtd_hrs_padr_mes_rh / 30) * i-dias-func) 
                                           else ((bturnotrb.qtd_hrs_padr_mes_rh / 30) * i-dias-func). /************************CHAMADA EPC MRS**********************************/
                 END.
         end.
         else do: /* Funcion†rio com Admiss∆o E Demiss∆o no Màs */             
             assign i-dias-func = i-dias-func + ((bfunc-ponto.dat_desligto_func - bfunc-ponto.dat_admis_func) + 1).
             if i-dias-func = 31 then 
                 assign i-dias-func = 30.
             assign d-tot-hrsmes = if v_log_upc_mrs then ((v_qtd_hrs_padr_mes_rh / 30) * i-dias-func) else
                                   ((bturnotrb.qtd_hrs_padr_mes_rh / 30) * i-dias-func). /************************CHAMADA EPC MRS**********************************/
         end.
       end.

       assign v_qtd_hrs_mes_rh_1 = if v_log_upc_mrs then v_qtd_hrs_padr_mes_rh else
                                      bturnotrb.qtd_hrs_padr_mes_rh. /***ESPECIFICO MRS**********************/

       if d-tot-hrsmes > v_qtd_hrs_mes_rh_1 then    /* Acerto para meses com 31 dias */   
          assign d-tot-hrsmes = if v_log_upc_mrs then v_qtd_hrs_padr_mes_rh else 
                                   bturnotrb.qtd_hrs_padr_mes_rh. /************************CHAMADA EPC MRS**********************************/
    end.
    ELSE DO:
/*           MESSAGE "ponto else"                                                                                                                                                              */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                                                                            */
/*           IF bcatponto.idi_pagto_horist = 2 /*pagamento horista = padrao*/ then do:                                                                                                         */
/*              if l-desligado then                                                                                                                                                            */
/*                 assign d-tot-hrsmes = bturnotrb.qtd_hrs_padr_dia_rh * ( day(bfunc-ponto.dat_desligto) + i-dias-func).                                                                       */
/*              else do:                                                                                                                                                                       */
/*                 if bcontrole-catponto.num_mes_primei_calc_realzdo <> 12 then                                                                                                                */
/*                    assign v_num_mes = bcontrole-catponto.num_mes_primei_calc_realzdo  + 1                                                                                                   */
/*                           v_num_ano = bcontrole-catponto.num_ano_primei_calc_ptoelet.                                                                                                       */
/*                 else                                                                                                                                                                        */
/*                    assign v_num_mes = 1                                                                                                                                                     */
/*                           v_num_ano = bcontrole-catponto.num_ano_primei_calc_ptoelet + 1.                                                                                                   */
/*                 /**** Admitido no Mes *****/                                                                                                                                                */
/*                 IF month(bfunc-ponto.dat_admis_func) = bcontrole-catponto.num_mes_primei_calc_realzdo AND                                                                                   */
/*                    YEAR(bfunc-ponto.dat_admis_func) = bcontrole-catponto.num_ano_primei_calc_ptoelet  THEN DO:                                                                              */
/*                    assign d-tot-hrsmes = bturnotrb.qtd_hrs_padr_dia_rh * (((date(v_num_mes,01,v_num_ano) - 1) - bfunc-ponto.dat_admis_func) + 1).                                           */
/*                 END.                                                                                                                                                                        */
/*                 ELSE DO:                                                                                                                                                                    */
/*                    /**** Funcionario Normal *******/                                                                                                                                        */
/*                    if bcatponto.num_livre_1 = 1 OR                                                                                                                                          */
/*                       bcatponto.num_livre_1 = 3 then do:                                                                                                                                    */
/*                       /***** Mes de Referencia ******/                                                                                                                                      */
/*                       assign d-tot-hrsmes = bturnotrb.qtd_hrs_padr_dia_rh * day(date(v_num_mes,01,v_num_ano) - 1).                                                                          */
/*                    END.                                                                                                                                                                     */
/*                    ELSE DO:                                                                                                                                                                 */
/*                       /******* Periodo de Ponto *******/                                                                                                                                    */
/*                       assign d-tot-hrsmes = bturnotrb.qtd_hrs_padr_dia_rh *  ((bcontrole-catponto.dat_term_period_apurac_pto_mes - bcontrole-catponto.dat_inic_period_apurac_pto_mes) + 1). */
/*                    END.                                                                                                                                                                     */
/*                 end.                                                                                                                                                                        */
/*              END.                                                                                                                                                                           */
/*           END.                                                                                                                                                                              */
/*           /*** FO Induscar 1573.024 ***/                                                                                                                                                    */
/*           ELSE DO:                                                                                                                                                                          */
/*             if bcatponto.num_livre_1 = 1 OR                                                                                                                                                 */
/*                bcatponto.num_livre_1 = 3 then do:                                                                                                                                           */
/*                do dt-aux = bcontrole-catponto.dat_inic_period_apurac_pto_mes to bcontrole-catponto.dat_term_period_apurac_pto_mes:                                                          */
/*                   find first bsit_afast_func of bfunc-ponto no-lock where                                                                                                                   */
/*                              bsit_afast_func.dat_term_proces_sit_afast >= dt-aux and                                                                                                        */
/*                              bsit_afast_func.dat_inic_proces_sit_afast <= dt-aux and                                                                                                        */
/*                              bsit_afast_func.dat_integr_sit_afast_func = ? no-error.                                                                                                        */
/*                   if not avail bsit_afast_func then do:                                                                                                                                     */
/*                       assign l-afast-periodo = no.                                                                                                                                          */
/*                       leave.                                                                                                                                                                */
/*                   end.                                                                                                                                                                      */
/*                   else do:                                                                                                                                                                  */
/*                       find first bsit_afast no-lock of bsit_afast_func no-error.                                                                                                            */
/*                       if bsit_afast.idi_signif_sit = 2 or                                                                                                                                   */
/*                          bsit_afast.idi_signif_sit = 7 THEN DO:                                                                                                                             */
/*                          assign l-afast-periodo = yes.                                                                                                                                      */
/*                       END.                                                                                                                                                                  */
/*                       else do:                                                                                                                                                              */
/*                           assign l-afast-periodo = no.                                                                                                                                      */
/*                           leave.                                                                                                                                                            */
/*                       end.                                                                                                                                                                  */
/*                   end.                                                                                                                                                                      */
/*                end.                                                                                                                                                                         */
/*                if bcontrole-catponto.num_mes_primei_calc_realzdo <> 12 then                                                                                                                 */
/*                    assign v_num_mes = bcontrole-catponto.num_mes_primei_calc_realzdo  + 1                                                                                                   */
/*                           v_num_ano = bcontrole-catponto.num_ano_primei_calc_ptoelet.                                                                                                       */
/*                 else                                                                                                                                                                        */
/*                    assign v_num_mes = 1                                                                                                                                                     */
/*                           v_num_ano = bcontrole-catponto.num_ano_primei_calc_ptoelet + 1.                                                                                                   */
/*                                                                                                                                                                                             */
/*                IF  l-afast-periodo = YES AND bsit_afast.idi_hrs_desc_dia = 2 THEN DO:                                                                                                       */
/*                    ASSIGN d-tot-hrsmes = bturnotrb.qtd_hrs_padr_dia_rh * day(date(v_num_mes,01,v_num_ano) - 1).                                                                             */
/*                END.                                                                                                                                                                         */
/*             END.                                                                                                                                                                            */
/*           END.                                                                                                                                                                              */
/*           /*** FO Induscar 1573.024 ***/                                                                                                                                                    */
    END.
    /* para periodo ponto -  acerto da quantidade de horas do mes para periodo ponto e qtd-horas total mes menor que total de horas mes do turno */
    if bcatponto.num_livre_1 = 2 then do: /* categoria ponto - periodo ponto */

       IF (d-qtd-hrs-diu + d-qtd-hrs-not + d-qtd-hrs-diu-dsr + d-qtd-hrs-not-dsr + d-qtd-hrs-diu-fer + d-qtd-hrs-not-fer) < d-tot-hrsmes THEN 
          IF d-qtd-hrs-diu = 0 THEN
             ASSIGN d-qtd-hrs-not = d-tot-hrsmes - (d-qtd-hrs-diu-dsr + d-qtd-hrs-not-dsr + d-qtd-hrs-diu-fer + d-qtd-hrs-not-fer).
          ELSE 
             ASSIGN d-qtd-hrs-diu = d-tot-hrsmes - (d-qtd-hrs-not + d-qtd-hrs-diu-dsr + d-qtd-hrs-not-dsr + d-qtd-hrs-diu-fer + d-qtd-hrs-not-fer).
    END.
    /* fim acerto da quantidade de horas para periodo ponto */
    if c-mmaa-folha <> c-mmaa-adm-func then do:
       for each tt-epc EXCLUSIVE-LOCK 
           where tt-epc.cod-event = "HORAS":
           delete tt-epc. 
       end.
       CREATE tt-epc.
       &if "{&cd_rel_hr}" >= "2.11" &then
           assign tt-epc.cod-event = "HORAS"
                  tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                         string(bfunc-ponto.cdn_estab) + "|" +
                                         string(bfunc-ponto.cdn_funcionario).
       &else
           assign tt-epc.cod-event = "HORAS"
                  tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                         string(bfunc-ponto.cdn_estab,"999") +
                                         string(bfunc-ponto.cdn_funcionario,"99999999").       
       &endif


       {include/i-epc201.i "HORAS"}
    END.

    find func_ptoelet of bfunc-ponto exclusive-lock no-error.
    assign substring(func_ptoelet.cod_livre_2,04,01) = substring(func_ptoelet.cod_livre_2,03,01)
           v_log_influi_rep = no   
           v_log_influi_fer = no.

    /* Dencontar DSR/Feriado por faltas ocorridas no periodo anterior */
    if substring(bfunc-ponto.cod_livre_2,03,01) = "s" then do:
       find FIRST tt-aux-semana exclusive-lock no-error.
       if avail tt-aux-semana then do:

          if bfunc-ponto.log_func_descta_dsr or bfunc-ponto.log_func_descta_fer then do:
              /* Descobrir qual o primeiro dia da ultima semana do periodo anterior - Flavio */
              /*Para buscar o periodo ponto corretamente*/
              for first categ_sal no-lock where
                  categ_sal.cdn_empresa = bfunc-ponto.cdn_empresa and
                  categ_sal.cdn_estab   = bfunc-ponto.cdn_estab and
                  categ_sal.cdn_categ_sal = bfunc-ponto.cdn_categ_sal: end.
              find first param_empres_rh no-lock where 
                  param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-error.

              for each b2tt-calendar where
                  b2tt-calendar.dat_refer_calend < date(string(categ_sal.num_dia_inic_period_pto) + "/" + string(param_empres_rh.num_mes_refer_calc_efetd) + "/" + string(param_empres_rh.num_ano_refer_calc_efetd)) and
                  b2tt-calendar.idi_sit_dia_trab = 3 /* Repouso */ 
                  break by b2tt-calendar.dat_refer_calend desc:

                  find first b3tt-calendar where
                      b3tt-calendar.dat_refer_calend = (b2tt-calendar.dat_refer_calend + 1) no-error.
                  if avail b3tt-calendar and b3tt-calendar.idi_sit_dia_trab <> 3 /* Repouso */ then do:
                      assign v_dat_ini_ult_sem_per_ant = b3tt-calendar.dat_refer_calend.
                      leave.
                  end.
              end.

              FOR EACH sit_afast_func OF bfunc-ponto WHERE
                  sit_afast_func.dat_inic_proces_sit_afast >= v_dat_ini_ult_sem_per_ant and
                  sit_afast_func.dat_term_proces_sit_afast <= bcontrole-catponto.dat_inic_period_apurac_pto_mes NO-LOCK
                  break by sit_afast_func.dat_inic_sit_afast desc:

                  if bfunc-ponto.log_func_descta_dsr and
                      not v_log_influi_rep and
                      can-find(first sit_afast where
                               sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func and
                               sit_afast.log_influi_repous) then
                      /* perde repouso referente ao mes passado independente da regra atual da empresa */
                      assign d-hrs-diu-dsr-per             = d-hrs-diu-dsr-per + tt-aux-semana.qtd-hrs-diu-dsr
                             d-hrs-not-dsr-per             = d-hrs-not-dsr-per + tt-aux-semana.qtd-hrs-not-dsr
                             i-qtd-dsr-per                 = i-qtd-dsr-per + tt-aux-semana.qtd-dsr-sem
                             tt-aux-semana.qtd-fer-sem     = 0
                             tt-aux-semana.qtd-hrs-diu-dsr = 0
                             tt-aux-semana.qtd-hrs-not-dsr = 0
                             v_log_influi_rep              = yes.
              
                  if bfunc-ponto.log_func_descta_fer and
                      not v_log_influi_fer and
                      can-find(first sit_afast where
                               sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func and
                               sit_afast.log_influi_para_fer) then
                      /* perde feriado referente mes passado */
                      assign d-hrs-diu-fer-per             = d-hrs-diu-fer-per + tt-aux-semana.qtd-hrs-diu-fer
                             d-hrs-not-fer-per             = d-hrs-not-fer-per + tt-aux-semana.qtd-hrs-not-fer
                             i-qtd-fer-per                 = i-qtd-fer-per + tt-aux-semana.qtd-fer-sem
                             tt-aux-semana.qtd-fer-sem     = 0
                             tt-aux-semana.qtd-hrs-diu-fer = 0
                             tt-aux-semana.qtd-hrs-not-fer = 0
                             v_log_influi_fer              = yes.
              END.
          end.

          /* EPC para tratar gravacao dos domingos de dsr perdidos */
          for each tt-epc EXCLUSIVE-LOCK where tt-epc.cod-event = "grava_dsr":
             delete tt-epc. 
          end.

          create tt-epc.
          &if "{&cd_rel_hr}" >= "2.11" &then
              assign tt-epc.cod-event = "grava_dsr"
                     tt-epc.val-parameter = string(func_ptoelet.cdn_empresa) + "|" +
                                            string(func_ptoelet.cdn_estab) + "|" +
                                            string(func_ptoelet.cdn_funcionario)
                     tt-epc.cod-parameter = string(tt-aux-semana.dat-fim) /*Domingo perdido*/.          
          &else
              assign tt-epc.cod-event = "grava_dsr"
                     tt-epc.val-parameter = string(func_ptoelet.cdn_empresa,"999") +
                                            string(func_ptoelet.cdn_estab,"999") +
                                            string(func_ptoelet.cdn_funcionario,"99999999")
                     tt-epc.cod-parameter = string(tt-aux-semana.dat-fim) /*Domingo perdido*/.          
          &endif



          {include/i-epc201.i "grava_dsr"}
          /********************************************************************/

       end.
       assign substring(func_ptoelet.cod_livre_2,03,01) = "n".
    end.

    /**** tratamento situaá‰es afastamento **************/

    assign l-afast-mes = no.
    run prghur/pep/pe4000r4.p.

end.

assign v_log_movto_extra = yes.

run pi-horas-extra-divergencia(input bfunc-ponto.cdn_empresa,
                               input bfunc-ponto.cdn_estab,
                               input bfunc-ponto.cdn_funcionario,
                               input yes).

/*Para verificar Horas Plant∆o da lotaá∆o anterior---------------------------------------*/
for first funcionario no-lock of bfunc-ponto where
    funcionario.dat_admis_transf <> ?, /*para garantir que teve transf*/
    first bffuncionario no-lock where
    bffuncionario.num_pessoa_fisic = funcionario.num_pessoa_fisic and
    bffuncionario.dat_desligto     > bcontrole-funcponto.dat_inic_period_apurac_pto_mes: 
    
    run pi-horas-extra-divergencia(input bffuncionario.cdn_empresa,
                                   input bffuncionario.cdn_estab,
                                   input bffuncionario.cdn_funcionario,
                                   input no).
end.
/*---------------------------------------------------------------------------------------*/

procedure pi-horas-extra-divergencia:
    def input param p-cdn-empresa       like funcionario.cdn_empresa no-undo.
    def input param p-cdn-estab         like funcionario.cdn_estab no-undo.
    def input param p-cdn-funcionario   like funcionario.cdn_funcionario no-undo.
    def input param p-log-status        as log no-undo.

    /**** busca no eve-parbat horas extras e horas divergecia **********************/ 
    for each efp_par_marcac_ptoelet exclusive-lock where
        efp_par_marcac_ptoelet.cdn_empresa     = p-cdn-empresa and
        efp_par_marcac_ptoelet.cdn_estab       = p-cdn-estab and
        efp_par_marcac_ptoelet.cdn_funcionario = p-cdn-funcionario and
        efp_par_marcac_ptoelet.dat_proces_mpe >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
        efp_par_marcac_ptoelet.dat_proces_mpe <= dat-fim-mes and /* cris func demit */
        ((efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet =  1 and p-log-status) or
         efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet =  5 or /* Hora Plant∆o */
         (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet =  6 and p-log-status)) /*Hora Extra Intervalo*/ and 
        efp_par_marcac_ptoelet.cdn_efp > ""/*0*/
        break by efp_par_marcac_ptoelet.dat_proces_mpe:   
        
        if efp_par_marcac_ptoelet.dat_proces_mpe > bcontrole-catponto.dat_term_period_apurac_pto_mes and
           not l-desligado then /* cris func demit */
           next.
    
        if bcatponto.idi_pagto_adc_notur  = 2 and
           efp_par_marcac_ptoelet.cdn_efp = if bcatponto.cdn_efp_adc_notur > "" 
                                            then bcatponto.cdn_efp_adc_notur
                                            else sindicato.cdn_efp_adc_notur   THEN
           NEXT.
    
        assign i-ev-codigo               = efp_par_marcac_ptoelet.cdn_efp
               i-qtd-hrs                 = efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet / 3600
               v_log_movto_extra         = yes
               efp_par_marcac_ptoelet.num_mes_ano_refer_fp = c-mmaa-folha.
    
    
        /***** EPC MRS Horas-passe *****/
        for each tt-epc EXCLUSIVE-LOCK where tt-epc.cod-event = "descont_hrs_aviso":
            delete tt-epc. 
        end.
        create tt-epc.
    
        &if "{&cd_rel_hr}" >= "2.11" &then
            assign tt-epc.cod-event = "descont_hrs_aviso"
                   tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa)               + "|" +
                                          string(bfunc-ponto.cdn_estab)                 + "|" +
                                          string(bfunc-ponto.cdn_funcionario)           + "|" +
                                          string(efp_par_marcac_ptoelet.dat_proces_mpe) + "|" +
                                          string(efp_par_marcac_ptoelet.num_horar_inic_proces_mpe).
        &ELSE
            assign tt-epc.cod-event = "descont_hrs_aviso"
                   tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                          string(bfunc-ponto.cdn_estab,"999") +
                                          string(bfunc-ponto.cdn_funcionario,"99999999") +
                                          string(efp_par_marcac_ptoelet.dat_proces_mpe,"99/99/9999") +
                                          string(efp_par_marcac_ptoelet.num_horar_inic_proces_mpe,"-999999").
        &ENDIF
        {include/i-epc201.i "descont_hrs_aviso"}
    
        if return-value = "OK-PASSE" then do:
            find first tt-epc 
                 where tt-epc.cod-event = "descont_hrs_aviso" no-error.
            if avail tt-epc then
                assign i-qtd-hrs = i-qtd-hrs - dec(tt-epc.val-parameter).
        end.
        /***** FIM EPC MRS Horas-passe *****/
    
    
        /************ EPC para verificar se extras ja foram geradas - finardi */
        for each tt-epc EXCLUSIVE-LOCK where tt-epc.cod-event = "valida_integracao_extra":
            delete tt-epc. 
        end.
        create tt-epc.
        &if "{&cd_rel_hr}" >= "2.11" &then
            assign tt-epc.cod-event = "valida_integracao_extra"
               tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                      string(bfunc-ponto.cdn_estab) + "|" +
                                      string(bfunc-ponto.cdn_funcionario) + "|" +
                                      STRING(efp_par_marcac_ptoelet.dat_proces_mpe).
    
        &else
            assign tt-epc.cod-event = "valida_integracao_extra"
               tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                      string(bfunc-ponto.cdn_estab,"999") +
                                      string(bfunc-ponto.cdn_funcionario,"99999999") +
                                      STRING(efp_par_marcac_ptoelet.dat_proces_mpe,"99/99/9999").
        &endif
    
    
    
        {include/i-epc201.i "valida_integracao_extra"}
    
        IF RETURN-VALUE = "OK-EXTRAS" THEN
           NEXT.

        run pi-movto-ptoelet.
    end.

end procedure.

MESSAGE "antes da epc BCO"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/******** Chamada EPC - Jeziel **********/
for each tt-epc EXCLUSIVE-LOCK where tt-epc.cod-event = "BCO":
    delete tt-epc. 
end.
create tt-epc.

&if "{&cd_rel_hr}" >= "2.11" &then
    assign tt-epc.cod-event = "BCO"
           tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                  string(bfunc-ponto.cdn_estab) + "|" +
                                  string(bfunc-ponto.cdn_funcionario).
&else
    assign tt-epc.cod-event = "BCO"
           tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                  string(bfunc-ponto.cdn_estab,"999") +
                                  string(bfunc-ponto.cdn_funcionario,"99999999").
&endif


{include/i-epc201.i "BCO"}

MESSAGE "depois da epc BCO"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

/********* fechamento -  banco de horas *****************************************/
find param_empres_rh no-lock where
     param_empres_rh.cdn_empresa = bfunc-ponto.cdn_empresa no-error.
find param_empres_tma no-lock where
     param_empres_tma.cdn_empresa = bfunc-ponto.cdn_empresa no-error.

if avail param_empres_tma then do:

   /***  cris - gravar saldos na tabela sit_calc_ptoelet_func. *******/
   assign v_num_hrs_pos      = 0
          v_num_hrs_neg      = 0.

   if l-desligado = yes then
      assign v_dat_term_compens = bfunc-ponto.dat_desligto_func.
   else
      assign v_dat_term_compens = bcontrole-catponto.dat_term_period_apurac_pto_mes.

   for each tt_hora_extra EXCLUSIVE-LOCK:  /* cris */
      delete tt_hora_extra.
   end.

   if (v_num_mes_ocor > param_empres_rh.num_mes_refer_calc_efetd and
      v_num_ano_ocor = param_empres_rh.num_ano_refer_calc_efetd) or
      (v_num_ano_ocor > param_empres_rh.num_ano_refer_calc_efetd) then do:
        for each bco_hrs_compens_func of bfunc-ponto exclusive-lock where
                 bco_hrs_compens_func.cod_mes_ano_refer_fp      = "" and
                 bco_hrs_compens_func.idi_tratam_lancto_bco_hrs = 1  and
                 bco_hrs_compens_func.cdn_tip_compcao_hrs       = 1  and
                 bco_hrs_compens_func.dat_atualiz_bco_hora     <= v_dat_term_compens:
           assign bco_hrs_compens_func.num_livre_1 = 0
                  bco_hrs_compens_func.val_livre_1 = 0
                  bco_hrs_compens_func.val_livre_2 = 0.
        end. /* bco */
   end.

   if (not param_empres_tma.log_lim_max_bco_hrs and
       not param_empres_tma.log_lim_max_period_pto and
       not param_empres_tma.log_lim_max_semana and
       v_log_fecha) or
      l-desligado then do:
      run prghur/pep/pe4000r2.p.
   end.
   else do:
      if param_empres_tma.log_lim_max_semana = yes then do:

         ASSIGN v_log_sem_5 = NO.

         /*chamada epc para a Codstil */
         for each tt-epc exclusive-lock where tt-epc.cod-event = "lim_bco_sem":
             delete tt-epc. 
         end.
         create tt-epc.
         &if "{&cd_rel_hr}" >= "2.11" &then
             assign tt-epc.cod-event = "lim_bco_sem"
                    tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                           string(bfunc-ponto.cdn_estab) + "|" +
                                           STRING(bfunc-ponto.cdn_funcionario).
         &else
             assign tt-epc.cod-event = "lim_bco_sem"
                    tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                           string(bfunc-ponto.cdn_estab,"999") +
                                           STRING(bfunc-ponto.cdn_funcionario,"99999999").
         &endif


         {include/i-epc201.i "lim_bco_sem"}
         if  return-value = "OK-lim_bco_sem" THEN DO:
             ASSIGN v_log_sem_5 = YES.
         END.

         assign v_ind_lim_bco = 3.
         do i1 = 1 to 7:
            if i1 = 1 then do:
               if weekday(bcontrole-catponto.dat_inic_period_apurac_pto_mes) = 2 then 
                  assign v_dat_inic_compens = bcontrole-catponto.dat_inic_period_apurac_pto_mes.
               else do:
                  assign d_aux_inic = (bcontrole-catponto.dat_inic_period_apurac_pto_mes - 1).
                  do i = 1 to 7:
                     if weekday(d_aux_inic) = 2 then 
                        assign v_dat_inic_compens = d_aux_inic
                               i                  = 7.
                     else 
                        assign d_aux_inic = (d_aux_inic - 1).
                  end.
               end.
               assign v_dat_term_compens = (v_dat_inic_compens + 6)
                      l_semanal          = yes.
            end.
            else do:
               if l-desligado then do:
                  if (v_dat_term_compens + 7) <= bfunc-ponto.dat_desligto_func then 
                     assign v_dat_inic_compens = (v_dat_term_compens + 1)
                            v_dat_term_compens = (v_dat_inic_compens + 6)
                            l_semanal          = yes.
                  else 
                     assign l_semanal = no
                            i1        = 7.
               end.
               else do:
                  if (v_dat_term_compens + 7) <= bcontrole-catponto.dat_term_period_apurac_pto_mes then 
                     assign v_dat_inic_compens = (v_dat_term_compens + 1)
                            v_dat_term_compens = (v_dat_inic_compens + 6)
                            l_semanal          = yes.
                  else 
                     assign l_semanal = no
                            i1        = 7.
               end.
            end. /* else */

            if l_semanal = yes THEN DO:
               ASSIGN v_dat_aux_term_compens = IF v_log_sem_5 
                                               THEN v_dat_term_compens - 2
                                               ELSE v_dat_term_compens.
               run pi-limite-bco (INPUT v_dat_aux_term_compens).
            END.
         end. /* do */

         assign v_dat_inic_compens = bcontrole-catponto.dat_inic_period_apurac_pto_mes /* para considerar a data  */
                v_dat_term_compens = if l-desligado then                               /* de in°cio corretamente  */
                                        bfunc-ponto.dat_desligto_func
                                     else   
                                        bcontrole-catponto.dat_term_period_apurac_pto_mes.
      end. /* semana */

      if param_empres_tma.log_lim_max_period_pto = yes then do:
         assign v_dat_inic_compens = bcontrole-catponto.dat_inic_period_apurac_pto_mes
                v_dat_term_compens = if l-desligado then
                                        bfunc-ponto.dat_desligto_func
                                     else   
                                        bcontrole-catponto.dat_term_period_apurac_pto_mes
                v_ind_lim_bco = 2.
         run pi-limite-bco (INPUT v_dat_term_compens).
      end.

      if param_empres_tma.log_lim_max_bco_hrs = yes then do:
         assign v_dat_inic_compens = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                v_dat_term_compens = if l-desligado then
                                        bfunc-ponto.dat_desligto_func
                                     else   
                                        bcontrole-catponto.dat_term_period_apurac_pto_mes
                v_ind_lim_bco = 1.
         run pi-limite-bco (INPUT v_dat_term_compens) .
      end.
   end. /* else */
   if (param_empres_tma.log_lim_max_semana or
       param_empres_tma.log_lim_max_period_pto or
       param_empres_tma.log_lim_max_bco_hrs) and
      v_log_fecha then do:
      run prghur/pep/pe4000r2.p.
   end.

   assign v_num_hrs_pos      = 0
          v_num_hrs_neg      = 0.
   for each bco_hrs_compens_func of bfunc-ponto no-lock where
            bco_hrs_compens_func.cod_mes_ano_refer_fp      = "" and
            bco_hrs_compens_func.idi_tratam_lancto_bco_hrs = 1  and
            bco_hrs_compens_func.cdn_tip_compcao_hrs       = 1  and
            bco_hrs_compens_func.dat_atualiz_bco_hora     <= v_dat_term_compens:
      if bco_hrs_compens_func.idi_hrs_posit = 1 then 
         assign v_num_hrs_pos = v_num_hrs_pos + bco_hrs_compens_func.qti_hrs_marcac_ptoelet.
      else do:
         if bco_hrs_compens_func.idi_hrs_posit = 2 then 
            assign v_num_hrs_neg = v_num_hrs_neg + bco_hrs_compens_func.qti_hrs_marcac_ptoelet.
      end.
   end. /* bco */
   assign bcontrole-funcponto.num_livre_1 = v_num_hrs_pos
          bcontrole-funcponto.num_livre_2 = v_num_hrs_neg.
end. /* fechto bco */
assign v_log_movto_extra = no.

/******** TRATA HORAS EXTRAS POR PER÷ODO **************************/
if can-find (first efp_par_marcac_ptoelet of bfunc-ponto where
   efp_par_marcac_ptoelet.dat_proces_mpe >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
   efp_par_marcac_ptoelet.dat_proces_mpe <= dat-fim-mes and  /* cris func demit */
  (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 1 or /* Hora Extra */
   efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 6) and 
   efp_par_marcac_ptoelet.cdn_efp = ""/*0*/) then do:
   assign v_num_tratam = 1.
   run pi-hrs-extra-periodo.
end.
if can-find (first efp_par_marcac_ptoelet of bfunc-ponto where
   efp_par_marcac_ptoelet.dat_proces_mpe >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
   efp_par_marcac_ptoelet.dat_proces_mpe <= dat-fim-mes and  /* cris func demit */
   efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet =  5 and   /* Hora Plant∆o */
   efp_par_marcac_ptoelet.cdn_efp = ""/*0*/) then do:
   assign v_num_tratam = 3.
   run pi-hrs-extra-periodo.
end.
assign v_log_movto_extra = no.

/**** Pagamento de Adicional das Horas Noturnas do Banco de Horas ****/ 
assign d_qtd_hrs_adic_not = 0
       i_efp_adic_not = if bcatponto.cdn_efp_adc_notur <> ""/*0*/ 
                        then bcatponto.cdn_efp_adc_notur
                        else sindicato.cdn_efp_adc_notur
       v_dat_fim_clc  = if bfunc-ponto.dat_desligto <> ? and bfunc-ponto.dat_desligto > bcontrole-funcponto.dat_term_period_apurac_pto_mes
                        then bfunc-ponto.dat_desligto 
                        else bcontrole-funcponto.dat_term_period_apurac_pto_mes. /* Caso func estiver demitido, considerar horas ate data de desligamento */

if bcatponto.idi_pagto_adc_notur = 2 then do: /* Selecionou Pagamento do Adicional Noturno no màs - PE0180 */

    run pi-adicional-noturno (input bfunc-ponto.cdn_empresa,
                              input bfunc-ponto.cdn_estab,
                              input bfunc-ponto.cdn_funcionario).

    for first funcionario no-lock of bfunc-ponto where
        funcionario.dat_admis_transf <> ?, /*para garantir que teve transf*/
        first bffuncionario no-lock where
        bffuncionario.num_pessoa_fisic = funcionario.num_pessoa_fisic and
        bffuncionario.dat_desligto     > bcontrole-funcponto.dat_inic_period_apurac_pto_mes: 

        run pi-adicional-noturno (input bffuncionario.cdn_empresa,
                                  input bffuncionario.cdn_estab,
                                  input bffuncionario.cdn_funcionario).
    end.

    if d_qtd_hrs_adic_not > 0 then do:
        
        assign i-ev-codigo = i_efp_adic_not
               i-qtd-hrs   = truncate(round(d_qtd_hrs_adic_not / 3600,3),3).

        run pi-movto-ptoelet.
    end.
end.

FOR EACH tt-epc WHERE tt-epc.cod-event = "TERMINO_APURACAO_HORAS":
    DELETE tt-epc. 
END.

CREATE tt-epc.
ASSIGN tt-epc.cod-event = "TERMINO_APURACAO_HORAS".

{include/i-epc201.i "TERMINO_APURACAO_HORAS"}
/*Conforme recomendaá∆o da f†brica,  toda chamada de epc deve tratar como return-value */
if return-value = "TERMINO_APURACAO_HORAS-OK" THEN DO:
END.

procedure pi-adicional-noturno:
    def input param p-cdn-empresa     like funcionario.cdn_empresa no-undo.
    def input param p-cdn-estab       like funcionario.cdn_estab no-undo.
    def input param p-cdn-funcionario like funcionario.cdn_funcionario no-undo.

    for each efp_par_marcac_ptoelet no-lock where
        efp_par_marcac_ptoelet.cdn_empresa = p-cdn-empresa and
        efp_par_marcac_ptoelet.cdn_estab   = p-cdn-estab and
        efp_par_marcac_ptoelet.cdn_funcionario = p-cdn-funcionario and
        efp_par_marcac_ptoelet.dat_proces_mpe >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
        efp_par_marcac_ptoelet.dat_proces_mpe <= v_dat_fim_clc and
        efp_par_marcac_ptoelet.cdn_efp         = i_efp_adic_not:
        assign d_qtd_hrs_adic_not = d_qtd_hrs_adic_not + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
    end.

end procedure.

IF NOT v_log_exc THEN DO:

    /**** cria mvtoponto com horas trabalhadas *********************************/
    assign d-hrs-diu-dsr-per = d-hrs-diu-dsr-per / 3600
           d-hrs-not-dsr-per = d-hrs-not-dsr-per / 3600
           d-hrs-diu-fer-per = d-hrs-diu-fer-per / 3600
           d-hrs-not-fer-per = d-hrs-not-fer-per / 3600.

    /***********          FECHAMENTO DAS HORAS DO FUNCIONµRIO   - Cris       ***********************/
    if bfunc-ponto.cdn_categ_sal = 1 and
       l-func-normal = yes then do:
       run prghur/pep/pe4000r6.p. /* CµLCULO HORAS MENSALISTAS */
    end.
    else do:
       run prghur/pep/pe4000r7.p. /* CµLCULO HORAS HORISTAS, ADMITIDOS E DEMITIDOS */
    end.
end.


if d-hrs-diu-dsr-per > 0 then do:
   assign i-ev-codigo = i-ev-dsrdiuper
          i-qtd-hrs   = d-hrs-diu-dsr-per.
   run pi-movto-ptoelet. 
end.

if d-hrs-not-dsr-per > 0 then do:
   assign i-ev-codigo = i-ev-dsrnotper 
          i-qtd-hrs   = d-hrs-not-dsr-per.

   run pi-movto-ptoelet. 
end.
if d-hrs-diu-fer-per > 0 then do:
   assign i-ev-codigo = i-ev-ferdiuper
          i-qtd-hrs   = d-hrs-diu-fer-per.

   run pi-movto-ptoelet. 
end.
if d-hrs-not-fer-per > 0 then do:
   assign i-ev-codigo = i-ev-fernotper 
          i-qtd-hrs   = d-hrs-not-fer-per.
   run pi-movto-ptoelet. 
end.

/********* Calculo de Adicional Noturno pelo periodo de ponto ***********/

 for each tt-epc EXCLUSIVE-LOCK where tt-epc.cod-event = "ADICNOT":
     delete tt-epc. 
 end.
 create tt-epc.

&if "{&cd_rel_hr}" >= "2.11" &then
     assign tt-epc.cod-event = "ADICNOT"
            tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                   string(bfunc-ponto.cdn_estab) + "|" +
                                   string(bfunc-ponto.cdn_funcionario).
&else
     assign tt-epc.cod-event = "ADICNOT"
            tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                   string(bfunc-ponto.cdn_estab,"999") +
                                   string(bfunc-ponto.cdn_funcionario,"99999999").
&endif

 {include/i-epc201.i "ADICNOT"}

IF bcatponto.num_livre_1 = 3 or
   return-value = "OK-EPC":U THEN DO:

    find idx_efp_funcao_espcif no-lock where
         idx_efp_funcao_espcif.cdn_idx_efp_funcao_espcif = 96 no-error. /* Horas Normais Trabalhadas Noturnas */    
    if avail idx_efp_funcao_espcif then do:
       find event_fp where 
            event_fp.cdn_empresa  = v_cdn_empresa_evento AND
            event_fp.cdn_event_fp = idx_efp_funcao_espcif.cdn_event_fp no-lock no-error. 
       if avail event_fp then do:
          if event_fp.cdn_efp_suplem_hrs_notur <> "" then
             assign l-sup-evnot = yes.
       end.
    end.
    
    if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
       v_log_sind_suplem = no then
       assign l-sup-evnot = no.

    ASSIGN v-qtd-adic-notur-per = 0
           v_dat_fim_clc = if bfunc-ponto.dat_desligto <> ? and bfunc-ponto.dat_desligto > bcontrole-funcponto.dat_term_period_apurac_pto_mes
                           then bfunc-ponto.dat_desligto 
                           else bcontrole-funcponto.dat_term_period_apurac_pto_mes.

    find sindicato no-lock where
         sindicato.cdn_sindicato = bfunc-ponto.cdn_sindicato no-error.
    if avail sindicato then do:
       if sindicato.cdn_efp_adc_notur > ""/*0*/ then do:
          ASSIGN i-ev-codigo = sindicato.cdn_efp_adc_notur. 

          FOR EACH par_marcac_ptoelet no-lock
             WHERE par_marcac_ptoelet.cdn_empresa     = bfunc-ponto.cdn_empresa
               AND par_marcac_ptoelet.cdn_estab       = bfunc-ponto.cdn_estab
               AND par_marcac_ptoelet.cdn_funcionario = bfunc-ponto.cdn_funcionario
               AND par_marcac_ptoelet.dat_proces_mpe >= bcontrole-funcponto.dat_inic_period_apurac_pto_mes
               AND par_marcac_ptoelet.dat_proces_mpe <= v_dat_fim_clc
               AND (par_marcac_ptoelet.idi_sit_mpe = 1 or
                    par_marcac_ptoelet.idi_sit_mpe = 9)
              break by par_marcac_ptoelet.dat_proces: 

              for each tt-epc EXCLUSIVE-LOCK where tt-epc.cod-event = "ADIC-FALTA":
                  delete tt-epc. 
              end.
              create tt-epc.

              &if "{&cd_rel_hr}" >= "2.11" &then
                  assign tt-epc.cod-event = "ADIC-FALTA"
                         tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                                string(bfunc-ponto.cdn_estab) + "|" +
                                                string(bfunc-ponto.cdn_funcionario) + "|" +
                                                string(replace(string(par_marcac_ptoelet.dat_proces_mpe),"/","")) + "|" +
                                                string(par_marcac_ptoelet.num_horar_inic_proces_mpe).              
              &else
                  assign tt-epc.cod-event = "ADIC-FALTA"
                         tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                                string(bfunc-ponto.cdn_estab,"999") +
                                                string(bfunc-ponto.cdn_funcionario,"99999999") +
                                                string(replace(string(par_marcac_ptoelet.dat_proces_mpe,"99/99/9999"),"/","")) + 
                                                string(par_marcac_ptoelet.num_horar_inic_proces_mpe,"-999999").              
              &endif
              
              {include/i-epc201.i "ADIC-FALTA"}

              if return-value = "NOK":U then
                  next.

              if first(par_marcac_ptoelet.dat_proces) then do:

                  for first funcionario no-lock where
                      funcionario.cdn_empresa       = par_marcac_ptoelet.cdn_empresa and
                      funcionario.cdn_estab         = par_marcac_ptoelet.cdn_estab and
                      funcionario.cdn_funcionario   = par_marcac_ptoelet.cdn_funcionario and
                      funcionario.dat_admis_transf <> ?, /*para garantir que teve transf*/
                      first bffuncionario no-lock where
                      rowid(bffuncionario)          <> rowid(funcionario) and
                      bffuncionario.num_pessoa_fisic = funcionario.num_pessoa_fisic and
                      bffuncionario.dat_desligto     > bcontrole-funcponto.dat_inic_period_apurac_pto_mes:

                      for each bfpar_marcac_ptoelet no-lock where 
                          bfpar_marcac_ptoelet.cdn_empresa     = bffuncionario.cdn_empresa and 
                          bfpar_marcac_ptoelet.cdn_estab       = bffuncionario.cdn_estab and 
                          bfpar_marcac_ptoelet.cdn_funcionario = bffuncionario.cdn_funcionario and 
                          bfpar_marcac_ptoelet.dat_proces_mpe >= bcontrole-funcponto.dat_inic_period_apurac_pto_mes AND  
                          bfpar_marcac_ptoelet.dat_proces_mpe <= v_dat_fim_clc and 
                          (bfpar_marcac_ptoelet.idi_sit_mpe    = 1 or
                           bfpar_marcac_ptoelet.idi_sit_mpe    = 9):

                          if bfpar_marcac_ptoelet.idi_tip_ocor_mpe = 2 then  /*** Evandro - Paga adicional pois hora neg. foi p/ banco ***/
                              assign v-qtd-adic-notur-per = v-qtd-adic-notur-per + bfpar_marcac_ptoelet.qti_hrs_notur.

                          if bfpar_marcac_ptoelet.idi_tip_ocor_mpe <> 1 then do:
                              if bfpar_marcac_ptoelet.idi_tip_ocor_mpe >= 7 and
                                 bfpar_marcac_ptoelet.idi_tip_ocor_mpe <= 15 then do:
                                  find first sit_afast no-lock where
                                      sit_afast.cdn_sit_afast_func = bfpar_marcac_ptoelet.cdn_sit_afast_func and 
                                      sit_afast.idi_signif_sit <> 2 no-error. /** para n∆o pagar adicional quando afastado **Cleiton**/
                                  if avail sit_afast then do:
                                      find event_fp no-lock where
                                          event_fp.cdn_empresa  = v_cdn_empresa_evento AND
                                          event_fp.cdn_event_fp = sit_afast.cdn_event_afast_notur no-error.
                                      if avail event_fp and
                                         not sit_afast.log_excec_adc_notur then do:
                                          if event_fp.idi_ident_efp <> 1 then
                                              next.
                                      end.
                                      else
                                          next.
                                  end.
                                  else
                                      next.
                              end.
                              else 
                                  if par_marcac_ptoelet.idi_tip_ocor_mpe <> 18 and 
                                     not bcatponto.idi_tratam_horar = 2 then next.
                          end.
                          assign v-qtd-adic-notur-per = v-qtd-adic-notur-per + bfpar_marcac_ptoelet.qti_hrs_notur.
                      end.
                  end.
              end. /*if first*/

              if par_marcac_ptoelet.idi_tip_ocor_mpe = 2 AND /*** Evandro - Paga adicional pois hora neg. foi p/ banco ***/
                 bcatponto.log_excec_bco_hrs_neg then /* nao esta parametrizada como excecao para adicional noturno */ 
                  next.

              if par_marcac_ptoelet.idi_tip_ocor_mpe = 2 then /*** Evandro - Paga adicional pois hora neg. foi p/ banco ***/
                  assign v-qtd-adic-notur-per = v-qtd-adic-notur-per + par_marcac_ptoelet.qti_hrs_notur.

              if par_marcac_ptoelet.idi_tip_ocor_mpe <> 1 then do:

                  if par_marcac_ptoelet.idi_tip_ocor_mpe >= 7 and
                     par_marcac_ptoelet.idi_tip_ocor_mpe <= 15 then do:
                      find sit_afast no-lock where
                          sit_afast.cdn_sit_afast_func = par_marcac_ptoelet.cdn_sit_afast_func AND
                          sit_afast.idi_signif_sit <> 2 no-error. /** para n∆o pagar adicional quando afastado **Cleiton**/
                      if avail sit_afast and
                         NOT sit_afast.log_excec_adc_notur then do: /* nao esta parametrizada como excecao adic not */ 

                          find event_fp no-lock where
                              event_fp.cdn_empresa  = v_cdn_empresa_evento AND
                              event_fp.cdn_event_fp = sit_afast.cdn_event_afast_notur no-error.
                          if avail event_fp then do:
                              if event_fp.idi_ident_efp <> 1 then
                                  next.
                          end.
                          else
                              next.
                      end.
                      else
                          next.
                  end.
                  else
                      if par_marcac_ptoelet.idi_tip_ocor_mpe <> 18 and
                         not bcatponto.idi_tratam_horar = 2 then next.
              end.
              ASSIGN v-qtd-adic-notur-per = v-qtd-adic-notur-per + par_marcac_ptoelet.qti_hrs_notur.
         END.

         for each tt-epc EXCLUSIVE-LOCK
             where tt-epc.cod-event = "SUPNOT":
             delete tt-epc. 
         end.
         create tt-epc.
         &if "{&cd_rel_hr}" >= "2.11" &then
             assign tt-epc.cod-event = "SUPNOT"
                    tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                           string(bfunc-ponto.cdn_estab) + "|" +
                                           string(bfunc-ponto.cdn_funcionario).         
         &else
             assign tt-epc.cod-event = "SUPNOT"
                    tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                           string(bfunc-ponto.cdn_estab,"999") +
                                           string(bfunc-ponto.cdn_funcionario,"99999999").         
         &endif

         {include/i-epc201.i "SUPNOT"}

         /*anderson*/
         ASSIGN v_qtd_horas_padrao   = 0
                i_dias_trab_apos_dem = 0.
         
         if l-desligado = yes AND bcatponto.idi_tratam_horar = 4 /*misto*/ then do:
            FOR EACH btt-calendar WHERE btt-calendar.dat_refer_calend <= bfunc-ponto.dat_desligto and
                                        btt-calendar.dat_refer_calend > bcontrole-catponto.dat_term_period_apurac_pto_mes and
                                        btt-calendar.idi_sit_dia_trab = 1  NO-LOCK:
                ASSIGN i_dias_trab_apos_dem = i_dias_trab_apos_dem + 1.
            END.
            /*logica para calcula adicional dos dias que o funcion†rio estiver desligado*/

            IF month(bfunc-ponto.dat_admis_func) = bcontrole-catponto.num_mes_primei_calc_realzdo AND
               YEAR(bfunc-ponto.dat_admis_func) = bcontrole-catponto.num_ano_primei_calc_ptoelet  THEN DO:
                   assign v_qtd_horas_padrao = tt-calendar.qti_padr_hrs_notur * i_dias_trab_apos_dem.
            END.
            ELSE 
               assign v_qtd_horas_padrao = tt-calendar.qti_padr_hrs_notur * i_dias_trab_apos_dem.
               ASSIGN v-qtd-adic-notur-per = v-qtd-adic-notur-per + v_qtd_horas_padrao.
         END.
         /*anderson*/

         ASSIGN i-qtd-hrs = if l-sup-evnot or
                               return-value = "OK-EPC":U THEN
                               TRUNCATE(round(((v-qtd-adic-notur-per / 3600) * 1.142857), 3),3)
                            else
                               TRUNCATE(round((v-qtd-adic-notur-per / 3600), 3),3).
         run pi-movto-ptoelet.

       END.
    END.
END.
/****************************************************************/

/*********** Adicional Repouso - Indice 33 ******************************/
if substring(bparam-pe.cod_livre_1,21,1) = "S" then
    run pi-adic-repouso.

/*********** Suplementaá∆o Noturna fora do fechamento das horas ********/
IF substr(bcatponto.cod_livre_2,23,1) = "S" THEN DO:

   if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
      v_log_sind_suplem = no then
      leave.


   find bmovto_ptoelet NO-LOCK where
      bmovto_ptoelet.cdn_empresa       = bfunc-ponto.cdn_empresa and
      bmovto_ptoelet.cdn_estab         = bfunc-ponto.cdn_estab and
      bmovto_ptoelet.cdn_funcionario   = bfunc-ponto.cdn_funcionario and
      bmovto_ptoelet.num_ano_refer_fp  = bcontrole-funcponto.num_ano_primei_calc_ptoelet and
      bmovto_ptoelet.num_mes_refer_fp  = bcontrole-funcponto.num_mes_primei_calc_realzdo and
      bmovto_ptoelet.idi_tip_fp_calcul = 1 and
      bmovto_ptoelet.num_parc_calc_movto_ptoelet = 9 AND 
      bmovto_ptoelet.cdn_efp = i-ev-trbnot NO-ERROR.
    IF AVAIL bmovto_ptoelet THEN DO:
       find event_fp where
            event_fp.cdn_empresa  = v_cdn_empresa_evento AND
            event_fp.cdn_event_fp = i-ev-trbnot no-lock no-error. 
       if avail event_fp then do:
          if event_fp.cdn_efp_suplem_hrs_notur <> "" /*0*/ then do:          /* busca evento suplementar horas noturnas */
             ASSIGN i-ev-codigo = event_fp.cdn_efp_suplem_hrs_notur
                    i-qtd-hrs = TRUNCATE(ROUND(bmovto_ptoelet.qtd_movto_ptoelet * 0.142857,3),3). 
             run pi-movto-ptoelet.
          END.
       END.
    end.    
END.
/**************************************************************************/

/******** Chamada EPC - MRS - Horas passe **********/
for each tt-epc EXCLUSIVE-LOCK
    where tt-epc.cod-event = "horas_passe":
    delete tt-epc. 
end.
create tt-epc.
&if "{&cd_rel_hr}" >= "2.11" &then
    assign tt-epc.cod-event = "horas_passe"
           tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa)                            + "|" +
                                  string(bfunc-ponto.cdn_estab)                              + "|" +
                                  string(bfunc-ponto.cdn_funcionario)                        + "|" +
                                  string(bcontrole-funcponto.num_mes_primei_calc_realzdo)    + "|" +
                                  string(bcontrole-funcponto.num_ano_primei_calc_ptoelet)    + "|" +
                                  string(bcontrole-funcponto.dat_inic_period_apurac_pto_mes) + "|" +
                                  string(v_dat_fim_clc).
&ELSE
    assign tt-epc.cod-event = "horas_passe"
           tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                  string(bfunc-ponto.cdn_estab,"999") +
                                  string(bfunc-ponto.cdn_funcionario,"99999999") +
                                  string(bcontrole-funcponto.num_mes_primei_calc_realzdo, "99") +
                                  string(bcontrole-funcponto.num_ano_primei_calc_ptoelet, "9999") +
                                  string(bcontrole-funcponto.dat_inic_period_apurac_pto_mes, "99/99/9999") + 
                                  string(v_dat_fim_clc, "99/99/9999").
&endif

{include/i-epc201.i "horas_passe"}
/******** FIM Chamada EPC - MRS - Horas passe **********/


/******** Chamada EPC - Jeziel **********/
for each tt-epc EXCLUSIVE-LOCK
    where tt-epc.cod-event = "DSR":
    delete tt-epc. 
end.

&if "{&cd_rel_hr}" >= "2.11" &then
    create tt-epc.
    assign tt-epc.cod-event = "DSR"
           tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa) + "|" +
                                  string(bfunc-ponto.cdn_estab) + "|" +
                                  string(bfunc-ponto.cdn_funcionario).
&else
    create tt-epc.
    assign tt-epc.cod-event = "DSR"
           tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                  string(bfunc-ponto.cdn_estab,"999") +
                                  string(bfunc-ponto.cdn_funcionario,"99999999").
&endif



{include/i-epc201.i "DSR"}

IF v_log_exc = yes THEN DO:
   if not can-find(first movto_ptoelet where
                         movto_ptoelet.cdn_empresa                 = bfunc-ponto.cdn_empresa and
                         movto_ptoelet.cdn_estab                   = bfunc-ponto.cdn_estab and
                         movto_ptoelet.cdn_funcionario             = bfunc-ponto.cdn_funcionario and
                         movto_ptoelet.num_ano_refer_fp            = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                         movto_ptoelet.num_mes_refer_fp            = bcontrole-catponto.num_mes_primei_calc_realzdo and
                         movto_ptoelet.idi_tip_fp_calcul           = 1 and
                         movto_ptoelet.idi_tip_movto_ptoelet       = 1 and
                         movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
                         movto_ptoelet.val_livre_1                 = 0) then do:

        create movto_ptoelet.
        assign movto_ptoelet.cdn_empresa           = bfunc-ponto.cdn_empresa
               movto_ptoelet.cdn_estab             = bfunc-ponto.cdn_estab
               movto_ptoelet.cdn_funcionario       = bfunc-ponto.cdn_funcionario
               movto_ptoelet.num_ano_refer_fp      = bcontrole-catponto.num_ano_primei_calc_ptoelet
               movto_ptoelet.num_mes_refer_fp      = bcontrole-catponto.num_mes_primei_calc_realzdo
               movto_ptoelet.idi_tip_fp_calcul     = 1
               movto_ptoelet.num_parc_calc_movto_ptoelet = 9
               movto_ptoelet.cdn_efp               = "" /*0*/
               movto_ptoelet.qtd_movto_ptoelet     = 0
               movto_ptoelet.idi_tip_movto_ptoelet = 1
               movto_ptoelet.log_livre_1           = no
               movto_ptoelet.val_livre_1           = 0.
               v_num_movtos = v_num_movtos + 1.
   END.
end.

/********** EPC CLIENTE GBARBOSA *****************/
for each tt-epc EXCLUSIVE-LOCK where 
         tt-epc.cod-event = "comp_semanal_gbarbosa":
    delete tt-epc.
end.

IF bfunc-ponto.dat_desligto_func = ? THEN
    ASSIGN v_dat_desligto_func = date(12,31,9999).
ELSE
    ASSIGN v_dat_desligto_func = bfunc-ponto.dat_desligto_func.

create tt-epc.
assign tt-epc.cod-event     = "comp_semanal_gbarbosa"
       tt-epc.cod-parameter = string(bfunc-ponto.cdn_empresa)                           + "|" +
                              string(bfunc-ponto.cdn_estab)                             + "|" +
                              string(bfunc-ponto.cdn_funcionario)                       + "|" +
                              string(bfunc-ponto.cdn_sindicato)                         + "|" +
                              STRING(v_dat_desligto_func)                               + "|" +
                              STRING(bcontrole-catponto.dat_inic_period_apurac_pto_mes) + "|" +
                              STRING(bcontrole-catponto.dat_term_period_apurac_pto_mes) + "|" +
                              STRING(v_log_fecha)                                       + "|" +
                              STRING(l-desligado)
       tt-epc.val-parameter = ?.
    
{include/i-epc201.i "comp_semanal_gbarbosa"}
/*************************************************/
return "ok":U.

PROCEDURE pi-movto-ptoelet:
   {prghur/pep/pe4000.i1}.
END.

PROCEDURE pi-cria-tt-mes-ref:
    /***** criando aux-semana para o mes anterior - cortar dsr/fer **************/
    for each tt-val-semana where
       tt-val-semana.cod_pais       = bfunc-ponto.cod_pais and
       tt-val-semana.cdn_localidade = bfunc-ponto.cdn_localidade and
       tt-val-semana.mes-refer      = "ant" no-lock:
       create tt-aux-semana.
       assign tt-aux-semana.mes-refer       = tt-val-semana.mes-refer
              tt-aux-semana.num-semana      = tt-val-semana.num-semana
              tt-aux-semana.dat-fim         = tt-val-semana.dat-fim
              tt-aux-semana.qtd-hrs-diu     = tt-val-semana.qtd-hrs-diu 
              tt-aux-semana.qtd-hrs-not     = tt-val-semana.qtd-hrs-not  
              tt-aux-semana.qtd-hrs-diu-dsr = tt-val-semana.qtd-hrs-diu-dsr
              tt-aux-semana.qtd-hrs-not-dsr = tt-val-semana.qtd-hrs-not-dsr  
              tt-aux-semana.qtd-hrs-diu-fer = tt-val-semana.qtd-hrs-diu-fer
              tt-aux-semana.qtd-hrs-not-fer = tt-val-semana.qtd-hrs-not-fer  
              tt-aux-semana.qtd-hrs-hpc-seg = tt-val-semana.qtd-hrs-hpc-seg   
              tt-aux-semana.qtd-hco         = tt-val-semana.qtd-hco
              tt-aux-semana.qtd-hpc         = tt-val-semana.qtd-hpc
              tt-aux-semana.qtd-trb-sem     = tt-val-semana.qtd-trb-sem
              tt-aux-semana.qtd-dsr-sem     = tt-val-semana.qtd-dsr-sem
              tt-aux-semana.qtd-fer-sem     = tt-val-semana.qtd-fer-sem.
    end.

    /* FO Dori 1343526 perda DSR mes anterior FunC Deslig*/

    IF  l-desligado THEN DO:
        for each tt-val-semana where
                 tt-val-semana.cod_pais       = bfunc-ponto.cod_pais and
                 tt-val-semana.cdn_localidade = bfunc-ponto.cdn_localidade and
                 tt-val-semana.dat-fim        < date(month(bcontrole-funcponto.dat_term_period_apurac_pto_mes),01,YEAR(bcontrole-funcponto.dat_term_period_apurac_pto_mes)) no-lock:

               IF NOT CAN-FIND(FIRST tt-aux-semana WHERE
                                     tt-aux-semana.mes-refer  = "ant" AND
                                     tt-aux-semana.num-semana = tt-val-semana.num-semana AND
                                     tt-aux-semana.dat-fim    = tt-val-semana.dat-fim) THEN DO:
                   create tt-aux-semana.
                   assign tt-aux-semana.mes-refer       = "ant"
                          tt-aux-semana.num-semana      = tt-val-semana.num-semana
                          tt-aux-semana.dat-fim         = tt-val-semana.dat-fim
                          tt-aux-semana.qtd-hrs-diu     = tt-val-semana.qtd-hrs-diu 
                          tt-aux-semana.qtd-hrs-not     = tt-val-semana.qtd-hrs-not  
                          tt-aux-semana.qtd-hrs-diu-dsr = tt-val-semana.qtd-hrs-diu-dsr
                          tt-aux-semana.qtd-hrs-not-dsr = tt-val-semana.qtd-hrs-not-dsr  
                          tt-aux-semana.qtd-hrs-diu-fer = tt-val-semana.qtd-hrs-diu-fer
                          tt-aux-semana.qtd-hrs-not-fer = tt-val-semana.qtd-hrs-not-fer  
                          tt-aux-semana.qtd-hrs-hpc-seg = tt-val-semana.qtd-hrs-hpc-seg   
                          tt-aux-semana.qtd-hco         = tt-val-semana.qtd-hco
                          tt-aux-semana.qtd-hpc         = tt-val-semana.qtd-hpc
                          tt-aux-semana.qtd-trb-sem     = tt-val-semana.qtd-trb-sem
                          tt-aux-semana.qtd-dsr-sem     = tt-val-semana.qtd-dsr-sem
                          tt-aux-semana.qtd-fer-sem     = tt-val-semana.qtd-fer-sem.
               END.
        END.
    END.

    /*Fim FO Dori 1343526 perda DSR mes anterior FunC Deslig*/

    MESSAGE "ponto 7 pe4000r1 criou tt-aux-semana"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.


    assign i-semana  = 1
           l-feriado = no.
    create tt-aux-semana.
    assign tt-aux-semana.mes-refer       = "ref"
           tt-aux-semana.num-semana      = i-semana.         
    if dt-fim-out <> ? then do :
       assign v_dat_fim_localid = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_dat_fim_emprest = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_dat_fim_lotac   = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_log_emprest     = no.
       do dt-dia-cal = dt-ini-out to dt-fim-out:

          {prghur/pep/pe9994.i dt-dia-cal}
          find tt-calendar where
               tt-calendar.cdn_turno_trab   = v_cdn_turno and
               tt-calendar.cdn_turma_trab   = v_cdn_turma and
               tt-calendar.dat_refer_calend = dt-dia-cal  and
               tt-calendar.cod_pais         = v_cod_pais  and
               tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
          IF NOT AVAIL tt-calendar THEN
              NEXT.
          if tt-calendar.idi_sit_dia_trab = 4 then do:
             assign l-feriado = yes.
             if l-desligado = yes then  /**** Cris 20/10/1999 *****/
                assign v_qti_hrs_diu_fer = v_qti_hrs_diu_fer + tt-calendar.qti_padr_hrs_diurno 
                       v_qti_hrs_not_fer = v_qti_hrs_not_fer + tt-calendar.qti_padr_hrs_notur
                       v_qti_fer_sem     = v_qti_fer_sem + 1.
          end.

          if tt-calendar.idi_sit_dia_trab = 3 then do:
             assign v_cdn_turno_ant       = v_cdn_turno
                    v_cdn_turma_ant       = v_cdn_turma
                    v_cdn_jorn_ant        = v_cdn_jorn
                    v_cdn_interv_ant      = v_cdn_interv
                    v_cod_pais_ant        = v_cod_pais
                    v_cdn_localid_ant     = v_cdn_localid
                    v_log_emprest_ant     = v_log_emprest
                    v_dat_fim_localid_ant = v_dat_fim_localid
                    v_dat_fim_emprest_ant = v_dat_fim_emprest
                    v_dat_fim_lotac_ant   = v_dat_fim_lotac
                    v_dat_prox            = dt-dia-cal + 1.

             {prghur/pep/pe9995.i v_dat_prox}
             if can-find(bdet_calend where
                         bdet_calend.cdn_turno_trab   = v_cdn_turno   and
                         bdet_calend.cdn_turma_trab   = v_cdn_turma   and
                         bdet_calend.cod_pais         = v_cod_pais    and
                         bdet_calend.cdn_localidade   = v_cdn_localid and
                         bdet_calend.dat_refer_calend = v_dat_prox    and
                         bdet_calend.idi_sit_dia_trab = 3) and
                v_dat_prox < dat-fim-mes and
                v_cdn_turno = v_cdn_turno_ant and
                v_cdn_turma = v_cdn_turma_ant then do:
                assign v_cdn_turno       = v_cdn_turno_ant
                       v_cdn_turma       = v_cdn_turma_ant
                       v_cdn_jorn        = v_cdn_jorn_ant
                       v_cdn_interv      = v_cdn_interv_ant
                       v_cod_pais        = v_cod_pais_ant
                       v_cdn_localid     = v_cdn_localid_ant
                       v_log_emprest     = v_log_emprest_ant
                       v_dat_fim_localid = v_dat_fim_localid_ant
                       v_dat_fim_emprest = v_dat_fim_emprest_ant
                       v_dat_fim_lotac   = v_dat_fim_lotac_ant.
                next.
             end.
             else do:
                assign v_cdn_turno       = v_cdn_turno_ant
                       v_cdn_turma       = v_cdn_turma_ant
                       v_cdn_jorn        = v_cdn_jorn_ant
                       v_cdn_interv      = v_cdn_interv_ant
                       v_cod_pais        = v_cod_pais_ant
                       v_cdn_localid     = v_cdn_localid_ant
                       v_log_emprest     = v_log_emprest_ant
                       v_dat_fim_localid = v_dat_fim_localid_ant
                       v_dat_fim_emprest = v_dat_fim_emprest_ant
                       v_dat_fim_lotac   = v_dat_fim_lotac_ant.
                find tt-calendar where
                     tt-calendar.cdn_turno_trab   = v_cdn_turno and
                     tt-calendar.cdn_turma_trab   = v_cdn_turma and
                     tt-calendar.dat_refer_calend = dt-dia-cal  and
                     tt-calendar.cod_pais         = v_cod_pais  and
                     tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
             end.

             assign tt-aux-semana.dat-fim         = tt-calendar.dat_refer_calend
                    tt-aux-semana.qtd-hrs-diu-dsr = tt-calendar.qti_hrs_dsr_diurno_sema 
                    tt-aux-semana.qtd-hrs-not-dsr = tt-calendar.qti_hrs_dsr_notur_sema 
                    tt-aux-semana.qtd-dsr-sem     = tt-calendar.qti_dsr_sema.
             if l-feriado = yes then
                assign tt-aux-semana.qtd-hrs-diu-fer = tt-calendar.qti_hrs_fer_diurno_sema
                       tt-aux-semana.qtd-hrs-not-fer = tt-calendar.qti_hrs_fer_notur_sema
                       tt-aux-semana.qtd-fer-sem     = tt-calendar.qti_fer_sema
                       v_qti_hrs_diu_fer             = 0 
                       v_qti_hrs_not_fer             = 0 
                       v_qti_fer_sem                 = 0. 
             if tt-calendar.dat_refer_calend < dat-fim-mes then do:              
                assign i-semana  = i-semana + 1
                       l-feriado = no.

                MESSAGE "ponto 9 pe4000r1 criou tt-aux-semana"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                create tt-aux-semana.
                assign tt-aux-semana.mes-refer       = "ref"
                       tt-aux-semana.num-semana      = i-semana.         
             end.
          end.      

          if tt-calendar.idi_sit_dia_trab = 1 then do:
             if can-find(first efp_par_marcac_ptoelet of bfunc-ponto where
                               efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                               efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3) then do:
                for each efp_par_marcac_ptoelet of bfunc-ponto no-lock where
                         efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                         (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3 or
                         efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 4):
                   if efp_par_marcac_ptoelet.log_hrs_diurno = yes then
                      assign tt-aux-semana.qtd-hrs-diu = tt-aux-semana.qtd-hrs-diu + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                   else
                      assign tt-aux-semana.qtd-hrs-not = tt-aux-semana.qtd-hrs-not + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                end.
             end.
             else do:
                assign tt-aux-semana.qtd-hrs-diu = tt-aux-semana.qtd-hrs-diu + tt-calendar.qti_padr_hrs_diurno 
                       tt-aux-semana.qtd-hrs-not = tt-aux-semana.qtd-hrs-not + tt-calendar.qti_padr_hrs_notur.
             end.
          end.

          if dt-dia-cal = dt-fim-out and   /**** Cris 20/10/1999 *****/
             l-desligado = yes and
             l-feriado   = yes then do:
             assign tt-aux-semana.qtd-hrs-diu-fer = v_qti_hrs_diu_fer
                    tt-aux-semana.qtd-hrs-not-fer = v_qti_hrs_not_fer
                    tt-aux-semana.qtd-fer-sem     = v_qti_fer_sem
                    v_qti_hrs_diu_fer             = 0
                    v_qti_hrs_not_fer             = 0
                    v_qti_fer_sem                 = 0
                    l-feriado                     = no.
          end.
          else do:
             if dt-dia-cal = dt-fim-out and
                tt-calendar.idi_sit_dia_trab <> 3 and
                l-feriado = yes then
                assign tt-aux-semana.qtd-hrs-diu-fer = tt-calendar.qti_hrs_fer_diurno_sema
                       tt-aux-semana.qtd-hrs-not-fer = tt-calendar.qti_hrs_fer_notur_sema
                       tt-aux-semana.qtd-fer-sem     = tt-calendar.qti_fer_sema. 
          end.
       end. /* do */
    end. /* if dt-fim-out <> ? */
    assign tt-aux-semana.dat-fim = dt-fim-out.
END PROCEDURE.

PROCEDURE pi-cria-tt-per-func:
    
    /***** criando aux-semana para o mes anterior - cortar dsr/fer **************/
    for each tt-val-semana where
       tt-val-semana.cod_pais       = bfunc-ponto.cod_pais and
       tt-val-semana.cdn_localidade = bfunc-ponto.cdn_localidade and
       tt-val-semana.mes-refer      = "ant" no-lock:
       create tt-aux-semana.
       assign tt-aux-semana.mes-refer       = tt-val-semana.mes-refer
              tt-aux-semana.num-semana      = tt-val-semana.num-semana
              tt-aux-semana.dat-fim         = tt-val-semana.dat-fim
              tt-aux-semana.qtd-hrs-diu     = tt-val-semana.qtd-hrs-diu 
              tt-aux-semana.qtd-hrs-not     = tt-val-semana.qtd-hrs-not  
              tt-aux-semana.qtd-hrs-diu-dsr = tt-val-semana.qtd-hrs-diu-dsr
              tt-aux-semana.qtd-hrs-not-dsr = tt-val-semana.qtd-hrs-not-dsr  
              tt-aux-semana.qtd-hrs-diu-fer = tt-val-semana.qtd-hrs-diu-fer
              tt-aux-semana.qtd-hrs-not-fer = tt-val-semana.qtd-hrs-not-fer  
              tt-aux-semana.qtd-hrs-hpc-seg = tt-val-semana.qtd-hrs-hpc-seg   
              tt-aux-semana.qtd-hco         = tt-val-semana.qtd-hco
              tt-aux-semana.qtd-hpc         = tt-val-semana.qtd-hpc
              tt-aux-semana.qtd-trb-sem     = tt-val-semana.qtd-trb-sem
              tt-aux-semana.qtd-dsr-sem     = tt-val-semana.qtd-dsr-sem
              tt-aux-semana.qtd-fer-sem     = tt-val-semana.qtd-fer-sem.
    end.

    MESSAGE "ponto 1 pe4000r1 criou tt-aux-semana"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    assign i-semana  = 1
           l-feriado = no.
    create tt-aux-semana.
    assign tt-aux-semana.mes-refer       = "ref"
           tt-aux-semana.num-semana      = i-semana.         
    if dt-fim-out <> ? then do :

       assign v_dat_fim_localid = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_dat_fim_emprest = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_dat_fim_lotac   = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_log_emprest     = no.
       do dt-dia-cal = dt-ini-out to dt-fim-out:

          {prghur/pep/pe9994.i dt-dia-cal}

          find tt-calendar where
               tt-calendar.cdn_turno_trab   = v_cdn_turno and
               tt-calendar.cdn_turma_trab   = v_cdn_turma and
               tt-calendar.dat_refer_calend = dt-dia-cal  and
               tt-calendar.cod_pais         = v_cod_pais  and
               tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
          IF NOT AVAIL tt-calendar THEN
              NEXT.
          if tt-calendar.idi_sit_dia_trab = 4 then
             assign l-feriado = yes.

          if tt-calendar.idi_sit_dia_trab = 3 or dt-dia-cal = dt-fim-out then do:

             assign v_cdn_turno_ant       = v_cdn_turno
                    v_cdn_turma_ant       = v_cdn_turma
                    v_cdn_jorn_ant        = v_cdn_jorn
                    v_cdn_interv_ant      = v_cdn_interv
                    v_cod_pais_ant        = v_cod_pais
                    v_cdn_localid_ant     = v_cdn_localid
                    v_log_emprest_ant     = v_log_emprest
                    v_dat_fim_localid_ant = v_dat_fim_localid
                    v_dat_fim_emprest_ant = v_dat_fim_emprest
                    v_dat_fim_lotac_ant   = v_dat_fim_lotac
                    v_dat_prox            = dt-dia-cal + 1.

             {prghur/pep/pe9995.i v_dat_prox}
             if can-find(bdet_calend where
                         bdet_calend.cdn_turno_trab   = v_cdn_turno   and
                         bdet_calend.cdn_turma_trab   = v_cdn_turma   and
                         bdet_calend.cod_pais         = v_cod_pais    and
                         bdet_calend.cdn_localidade   = v_cdn_localid and
                         bdet_calend.dat_refer_calend = v_dat_prox    and
                         bdet_calend.idi_sit_dia_trab = 3) and
                v_dat_prox < dat-fim-mes and
                v_cdn_turno = v_cdn_turno_ant and
                v_cdn_turma = v_cdn_turma_ant then do:
                assign v_cdn_turno       = v_cdn_turno_ant
                       v_cdn_turma       = v_cdn_turma_ant
                       v_cdn_jorn        = v_cdn_jorn_ant
                       v_cdn_interv      = v_cdn_interv_ant
                       v_cod_pais        = v_cod_pais_ant
                       v_cdn_localid     = v_cdn_localid_ant
                       v_log_emprest     = v_log_emprest_ant
                       v_dat_fim_localid = v_dat_fim_localid_ant
                       v_dat_fim_emprest = v_dat_fim_emprest_ant
                       v_dat_fim_lotac   = v_dat_fim_lotac_ant.
                next.
             end.
             else do:
                assign v_cdn_turno       = v_cdn_turno_ant
                       v_cdn_turma       = v_cdn_turma_ant
                       v_cdn_jorn        = v_cdn_jorn_ant
                       v_cdn_interv      = v_cdn_interv_ant
                       v_cod_pais        = v_cod_pais_ant
                       v_cdn_localid     = v_cdn_localid_ant
                       v_log_emprest     = v_log_emprest_ant
                       v_dat_fim_localid = v_dat_fim_localid_ant
                       v_dat_fim_emprest = v_dat_fim_emprest_ant
                       v_dat_fim_lotac   = v_dat_fim_lotac_ant.
                find tt-calendar where
                     tt-calendar.cdn_turno_trab   = v_cdn_turno and
                     tt-calendar.cdn_turma_trab   = v_cdn_turma and
                     tt-calendar.dat_refer_calend = dt-dia-cal  and
                     tt-calendar.cod_pais         = v_cod_pais  and
                     tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
             end.

             assign tt-aux-semana.dat-fim         = tt-calendar.dat_refer_calend
                    tt-aux-semana.qtd-dsr-sem     = tt-calendar.qti_dsr_sema.

             if tt-calendar.dat_refer_calend <= bcontrole-funcponto.dat_term_period_apurac_pto_mes then
                assign tt-aux-semana.qtd-hrs-diu-dsr = tt-calendar.qti_hrs_dsr_diurno_sema 
                       tt-aux-semana.qtd-hrs-not-dsr = tt-calendar.qti_hrs_dsr_notur_sema.
             else
                assign tt-aux-semana.qtd-hrs-diu-dsr = tt-calendar.qti_hrs_dsr_diurno_sema +
                                                       tt-calendar.qti_hrs_dsr_notur_sema.
             if l-feriado = yes then do:
                assign tt-aux-semana.qtd-fer-sem     = tt-calendar.qti_fer_sema.
                if tt-calendar.dat_refer_calend <= bcontrole-funcponto.dat_term_period_apurac_pto_mes then
                   assign tt-aux-semana.qtd-hrs-diu-fer = tt-calendar.qti_hrs_fer_diurno_sema
                          tt-aux-semana.qtd-hrs-not-fer = tt-calendar.qti_hrs_fer_notur_sema.
                else
                   assign tt-aux-semana.qtd-hrs-diu-fer = tt-calendar.qti_hrs_fer_diurno_sema +
                                                          tt-calendar.qti_hrs_fer_notur_sema.
             end.

             if tt-calendar.dat_refer_calend < dat-fim-mes then do:              
                assign i-semana  = i-semana + 1
                       l-feriado = no.

                MESSAGE "ponto 2 pe4000r1 criou tt-aux-semana"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.


                create tt-aux-semana.
                assign tt-aux-semana.mes-refer       = "ref"
                       tt-aux-semana.num-semana      = i-semana.         
             end.
          end.      

          if tt-calendar.idi_sit_dia_trab = 1 then do:
             if can-find(first efp_par_marcac_ptoelet of bfunc-ponto where
                               efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                               efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3) then do:
                for each efp_par_marcac_ptoelet of bfunc-ponto no-lock where
                         efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                         (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3 or
                         efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 4):
                   if tt-calendar.dat_refer_calend <= bcontrole-funcponto.dat_term_period_apurac_pto_mes then do:
                      if efp_par_marcac_ptoelet.log_hrs_diurno = yes then
                         assign tt-aux-semana.qtd-hrs-diu = tt-aux-semana.qtd-hrs-diu + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                      else
                         assign tt-aux-semana.qtd-hrs-not = tt-aux-semana.qtd-hrs-not + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                   end.
                   else
                      assign tt-aux-semana.qtd-hrs-diu = tt-aux-semana.qtd-hrs-diu + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                end.
             end.
             else do:
                if tt-calendar.dat_refer_calend <= bcontrole-funcponto.dat_term_period_apurac_pto_mes then
                   assign tt-aux-semana.qtd-hrs-diu = tt-aux-semana.qtd-hrs-diu + 
                                                      tt-calendar.qti_padr_hrs_diurno 
                          tt-aux-semana.qtd-hrs-not = tt-aux-semana.qtd-hrs-not + 
                                                      tt-calendar.qti_padr_hrs_notur.
                else
                   assign tt-aux-semana.qtd-hrs-diu = tt-aux-semana.qtd-hrs-diu + tt-calendar.qti_padr_hrs_diurno +
                                                      tt-calendar.qti_padr_hrs_notur.
             end.
          end. /* trab */
       end. /* do */
    end. /* if dt-fim-out <> ? */
    assign tt-aux-semana.dat-fim = dt-fim-out.

END PROCEDURE.

PROCEDURE pi-cria-tt-mes-per:
    assign i-semana          = 1
           v_qti_hrs_diu     = 0
           v_qti_hrs_not     = 0
           v_qti_hrs_diu_dsr = 0
           v_qti_hrs_not_dsr = 0
           v_qti_hrs_fer_diu = 0
           v_qti_hrs_fer_not = 0
           v_qti_dias_trab   = 0
           v_qti_dias_dsr    = 0
           v_qti_dias_fer    = 0.

    create tt-aux-semana.
    assign tt-aux-semana.mes-refer       = "ref"
           tt-aux-semana.num-semana      = i-semana.         

    assign v_dat_fim_localid = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
           v_dat_fim_emprest = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
           v_dat_fim_lotac   = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
           v_log_emprest     = no
           v_log_pri_sit     = no
           dt-dia-cal-aux    = bcontrole-funcponto.dat_inic_period_apurac_pto_mes.

    for first funcionario no-lock where
        funcionario.cdn_empresa           = bfunc-ponto.cdn_empresa and
        funcionario.cdn_estab             = bfunc-ponto.cdn_estab and
        funcionario.cdn_funcionario       = bfunc-ponto.cdn_funcionario and
        funcionario.dat_admis_transf_func > bcontrole-funcponto.dat_inic_period_apurac_pto_mes,
        last bffuncionario no-lock where 
        bffuncionario.num_pessoa_fisic  = funcionario.num_pessoa_fisic and
        bffuncionario.dat_desligto_func > bcontrole-funcponto.dat_inic_period_apurac_pto_mes,
        last func_turno_trab no-lock of funcionario:

        assign dt-dia-cal-aux = if func_turno_trab.dat_inic_lotac_func_turno_trab > bcontrole-funcponto.dat_inic_period_apurac_pto_mes then  
                                    bcontrole-funcponto.dat_inic_period_apurac_pto_mes 
                                else func_turno_trab.dat_inic_lotac_func_turno_trab.
    end.


    ASSIGN d-qtd-hrs-not     = 0 .

    for each bf-tt-aux-semana NO-LOCK where
        bf-tt-aux-semana.mes-refer = "ref":
        MESSAGE "bf-tt-aux-semana.qtd-hrs-not " bf-tt-aux-semana.qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN d-qtd-hrs-not     = d-qtd-hrs-not     + bf-tt-aux-semana.qtd-hrs-not.
    END.
    MESSAGE "1 d-qtd-hrs-not " d-qtd-hrs-not
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN d-qtd-hrs-not = 0.    


    do dt-dia-cal = dt-dia-cal-aux to bcontrole-funcponto.dat_term_period_apurac_pto_mes:
       {prghur/pep/pe9994.i dt-dia-cal}

       IF v_cdn_turno = 0 OR v_cdn_turma = 0 THEN DO:
          FIND LAST func_turno_trab OF bfunc-ponto NO-LOCK NO-ERROR.
          IF AVAIL func_turno_trab THEN
             ASSIGN v_cdn_turno = func_turno_trab.cdn_turno_trab
                    v_cdn_turma = func_turno_trab.cdn_turma_trab.
          FIND LAST func_localid OF bfunc-ponto USE-INDEX fnclcld_fncnr NO-LOCK NO-ERROR.
          IF AVAIL func_localid THEN
             ASSIGN v_cod_pais    = func_localid.cod_pais
                    v_cdn_localid = func_localid.cdn_localidade.
       END.

       ASSIGN v_log_afast_func = NO.
       find tt-calendar where
            tt-calendar.cdn_turno_trab   = v_cdn_turno and
            tt-calendar.cdn_turma_trab   = v_cdn_turma and
            tt-calendar.dat_refer_calend = dt-dia-cal  and
            tt-calendar.cod_pais         = v_cod_pais  and
            tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
       IF NOT AVAIL tt-calendar THEN DO:
          FIND FIRST tt-calendar WHERE 
               tt-calendar.dat_refer_calend = dt-dia-cal 
               NO-LOCK NO-ERROR.
          IF AVAIL tt-calendar THEN DO:
             ASSIGN tt-calendar.cdn_turno_trab   = v_cdn_turno 
                    tt-calendar.cdn_turma_trab   = v_cdn_turma 
                    tt-calendar.dat_refer_calend = dt-dia-cal  
                    tt-calendar.cod_pais         = v_cod_pais  
                    tt-calendar.cdn_localidade   = v_cdn_localid. 

          END.
       END.
       IF NOT AVAIL tt-calendar THEN DO: 
          FIND FIRST tt-calendar where
               tt-calendar.dat_refer_calend = dt-dia-cal no-lock no-error.
       END.
       if avail tt-calendar then do:
          assign v_log_atualiz_tt = no
                 v_log_afast_func = no.
          if dt-dia-cal < dat-ini-mes then do:
             find first sit_afast_func of bfunc-ponto no-lock where
                        sit_afast_func.dat_inic_sit_afast <= dt-dia-cal and
                        sit_afast_func.dat_term_sit_afast >= dt-dia-cal no-error.
             if avail sit_afast_func then do:
                find first sit_afast no-lock where
                           sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func and
                          (sit_afast.idi_signif_sit     = 2  or
                           sit_afast.idi_signif_sit     = 10 or /** Contrato desativado - Jeziel **/
                           sit_afast.idi_signif_sit     = 5) no-error.
                if avail sit_afast and
                   ((sit_afast_func.dat_inic_sit_afast < dt-dia-cal-aux or
                   sit_afast_func.dat_integr_sit_afast_func <> ?) or 
                   sit_afast.idi_signif_sit = 5) THEN DO:
                   assign v_log_afast_func = yes.
                END.
             end.
          end.
          if tt-calendar.idi_sit_dia_trab = 1 then do:
             if can-find(first efp_par_marcac_ptoelet of bfunc-ponto where
                               efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                               efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3) then do:
                assign v_qti_dias_trab = v_qti_dias_trab + 1.
                for each efp_par_marcac_ptoelet of bfunc-ponto no-lock where
                         efp_par_marcac_ptoelet.dat_proces_mpe = dt-dia-cal and
                         (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 3 or
                         efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 4):
                   if v_log_afast_func = NO then do:


                       MESSAGE 
                           "antes" SKIP
                           "v_qti_hrs_not " v_qti_hrs_not SKIP
                           "efp_par_marcac_ptoelet.log_hrs_diurno = " efp_par_marcac_ptoelet.log_hrs_diurno
                           VIEW-AS ALERT-BOX INFO BUTTONS OK.

                      if efp_par_marcac_ptoelet.log_hrs_diurno = yes then
                         assign v_qti_hrs_diu = v_qti_hrs_diu + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                      else
                         assign v_qti_hrs_not = v_qti_hrs_not + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                   end.
                   else
                      assign v_qti_hrs_diu = v_qti_hrs_diu + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                end.
             end.
             else do:
                if v_log_afast_func = yes then do:
                   assign v_qti_hrs_diu   = v_qti_hrs_diu + tt-calendar.qti_padr_hrs_diurno + tt-calendar.qti_padr_hrs_notur + (tt-calendar.qti_padr_hrs_notur * 0.1428571)
                          v_qti_dias_trab = v_qti_dias_trab + 1.

                   if v_log_param_modul_agric and  /*** Evandro - Folha agricola, sindicato, hora suplementar, FO 1422.166 **/
                      v_log_sind_suplem = no then
                      assign v_qti_hrs_diu   = v_qti_hrs_diu + tt-calendar.qti_padr_hrs_diurno + tt-calendar.qti_padr_hrs_notur.

                   if v_log_pri_sit = no then /*** Evandro - Sobra de horas diurnas quando fÇrias em 2 meses***/
                      assign v_qti_hra_dia_real = v_qti_hrs_diu
                             v_log_pri_sit = yes.
                end.
                else do:
                   assign v_qti_hrs_diu   = v_qti_hrs_diu + tt-calendar.qti_padr_hrs_diurno 
                          v_qti_hrs_not   = v_qti_hrs_not + tt-calendar.qti_padr_hrs_notur
                          v_qti_dias_trab = v_qti_dias_trab + 1.
                                          /*** Evandro - Sobra de horas diurnas quando fÇrias em 2 meses apenas
                                               para horista pagando pela real ***/
                   if v_log_pri_sit and   
                      dt-dia-cal >= dat-ini-mes and
                      bcatponto.idi_pagto_horist = 1 and
                      bcatponto.cdn_categ_sal = 2 then do: 
                      find first sit_afast_func of bfunc-ponto no-lock where
                                 sit_afast_func.dat_inic_sit_afast <= dt-dia-cal and
                                 sit_afast_func.dat_term_sit_afast >= dt-dia-cal no-error.
                      if avail sit_afast_func AND
                               sit_afast_func.dat_term_sit_afast > dat-fim-mes then do:
                         find first sit_afast no-lock where
                                    sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func and
                                   (sit_afast.idi_signif_sit     = 2  or
                                    sit_afast.idi_signif_sit     = 10 or /** Contrato desativado - Jeziel **/
                                    sit_afast.idi_signif_sit     = 5) no-error.
                         if avail sit_afast and
                                ((sit_afast_func.dat_inic_sit_afast < dt-dia-cal-aux or
                                  sit_afast_func.dat_integr_sit_afast_func <> ?) or 
                                  sit_afast.idi_signif_sit = 5) AND
                                  sit_afast.idi_hrs_desc_dia = 2 THEN DO:
                            if dt-dia-cal <= sit_afast_func.dat_term_sit_afast then do:
                               find first turno_trab no-lock where
                                          turno_trab.cdn_turno_trab = v_cdn_turno no-error.
                               if avail turno_trab then
                                  assign v_qti_hra_dia_padr = turno_trab.qtd_hrs_padr_dia_rh * 3600
                                         v_qti_dif_real_sit = v_qti_hra_dia_real - v_qti_hra_dia_padr
                                         v_qti_hrs_diu      = v_qti_hrs_diu - v_qti_dif_real_sit.
                            end.
                         end.
                      end.
                   end.                   /************************/

                end.
             end.
          end.
          if tt-calendar.idi_sit_dia_trab = 4 then do:
             if v_log_afast_func = yes then
                assign v_qti_hrs_fer_diu = v_qti_hrs_fer_diu + tt-calendar.qti_padr_hrs_diurno + tt-calendar.qti_padr_hrs_notur
                       v_qti_dias_fer    = v_qti_dias_fer + 1.
             else
                assign v_qti_hrs_fer_diu = v_qti_hrs_fer_diu + tt-calendar.qti_padr_hrs_diurno 
                       v_qti_hrs_fer_not = v_qti_hrs_fer_not + tt-calendar.qti_padr_hrs_notur
                       v_qti_dias_fer    = v_qti_dias_fer + 1.
          end.
          if tt-calendar.idi_sit_dia_trab = 3 then do:
             if v_log_afast_func = yes then
                assign v_qti_hrs_diu_dsr = v_qti_hrs_diu_dsr + tt-calendar.qti_padr_hrs_diurno + tt-calendar.qti_padr_hrs_notur
                       v_qti_dias_dsr    = v_qti_dias_dsr + 1.
             else
                assign v_qti_hrs_diu_dsr = v_qti_hrs_diu_dsr + tt-calendar.qti_padr_hrs_diurno 
                       v_qti_hrs_not_dsr = v_qti_hrs_not_dsr + tt-calendar.qti_padr_hrs_notur
                       v_qti_dias_dsr    = v_qti_dias_dsr + 1.

             assign v_dat_aux_lotac = (dt-dia-cal + 1)
                    v_log_emprest_1 = no.

             {prghur/pep/pe9995.i v_dat_aux_lotac} 

             find btt-calendar no-lock where
                  btt-calendar.cdn_turno_trab   = v_cdn_turno   and
                  btt-calendar.cdn_turma_trab   = v_cdn_turma   and
                  btt-calendar.cod_pais         = v_cod_pais    and
                  btt-calendar.cdn_localidade   = v_cdn_localid and
                  btt-calendar.dat_refer_calend = v_dat_aux_lotac no-error.
             if avail btt-calendar THEN DO: 
                IF btt-calendar.idi_sit_dia_trab = 1 or 
                   btt-calendar.idi_sit_dia_trab = 4 OR
                   btt-calendar.idi_sit_dia_trab = 5 then do:
                   assign i-semana         = i-semana + 1
                          v_log_atualiz_tt = yes.
                   assign tt-aux-semana.dat-fim         = dt-dia-cal
                          tt-aux-semana.qtd-hrs-diu     = v_qti_hrs_diu 
                          tt-aux-semana.qtd-hrs-not     = v_qti_hrs_not   
                          tt-aux-semana.qtd-hrs-diu-dsr = v_qti_hrs_diu_dsr 
                          tt-aux-semana.qtd-hrs-diu-fer = v_qti_hrs_fer_diu
                          tt-aux-semana.qtd-hrs-not-dsr = v_qti_hrs_not_dsr 
                          tt-aux-semana.qtd-hrs-not-fer = v_qti_hrs_fer_not
                          tt-aux-semana.qtd-trb-sem     = v_qti_dias_trab
                          tt-aux-semana.qtd-dsr-sem     = v_qti_dias_dsr
                          tt-aux-semana.qtd-fer-sem     = v_qti_dias_fer.

                   MESSAGE "gravou v_qti_hrs_not  = " v_qti_hrs_not 
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN d-qtd-hrs-not     = 0 .

    for each bf-tt-aux-semana NO-LOCK where
        bf-tt-aux-semana.mes-refer = "ref":
        MESSAGE "bf-tt-aux-semana.qtd-hrs-not " bf-tt-aux-semana.qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN d-qtd-hrs-not     = d-qtd-hrs-not     + bf-tt-aux-semana.qtd-hrs-not.
    END.
    MESSAGE "2 d-qtd-hrs-not " d-qtd-hrs-not
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN d-qtd-hrs-not = 0.  

                   assign v_qti_hrs_diu     = 0
                          v_qti_hrs_not     = 0
                          v_qti_hrs_diu_dsr = 0
                          v_qti_hrs_not_dsr = 0
                          v_qti_hrs_fer_diu = 0
                          v_qti_hrs_fer_not = 0
                          v_qti_dias_trab   = 0
                          v_qti_dias_dsr    = 0
                          v_qti_dias_fer    = 0.
                   if dt-dia-cal < bcontrole-funcponto.dat_term_period_apurac_pto_mes then do:
                      create tt-aux-semana.
                      assign tt-aux-semana.mes-refer       = "ref"
                             tt-aux-semana.num-semana      = i-semana.         
                   end.
                END.
             END.
             ELSE DO:
                find det_calend_func OF bfunc-ponto no-lock where
                     det_calend_func.dat_refer_calend = v_dat_aux_lotac no-error.
                IF AVAIL det_calend_func THEN DO:
                   IF det_calend_func.idi_sit_dia_trab = 1 or 
                      det_calend_func.idi_sit_dia_trab = 4 OR
                      det_calend_func.idi_sit_dia_trab = 5 then do:
                      assign i-semana         = i-semana + 1
                             v_log_atualiz_tt = yes.
                      assign tt-aux-semana.dat-fim         = dt-dia-cal
                             tt-aux-semana.qtd-hrs-diu     = v_qti_hrs_diu 
                             tt-aux-semana.qtd-hrs-not     = v_qti_hrs_not   
                             tt-aux-semana.qtd-hrs-diu-dsr = v_qti_hrs_diu_dsr 
                             tt-aux-semana.qtd-hrs-diu-fer = v_qti_hrs_fer_diu
                             tt-aux-semana.qtd-hrs-not-dsr = v_qti_hrs_not_dsr 
                             tt-aux-semana.qtd-hrs-not-fer = v_qti_hrs_fer_not
                             tt-aux-semana.qtd-trb-sem     = v_qti_dias_trab
                             tt-aux-semana.qtd-dsr-sem     = v_qti_dias_dsr
                             tt-aux-semana.qtd-fer-sem     = v_qti_dias_fer.
    ASSIGN d-qtd-hrs-not     = 0 .

    for each bf-tt-aux-semana NO-LOCK where
        bf-tt-aux-semana.mes-refer = "ref":
        MESSAGE "bf-tt-aux-semana.qtd-hrs-not " bf-tt-aux-semana.qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN d-qtd-hrs-not     = d-qtd-hrs-not     + bf-tt-aux-semana.qtd-hrs-not.
    END.
    MESSAGE "3 d-qtd-hrs-not " d-qtd-hrs-not
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN d-qtd-hrs-not = 0.  
                  
                      assign v_qti_hrs_diu     = 0
                             v_qti_hrs_not     = 0
                             v_qti_hrs_diu_dsr = 0
                             v_qti_hrs_not_dsr = 0
                             v_qti_hrs_fer_diu = 0
                             v_qti_hrs_fer_not = 0
                             v_qti_dias_trab   = 0
                             v_qti_dias_dsr    = 0
                             v_qti_dias_fer    = 0.
                      if dt-dia-cal < bcontrole-funcponto.dat_term_period_apurac_pto_mes then do:
                         create tt-aux-semana.
                         assign tt-aux-semana.mes-refer       = "ref"
                                tt-aux-semana.num-semana      = i-semana.         
                      end.
                   END.
                END.
                ELSE DO:
                   find bdet_calend_localid no-lock where
                        bdet_calend_localid.cdn_turno_trab   = v_cdn_turno   and
                        bdet_calend_localid.cdn_turma_trab   = v_cdn_turma   and
                        bdet_calend_localid.cod_pais         = v_cod_pais    and
                        bdet_calend_localid.cdn_localidade   = v_cdn_localid and
                        bdet_calend_localid.dat_refer_calend = (dt-dia-cal + 1) no-error.
                   if avail bdet_calend_localid THEN DO: 
                      IF bdet_calend_localid.idi_sit_dia_trab = 1 or 
                         bdet_calend_localid.idi_sit_dia_trab = 4 OR
                         bdet_calend_localid.idi_sit_dia_trab = 5 then do:
                         assign i-semana         = i-semana + 1
                                v_log_atualiz_tt = yes.
                         assign tt-aux-semana.dat-fim         = dt-dia-cal
                                tt-aux-semana.qtd-hrs-diu     = v_qti_hrs_diu 
                                tt-aux-semana.qtd-hrs-not     = v_qti_hrs_not   
                                tt-aux-semana.qtd-hrs-diu-dsr = v_qti_hrs_diu_dsr 
                                tt-aux-semana.qtd-hrs-diu-fer = v_qti_hrs_fer_diu
                                tt-aux-semana.qtd-hrs-not-dsr = v_qti_hrs_not_dsr 
                                tt-aux-semana.qtd-hrs-not-fer = v_qti_hrs_fer_not
                                tt-aux-semana.qtd-trb-sem     = v_qti_dias_trab
                                tt-aux-semana.qtd-dsr-sem     = v_qti_dias_dsr
                                tt-aux-semana.qtd-fer-sem     = v_qti_dias_fer.

    ASSIGN d-qtd-hrs-not     = 0 .

    for each bf-tt-aux-semana NO-LOCK where
        bf-tt-aux-semana.mes-refer = "ref":
        MESSAGE "bf-tt-aux-semana.qtd-hrs-not " bf-tt-aux-semana.qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN d-qtd-hrs-not     = d-qtd-hrs-not     + bf-tt-aux-semana.qtd-hrs-not.
    END.
    MESSAGE "4 d-qtd-hrs-not " d-qtd-hrs-not
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN d-qtd-hrs-not = 0.  
                  
                         assign v_qti_hrs_diu     = 0
                                v_qti_hrs_not     = 0
                                v_qti_hrs_diu_dsr = 0
                                v_qti_hrs_not_dsr = 0
                                v_qti_hrs_fer_diu = 0
                                v_qti_hrs_fer_not = 0
                                v_qti_dias_trab   = 0
                                v_qti_dias_dsr    = 0
                                v_qti_dias_fer    = 0.
                         if dt-dia-cal < bcontrole-funcponto.dat_term_period_apurac_pto_mes then do:
                            create tt-aux-semana.
                            assign tt-aux-semana.mes-refer       = "ref"
                                   tt-aux-semana.num-semana      = i-semana.         
                         end.
                      END.
                   END.
                   ELSE DO:
                      assign v_num_erro = v_num_erro + 1.
                      {utp/ut-liter.i Calendario_Funcionario_Màs_Seguinte_Inexistente mpe L}
                      create tt_erro.
                      assign tt_erro.num_erro = v_num_erro
                             tt_erro.des_erro = trim(return-value).
                             tt_erro.log_erro = YES.
                      NEXT.
                   END.
                END.
             END.
          end. /* repouso */
          if dt-dia-cal = bcontrole-funcponto.dat_term_period_apurac_pto_mes then do:
             if not v_log_atualiz_tt then do:
                assign tt-aux-semana.dat-fim         = tt-calendar.dat_refer_calend
                       tt-aux-semana.qtd-hrs-diu     = v_qti_hrs_diu 
                       tt-aux-semana.qtd-hrs-not     = v_qti_hrs_not   
                       tt-aux-semana.qtd-hrs-diu-dsr = v_qti_hrs_diu_dsr 
                       tt-aux-semana.qtd-hrs-diu-fer = v_qti_hrs_fer_diu
                       tt-aux-semana.qtd-hrs-not-dsr = v_qti_hrs_not_dsr 
                       tt-aux-semana.qtd-hrs-not-fer = v_qti_hrs_fer_not
                       tt-aux-semana.qtd-trb-sem     = v_qti_dias_trab
                       tt-aux-semana.qtd-dsr-sem     = v_qti_dias_dsr
                       tt-aux-semana.qtd-fer-sem     = v_qti_dias_fer.

    ASSIGN d-qtd-hrs-not     = 0 .

    for each bf-tt-aux-semana NO-LOCK where
        bf-tt-aux-semana.mes-refer = "ref":
        MESSAGE "bf-tt-aux-semana.qtd-hrs-not " bf-tt-aux-semana.qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN d-qtd-hrs-not     = d-qtd-hrs-not     + bf-tt-aux-semana.qtd-hrs-not.
    END.
    MESSAGE "5 d-qtd-hrs-not " d-qtd-hrs-not
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN d-qtd-hrs-not = 0.  

                assign v_qti_hrs_diu     = 0
                       v_qti_hrs_not     = 0
                       v_qti_hrs_diu_dsr = 0
                       v_qti_hrs_not_dsr = 0
                       v_qti_hrs_fer_diu = 0
                       v_qti_hrs_fer_not = 0
                       v_qti_dias_trab   = 0
                       v_qti_dias_dsr    = 0
                       v_qti_dias_fer    = 0.
             end.
          end. /* ultimo dia semana */
       end. /* calend */
       else do:
           if v_log_calend_func then do:
              if v_cdn_estab_mens  <> bfunc-ponto.cdn_estab       or
                 v_cdn_func_mens   <> bfunc-ponto.cdn_funcionario or 
                 v_mes_men         <> month(dt-dia-cal)           or
                 v_ano_men         <> year(dt-dia-cal)  then do:

                 assign v_cdn_estab_mens  = bfunc-ponto.cdn_estab       
                        v_cdn_func_mens   = bfunc-ponto.cdn_funcionario 
                        v_mes_men         = month(dt-dia-cal)           
                        v_ano_men         = year(dt-dia-cal).

                 disp v_cdn_estab_mens
                      v_cdn_func_mens
                      v_mes_men
                      v_ano_men
                      with frame f-erro-calen-func.
                 down with frame f-erro-calen-func.
              end.
           end.
       end.
    end. /* do */

    ASSIGN d-qtd-hrs-not     = 0 .

    for each bf-tt-aux-semana NO-LOCK where
        bf-tt-aux-semana.mes-refer = "ref":
        MESSAGE "bf-tt-aux-semana.qtd-hrs-not " bf-tt-aux-semana.qtd-hrs-not
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN d-qtd-hrs-not     = d-qtd-hrs-not     + bf-tt-aux-semana.qtd-hrs-not.
    END.
    MESSAGE "6 d-qtd-hrs-not " d-qtd-hrs-not
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN d-qtd-hrs-not = 0.  
END PROCEDURE.

PROCEDURE PI-LIMITE-BCO:
DEF INPUT PARAMETER p_dat_term_compens AS DATE FORMAT  "99/99/9999" NO-UNDO.

   assign v_num_hrs_pos = 0.
   for each bco_hrs_compens_func of bfunc-ponto no-lock where
            bco_hrs_compens_func.cod_mes_ano_refer_fp       = ""                 and
            bco_hrs_compens_func.idi_tratam_lancto_bco_hrs  = 1                  and
            bco_hrs_compens_func.cdn_tip_compcao_hrs        = 1                  and
            bco_hrs_compens_func.dat_atualiz_bco_hora      >= v_dat_inic_compens and
            bco_hrs_compens_func.dat_atualiz_bco_hora      <= p_dat_term_compens:
      if bco_hrs_compens_func.idi_hrs_posit = 1 then 
         assign v_num_hrs_pos = v_num_hrs_pos + bco_hrs_compens_func.qti_hrs_marcac_ptoelet.
   end. /* bco */

   if v_ind_lim_bco = 1 then do:
      if v_num_hrs_pos > bfunc-ponto.qti_lim_hrs_posit_bco_period then 
         assign v_num_dif_pos = v_num_hrs_pos - bfunc-ponto.qti_lim_hrs_posit_bco_period.
      else 
         assign v_num_dif_pos = 0.
   end.

   if v_ind_lim_bco = 2 then do:
      if v_num_hrs_pos > bfunc-ponto.qti_lim_max_period_pto then 
         assign v_num_dif_pos = v_num_hrs_pos - bfunc-ponto.qti_lim_max_period_pto.
      else 
         assign v_num_dif_pos = 0.
   end.

   if v_ind_lim_bco = 3 then do:
      if v_num_hrs_pos > bfunc-ponto.qti_lim_max_semana then 
         assign v_num_dif_pos = v_num_hrs_pos - bfunc-ponto.qti_lim_max_semana.
      else 
         assign v_num_dif_pos = 0.
   end.

   if v_num_dif_pos > 0 then do:
      assign v_dat_inic_comp_transf = v_dat_inic_compens
             v_dat_term_comp_transf = p_dat_term_compens.
      run prghur/pep/pe4000r3.p. 
   end.

   /**** trata horas a pagar / horas a descontar *********************/
   if can-find(first bco_hrs_compens_func of bfunc-ponto where
               bco_hrs_compens_func.cod_mes_ano_refer_fp       =  ""                and
               bco_hrs_compens_func.cdn_tip_compcao_hrs        =  1                 and
               bco_hrs_compens_func.idi_tratam_lancto_bco_hrs  >  1                 and
               bco_hrs_compens_func.dat_atualiz_bco_hora      >= v_dat_inic_compens and
               bco_hrs_compens_func.dat_atualiz_bco_hora      <= p_dat_term_compens) then do:
      if can-find(first efp_hora_extra_tip_dia_sind where 
                  efp_hora_extra_tip_dia_sind.cdn_empresa           = bfunc-ponto.cdn_empresa and
                  efp_hora_extra_tip_dia_sind.cdn_sindicato         = bfunc-ponto.cdn_sindicato and
                  efp_hora_extra_tip_dia_sind.idi_tratam_hora       = 1 and
                  efp_hora_extra_tip_dia_sind.idi_integr_period_mpe = 4) then do:
         for each tt-extra-semana exclusive-lock:
            delete tt-extra-semana.
         end.
         if weekday(v_dat_inic_compens) > 2 then do:
            assign v_num_dif_sem = weekday(v_dat_inic_compens) - 2
                   v_dat_ini_sem = v_dat_inic_compens - v_num_dif_sem.
         end.
         else do:
            if weekday(v_dat_inic_compens) < 2 then
               assign v_dat_ini_sem = v_dat_inic_compens + 1.
            else
               assign v_dat_ini_sem = v_dat_inic_compens.
         end.
         assign v_num_sem = 0. 
         repeat while v_dat_ini_sem < p_dat_term_compens:
            assign v_num_sem = v_num_sem + 1.
            create tt-extra-semana.
            assign tt-extra-semana.num_sem = v_num_sem
                   tt-extra-semana.dat_ini = v_dat_ini_sem
                   tt-extra-semana.dat_fim = v_dat_ini_sem + 6.
            assign v_dat_ini_sem = v_dat_ini_sem + 7.

            /* quando o limite for semanal, zerar a quantidade de horas extras da semana 
               pois n∆o passa na l¢gica que faz isso no PE4000.i2 */
            if v_ind_lim_bco = 3 THEN DO:
               FOR EACH tt_hora_extra NO-LOCK:
                   assign tt_hora_extra.qtd_hrs_extra = 0.
               END.
            END.
         end.
      end.
   end.

   EMPTY TEMP-TABLE tt_hex_ja_lida.

   for each bco_hrs_compens_func of bfunc-ponto exclusive-lock where
            bco_hrs_compens_func.cod_mes_ano_refer_fp       =  ""                and
            bco_hrs_compens_func.cdn_tip_compcao_hrs        =  1                 and
            bco_hrs_compens_func.idi_tratam_lancto_bco_hrs  >  1                 and
            bco_hrs_compens_func.dat_atualiz_bco_hora      >= v_dat_inic_compens and
            bco_hrs_compens_func.dat_atualiz_bco_hora      <= p_dat_term_compens:
      if bco_hrs_compens_func.idi_hrs_posit = 1 and
         bco_hrs_compens_func.idi_tratam_lancto_bco_hrs = 2 then do:
         assign v_programa = "PE4000R1".
         {prghur/pep/pe4000.i2} /*** monta movtoponto com horas extras tratadas ***/
         if not bco_hrs_compens_func.log_livre_2 then
            assign bco_hrs_compens_func.idi_tratam_lancto_bco_hrs = 1.
         assign bco_hrs_compens_func.idi_hrs_posit = 5
                bco_hrs_compens_func.cod_mes_ano_refer_fp = c-mmaa-folha.
      end.
      else do: /** desconto horas autorizadas diretamento pelo usuario *************/ 
         if bco_hrs_compens_func.idi_hrs_posit = 2 and 
            bco_hrs_compens_func.idi_tratam_lancto_bco_hrs        = 3 then do:
            if not bco_hrs_compens_func.log_livre_2 then
               assign bco_hrs_compens_func.idi_tratam_lancto_bco_hrs = 1.
            assign bco_hrs_compens_func.idi_hrs_posit             = 6
                   bco_hrs_compens_func.cod_mes_ano_refer_fp      = c-mmaa-folha.
            if bco_hrs_compens_func.log_hrs_diurno = yes then do: /** horas diurnas **/
               assign i-ev-codigo = i-ev-desc-hrs-diu
                      i-qtd-hrs   = 
                      truncate(round(bco_hrs_compens_func.qti_hrs_marcac_ptoelet / 3600,3),3).
                      d-hrs-diu-trb-sit = d-hrs-diu-trb-sit + i-qtd-hrs.
               run pi-movto-ptoelet.  
            end.     
            else do: /** horas noturnas **/
               assign i-ev-codigo       = i-ev-desc-hrs-not
                      i-qtd-hrs         = truncate(round(bco_hrs_compens_func.qti_hrs_marcac_ptoelet / 3600,3),3).
                      d-hrs-not-trb-sit = d-hrs-not-trb-sit + i-qtd-hrs.
               run pi-movto-ptoelet.  
            end.
         end.
      end. /* else */
   end. /*for each bco */
END PROCEDURE.

PROCEDURE pi-hrs-extra-periodo:

    def var v_row_efp as rowid no-undo.

    /******* Inicializando Tabelas *********/
    empty temp-table tt-hextra.
    empty temp-table tt-efp-par-marcac.
    empty temp-table tt-qti-hrs-evento.
      
    assign v_log_movto_extra = yes.
    if can-find(first efp_hora_extra_tip_dia_sind where 
                efp_hora_extra_tip_dia_sind.cdn_empresa           = bfunc-ponto.cdn_empresa and
                efp_hora_extra_tip_dia_sind.cdn_sindicato         = bfunc-ponto.cdn_sindicato and
                efp_hora_extra_tip_dia_sind.idi_tratam_hora       = v_num_tratam and
                efp_hora_extra_tip_dia_sind.idi_integr_period_mpe = 4) then do:
       for each tt-extra-semana exclusive-lock:
          delete tt-extra-semana.
       end.
       if weekday(bcontrole-catponto.dat_inic_period_apurac_pto_mes) > 2 then do:
          assign v_num_dif_sem = weekday(bcontrole-catponto.dat_inic_period_apurac_pto_mes) - 2
                 v_dat_ini_sem = bcontrole-catponto.dat_inic_period_apurac_pto_mes - v_num_dif_sem.
       end.
       else do:
          if weekday(bcontrole-catponto.dat_inic_period_apurac_pto_mes) < 2 then
             assign v_dat_ini_sem = bcontrole-catponto.dat_inic_period_apurac_pto_mes + 1.
          else
             assign v_dat_ini_sem = bcontrole-catponto.dat_inic_period_apurac_pto_mes.
       end.
       assign v_num_sem = 0.
       repeat while v_dat_ini_sem < dat-fim-mes:
          assign v_num_sem = v_num_sem + 1.
          create tt-extra-semana.
          assign tt-extra-semana.num_sem = v_num_sem
                 tt-extra-semana.dat_ini = v_dat_ini_sem
                 tt-extra-semana.dat_fim = v_dat_ini_sem + 6.
          assign v_dat_ini_sem = v_dat_ini_sem + 7.
       end.
    end.

    /******* Crianto temp-table com os eventos *********/
    if v_num_tratam = 1 then do:
       for each efp_par_marcac_ptoelet of bfunc-ponto NO-LOCK where
           efp_par_marcac_ptoelet.dat_proces_mpe       >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
           efp_par_marcac_ptoelet.dat_proces_mpe       <= dat-fim-mes and  
          (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet  = 1 or
           efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet  = 6) and
           efp_par_marcac_ptoelet.cdn_efp               = "" /*0*/:
           create tt-efp-par-marcac.
           buffer-copy efp_par_marcac_ptoelet to tt-efp-par-marcac.
           assign tt-efp-par-marcac.log_marcac_ptoelet_tratada = no.
       end.
    end.
    else do:
       for each efp_par_marcac_ptoelet of bfunc-ponto NO-LOCK where
           efp_par_marcac_ptoelet.dat_proces_mpe       >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
           efp_par_marcac_ptoelet.dat_proces_mpe       <= dat-fim-mes and  
           efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet  = 5 and
           efp_par_marcac_ptoelet.cdn_efp               = ""/*0*/:
           create tt-efp-par-marcac.
           buffer-copy efp_par_marcac_ptoelet to tt-efp-par-marcac.
           assign tt-efp-par-marcac.log_marcac_ptoelet_tratada = no.
       end.
    end.
    assign v_cod_tip_dia_ant = "".

    /******** Processo de rateio de Horas *********/
    for each tt-efp-par-marcac exclusive-lock use-index od-cresc where
             tt-efp-par-marcac.log_marcac_ptoelet_tratada = no:
       if tt-efp-par-marcac.dat_proces_mpe > bcontrole-catponto.dat_term_period_apurac_pto_mes and
          not l-desligado then /* cris func demit */
          next.
       if v_cod_tip_dia_ant <> tt-efp-par-marcac.cod_tip_dia then do:
          assign v_cod_tip_dia_ant = tt-efp-par-marcac.cod_tip_dia
                 v_cod_tip_dia_aux = tt-efp-par-marcac.cod_tip_dia.
/****************************/

          find efp_tip_dias_sind where
               efp_tip_dias_sind.cdn_empresa   = bfunc-ponto.cdn_empresa and
               efp_tip_dias_sind.cdn_sindicato = bfunc-ponto.cdn_sindicato and
               efp_tip_dias_sind.cod_tip_dia   = v_cod_tip_dia_aux no-lock no-error.
          if not available efp_tip_dias_sind then do:
             find agrup_tip_dia_sind where
                  agrup_tip_dia_sind.cdn_empresa       = bfunc-ponto.cdn_empresa and
                  agrup_tip_dia_sind.cdn_sindicato     = bfunc-ponto.cdn_sindicato and
                  agrup_tip_dia_sind.cod_tip_dia_agrup = v_cod_tip_dia_aux no-lock no-error.
             if not available agrup_tip_dia_sind then do:
                assign v_num_erro = v_num_erro + 1.
                run utp/ut-msgs.p (input "msg",input 16602,
                                   input v_cod_tip_dia_aux + "~~" + string(bfunc-ponto.cdn_sindicato)).
                create tt_erro.
                assign tt_erro.num_erro = v_num_erro
                       tt_erro.des_erro = trim(return-value)
                       tt_erro.log_erro = yes.
                next.
             end.
             find efp_tip_dias_sind where
                  efp_tip_dias_sind.cdn_empresa   = bfunc-ponto.cdn_empresa and
                  efp_tip_dias_sind.cdn_sindicato = bfunc-ponto.cdn_sindicato and
                  efp_tip_dias_sind.cod_tip_dia   = agrup_tip_dia_sind.cod_tip_dia no-lock no-error.
             if not available efp_tip_dias_sind then do:
                assign v_num_erro = v_num_erro + 1.
                run utp/ut-msgs.p (input "msg",input 16602,
                                   input agrup_tip_dia_sind.cod_tip_dia + "~~" + string(bfunc-ponto.cdn_sindicato)).
                create tt_erro.
                assign tt_erro.num_erro = v_num_erro
                       tt_erro.des_erro = trim(return-value)
                       tt_erro.log_erro = yes.
                next.
             end.
             assign v_cod_tip_dia_aux = agrup_tip_dia_sind.cod_tip_dia.
          end.

          /******************************/
          find first efp_hora_extra_tip_dia_sind where 
               efp_hora_extra_tip_dia_sind.cdn_empresa     = bfunc-ponto.cdn_empresa and
               efp_hora_extra_tip_dia_sind.cdn_sindicato   = bfunc-ponto.cdn_sindicato and
               efp_hora_extra_tip_dia_sind.cod_tip_dia     = v_cod_tip_dia_aux and
               efp_hora_extra_tip_dia_sind.idi_tratam_hora = v_num_tratam no-lock no-error. 
          if not available efp_hora_extra_tip_dia_sind then do:
             assign v_num_erro = v_num_erro + 1.
             {utp/ut-liter.i Evento_n∆o_cadastrado_para_Tipo_Dia mpe l}
             create tt_erro.
             assign tt_erro.num_erro = v_num_erro
                    tt_erro.des_erro = trim(return-value) + " " + v_cod_tip_dia_aux.
                    tt_erro.log_erro = yes.
             leave.
          end.
       end.

       find tt-hextra where tt-hextra.tp-dia = v_cod_tip_dia_aux NO-LOCK no-error. 
       if not available tt-hextra then do:
          create tt-hextra.
          assign tt-hextra.tp-dia = v_cod_tip_dia_aux
                 tt-hextra.i-tot-hrs = 0.
          IF efp_hora_extra_tip_dia_sind.idi_integr_period_mpe = 2 THEN DO: /* fechamento por periodo */
             for each bco_hrs_compens_func of bfunc-ponto no-lock where
                 bco_hrs_compens_func.cdn_tip_compcao_hrs     = 1            and
                 bco_hrs_compens_func.idi_hrs_posit           = 5            and
                 bco_hrs_compens_func.cod_mes_ano_refer_fp    = c-mmaa-folha AND
                 substr(bco_hrs_compens_func.cod_livre_1,1,2) = v_cod_tip_dia_aux:
                 assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig.
             end.
          END.
          if efp_hora_extra_tip_dia.idi_integr_period_mpe = 3 then do:
             assign tt-hextra.num_mes_extra = month(tt-efp-par-marcac.dat_proces_mpe).
             if bcontrole-catponto.dat_inic_period_apurac_pto_mes < dat-ini-mes and
                month(tt-efp-par-marcac.dat_proces_mpe) = month(bcontrole-catponto.dat_inic_period_apurac_pto_mes) then do:
                assign v_dat_ini_extra = date(month(bcontrole-catponto.dat_inic_period_apurac_pto_mes),01,year(bcontrole-catponto.dat_inic_period_apurac_pto_mes)).
                for each efp_par_marcac_ptoelet of bfunc-ponto no-lock where
                         efp_par_marcac_ptoelet.dat_proces_mpe >= v_dat_ini_extra and
                         efp_par_marcac_ptoelet.dat_proces_mpe < bcontrole-catponto.dat_inic_period_apurac_pto_mes and  /* cris func demit */
                         (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = if v_num_tratam = 1 then 1 
                                                                        else 5) and
                         efp_par_marcac_ptoelet.cdn_efp              = "" /*0*/ and
                         efp_par_marcac_ptoelet.cod_tip_dia          = v_cod_tip_dia_aux:
                   assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                end.
             end.
             else
                assign v_dat_ini_extra = dat-ini-mes.
          end.
          else do:
             if efp_hora_extra_tip_dia.idi_integr_period_mpe = 4 then do:
                find last tt-extra-semana no-lock where
                          tt-extra-semana.dat_ini <= tt-efp-par-marcac.dat_proces_mpe no-error.
                if avail tt-extra-semana then do:
                   assign v_dat_ini_extra = tt-extra-semana.dat_ini
                          v_num_sem       = tt-extra-semana.num_sem.
                   for each efp_par_marcac_ptoelet of bfunc-ponto no-lock where
                            efp_par_marcac_ptoelet.dat_proces_mpe >= v_dat_ini_extra and
                            efp_par_marcac_ptoelet.dat_proces_mpe < tt-efp-par-marcac.dat_proces_mpe and  /* cris func demit */
                           ((efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = if v_num_tratam = 1 then 1
                                                                           else 5) or   /* Hora Plant∆o */
                            (efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 6 and
                             v_num_tratam = 1)) and
                            efp_par_marcac_ptoelet.cdn_efp              = "" /*0*/ and
                            efp_par_marcac_ptoelet.cod_tip_dia          = v_cod_tip_dia_aux:
                      assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet.
                   end.
                end.
             end.
             else
                assign v_dat_ini_extra = bcontrole-catponto.dat_inic_period_apurac_pto_mes.
          end.
       end. /* create */
       else do:
          if efp_hora_extra_tip_dia.idi_integr_period_mpe = 3 and
             tt-hextra.num_mes_extra <> month(tt-efp-par-marcac.dat_proces_mpe) then
             assign tt-hextra.i-tot-hrs = 0
                    tt-hextra.num_mes_extra = month(tt-efp-par-marcac.dat_proces_mpe).
          if efp_hora_extra_tip_dia.idi_integr_period_mpe = 4 then do:
             find last tt-extra-semana no-lock where
                       tt-extra-semana.dat_ini <= tt-efp-par-marcac.dat_proces_mpe no-error.
             if avail tt-extra-semana and v_num_sem <> tt-extra-semana.num_sem then do:
                assign tt-hextra.i-tot-hrs = 0
                       v_num_sem           = tt-extra-semana.num_sem.
             end.
          end.
       end.
       if v_num_tratam = 1 then do:
          IF efp_hora_extra_tip_dia_sind.idi_integr_period_mpe <> 2 THEN DO: /* n∆o Ç por periodo*/
             for each bco_hrs_compens_func of bfunc-ponto no-lock where
                 bco_hrs_compens_func.cdn_tip_compcao_hrs     =  1              and
                 bco_hrs_compens_func.idi_hrs_posit           =  5              and
                 bco_hrs_compens_func.dat_atualiz_bco_hora   >= v_dat_ini_extra and
                 bco_hrs_compens_func.dat_atualiz_bco_hora   <=  tt-efp-par-marcac.dat_proces_mpe and
                 substr(bco_hrs_compens_func.cod_livre_1,1,2) = v_cod_tip_dia_aux:
                 if bco_hrs_compens_func.dat_atualiz_bco_hora = v_dat_ini_extra and
                    bco_hrs_compens_func.num_horar_inic_mpe > tt-efp-par-marcac.num_horar_inic_proces_mpe then
                    next.

                 if efp_hora_extra_tip_dia_sind.idi_integr_period_mpe = 3 then do:
                    if tt-hextra.num_mes_extra = month(bco_hrs_compens_func.dat_atualiz_bco_hora) then
                       assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig.
                 end.
                 else do:
                   if efp_hora_extra_tip_dia_sind.idi_integr_period_mpe = 4 then do:
                      find last tt-extra-semana no-lock where
                           tt-extra-semana.dat_ini <= bco_hrs_compens_func.dat_atualiz_bco_hora no-error.
                      if avail tt-extra-semana and v_num_sem = tt-extra-semana.num_sem then
                         assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig.
                   end.
                   else
                      assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig.
                end.
             end.
             if tt-efp-par-marcac.dat_proces_mpe > v_dat_ini_extra then
                assign v_dat_ini_extra = tt-efp-par-marcac.dat_proces_mpe.
          end.
          
       END.
       find last efp_hora_extra_tip_dia_sind no-lock where 
                 efp_hora_extra_tip_dia_sind.cdn_empresa     = bfunc-ponto.cdn_empresa and 
                 efp_hora_extra_tip_dia_sind.cdn_sindicato   = bfunc-ponto.cdn_sindicato and 
                 efp_hora_extra_tip_dia_sind.cod_tip_dia     = v_cod_tip_dia_aux and 
                 efp_hora_extra_tip_dia_sind.idi_tratam_hora = v_num_tratam and
                 efp_hora_extra_tip_dia_sind.num_horar_inic_interv_refei <= int(tt-hextra.i-tot-hrs + 60) no-error.
       if not avail efp_hora_extra_tip_dia_sind then do:
          leave.
       end.
       /*Hora Extra Intervalo - Protege*/
       if  bcatponto.log_paga_hext_interv_movel then do:
           if tt-efp-par-marcac.idi_tip_ocor_ptoelet = 6 then do:
               if bcatponto.log_influi_outras_hext then
                   assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + tt-efp-par-marcac.qti_hrs_marcac_ptoelet.

               assign i-ev-codigo         = if tt-efp-par-marcac.log_hrs_diurno
                                            then bcatponto.cdn_efp_hext_diurno_interv
                                            else bcatponto.cdn_efp_hext_notur_interv
                      tt-efp-par-marcac.num_mes_ano_refer_fp = c-mmaa-folha
                      tt-efp-par-marcac.log_integr_gerac     = yes
                      tt-efp-par-marcac.cdn_efp              = i-ev-codigo. /** par_marcac alterado pelo 4000 **/

               FIND FIRST tt-qti-hrs-evento NO-LOCK WHERE
                  tt-qti-hrs-evento.v_cdn_event_fp = i-ev-codigo NO-ERROR.
               IF NOT AVAIL tt-qti-hrs-evento THEN DO:
                  CREATE tt-qti-hrs-evento.
                  ASSIGN tt-qti-hrs-evento.v_cdn_event_fp = i-ev-codigo.
               END.
               ASSIGN tt-qti-hrs-evento.v_qti_hrs_evento = tt-qti-hrs-evento.v_qti_hrs_evento + (tt-efp-par-marcac.qti_hrs_marcac_ptoelet / 3600).
               NEXT.
           END.
       end.

       if (tt-hextra.i-tot-hrs + tt-efp-par-marcac.qti_hrs_marcac_ptoelet) > efp_hora_extra_tip_dia_sind.num_horar_term_interv_refei then do:
          create btt-efp-par-marcac.
          assign btt-efp-par-marcac.num_horar_fim_proces_mpe   = tt-efp-par-marcac.num_horar_fim_proces_mpe
                 btt-efp-par-marcac.cdn_empresa                = tt-efp-par-marcac.cdn_empresa 
                 btt-efp-par-marcac.cdn_estab                  = tt-efp-par-marcac.cdn_estab
                 btt-efp-par-marcac.cdn_funcionario            = tt-efp-par-marcac.cdn_funcionario
                 btt-efp-par-marcac.dat_proces_mpe             = tt-efp-par-marcac.dat_proces_mpe
                 btt-efp-par-marcac.dat_marcac_ptoelet         = tt-efp-par-marcac.dat_marcac_ptoelet
                 btt-efp-par-marcac.num_horar_inic_proces_mpe  = tt-efp-par-marcac.num_horar_inic_proces_mpe +
                                                                (efp_hora_extra_tip_dia_sind.num_horar_term_interv_refei - tt-hextra.i-tot-hrs)
                 btt-efp-par-marcac.cdn_efp                    = tt-efp-par-marcac.cdn_efp
                 btt-efp-par-marcac.qti_hrs_marcac_ptoelet     = btt-efp-par-marcac.num_horar_fim_proces_mpe -
                                                                 btt-efp-par-marcac.num_horar_inic_proces_mpe
                 btt-efp-par-marcac.idi_sit_mpe                = tt-efp-par-marcac.idi_sit_mpe
                 btt-efp-par-marcac.idi_tip_ocor_ptoelet       = tt-efp-par-marcac.idi_tip_ocor_ptoelet
                 btt-efp-par-marcac.cdn_sit_afast_func         = tt-efp-par-marcac.cdn_sit_afast_func
                 btt-efp-par-marcac.log_hrs_diurno             = tt-efp-par-marcac.log_hrs_diurno
                 btt-efp-par-marcac.cod_tip_dia                = tt-efp-par-marcac.cod_tip_dia
                 btt-efp-par-marcac.cdn_sindicato              = tt-efp-par-marcac.cdn_sindicato
                 btt-efp-par-marcac.log_integr_gerac           = tt-efp-par-marcac.log_integr_gerac
                 btt-efp-par-marcac.log_marcac_ptoelet_tratada = no
                 tt-efp-par-marcac.num_horar_fim_proces_mpe    = tt-efp-par-marcac.num_horar_inic_proces_mpe +
                                                                (efp_hora_extra_tip_dia_sind.num_horar_term_interv_refei - tt-hextra.i-tot-hrs)
                 tt-efp-par-marcac.qti_hrs_marcac_ptoelet      = tt-efp-par-marcac.num_horar_fim_proces_mpe -
                                                                 tt-efp-par-marcac.num_horar_inic_proces_mpe
                 tt-efp-par-marcac.log_marcac_ptoelet_tratada  = yes.

          assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + tt-efp-par-marcac.qti_hrs_marcac_ptoelet           
                 i-ev-codigo         = if tt-efp-par-marcac.log_hrs_diurno 
                                       then efp_hora_extra_tip_dia_sind.cdn_efp_hora_extra_diurno
                                       else efp_hora_extra_tip_dia_sind.cdn_efp_hora_extra_notur
                 tt-efp-par-marcac.num_mes_ano_refer_fp = c-mmaa-folha
                 tt-efp-par-marcac.log_integr_gerac     = yes
                 tt-efp-par-marcac.cdn_efp              = i-ev-codigo. /** par_marcac alterado pelo 4000 **/

          FIND FIRST tt-qti-hrs-evento NO-LOCK WHERE
              tt-qti-hrs-evento.v_cdn_event_fp = i-ev-codigo NO-ERROR.
          IF NOT AVAIL tt-qti-hrs-evento THEN DO:
             CREATE tt-qti-hrs-evento.
             ASSIGN tt-qti-hrs-evento.v_cdn_event_fp = i-ev-codigo.
          END. 
          ASSIGN tt-qti-hrs-evento.v_qti_hrs_evento = tt-qti-hrs-evento.v_qti_hrs_evento + (tt-efp-par-marcac.qti_hrs_marcac_ptoelet / 3600).
                 
       END.
       else do:
          assign tt-hextra.i-tot-hrs = tt-hextra.i-tot-hrs + tt-efp-par-marcac.qti_hrs_marcac_ptoelet           
                 i-ev-codigo         = if tt-efp-par-marcac.log_hrs_diurno 
                                       then efp_hora_extra_tip_dia_sind.cdn_efp_hora_extra_diurno
                                       else efp_hora_extra_tip_dia_sind.cdn_efp_hora_extra_notur
                 tt-efp-par-marcac.log_marcac_ptoelet_tratada = yes
                 tt-efp-par-marcac.num_mes_ano_refer_fp       = c-mmaa-folha
                 tt-efp-par-marcac.log_integr_gerac           = yes
                 tt-efp-par-marcac.cdn_efp                    = i-ev-codigo. /** par_marcac alterado pelo 4000 **/

           FIND FIRST tt-qti-hrs-evento NO-LOCK WHERE                                                                                          
               tt-qti-hrs-evento.v_cdn_event_fp = i-ev-codigo NO-ERROR.                                                                        
           IF NOT AVAIL tt-qti-hrs-evento THEN DO:                                                                                             
              CREATE tt-qti-hrs-evento.                                                                                                        
              ASSIGN tt-qti-hrs-evento.v_cdn_event_fp = i-ev-codigo.                                                                           
           END.                                                                                                                                
           ASSIGN tt-qti-hrs-evento.v_qti_hrs_evento = tt-qti-hrs-evento.v_qti_hrs_evento + (tt-efp-par-marcac.qti_hrs_marcac_ptoelet / 3600). 
       end.
    end.        
    /********* Gravaá∆o dos eventos **************/
    /*do i-ev-codigo = 1 to 999: */
    FOR EACH tt-qti-hrs-evento no-lock where
        tt-qti-hrs-evento.v_qti_hrs_evento > 0:

       assign i-qtd-hrs = truncate(round(tt-qti-hrs-evento.v_qti_hrs_evento,3),3)
              i-ev-codigo = tt-qti-hrs-evento.v_cdn_event_fp.

       run pi-movto-ptoelet.
    END.
end.

procedure pi-adic-repouso:
    def var v_dat_ini        as date no-undo.
    def var v_dat_fim        as date no-undo.
    def var v_qtd_dias_uteis as int  no-undo.
    def var v_qtd_repouso    as int  no-undo.
    def var v_dia_fim        as int  no-undo.
          
    assign v_dat_ini        = bcontrole-funcponto.dat_inic_period_apurac_pto_mes
           v_dat_fim        = bcontrole-funcponto.dat_term_period_apurac_pto_mes
           v_qtd_dias_uteis = 0
           v_qtd_repouso    = 0
           v_dia_fim        = if (month(v_dat_fim) + 1) > 12
                              then day(date(01,01,(year(v_dat_fim) + 1)) - 1)
                              else day(date((month(v_dat_fim) + 1),01,year(v_dat_fim)) - 1)
           v_dat_fim_lotac  = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF .

    if bfunc-ponto.dat_admis_func > v_dat_ini then
        assign v_dat_ini = bfunc-ponto.dat_admis_func.

    if bfunc-ponto.dat_desligto_func <> ? then
        assign v_dat_fim = bfunc-ponto.dat_desligto_func.
    
    if l-afast-periodo then
        next.
    
    do dt-dia-cal = v_dat_ini to v_dat_fim:
        {prghur/pep/pe9994.i dt-dia-cal}
        
        if v_cdn_turno = 0 or v_cdn_turma = 0 then do:
           find first func_turno_trab of bfunc-ponto no-lock no-error.
           if avail func_turno_trab then
              assign v_cdn_turno = func_turno_trab.cdn_turno_trab
                     v_cdn_turma = func_turno_trab.cdn_turma_trab.
           find first func_localid of bfunc-ponto no-lock no-error.
           if avail func_localid then
              assign v_cod_pais    = func_localid.cod_pais
                     v_cdn_localid = func_localid.cdn_localidade.
        end.
        find tt-calendar where
             tt-calendar.cdn_turno_trab   = v_cdn_turno and
             tt-calendar.cdn_turma_trab   = v_cdn_turma and
             tt-calendar.dat_refer_calend = dt-dia-cal  and
             tt-calendar.cod_pais         = v_cod_pais  and
             tt-calendar.cdn_localidade   = v_cdn_localid no-lock no-error.
        if avail tt-calendar then do:
           if tt-calendar.idi_sit_dia_trab = 1 or
              tt-calendar.idi_sit_dia_trab = 2 or
              tt-calendar.idi_sit_dia_trab = 5 then do:
              find first sit_afast_func of bfunc-ponto no-lock where
                         sit_afast_func.dat_inic_sit_afast <= dt-dia-cal and
                         sit_afast_func.dat_term_sit_afast >= dt-dia-cal no-error.
              if avail sit_afast_func then do:
                 find first sit_afast no-lock where
                            sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func and
                           (sit_afast.idi_signif_sit = 2  or
                            sit_afast.idi_signif_sit = 5  or 
                            sit_afast.idi_signif_sit = 7) no-error.
                 if avail sit_afast_func then
                    next.
                 else
                    assign v_qtd_dias_uteis = v_qtd_dias_uteis + 1.
              end.
              else
                    assign v_qtd_dias_uteis = v_qtd_dias_uteis + 1.
           end.
           else
              assign v_qtd_repouso = v_qtd_repouso + 1.
        end.
    end.

    if bfunc-ponto.cdn_categ_sal     = 1 and
       bfunc-ponto.dat_desligto_func = ? and
       bfunc-ponto.dat_admis_func   <= v_dat_ini then do:
       if v_dia_fim = 31 then
          assign v_qtd_dias_uteis = v_qtd_dias_uteis - 1.
       else
           if v_dia_fim = 28 then
              assign v_qtd_dias_uteis = v_qtd_dias_uteis + 2.
           else
               if v_dia_fim = 29 then
                   assign v_qtd_dias_uteis = v_qtd_dias_uteis + 1.
    end.

    if i-qtd-dsr-per <> 0 then
        assign v_qtd_repouso = v_qtd_repouso - i-qtd-dsr-per.

    if i-qtd-fer-per <> 0 then
        assign v_qtd_repouso = v_qtd_repouso - i-qtd-fer-per.

    if v_qtd_repouso < 0 then
        next.

    find first idx_efp_funcao_espcif no-lock where
               idx_efp_funcao_espcif.cdn_idx_efp_funcao_espcif = 33 no-error.
    if avail idx_efp_funcao_espcif then do:

       find event_fp no-lock where
            event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento AND
            event_fp.cdn_event_fp = idx_efp_funcao_espcif.cdn_event_fp NO-ERROR.
       if avail event_fp then do:
          assign i-ev-codigo =  event_fp.cdn_event_fp
                 i-qtd-hrs   = (v_qtd_repouso / v_qtd_dias_uteis) * 100.
          run pi-movto-ptoelet. 
       end.
    end.
end.
