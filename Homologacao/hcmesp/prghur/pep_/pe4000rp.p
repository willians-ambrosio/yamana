/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i PE4000RP 1.02.10.041 } /*** 0101041 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i pe4000rp MPE}
&ENDIF

{include/i_fnctrad.i}
/*******************************************************************************
**        Programa: PEP4000RP.P.
**        data....: 06/1997
**        Autor...: Datasul S/A.
**        Objetivo: Gera Movimento de Ponto para Folha. 
*******************************************************************************/
{include/i-rpvar.i} 
{include/i_dbvers.i}

&global-define segur_estab_calculo yes

{include/i-epc200.i pe4000rp }

def new shared var i-ev-trbdiu         /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-trbnot         /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-supnot         /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var l-sup-evnot         as log no-undo.
def new shared var l-adc-evnot         as log no-undo. /* cris turno m¢vel / period */
def new shared var i-ev-dsrdiu         /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-dsrnot         /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-dsrdiuper      /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-dsrnotper      /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-ferdiuper      /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-fernotper      /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-desc-hrs-diu   /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var i-ev-desc-hrs-not   /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
DEF NEW SHARED VAR i-ev-hrs-dif-diu    /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
DEF NEW SHARED VAR i-ev-hrs-dif-not    /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def new shared var c-mmaa-folha like bco_hrs_compens_func.cod_mes_ano_refer_fp no-undo.
def new shared var i-ult-diames        as int no-undo.
def new shared var dat-ini-mes         as date no-undo.
def new shared var dat-fim-mes         as date no-undo.
def new shared var v_log_fecha         as log  no-undo.
def new shared var v_num_erro          as int  no-undo.
def new shared var d-ini-comp          as date no-undo.
def new shared var d-fim-comp          as date no-undo.
DEF NEW SHARED VAR v_log_mes_seg       AS LOG  NO-UNDO.
DEF NEW SHARED VAR v_log_calend_func   AS LOG  NO-UNDO.

def var i-mes-ant                as int                                     no-undo.
def var i-ano-ant                as int                                     no-undo.
def var i-ev-codigo              /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def var i-ev-percdsr             /*as int*/ LIKE event_fp.cdn_event_fp no-undo.
def var i-qtd-hrs                as int                                     no-undo.
def var i-semana                 as int                                     no-undo.      
def var i-qtd-dsr-per            as int                                     no-undo.
def var dt-dia-cal               as date                                    no-undo.
def var dt-aux-sitini            as date                                    no-undo.
def var dt-aux-sitfim            as date                                    no-undo. 
def var i-aux-min-atraso         as int                                     no-undo.
def var i-aux-qtd-atraso         as int                                     no-undo.
def var i-tot-hrs-neg            as int                                     no-undo.
def var i-tot-hrs-pos            as int                                     no-undo.
def var i-qtd-hrs-hpc-seg        as int                                     no-undo.
def var d-per-calc-diu           as dec                                     no-undo.
def var l-tratada-situa          as log                                     no-undo.
def var i-ini-interv             as int                                     no-undo.
def var i-ini-intclc             as int                                     no-undo.
def var i-fim-interv             as int                                     no-undo.
def var i-fim-intclc             as INT                                     no-undo.
def var i-interv-diu             as int                                     no-undo.
def var i-interv-not             as int                                     no-undo.
def var i-tot-hra-extra          as int                                     no-undo.
def var i-dif-hrs-pag            as INT                                     no-undo.
def var i-dif-qtd-hrs            as int                                     no-undo.
def var v_des_mensagem           as char format "x(40)"                     no-undo.
def var prh_acomp                as handle                                  no-undo.
def var v_des_selecao            as char format "x(15)"                     no-undo.
def var v_des_classificacao      AS char format "x(18)"                     no-undo.
def var v_des_impressao          as char format "x(20)"                     no-undo.  
def var v_des_parametro          as char format "x(15)"                     no-undo.
def var v_des_destino            as char format "x(15)"                     no-undo.
def var v_des_ger                as char format "x(28)"                     no-undo.
def var v_log_imprime            as log                                     no-undo.
def new shared var v_num_movtos             as int  format ">>>>>>>>>9"                no-undo.
def var v_des_erro1              as char format "x(60)"                     no-undo.
def var v_mes_empresa            as int                                     no-undo.
def var v_ano_empresa            as INT                                     no-undo.
def var i-st-ant                 as int                                     no-undo.
def var i-st-atu                 as INT                                     no-undo.
def var dt-proces                as DATE                                    no-undo.
def var i-horar-ini              as int                                     no-undo.
def var i-horar-fim              as int                                     no-undo.
def var v_dat_ref                as date format "99/99/9999"                no-undo.
def var v_dat_ref_orig           as date format "99/99/9999"                no-undo.
def var v_log_nao_calc           as log                                     no-undo.
def var v_dat_ini_per            as date format "99/99/9999"                no-undo.
def var v_dat_fim_per            as date format "99/99/9999"                no-undo.
def var v_dat_ini_per_orig       as date format "99/99/9999"                no-undo.
def var v_dat_fim_per_orig       as date format "99/99/9999"                no-undo.
def var v_log_transf             as log  initial no                         no-undo.
def var v_turno_ant            like   funcionario.cdn_turno_trab            no-undo.
def var v_turma_ant            like   funcionario.cdn_turma_trab            no-undo.
def var v_pais_ant             like   funcionario.cod_pais_localid          no-undo.
def var v_localid_ant          like   funcionario.cdn_localidade            no-undo.
DEF VAR v_log_fer_mes_atu        AS LOG                                     NO-UNDO.
def var v_log_utiliz_folha_compl as logical initial no                      no-undo.
DEF VAR v_log_criou              AS LOG                                     NO-UNDO.
DEF VAR v_num_mes_ant            AS INT                                     NO-UNDO.
DEF VAR v_num_ano_ant            AS INT  FORMAT 9999                        NO-UNDO.
DEF VAR v_dat_ini_aux            AS DATE FORMAT "99/99/9999"                NO-UNDO.
DEF VAR v_dat_fim_aux            AS DATE FORMAT "99/99/9999"                NO-UNDO.
def var v_dat_aux_proc           as date format "99/99/9999"                no-undo.
def var v_dat_fim_localid        as date format "99/99/9999"                no-undo.
def var v_dat_fim_emprest        as date format "99/99/9999"                no-undo.
def var v_dat_fim_lotac          as date format "99/99/9999"                no-undo.
def var v_cdn_turno            like turno_trab.cdn_turno_trab               no-undo.
def var v_cdn_turma            like turma_trab.cdn_turma_trab               no-undo.
def var v_cdn_jorn             like jorn_trab.cdn_jorn_trab                 no-undo.
def var v_cdn_interv           like interv_refei_jorn_trab.cdn_interv_refei no-undo.
def var v_cod_pais             like localidade.cod_pais                     no-undo.
def var v_cdn_localid          like localidade.cdn_localidade               no-undo.
def var v_log_emprest            as logical                                 no-undo.
def var v_log_verifica_ocor      as log                                     no-undo.
DEF VAR v_dat_aux_ini            AS DATE FORMAT "99/99/9999"                NO-UNDO.
DEF VAR v_dat_aux_fim            AS DATE FORMAT "99/99/9999"                NO-UNDO.
DEF VAR v_num_mes_seg            AS INT                                     NO-UNDO.
DEF VAR v_num_ano_seg            AS INT                                     NO-UNDO.
DEF VAR v_cdn_estab_orig       LIKE funcionario.cdn_estab                   NO-UNDO.
DEF VAR v_cdn_emp_orig         LIKE funcionario.cdn_empresa                 NO-UNDO.
DEF VAR v_cdn_func_orig        LIKE funcionario.cdn_funcionario             NO-UNDO.
DEF VAR v_cod_mes_ano            as character                               no-undo.
def var l_log_error              as logical                                 no-undo.
DEF VAR v-dia                    AS INT                                     NO-UNDO.
DEF VAR l-erro                   AS LOG                                     NO-UNDO.

def var v_cdn_turno_mens   like turno_trab.cdn_turno_trab no-undo.
def var v_cdn_turma_mens   like turma_trab.cdn_turma_trab no-undo.
def var v_cdn_localid_mens like localidade.cdn_localidade no-undo.
def var v_mes_mens         as int format "99"             no-undo.
def var v_ano_mens         as int format "9999"           no-undo.
DEF VAR i-mes-ant-transf   AS INT format "99"             no-undo.
DEF VAR i-ano-ant-transf   AS INT format "9999"           no-undo.
DEF VAR v_des_opcao        AS CHAR FORMAT "x(15)"         NO-UNDO.

/* PROCEDURE fechamento das horas por per°odo ponto - cris ****/
def var v_qti_hrs_diu     as int no-undo.
def var v_qti_hrs_not     as int no-undo.
def var v_qti_hrs_diu_dsr as int no-undo.
def var v_qti_hrs_not_dsr as int no-undo.
def var v_qti_hrs_diu_fer as int no-undo.
def var v_qti_hrs_not_fer as int no-undo.
def var v_qti_dias_trab   as int no-undo.
def var v_qti_dias_dsr    as int no-undo.
def var v_qti_dias_fer    as int no-undo.
def var v_log_atualiz_tt  as log no-undo.
def var v_log_folha_educnal as logical initial no          no-undo.
/*DEF VAR v_log_func_transf   AS LOG INIT NO                 NO-UNDO.*/
/*SOCOCO*/
DEF VAR v_log_period_pto_difer AS LOG                      NO-UNDO.
def var v_log_alterou   as log  no-undo.
/*SOCOCO*/

/* claudemir */
DEF VAR v_dat_ini_emprest_aux AS DATE FORMAT "99/99/9999"                NO-UNDO.
DEF VAR v_dat_fim_emprest_aux AS DATE FORMAT "99/99/9999"                NO-UNDO.
/* claudemir */
def new shared temp-table tt_erro 
    field num_erro as int  format ">>>>>>>>>9"
    field des_erro as char format "x(100)"
    field log_erro as log.

def new shared temp-table tt-hextra 
    field tp-dia         as char
    field i-tot-hrs      as int
    field num_mes_extra  as int
    index tt-extra       is unique primary
          tp-dia         ascending.

def new shared temp-table tt-val-semana
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

def temp-table tt-orig-contratac-func no-undo
    field idi_orig_contratac_func     as int.

def temp-table tt_categ no-undo
    field cdn_categ_sal like categ_sal.cdn_categ_sal.

DEF NEW SHARED TEMP-TABLE tt-calendar LIKE det_calend_turma_localid.

def new shared buffer bfunc-ponto         for func_ptoelet. 
def new shared buffer bcontrole-funcponto for sit_calc_ptoelet_func.
def new shared buffer bfunc-bcohrs        for bco_hrs_compens_func. 
def new shared buffer cfunc-bcohrs        for bco_hrs_compens_func. 
def new shared buffer bcontrole-catponto  for sit_calc_ptoelet_categ. 
def new shared buffer bcalentur           for det_calend_turma_localid.   
def new shared buffer bturnotrb           for turno_trab. 
def new shared buffer bturmatrb           for turma_trab. 
def new shared buffer bcatponto           for categ_ptoelet.
def new shared buffer bparam-pe           for param_empres_tma.
def new shared buffer bsit_afast          for sit_afast.
def new shared buffer bsit_afast_func     for sit_afast_func.


def buffer bdet_calend_localid          for tt-calendar. /* cris - period */
def buffer bcontrole-funcponto-orig     for sit_calc_ptoelet_func.
def buffer bcatponto-orig               for categ_ptoelet.
def buffer bfunc-ponto-orig             for func_ptoelet. 
def buffer b-autoriz_hora_extra_compens for autoriz_hora_extra_compens.
def buffer b-sit_calc_ptoelet_categ     for sit_calc_ptoelet_categ.
def buffer b-bco_hrs_compens_func       for bco_hrs_compens_func.
def buffer b-movto_mpe_refeit           for movto_mpe_refeit.
def buffer b-marcac_ptoelet             for marcac_ptoelet.
def buffer b-efp_par_marcac             for efp_par_marcac.
def buffer b-funciona                   for funcionario.
DEF BUFFER bdet_calend_func             FOR det_calend_func.


{prghur/pep/pe4000tt.i NEW SHARED }

def temp-table tt-raw-digita no-undo
    field raw-digita as raw.     

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.    

create tt-param.
raw-transfer raw-param to tt-param.
{prghur/fpp/fp9200.i11}

for each tt-raw-digita:
   create tt-digita.
   raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

find param_folha_educnal where param_folha_educnal.cdn_empresa = tt-param.v_cdn_empres_usuar no-lock no-error.
if avail param_folha_educnal then
   assign v_log_folha_educnal = yes.

run utp/ut-acomp.p persistent set prh_acomp.

form
   tt_erro.num_erro at 02 space(02)
   tt_erro.des_erro
   with stream-io no-attr-space no-box down width 132 frame f-erro.

{utp/ut-liter.i Erro mpe L}
assign tt_erro.num_erro:label in frame f-erro = trim(return-value).
{utp/ut-liter.i Descriá∆o mpe L}
assign tt_erro.des_erro:label in frame f-erro = trim(return-value).

/*EPC chamada fppl10 MRS - atribuiá∆o de campo ********************************************/
for each tt-epc where tt-epc.cod-event = "epc_pe4000_fppl10_mrs_num_opcao":
    delete tt-epc.
end.

create tt-epc.
assign tt-epc.cod-event = "epc_pe4000_fppl10_mrs_num_opcao".

{include/i-epc201.i "epc_pe4000_fppl10_mrs_num_opcao"}
/************** fim EPC ********************************************/
form
    skip(2) 
    v_des_mensagem                           no-label colon 30
    skip(3)
    v_des_selecao                            no-label colon 15 skip(1)
    tt-param.i-es-ini    format ">>9"                 colon 40 
    "|< >|"                                           at 55
    tt-param.i-es-fim    format ">>9"        no-label colon 70 skip        
    tt-param.i-fc-ini format "zzzzzzz9"               colon 40
    "|< >|"                                           at 55
    tt-param.i-fc-fim format "zzzzzzz9"      no-label colon 70 
    tt-param.i-turno-ini format "zzzzzzz9"            colon 40
    "|< >|"                                           at 55
    tt-param.i-turno-fim format "zzzzzzz9"   no-label colon 70 skip
    tt-param.v_cdn_prestdor_ini                       colon 40 
    "|< >|"                                           at 55
    tt-param.v_cdn_prestdor_fim              no-label colon 70 SKIP 
    tt-param.c-ccusto-ini                             COLON 40  
    "|< >|"                                           at 55
    tt-param.c-ccusto-fim                    no-label colon 70
    tt-param.i-classe-ini                             COLON 40  
    "|< >|"                                           at 55
    tt-param.i-classe-fim                    no-label colon 70
    with stream-io side-labels no-attr-space no-box width 132 frame f-selecao.

FORM
    tt-param.i-ctr-ini               label "Contrato" colon 40
    "|< >|"                                           at 55
    tt-param.i-ctr-fim                       no-label colon 70 
    with stream-io side-labels no-attr-space no-box width 132 frame f-contrato.

FORM
    skip (2) 
    v_des_parametro                          no-label colon 15 skip(1)
    tt-param.log_fecha  FORMAT "Sim/N∆o"              COLON 40 
    tt-param.v_dat_ini  FORMAT "99/99/9999"           COLON 40
    "|< >|"                                           at 55
    tt-param.v_dat_fim  FORMAT "99/99/9999"  NO-LABEL COLON 70
    tt-param.v_log_ms   FORMAT "Sim/N∆o"              COLON 40
    v_des_opcao                                       colon 40
    tt-param.v_log_mensal                             colon 40 
    tt-param.v_log_horista                            colon 40 
    tt-param.v_log_semanal                            colon 40 
    tt-param.v_log_quinzenal                          colon 40 
    tt-param.v_log_tarefa                             colon 40 
    tt-param.v_log_diarista                           colon 40 
    tt-param.v_orig_func                              colon 40 format "Sim/N∆o" 
    tt-param.v_orig_temp                              colon 40 format "Sim/N∆o" 
    tt-param.v_orig_contratado                        colon 40 format "Sim/N∆o" 
    tt-param.v_orig_cooperado                         colon 40 format "Sim/N∆o"
    tt-param.v_orig_socio                             colon 40 format "Sim/N∆o" 
    tt-param.v_orig_estag                             colon 40 format "Sim/N∆o" 
    tt-param.v_orig_terc                              colon 40 format "Sim/N∆o" skip(2)
    v_des_classificacao                      no-label colon 15 skip(1)
    tt-param.desc-classifica                 no-label colon 40 skip(2)
    v_des_impressao                          no-label colon 15 skip(1)
    v_des_destino                                     colon 40
    tt-param.arquivo                                           FORMAT "x(40)" skip
    tt-param.usuario                                  colon 40 skip
    with stream-io side-labels no-attr-space no-box width 132 frame f-param.

form "ERRO CALENDARIO LOCALIDADE N∆o existem calend†rios gerados para:" at 18 skip
     v_cdn_turno                            at 18 skip
     v_cdn_turma                            at 18 skip
     v_cdn_localid                          at 18 skip
     v_cod_pais                             at 18 skip
     "Mes Refer: "                          at 18 
     v_mes_mens no-label 
     "/" 
     v_ano_mens no-label skip(2)
     with stream-io side-labels no-attr-space no-box width 132 frame f-erro-calen.
run utp/ut-trfrrp.p (input frame f-erro-calen:handle).

def var v_cdn_estab_mens  like func_ptoelet.cdn_estab        no-undo.
def var v_cdn_func_mens   like func_ptoelet.cdn_funcionario no-undo.
def var v_mes_men         as int format "99"                no-undo.
def var v_ano_men         as int format "9999"              no-undo.

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

{utp/ut-liter.i Gera_Movimento_Folha_de_Pagamento MDS R}
assign c-titulo-relat = return-value.

run pi-inicializar in prh_acomp (input return-value).

{utp/ut-liter.i Controle_de_Frequància MPE C}
assign c-sistema = return-value.

{utp/ut-liter.i IMPRESS«O MDS L}
assign v_des_impressao = return-value.

{utp/ut-liter.i CLASSIFICAÄ«O MDS L}
assign v_des_classificacao = return-value.

{utp/ut-liter.i SELEÄ«O MDS L}
assign v_des_selecao = return-value.

{utp/ut-liter.i parametroS MDS L}
assign v_des_parametro = return-value.

{utp/ut-liter.i Destino MDS L}
assign v_des_destino:label in frame f-param = return-value.

{utp/ut-liter.i Mensal MDS L}
assign tt-param.v_log_mensal:label in frame f-param = return-value.

{utp/ut-liter.i Horista MDS L}
assign tt-param.v_log_horista:label in frame f-param = return-value.

{utp/ut-liter.i Semanal MDS L}
assign tt-param.v_log_semanal:label in frame f-param = return-value.

{utp/ut-liter.i Quinzenal MDS L}
assign tt-param.v_log_quinzenal:label in frame f-param = return-value.

{utp/ut-liter.i Tarefa MDS L}
assign tt-param.v_log_tarefa:label in frame f-param = return-value.

{utp/ut-liter.i Diarista MDS L}
assign tt-param.v_log_diarista:label in frame f-param = return-value.

{utp/ut-liter.i Processo_Conclu°do_com_Sucesso MDS L}
assign v_des_mensagem = return-value.

{utp/ut-liter.i Opá∆o MPE L}
ASSIGN v_des_opcao:LABEL IN FRAME f-param = RETURN-VALUE.

{utp/ut-liter.i Trata_Màs_/_Ano_Seguinte MPE L}
ASSIGN tt-param.v_log_ms:LABEL IN FRAME f-param = RETURN-VALUE.

IF tt-param.i-ind-selec = 1 THEN DO:
   {utp/ut-liter.i Seleá∆o MPE R}
END.
ELSE DO:
   IF tt-param.i-ind-selec = 2 THEN DO:
      {utp/ut-liter.i Digitaá∆o MPE R}
   END.
   ELSE DO:
      {utp/ut-liter.i Folha_Complementar MPE R}
   END.
END.
ASSIGN v_des_opcao = RETURN-VALUE.

{utp/ut-liter.i Fechamento_Banco_Horas MPE L}
ASSIGN tt-param.log_fecha:LABEL IN FRAME f-param = RETURN-VALUE.

{utp/ut-liter.i Data Compensaá∆o MPE L}
ASSIGN tt-param.v_dat_ini:LABEL IN FRAME f-param = RETURN-VALUE.

run utp/ut-trfrrp.p (input frame f-param:handle).
run utp/ut-trfrrp.p (input frame f-erro:handle).
run utp/ut-trfrrp.p (input frame f-selecao:handle).
run utp/ut-trfrrp.p (input frame f-contrato:handle).

{include/i-rpcab.i}                                                                     
{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

{utp/ut-liter.i Sim/N∆o MCA R}
assign tt-param.v_log_mensal:format    in frame f-param = trim(return-value)
       tt-param.v_log_horista:format   in frame f-param = trim(return-value)
       tt-param.v_log_semanal:format   in frame f-param = trim(return-value)
       tt-param.v_log_quinzenal:format in frame f-param = trim(return-value)
       tt-param.v_log_tarefa:format    in frame f-param = trim(return-value)
       tt-param.v_log_diarista:format  in frame f-param = trim(return-value).

ASSIGN v_log_mes_seg = tt-param.v_log_ms.
/***************** monta temp-table com as categorias v†lidas ******************/
empty temp-table tt_categ no-error.

if tt-param.v_log_mensal then do:
   create tt_categ.
   assign tt_categ.cdn_categ_sal = 1.
end.
if tt-param.v_log_horista then do:
   create tt_categ.
   assign tt_categ.cdn_categ_sal = 2.
end.
if tt-param.v_log_semanal then do:
   create tt_categ.
   assign tt_categ.cdn_categ_sal = 3.
end.
if tt-param.v_log_quinzenal then do:
   create tt_categ.
   assign tt_categ.cdn_categ_sal = 4.
end.
if tt-param.v_log_tarefa then do:
   create tt_categ.
   assign tt_categ.cdn_categ_sal = 5.
end.
if tt-param.v_log_diarista then do:
   create tt_categ.
   assign tt_categ.cdn_categ_sal = 6.
end.

empty temp-table tt-orig-contratac-func no-error.

/*** Empresa ******/
if tt-param.v_orig_func = yes then do:
    create tt-orig-contratac-func.
    assign tt-orig-contratac-func.idi_orig_contratac_func = 1.
end.
/*** Tempor†rio ***/
if tt-param.v_orig_temp = yes then do:
    create tt-orig-contratac-func.
    assign tt-orig-contratac-func.idi_orig_contratac_func = 2.
end.

/*** Cooperado ****/
if tt-param.v_orig_cooperado = yes then do:
    create tt-orig-contratac-func.
    assign tt-orig-contratac-func.idi_orig_contratac_func = 3.
end.

/*** Contratado ***/
if tt-param.v_orig_contratado = yes then do:
    create tt-orig-contratac-func.
    assign tt-orig-contratac-func.idi_orig_contratac_func = 4.
end.
/*** S¢cio*********/
if tt-param.v_orig_socio = yes then do:
    create tt-orig-contratac-func.
    assign tt-orig-contratac-func.idi_orig_contratac_func = 5.
end.
/*** Estagi†rio ***/
if tt-param.v_orig_estag = yes then do:
    create tt-orig-contratac-func.
    assign tt-orig-contratac-func.idi_orig_contratac_func = 6.
end.
/*** Terceiro Ponto ***/
if tt-param.v_orig_terc = yes then do:
    create tt-orig-contratac-func.
    assign tt-orig-contratac-func.idi_orig_contratac_func = 7.
end.

/* Sarah - epc */
assign v_log_verifica_ocor = yes.

for each tt-epc exclusive-lock where tt-epc.cod-event = "INTEGRA":U:
   delete tt-epc. 
end.

create tt-epc.
assign tt-epc.cod-event     = "INTEGRA":U
       tt-epc.val-parameter = string(v_log_verifica_ocor).

{include/i-epc201.i "INTEGRA"}

if return-value = "ok-integra":U then 
   assign v_log_verifica_ocor = no.
else 
   assign v_log_verifica_ocor = yes.

/**/

{prghur/fpp/fp9200.i21 &tabela=func_ptoelet &funcionario=yes}

assign v_mes_empresa = tt-param.mes-ref
       v_ano_empresa = tt-param.ano-ref
       v_num_mes_seg = if tt-param.mes-ref = 12 
                       then 1 
                      ELSE tt-param.mes-ref + 1
       v_num_ano_seg = if tt-param.mes-ref = 12 
                       then tt-param.ano-ref + 1 
                       else tt-param.ano-ref
       d-ini-comp    = tt-param.v_dat_ini
       d-fim-comp    = tt-param.v_dat_fim.

if tt-param.v_log_ms then /* Trata màs seguinte para demitidos */
   assign tt-param.mes-ref = if tt-param.mes-ref = 12 then 1 else
                                tt-param.mes-ref + 1
          tt-param.ano-ref = if tt-param.mes-ref = 1 then tt-param.ano-ref + 1 else 
                                tt-param.ano-ref
          v_num_mes_seg    = tt-param.mes-ref
          v_num_ano_seg    = tt-param.ano-ref.

find bparam-pe where bparam-pe.cdn_empresa = tt-param.v_cdn_empres_usuar no-lock no-error.
if not avail bparam-pe then
   return.
find first param_geral_rh no-lock no-error.    

find mgcad.empresa no-lock where
     empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.

assign c-empresa    = empresa.razao-social
       dat-ini-mes  = date(tt-param.mes-ref,01,tt-param.ano-ref)
       dat-fim-mes  = dat-ini-mes + 32
       dat-fim-mes  = date(month(dat-fim-mes),01,year(dat-fim-mes)) - 1
       i-ult-diames = day(dat-fim-mes)
       c-mmaa-folha = string(month(dat-ini-mes),"99") + string(year(dat-ini-mes),"9999")
       
       v_turno_ant   = 0
       v_turma_ant   = 0
       v_pais_ant    = ""
       v_localid_ant = 0
       v_num_erro    = 0
       v_log_fecha   = tt-param.log_fecha.

/*-- Campo Logico Utiliza Recalculo Folha, da tabela param_geral_rh
     somente para as versoes anteriores a HR 2.08.
     A partir da vers∆o 2.08 busca esse campo na tabela param_empres_rh ---*/
&IF "{&dthrpyc_version}" <  "2.08" &then
    if avail param_geral_rh then 
       assign v_log_utiliz_folha_compl = param_geral_rh.log_utiliz_folha_compl.
&ELSE
    find param_empres_rh where
         param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-lock no-error.
    if avail param_empres_rh then 
       assign v_log_utiliz_folha_compl = param_empres_rh.log_utiliz_calc_compl.
&ENDIF

if not can-find(first tt_erro where
                      tt_erro.log_erro = yes) then do:
    /** Selecao ***/
    if tt-param.i-ind-selec = 1 then
       run pi-funcionario.
    

    /** Digitacao **/
    else do:
       for each tt-digita break by tt-digita.i-cdn-turno
                                by tt-digita.i-cdn_turma_trab
                                by tt-digita.c-pais
                                by tt-digita.i-localid:  
          find bfunc-ponto no-lock where
               bfunc-ponto.cdn_empresa     = tt-digita.v_cdn_empres_usuar and
               bfunc-ponto.cdn_estab       = tt-digita.i-es-codigo and
               bfunc-ponto.cdn_funcionario = tt-digita.i-fc-codigo no-error.  

          if avail bfunc-ponto then do:   

            if tt-param.v_log_ms then do: 
               if bfunc-ponto.dat_desligto_func = ? then do:
                  assign v_num_erro = v_num_erro + 1.   
                  {utp/ut-liter.i N∆o_encontra-se_desligado_funcion†rio mpe L}
                  create tt_erro.
                  assign tt_erro.num_erro = v_num_erro
                         tt_erro.des_erro = trim(return-value) + " " + string(bfunc-ponto.cdn_funcionario,"99999999") + ".".
                         tt_erro.log_erro = yes.
                  next.
               end.
               else do:
                  if bfunc-ponto.dat_desligto_func < date(tt-param.mes-ref,01,tt-param.ano-ref) then do:
                     assign v_num_erro = v_num_erro + 1.   
                     {utp/ut-liter.i Data_desligamento_inferior_ao_Màs/Ano_Folha_p/_Funcion†rio mpe L}
                     create tt_erro.
                     assign tt_erro.num_erro = v_num_erro
                            tt_erro.des_erro = trim(return-value) + " " + string(bfunc-ponto.cdn_funcionario,"99999999") + ".".
                            tt_erro.log_erro = yes.
                     next.
                  end.
               end.

               if (bfunc-ponto.dat_admis_func < date(tt-param.mes-ref,01,tt-param.ano-ref)) and
                   not can-find(first movto_ptoelet of bfunc-ponto where
                                movto_ptoelet.num_ano_refer_fp  = v_ano_empresa and
                                movto_ptoelet.num_mes_refer_fp  = v_mes_empresa and
                                movto_ptoelet.idi_tip_fp_calcul = 1 and
                                movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
                                movto_ptoelet.idi_tip_movto_ptoelet = 1) and
                   not can-find(categ_sal where
                                categ_sal.cdn_empresa   = bfunc-ponto.cdn_empresa and
                                (categ_sal.cdn_estab    = bfunc-ponto.cdn_estab or
                                 categ_sal.cdn_estab     = bfunc-ponto.cdn_estab_lotac_func_ptoelet) and
                                 categ_sal.cdn_categ_sal = bfunc-ponto.cdn_categ_sal and
                                 categ_sal.num_livre_1   = tt-param.mes-ref and
                                 categ_sal.num_livre_2   = tt-param.ano-ref) then do:
                  assign v_num_erro = v_num_erro + 1.
                  {utp/ut-liter.i N∆o_encontra-se_integrado_funcion†rio mpe L}
                  create tt_erro.
                  assign tt_erro.log_erro = yes
                         tt_erro.num_erro = v_num_erro
                         tt_erro.des_erro = trim(return-value) + " " + string(bfunc-ponto.cdn_funcionario,"99999999") + ". ".
                  next.
               END.
            end.
            else do:
               if bfunc-ponto.dat_desligto_func <> ? and
                  bfunc-ponto.dat_desligto_func < date(tt-param.mes-ref,01,tt-param.ano-ref) then
                  next.
            end.

            if not bfunc-ponto.log_consid_calc_ptoelet then
                next.

            IF SUBSTR(bfunc-ponto.cod_livre_1,029,01) = "S" AND
               NOT CAN-FIND(FIRST det_calend_func WHERE
                                  det_calend_func.cdn_empresa      = bfunc-ponto.cdn_empresa AND
                                  det_calend_func.cdn_estab        = bfunc-ponto.cdn_estab AND
                                  det_calend_func.cdn_funcionario  = bfunc-ponto.cdn_funcionario AND
                                  det_calend_func.dat_refer_calend = DATE(tt-param.mes-ref,01,tt-param.ano-ref)) THEN DO:

               assign v_cdn_estab_mens  = bfunc-ponto.cdn_estab       
                      v_cdn_func_mens   = bfunc-ponto.cdn_funcionario 
                      v_mes_men         = tt-param.mes-ref           
                      v_ano_men         = tt-param.ano-ref.

               disp v_cdn_estab_mens
                    v_cdn_func_mens
                    v_mes_men
                    v_ano_men
                    with frame f-erro-calen-func.
               down with frame f-erro-calen-func.

               assign v_num_erro = v_num_erro + 1.
               {utp/ut-liter.i Calendario_Funcionario_Inexistente mpe L}
               create tt_erro.
               assign tt_erro.num_erro = v_num_erro
                      tt_erro.des_erro = trim(return-value)
                      tt_erro.log_erro = yes.
               next.
            END.
          
            /*FO 1700.858 - MRS*/
            for each tt-epc where tt-epc.cod-event = "VALIDA_INT":
                delete tt-epc. 
            end.
            create tt-epc.
            assign tt-epc.cod-event = "VALIDA_INT"
                   tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa,"999") +
                                          string(bfunc-ponto.cdn_estab,"999") +
                                          string(bfunc-ponto.cdn_funcionario,"99999999").
            {include/i-epc201.i "VALIDA_INT"}
            /*FO 1700.858 - MRS*/

            /*** Funcionario com transferencia nao deve ser integrado Evandro ***/
            FOR EACH sit_afast_func fields(dat_inic_sit_afast
                                           cdn_sit_afast_func) of bfunc-ponto no-lock where
                     sit_afast_func.dat_inic_sit_afast = bfunc-ponto.dat_desligto:

                if can-find(first sit_afast no-lock where
                            sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func and
                            sit_afast.idi_signif_sit     = 4) then do:
                    
                    assign v_num_erro = v_num_erro + 1.
                    {utp/ut-liter.i Funcion†rio_transferido_! mpe L}
                    assign v_des_erro1 = trim(return-value).
                    create tt_erro.
                    assign tt_erro.num_erro  = v_num_erro
                           tt_erro.des_erro  = v_des_erro1
                           tt_erro.log_erro  = yes.
                    NEXT.
                end.
            END.
     
            /** FO 1392.772 - Engenharia Transferencia x Ponto Eletronico **/
            if substring(bfunc-ponto.cod_livre_1,47,1) = "S" then do:
                /** Integra informaá‰es do Funcion†rio Origem **/
                assign l_log_error = no.
                run pi-integra-func-origem (output l_log_error).
                if l_log_error then
                   next.
            end.
            /******** Chamada EPC - Evandro **********/
            for each tt-epc where tt-epc.cod-event = "CIENTE":
                delete tt-epc. 
            end.
            create tt-epc.
            assign tt-epc.cod-event = "CIENTE"
                   tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa)     + ";" +
                                          string(bfunc-ponto.cdn_estab)       + ";" +
                                          string(bfunc-ponto.cdn_funcionario) + ";" +
                                          string(tt-param.ano-ref)            + ";" +
                                          string(tt-param.mes-ref)            + ";". 
            {include/i-epc201.i "CIENTE"}
            
            IF RETURN-VALUE = "N-OK" THEN
               NEXT.

            find bcatponto no-lock where
                 bcatponto.cdn_clas_func = bfunc-ponto.cdn_clas_func and
                 bcatponto.cdn_empresa   = bfunc-ponto.cdn_empresa   and
                 bcatponto.cdn_estab     = bfunc-ponto.cdn_estab     and
                 bcatponto.cdn_categ_sal = bfunc-ponto.cdn_categ_sal no-error.
            if avail bcatponto then do:

                find first funcionario of bfunc-ponto no-lock no-error.
                find first event_integr no-lock where
                    event_integr.cdn_empresa    = funcionario.cdn_empresa   and
                    event_integr.cdn_estab      = funcionario.cdn_estab     and
                    event_integr.cdn_categ_sal  = funcionario.cdn_categ_sal and
                    event_integr.idi_tip_func   = funcionario.idi_tip_func  no-error.
                if avail event_integr then do:
                    assign i-ev-trbdiu       = event_integr.cdn_event_hrs_diurno
                           i-ev-trbnot       = event_integr.cdn_event_hrs_notur
                           i-ev-dsrdiu       = event_integr.cdn_event_dsr_diurno
                           i-ev-dsrnot       = event_integr.cdn_event_dsr_notur
                           i-ev-dsrdiuper    = event_integr.cdn_event_desc_dsr_diurno
                           i-ev-dsrnotper    = event_integr.cdn_event_desc_dsr_notur
                           i-ev-desc-hrs-diu = event_integr.cdn_event_falta_diurno
                           i-ev-desc-hrs-not = event_integr.cdn_event_falta_notur
                           i-ev-hrs-dif-diu  = event_integr.cdn_event_difer_diurno 
                           i-ev-hrs-dif-not  = event_integr.cdn_event_difer_notur 
                           i-ev-ferdiuper    = i-ev-dsrdiuper
                           i-ev-fernotper    = i-ev-dsrnotper

                           l-adc-evnot = no. /* cris */

                    find first event_fp no-lock WHERE
                               event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento AND
                               event_fp.cdn_event_fp = i-ev-trbnot no-error.
                    if event_fp.cdn_efp_suplem_hrs_notur > ""/*0*/ then 
                        assign i-ev-supnot = event_fp.cdn_efp_suplem_hrs_notur
                               l-sup-evnot = yes.
                    else 
                        assign i-ev-supnot = ""
                               l-sup-evnot = no.
                    if event_fp.idi_tip_adc_notur <> 3 then  /* cris turno m¢vel */
                        assign l-adc-evnot = yes.

                    if l-sup-evnot = yes then do:
                       if not can-find(first event_fp no-lock where
                                       event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento AND
                                       event_fp.cdn_event_fp = i-ev-supnot) then do:
                           assign v_num_erro = v_num_erro + 1.
                           {utp/ut-liter.i N∆o_encontrado_Evento_de_Suplementaá∆o_Noturna_= mpe L}
                           assign v_des_erro1 = trim(return-value) + " " + string(i-ev-supnot).
                           create tt_erro.
                           assign tt_erro.num_erro = v_num_erro
                                  tt_erro.des_erro = v_des_erro1
                                  tt_erro.log_erro = yes.
                       end.
                    end.
                end.
                else do:
                    assign v_num_erro = v_num_erro + 1.
                    {utp/ut-liter.i Eventos_Integracao_nao_cadastrados_para_o_Estab. mpe L}
                    assign v_des_erro1 = trim(return-value) + " " + string(funcionario.cdn_estab) + " ".
                    {utp/ut-liter.i Categoria mpe L}
                    assign v_des_erro1 = v_des_erro1 + " " + trim(return-value) + " " + string(funcionario.cdn_categ_sal) + " ".
                    {utp/ut-liter.i e_Tipo_Func. mpe L}
                    assign v_des_erro1 = v_des_erro1 + " " + trim(return-value) + " " + {database/intm/i01tm075.i 04 funcionario.idi_tip_func}.

                    create tt_erro.
                    assign tt_erro.num_erro = v_num_erro
                           tt_erro.des_erro = v_des_erro1
                           tt_erro.log_erro = yes.
                    next.
                end.

               find bcontrole-catponto of bcatponto no-lock where
                    bcontrole-catponto.num_mes_primei_calc_realzdo = tt-param.mes-ref and
                    bcontrole-catponto.num_ano_primei_calc_ptoelet = tt-param.ano-ref no-error.
               if avail bcontrole-catponto then do:
                  if can-find(first movto_ptoelet of bfunc-ponto where
                              movto_ptoelet.num_ano_refer_fp  = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                              movto_ptoelet.num_mes_refer_fp  = bcontrole-catponto.num_mes_primei_calc_realzdo and
                              movto_ptoelet.idi_tip_fp_calcul = 1 and
                              movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
                              movto_ptoelet.idi_tip_movto_ptoelet = 1 and
                              movto_ptoelet.val_livre_1 = 0) then do:
                     assign v_num_erro = v_num_erro + 1.
                     {utp/ut-liter.i Per°odo_Ponto_do_Funcionario mpe L}
                     assign v_des_erro1 = trim(return-value) + " " + string(bfunc-ponto.cdn_funcionario) + " ".
                     {utp/ut-liter.i j†_integrado mpe L}
                     assign v_des_erro1 = v_des_erro1 + " " + trim(return-value).
                     create tt_erro.
                     assign tt_erro.num_erro = v_num_erro
                            tt_erro.des_erro = v_des_erro1
                            tt_erro.log_erro = yes.
                     next.
                  end.

                  if tt-param.i-ind-selec = 3 then do:
                     &if "{&dthrpyc_version}" >= "2.05" &then
                         if v_log_utiliz_folha_compl then do:
                            if bcatponto.cdn_estab = "" then 
                               find funcionario of bcatponto no-lock no-error.
                            if not can-find(last movto_ptoelet no-lock where
                                            movto_ptoelet.cdn_empresa = funcionario.cdn_empresa and
                                            movto_ptoelet.cdn_estab   = funcionario.cdn_estab   and
                                            movto_ptoelet.cdn_funcionario   = funcionario.cdn_funcionario               and
                                            movto_ptoelet.num_mes_refer_fp  = bcontrole-catponto.num_mes_primei_calc_realzdo and
                                            movto_ptoelet.num_ano_refer_fp  = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                                            movto_ptoelet.idi_tip_fp_calcul           = 1 and
                                            movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
                                            movto_ptoelet.idi_tip_movto_ptoelet = 1) then do:
                                find func_calc_compl of funcionario no-lock no-error.
                                if avail func_calc_compl then do:
                                    if can-find(movto_calcul_func of funcionario where
                                                movto_calcul_func.num_ano_refer_fp         = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                                                movto_calcul_func.num_mes_refer_fp         = bcontrole-catponto.num_mes_primei_calc_realzdo and
                                                movto_calcul_func.idi_tip_fp               = 1                                        and
                                                movto_calcul_func.qti_parc_habilit_calc_fp = 2                                        and
                                                movto_calcul_func.num_seq_movto_calcul_fp  = 0) then do: /** funcionario com calculo de folha complementar efetuado **/

                                        assign v_num_erro = v_num_erro + 1.
                                        run utp/ut-msgs.p (input "msg", input 19397, input "").
                                        create tt_erro.
                                        assign tt_erro.num_erro = v_num_erro
                                               tt_erro.des_erro = trim(return-value) + " " + 
                                               string(bfunc-ponto.cdn_funcionario,"99999999")
                                               tt_erro.log_erro = yes.
                                        next.
                                    end.
                                end.
                            end.   
                         end.   
                     &endif
                  end.

                  empty temp-table tt-calendar no-error.

                  /* carregar a tt-calendar com todos os calendarios dos turnos que
                     o funcionario possar estar lotado nos meses do periodo de ponto */
                  find first bcontrole-funcponto exclusive-lock where
                             bcontrole-funcponto.cdn_empresa     = bfunc-ponto.cdn_empresa  and
                             bcontrole-funcponto.cdn_estab       = bfunc-ponto.cdn_estab      and                      
                             bcontrole-funcponto.cdn_funcionario = bfunc-ponto.cdn_funcionario  and
                             bcontrole-funcponto.num_ano_primei_calc_ptoelet = tt-param.ano-ref and
                             bcontrole-funcponto.num_mes_primei_calc_realzdo = tt-param.mes-ref no-error.
                  if avail bcontrole-funcponto then do:
                     assign v_dat_aux_ini = date(month(bcontrole-funcponto.dat_inic_period_apurac_pto_mes),
                                                 01,YEAR(bcontrole-funcponto.dat_inic_period_apurac_pto_mes)).

                     RUN prghur/fpp/fpapi002.p (INPUT MONTH(bcontrole-funcponto.dat_term_period_apurac_pto_mes),
                                                INPUT YEAR(bcontrole-funcponto.dat_term_period_apurac_pto_mes),
                                                INPUT-OUTPUT v_dat_aux_fim).
                     /* verifica se o funcionario possui calendario - gabriela */
                     RUN pi_procura_calend_func (INPUT v_dat_aux_ini,
                                                 INPUT v_dat_aux_fim).
                  END.
                  ELSE 
                     ASSIGN v_dat_aux_ini = dat-ini-mes
                            v_dat_aux_fim = dat-fim-mes.

                  FIND FIRST tt-calendar NO-ERROR.

                  IF NOT AVAIL tt-calendar THEN
                     /* se nao tem calendario por funcionario, ler o calendario da localidade */
                     RUN pi_grava_tt_calendar (INPUT v_dat_aux_ini,
                                               INPUT v_dat_aux_fim).
                      
                  FIND funcionario OF bfunc-ponto NO-LOCK NO-ERROR.
                  IF AVAIL funcionario THEN
                     IF funcionario.dat_admis_transf_func = DATE(tt-param.mes-ref,01,tt-param.ano-ref) AND
                        CAN-FIND(FIRST tt-calendar) AND
                        NOT CAN-FIND(tt-calendar WHERE
                                     tt-calendar.dat_refer_calend = bcontrole-funcponto.dat_inic_period_apurac_pto_mes) THEN DO:
                        ASSIGN i-mes-ant-transf = IF MONTH(funcionario.dat_admis_transf_func) = 1
                                                  THEN 12
                                                  ELSE MONTH(funcionario.dat_admis_transf_func) - 1
                               i-ano-ant-transf = IF MONTH(funcionario.dat_admis_transf_func) = 1
                                                  THEN YEAR(funcionario.dat_admis_transf_func) - 1
                                                  ELSE YEAR(funcionario.dat_admis_transf_func).
                         
                        RUN pi_grava_tt_calendar (INPUT DATE(i-mes-ant-transf,01,i-ano-ant-transf),
                                                  INPUT funcionario.dat_admis_transf_func).
                     END.

                  RUN pi-monta-tt-calend.
                  if avail bturnotrb and avail bturmatrb then do:
                      {prghur/pep/pe4000.i}
                  end.
               end.
               else do:
                  assign v_num_erro = v_num_erro + 1.
                  {utp/ut-liter.i Habilitaá∆o_Ponto_Inexistente mpe L}
                  create tt_erro.
                  assign tt_erro.num_erro = v_num_erro
                         tt_erro.des_erro = trim(return-value)
                         tt_erro.log_erro = yes.
                  next.
               end.
            end.


            if can-find(first sit_calc_ptoelet_func where
                        sit_calc_ptoelet_func.cdn_clas_func = bcatponto.cdn_clas_func and
                        sit_calc_ptoelet_func.cdn_empresa   = bcatponto.cdn_empresa   and
                        (sit_calc_ptoelet_func.cdn_estab    = bcatponto.cdn_estab     or
                        sit_calc_ptoelet_func.cdn_estab     = "")                     and
                        sit_calc_ptoelet_func.cdn_categ_sal = bcatponto.cdn_categ_sal and
                        sit_calc_ptoelet_func.num_ano_primei_calc_ptoelet = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                        sit_calc_ptoelet_func.num_mes_primei_calc_realzdo = bcontrole-catponto.num_mes_primei_calc_realzdo and
                        sit_calc_ptoelet_func.idi_sit_ptoelet_mes_func = 1) then
               assign i-st-atu = 1.
            else do:
               if can-find(first sit_calc_ptoelet_func where
                           sit_calc_ptoelet_func.cdn_clas_func = bcatponto.cdn_clas_func and
                           sit_calc_ptoelet_func.cdn_empresa   = bcatponto.cdn_empresa   and
                           (sit_calc_ptoelet_func.cdn_estab    = bcatponto.cdn_estab     or
                           sit_calc_ptoelet_func.cdn_estab     = "")                     and
                           sit_calc_ptoelet_func.cdn_categ_sal = bcatponto.cdn_categ_sal and
                           sit_calc_ptoelet_func.num_ano_primei_calc_ptoelet = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                           sit_calc_ptoelet_func.num_mes_primei_calc_realzdo = bcontrole-catponto.num_mes_primei_calc_realzdo and
                           sit_calc_ptoelet_func.idi_sit_ptoelet_mes_func = 2) then
                  assign i-st-atu = 2.
               else 
                  assign i-st-atu = 3.
            end.                                                       
            find sit_calc_ptoelet_categ exclusive-lock where
                 rowid(sit_calc_ptoelet_categ) = rowid(bcontrole-catponto) no-error.
            assign sit_calc_ptoelet_categ.idi_sit_ptoelet_categ = i-st-atu.

            /** Folha Complementar **/
            IF v_log_utiliz_folha_compl AND tt-param.i-ind-selec = 3 THEN DO: 
               FIND FIRST func_calc_compl OF bfunc-ponto EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL func_calc_compl THEN 
                  ASSIGN func_calc_compl.log_integr_ptoelet = YES.
            END.
         end. /* bfunc */
       end. /* tt-digita */
    end. /* digitaá∆o */
end. /* n∆o possui erro */

/* Toni EPC - Senac */
for each tt-epc exclusive-lock
   where tt-epc.cod-event = "instrutor_B":U :
   delete tt-epc. 
end.
create tt-epc.
assign tt-epc.cod-event     = "instrutor_B":U.

{include/i-epc201.i "instrutor_B"}
/* Toni EPC - Senac */


find first tt_erro no-lock no-error.
if avail tt_erro then
   assign v_log_imprime = yes.
for each tt_erro where
         tt_erro.log_erro = yes break by tt_erro.num_erro:
   disp tt_erro.num_erro
        tt_erro.des_erro with frame f-erro.
   down with frame f-erro.
end.
assign v_num_movtos = 0.
for each tt_erro where
         tt_erro.log_erro = no:
   assign v_num_movtos = v_num_movtos + 1.
end.
IF v_num_movtos > 0 THEN DO:
    /*EPC chamada fppl10 MRS ********************************************/
    for each tt-epc where tt-epc.cod-event = "epc_pe4000_fppl10_mrs":
        delete tt-epc.
    end.
    
    create tt-epc.
    assign tt-epc.cod-event = "epc_pe4000_fppl10_mrs".
    
    {include/i-epc201.i "epc_pe4000_fppl10_mrs"}
    
        DEF VAR c_arquivo AS CHAR NO-UNDO.
        DEF VAR r_msg     AS CHAR NO-UNDO.
        DEF VAR c-arquivo AS CHAR NO-UNDO.
        DEF VAR i-cont    AS INT  NO-UNDO.  
        

        ASSIGN c_arquivo = SUBSTRING(RETURN-VALUE, 26)
               r_msg     = SUBSTRING(RETURN-VALUE, 1, 24)
               i-cont    = INT(SUBSTRING(RETURN-VALUE, 25, 1)).
    
        IF r_msg = "epc_pe4000_fppl10_mrs_OK" THEN DO:   

            ASSIGN v_num_movtos = v_num_movtos + i-cont.
            IF tt-param.destino = 3 THEN DO:            
                DEF VAR cEditor  AS CHAR   NO-UNDO.
                PROCEDURE WinExec EXTERNAL "kernel32.dll":
                  DEF INPUT PARAM prg_name AS CHARACTER.
                  DEF INPUT PARAM prg_style AS SHORT.
                END PROCEDURE.
                ASSIGN cEditor = OS-GETENV("windir") + "~\notepad.exe".
                run WinExec (input cEditor + chr(32) + c_arquivo,
                             input 1).
            END.
        
        END.
    
    
    /************** fim EPC ********************************************/
END.
if v_num_movtos > 0 then do:
   {utp/ut-liter.i Movimentos_Integrados: mpe L}
   assign v_des_ger = trim(return-value).
   put skip(02)
       v_des_ger    at 25
       v_num_movtos at 55.
end.
else do:
   if v_log_imprime then do:
      {utp/ut-liter.i Movimentos_n∆o_Integrados mpe L}
      assign v_des_ger = trim(return-value).
      put skip(02)
          v_des_ger at 25.
   end.
   else do:
      {utp/ut-liter.i TÇrmino_do_Processo mpe L}
      assign v_des_ger = trim(return-value).
      put skip(02)
          v_des_ger at 25.
   end.
end.

if v_log_imprime = yes then
   page.
&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {varinc/var00002.i 04 tt-param.destino}.
    run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                        INPUT "",
                        INPUT "").
    ASSIGN  v_des_destino = RETURN-VALUE.
&else
    ASSIGN v_des_destino = {varinc/var00002.i 04 tt-param.destino}.
&endif

if tt-param.parametro = yes then do:
   disp v_des_mensagem
        v_des_selecao
        tt-param.i-es-ini
        tt-param.i-es-fim
        tt-param.i-fc-ini
        tt-param.i-fc-fim
        tt-param.i-turno-ini
        tt-param.i-turno-fim
        tt-param.v_cdn_prestdor_ini
        tt-param.v_cdn_prestdor_fim
        tt-param.c-ccusto-ini
        tt-param.c-ccusto-fim
        tt-param.i-classe-ini
        tt-param.i-classe-fim 
        WITH FRAME f-selecao.

   IF v_log_folha_educnal THEN
      DISP tt-param.i-ctr-ini
           tt-param.i-ctr-fim WITH FRAME f-contrato.

   DISP v_des_parametro
        tt-param.log_fecha   
        tt-param.v_dat_ini   
        tt-param.v_dat_fim   
        tt-param.v_log_ms    
        v_des_opcao          
        tt-param.v_log_mensal
        tt-param.v_log_horista
        tt-param.v_log_semanal
        tt-param.v_log_quinzenal
        tt-param.v_log_tarefa 
        tt-param.v_log_diarista
        tt-param.v_orig_func
        tt-param.v_orig_temp
        tt-param.v_orig_contratado
        tt-param.v_orig_cooperado
        tt-param.v_orig_socio
        tt-param.v_orig_estag
        tt-param.v_orig_terc
        v_des_classificacao
        tt-param.desc-classifica
        v_des_impressao        
        v_des_destino          
        tt-param.arquivo       
        tt-param.usuario with frame f-param.
end.
else.

{include/i-rpclo.i}

run pi-finalizar in prh_acomp.

RETURN "OK".

PROCEDURE pi-monta-tt-calend:

DEF VAR v_log_troca_turno AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR v_dat_proc        AS DATE               NO-UNDO.
DEF VAR v_log_emprest_1   AS LOGICAL            NO-UNDO.
DEF VAR v_log_achou       AS LOGICAL            NO-UNDO.

   if v_turno_ant   <> bfunc-ponto.cdn_turno_trab   or
      v_turma_ant   <> bfunc-ponto.cdn_turma_trab   or
      v_pais_ant    <> bfunc-ponto.cod_pais         or
      v_localid_ant <> bfunc-ponto.cdn_localidade then do:
      assign v_turno_ant   = bfunc-ponto.cdn_turno_trab 
             v_turma_ant   = bfunc-ponto.cdn_turma_trab
             v_pais_ant    = bfunc-ponto.cod_pais
             v_localid_ant = bfunc-ponto.cdn_localidade.
      find bturnotrb of bfunc-ponto no-lock no-error.
      find bturmatrb of bfunc-ponto no-lock no-error.
      /** monta temp-table com valores padrao para semanas e mes ***/
      empty temp-table tt-val-semana no-error.

      if avail bturnotrb and avail bturmatrb then do:
         /**** mes anterior *******************************************************/
         assign i-ano-ant = year(date(tt-param.mes-ref,01,tt-param.ano-ref) - 1)
                i-mes-ant = month(date(tt-param.mes-ref,01,tt-param.ano-ref) - 1).
         find bcalentur where
              bcalentur.cdn_turno_trab   = bturnotrb.cdn_turno_trab and
              bcalentur.cdn_turma_trab   = bturmatrb.cdn_turma_trab and
              bcalentur.cod_pais         = bfunc-ponto.cod_pais AND
              bcalentur.cdn_localidade   = bfunc-ponto.cdn_localidade AND
              bcalentur.dat_refer_calend = (dat-ini-mes - 1) no-lock no-error.

         if not available bcalentur THEN DO:
            if bcatponto.num_livre_1 = 1 or
               bcatponto.num_livre_1 = 3 then do:  

               ASSIGN v_dat_proc = dat-ini-mes - 1.
               {prghur/pep/pe9995.i v_dat_proc}

               FIND bcalentur NO-LOCK WHERE
                    bcalentur.cdn_turno_trab   = v_cdn_turno AND
                    bcalentur.cdn_turma_trab   = v_cdn_turma AND
                    bcalentur.cod_pais         = v_cod_pais AND
                    bcalentur.cdn_localidade   = v_cdn_localid AND 
                    bcalentur.dat_refer_calend = v_dat_proc NO-ERROR.
               IF NOT AVAIL bcalentur THEN
                  NEXT.
               ELSE
                   ASSIGN v_log_troca_turno = YES.
            END.
            ELSE
              next.
         END.

         /************ FECHA POR M“S DE REFER“NCIA *******************************/
         if  bcatponto.num_livre_1 = 1 or
             bcatponto.num_livre_1 = 3 then do:  
            assign i-semana = 0.
            do i-semana = 1 to bcalentur.num_sema_dia_calend:
               IF v_log_troca_turno THEN DO:
                  ASSIGN v_dat_proc = dat-ini-mes - 1.
                  {prghur/pep/pe9995.i v_dat_proc}

               find last det_calend_turma_localid where
                       det_calend_turma_localid.cdn_turno_trab      = v_cdn_turno and
                       det_calend_turma_localid.cdn_turma_trab      = v_cdn_turma and
                       det_calend_turma_localid.dat_refer_calend   >= date(i-mes-ant,01,i-ano-ant) and
                       det_calend_turma_localid.dat_refer_calend   <= v_dat_proc and
                       det_calend_turma_localid.num_sema_dia_calend = i-semana and
                       det_calend_turma_localid.cod_pais            = v_cod_pais and
                       det_calend_turma_localid.cdn_localidade      = v_cdn_localid no-lock no-error.
               END.
               ELSE 
                   find last det_calend_turma_localid where
                        det_calend_turma_localid.cdn_turno_trab      = bturnotrb.cdn_turno_trab and
                        det_calend_turma_localid.cdn_turma_trab      = bturmatrb.cdn_turma_trab and
                        det_calend_turma_localid.dat_refer_calend   >= date(i-mes-ant,01,i-ano-ant) and
                        det_calend_turma_localid.dat_refer_calend   <= (dat-ini-mes - 1) and
                        det_calend_turma_localid.num_sema_dia_calend = i-semana and
                        det_calend_turma_localid.cod_pais            = bfunc-ponto.cod_pais and
                        det_calend_turma_localid.cdn_localidade      = bfunc-ponto.cdn_localidade no-lock no-error.

               if (det_calend_turma_localid.qti_dsr_sema = 0 and 
                   det_calend_turma_localid.qti_fer_sema = 0) or
                   det_calend_turma_localid.dat_refer_calend < 
                   bcontrole-catponto.dat_inic_period_apurac_pto_mes then 
                   next.

               create tt-val-semana.
               assign tt-val-semana.mes-refer       = "ant"
                      tt-val-semana.cod_pais        = bfunc-ponto.cod_pais
                      tt-val-semana.cdn_localidade  = bfunc-ponto.cdn_localidade
                      tt-val-semana.num-semana      = i-semana         
                      tt-val-semana.dat-fim         = det_calend_turma_localid.dat_refer_calend
                      tt-val-semana.qtd-hrs-diu     = det_calend_turma_localid.qti_padr_hrs_diurno_sema 
                      tt-val-semana.qtd-hrs-not     = det_calend_turma_localid.qti_padr_hrs_notur_sema   
                      tt-val-semana.qtd-hrs-diu-dsr = det_calend_turma_localid.qti_hrs_dsr_diurno_sema 
                      tt-val-semana.qtd-hrs-diu-fer = det_calend_turma_localid.qti_hrs_fer_diurno_sema
                      tt-val-semana.qtd-hrs-not-dsr = det_calend_turma_localid.qti_hrs_dsr_notur_sema 
                      tt-val-semana.qtd-hrs-not-fer = det_calend_turma_localid.qti_hrs_fer_notur_sema   
                      tt-val-semana.qtd-hco         = det_calend_turma_localid.qti_hrs_compens_sema
                      tt-val-semana.qtd-hpc         = det_calend_turma_localid.qti_hrs_compcao_sema 
                      tt-val-semana.qtd-trb-sem     = det_calend_turma_localid.qti_dias_trabdo_sema       
                      tt-val-semana.qtd-dsr-sem     = det_calend_turma_localid.qti_dsr_sema
                      tt-val-semana.qtd-fer-sem     = det_calend_turma_localid.qti_fer_sema.
            end. /* do */

            /**** mes processo *******************************************************/  
            find bcalentur where
                 bcalentur.cdn_turno_trab   = bturnotrb.cdn_turno_trab and
                 bcalentur.cdn_turma_trab   = bturmatrb.cdn_turma_trab and
                 bcalentur.cod_pais         = bfunc-ponto.cod_pais AND
                 bcalentur.cdn_localidade   = bfunc-ponto.cdn_localidade AND
                 bcalentur.dat_refer_calend = dat-fim-mes no-lock no-error.
            if not available bcalentur then 
               next.
            assign i-semana = 0.
            do i-semana = 1 to bcalentur.num_sema_dia_calend:
               find last det_calend_turma_localid where
                    det_calend_turma_localid.cdn_turno_trab      = bturnotrb.cdn_turno_trab and
                    det_calend_turma_localid.cdn_turma_trab      = bturmatrb.cdn_turma_trab and
                    det_calend_turma_localid.dat_refer_calend   >= dat-ini-mes and
                    det_calend_turma_localid.dat_refer_calend   <= dat-fim-mes and
                    det_calend_turma_localid.num_sema_dia_calend = i-semana and
                    det_calend_turma_localid.cod_pais            = bfunc-ponto.cod_pais and
                    det_calend_turma_localid.cdn_localidade      = bfunc-ponto.cdn_localidade no-lock no-error.

               ASSIGN v_log_fer_mes_atu = NO.
               IF i-semana = 1 THEN DO:
                  IF CAN-FIND(FIRST det_calend_turma_localid WHERE
                                    det_calend_turma_localid.cdn_turno_trab = bturnotrb.cdn_turno_trab AND
                                    det_calend_turma_localid.cdn_turma_trab = bturmatrb.cdn_turma_trab AND
                                    det_calend_turma_localid.cod_pais       = bfunc-ponto.cod_pais AND
                                    det_calend_turma_localid.cdn_localidade = bfunc-ponto.cdn_localidade AND
                                    det_calend_turma_localid.dat_refer_calend   <= det_calend_turma_localid.dat_refer_calend AND
                                    det_calend_turma_localid.dat_refer_calend   >= dat-ini-mes AND
                                    det_calend_turma_localid.num_sema_dia_calend = i-semana AND
                                    det_calend_turma_localid.idi_sit_dia_trab    = 4) THEN
                  ASSIGN v_log_fer_mes_atu = YES.
               END.
               create tt-val-semana.
               assign tt-val-semana.mes-refer       = "ref"
                      tt-val-semana.cod_pais        = bfunc-ponto.cod_pais
                      tt-val-semana.cdn_localidade  = bfunc-ponto.cdn_localidade
                      tt-val-semana.num-semana      = i-semana         
                      tt-val-semana.dat-fim         = det_calend_turma_localid.dat_refer_calend
                      tt-val-semana.qtd-hrs-diu     = det_calend_turma_localid.qti_padr_hrs_diurno_sema 
                      tt-val-semana.qtd-hrs-not     = det_calend_turma_localid.qti_padr_hrs_notur_sema   
                      tt-val-semana.qtd-hrs-diu-dsr = det_calend_turma_localid.qti_hrs_dsr_diurno_sema 
                      tt-val-semana.qtd-hrs-diu-fer = IF i-semana = 1 then
                                                         IF v_log_fer_mes_atu = YES THEN
                                                            det_calend_turma_localid.qti_hrs_fer_diurno_sema
                                                         ELSE 0
                                                      ELSE det_calend_turma_localid.qti_hrs_fer_diurno_sema
                      tt-val-semana.qtd-hrs-not-dsr = det_calend_turma_localid.qti_hrs_dsr_notur_sema 
                      tt-val-semana.qtd-hrs-not-fer = IF i-semana = 1 then
                                                         IF v_log_fer_mes_atu = YES THEN
                                                            det_calend_turma_localid.qti_hrs_fer_notur_sema
                                                         ELSE 0
                                                      ELSE det_calend_turma_localid.qti_hrs_fer_notur_sema
                      tt-val-semana.qtd-hco         = det_calend_turma_localid.qti_hrs_compens_sema
                      tt-val-semana.qtd-hpc         = det_calend_turma_localid.qti_hrs_compcao_sema 
                      tt-val-semana.qtd-trb-sem     = det_calend_turma_localid.qti_dias_trabdo_sema       
                      tt-val-semana.qtd-dsr-sem     = det_calend_turma_localid.qti_dsr_sema
                      tt-val-semana.qtd-fer-sem     = det_calend_turma_localid.qti_fer_sema.
            end. /* do */
         end. /* fecha por màs ref */

         /************ FECHA POR PER÷ODO PONTO *******************************/
         else do:
            assign i-semana          = 0
                   v_qti_hrs_diu     = 0
                   v_qti_hrs_not     = 0
                   v_qti_hrs_diu_dsr = 0
                   v_qti_hrs_not_dsr = 0
                   v_qti_hrs_diu_fer = 0
                   v_qti_hrs_not_fer = 0
                   v_qti_dias_trab   = 0
                   v_qti_dias_dsr    = 0
                   v_qti_dias_fer    = 0
                   v_log_atualiz_tt  = no.
            for each det_calend_turma_localid no-lock where
                     det_calend_turma_localid.cdn_turno_trab      = bturnotrb.cdn_turno_trab and
                     det_calend_turma_localid.cdn_turma_trab      = bturmatrb.cdn_turma_trab and
                     det_calend_turma_localid.dat_refer_calend   >= bcontrole-catponto.dat_inic_period_apurac_pto_mes and
                     det_calend_turma_localid.dat_refer_calend   <= bcontrole-catponto.dat_term_period_apurac_pto_mes and
                     det_calend_turma_localid.cod_pais            = bfunc-ponto.cod_pais and
                     det_calend_turma_localid.cdn_localidade      = bfunc-ponto.cdn_localidade:

               assign v_log_atualiz_tt = no.
               if det_calend_turma_localid.idi_sit_dia_trab = 1 then
                  assign v_qti_hrs_diu   = v_qti_hrs_diu + det_calend_turma_localid.qti_padr_hrs_diurno 
                         v_qti_hrs_not   = v_qti_hrs_not + det_calend_turma_localid.qti_padr_hrs_notur
                         v_qti_dias_trab = v_qti_dias_trab + 1.

               if det_calend_turma_localid.idi_sit_dia_trab = 4 then
                  assign v_qti_hrs_diu_fer = v_qti_hrs_diu_fer + det_calend_turma_localid.qti_padr_hrs_diurno 
                         v_qti_hrs_not_fer = v_qti_hrs_not_fer + det_calend_turma_localid.qti_padr_hrs_notur
                         v_qti_dias_fer    = v_qti_dias_fer + 1.

               if det_calend_turma_localid.idi_sit_dia_trab = 3 then do:
                  assign v_qti_hrs_diu_dsr = v_qti_hrs_diu_dsr + det_calend_turma_localid.qti_padr_hrs_diurno 
                         v_qti_hrs_not_dsr = v_qti_hrs_not_dsr + det_calend_turma_localid.qti_padr_hrs_notur
                         v_qti_dias_dsr    = v_qti_dias_dsr + 1.
                  if can-find(first bdet_calend_localid where
                              bdet_calend_localid.cdn_turno_trab   = det_calend_turma_localid.cdn_turno_trab   and
                              bdet_calend_localid.cdn_turma_trab   = det_calend_turma_localid.cdn_turma_trab   and
                              bdet_calend_localid.cod_pais         = det_calend_turma_localid.cod_pais         and
                              bdet_calend_localid.cdn_localidade   = det_calend_turma_localid.cdn_localidade   and
                              bdet_calend_localid.dat_refer_calend = (det_calend_turma_localid.dat_refer_calend + 1) and
                              bdet_calend_localid.idi_sit_dia_trab = 1) then do:
                     assign i-semana         = i-semana + 1
                            v_log_atualiz_tt = yes.
                     create tt-val-semana.
                     assign tt-val-semana.mes-refer       = "ref"
                            tt-val-semana.cod_pais        = det_calend_turma_localid.cod_pais
                            tt-val-semana.cdn_localidade  = det_calend_turma_localid.cdn_localidade
                            tt-val-semana.num-semana      = i-semana         
                            tt-val-semana.dat-fim         = det_calend_turma_localid.dat_refer_calend
                            tt-val-semana.qtd-hrs-diu     = v_qti_hrs_diu 
                            tt-val-semana.qtd-hrs-not     = v_qti_hrs_not   
                            tt-val-semana.qtd-hrs-diu-dsr = v_qti_hrs_diu_dsr 
                            tt-val-semana.qtd-hrs-diu-fer = v_qti_hrs_diu_fer
                            tt-val-semana.qtd-hrs-not-dsr = v_qti_hrs_not_dsr 
                            tt-val-semana.qtd-hrs-not-fer = v_qti_hrs_not_fer
                            tt-val-semana.qtd-trb-sem     = v_qti_dias_trab
                            tt-val-semana.qtd-dsr-sem     = v_qti_dias_dsr
                            tt-val-semana.qtd-fer-sem     = v_qti_dias_fer.
                     assign v_qti_hrs_diu     = 0
                            v_qti_hrs_not     = 0
                            v_qti_hrs_diu_dsr = 0
                            v_qti_hrs_not_dsr = 0
                            v_qti_hrs_diu_fer = 0
                            v_qti_hrs_not_fer = 0
                            v_qti_dias_trab   = 0
                            v_qti_dias_dsr    = 0
                            v_qti_dias_fer    = 0.
                  end.
               end. /* repouso */
               if det_calend_turma_localid.dat_refer_calend = bcontrole-catponto.dat_term_period_apurac_pto_mes then do:
                  if not v_log_atualiz_tt then do:
                     create tt-val-semana.
                     assign tt-val-semana.mes-refer       = "ref"
                            tt-val-semana.cod_pais        = det_calend_turma_localid.cod_pais
                            tt-val-semana.cdn_localidade  = det_calend_turma_localid.cdn_localidade
                            tt-val-semana.num-semana      = i-semana         
                            tt-val-semana.dat-fim         = det_calend_turma_localid.dat_refer_calend
                            tt-val-semana.qtd-hrs-diu     = v_qti_hrs_diu 
                            tt-val-semana.qtd-hrs-not     = v_qti_hrs_not   
                            tt-val-semana.qtd-hrs-diu-dsr = v_qti_hrs_diu_dsr 
                            tt-val-semana.qtd-hrs-diu-fer = v_qti_hrs_diu_fer
                            tt-val-semana.qtd-hrs-not-dsr = v_qti_hrs_not_dsr 
                            tt-val-semana.qtd-hrs-not-fer = v_qti_hrs_not_fer
                            tt-val-semana.qtd-trb-sem     = v_qti_dias_trab
                            tt-val-semana.qtd-dsr-sem     = v_qti_dias_dsr
                            tt-val-semana.qtd-fer-sem     = v_qti_dias_fer.
                     assign v_qti_hrs_diu     = 0
                            v_qti_hrs_not     = 0
                            v_qti_hrs_diu_dsr = 0
                            v_qti_hrs_not_dsr = 0
                            v_qti_hrs_diu_fer = 0
                            v_qti_hrs_not_fer = 0
                            v_qti_dias_trab   = 0
                            v_qti_dias_dsr    = 0
                            v_qti_dias_fer    = 0.
                  end.
               end. /* ultimo dia semana */
            end. /* det calend */
         end. /* fecha por per°odo pto */
      end. /* turno turma */
   end. /* if diferente */
END PROCEDURE.

PROCEDURE pi-atualiza-sit-calc:
    
   for each tt_categ no-lock:
      for each sit_calc_ptoelet_categ fields(cdn_empresa
                                             cdn_estab
                                             cdn_categ_sal
                                             num_mes_primei_calc_realzdo
                                             num_ano_primei_calc_ptoelet
                                             idi_sit_ptoelet_categ) exclusive-lock where
         sit_calc_ptoelet_categ.cdn_empresa                 = bparam-pe.cdn_empresa  and
         sit_calc_ptoelet_categ.cdn_estab                  >= tt-param.i-es-ini      and
         sit_calc_ptoelet_categ.cdn_estab                  <= tt-param.i-es-fim      and
         sit_calc_ptoelet_categ.cdn_categ_sal               = tt_categ.cdn_categ_sal and
         sit_calc_ptoelet_categ.num_mes_primei_calc_realzdo = tt-param.mes-ref and
         sit_calc_ptoelet_categ.num_ano_primei_calc_ptoelet = tt-param.ano-ref:

          if can-find (first sit_calc_ptoelet_func no-lock where
                      sit_calc_ptoelet_func.cdn_clas_func      = bcatponto.cdn_clas_func      and
                      sit_calc_ptoelet_func.cdn_empresa        = bcatponto.cdn_empresa        and
                      (sit_calc_ptoelet_func.cdn_estab          = bcatponto.cdn_estab          or
                       sit_calc_ptoelet_func.cdn_estab          = "")                          and
                      sit_calc_ptoelet_func.cdn_categ_sal      = bcatponto.cdn_categ_sal      and
                      sit_calc_ptoelet_func.num_ano_primei_calc_ptoelet = sit_calc_ptoelet_categ.num_ano_primei_calc_ptoelet and
                      sit_calc_ptoelet_func.num_mes_primei_calc_realzdo = sit_calc_ptoelet_categ.num_mes_primei_calc_realzdo and
                      sit_calc_ptoelet_func.idi_sit_ptoelet_mes_func = 1) then 
              assign i-st-atu = 1.
          else do:
              if can-find (first sit_calc_ptoelet_func no-lock where
                           sit_calc_ptoelet_func.cdn_clas_func     = bcatponto.cdn_clas_func and
                           sit_calc_ptoelet_func.cdn_empresa        = bcatponto.cdn_empresa  and
                           (sit_calc_ptoelet_func.cdn_estab         = bcatponto.cdn_estab    or
                            sit_calc_ptoelet_func.cdn_estab          = "")                    and
                           sit_calc_ptoelet_func.num_ano_primei_calc_ptoelet = sit_calc_ptoelet_categ.num_ano_primei_calc_ptoelet and
                           sit_calc_ptoelet_func.num_mes_primei_calc_realzdo = sit_calc_ptoelet_categ.num_mes_primei_calc_realzdo and
                           sit_calc_ptoelet_func.idi_sit_ptoelet_mes_func = 2) then
                  assign i-st-atu = 2.
              else 
                  assign i-st-atu = 3.     
          end.
          assign sit_calc_ptoelet_categ.idi_sit_ptoelet_categ = i-st-atu.
      end. /* sit categ */
   end. /* tt-categ */
END.


PROCEDURE pi-funcionario:

    for each bfunc-ponto exclusive-lock where
             bfunc-ponto.cdn_empresa        = bparam-pe.cdn_empresa       and
             bfunc-ponto.cdn_estab         >= tt-param.i-es-ini           and
             bfunc-ponto.cdn_estab         <= tt-param.i-es-fim           and
             bfunc-ponto.cdn_funcionario   >= tt-param.i-fc-ini           and
             bfunc-ponto.cdn_funcionario   <= tt-param.i-fc-fim           and
             bfunc-ponto.cdn_turno_trab    >= tt-param.i-turno-ini        and
             bfunc-ponto.cdn_turno_trab    <= tt-param.i-turno-fim        and
             bfunc-ponto.cdn_prestdor_serv >= tt-param.v_cdn_prestdor_ini and
             bfunc-ponto.cdn_prestdor_serv <= tt-param.v_cdn_prestdor_fim and
             bfunc-ponto.cod_rh_ccusto     >= tt-param.c-ccusto-ini       and
             bfunc-ponto.cod_rh_ccusto     <= tt-param.c-ccusto-fim       and
             bfunc-ponto.cdn_clas_func     >= tt-param.i-classe-ini       and
             bfunc-ponto.cdn_clas_func     <= tt-param.i-classe-fim       and
             bfunc-ponto.log_consid_calc_ptoelet,
        first tt-orig-contratac-func no-lock where
        tt-orig-contratac-func.idi_orig_contratac_func = bfunc-ponto.idi_orig_contratac_func
        break by bfunc-ponto.cdn_turno_trab
              by bfunc-ponto.cdn_turma_trab
              by bfunc-ponto.cod_pais
              by bfunc-ponto.cdn_localidade:

       if tt-param.v_log_ms then do: 
          if bfunc-ponto.dat_desligto_func = ? then do:
             assign v_num_erro = v_num_erro + 1.   
             {utp/ut-liter.i N∆o_encontra-se_desligado_funcion†rio mpe L}
             create tt_erro.
             assign tt_erro.num_erro = v_num_erro
                    tt_erro.des_erro = trim(return-value) + " " + string(bfunc-ponto.cdn_funcionario,"99999999") + ".".
                    tt_erro.log_erro = yes.
             next.
          end.
          else do:

             if bfunc-ponto.dat_desligto_func < date(tt-param.mes-ref,01,tt-param.ano-ref) then do:
                assign v_num_erro = v_num_erro + 1.   
                {utp/ut-liter.i Data_desligamento_inferior_ao_Màs/Ano_Folha_p/_Funcion†rio mpe L}
                create tt_erro.
                assign tt_erro.num_erro = v_num_erro
                       tt_erro.des_erro = trim(return-value) + " " + string(bfunc-ponto.cdn_funcionario,"99999999") + ".".
                       tt_erro.log_erro = yes.
                next.
             end.
          end.

          if (bfunc-ponto.dat_admis_func < date(tt-param.mes-ref,01,tt-param.ano-ref)) and
              not can-find(first movto_ptoelet of bfunc-ponto where
                           movto_ptoelet.num_ano_refer_fp  = v_ano_empresa and
                           movto_ptoelet.num_mes_refer_fp  = v_mes_empresa and
                           movto_ptoelet.idi_tip_fp_calcul = 1 and
                           movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
                           movto_ptoelet.idi_tip_movto_ptoelet = 1) and
              not can-find(categ_sal where
                           categ_sal.cdn_empresa   = bfunc-ponto.cdn_empresa and
                           (categ_sal.cdn_estab    = bfunc-ponto.cdn_estab or
                            categ_sal.cdn_estab     = bfunc-ponto.cdn_estab_lotac_func_ptoelet) and
                            categ_sal.cdn_categ_sal = bfunc-ponto.cdn_categ_sal and
                            categ_sal.num_livre_1   = tt-param.mes-ref and
                            categ_sal.num_livre_2   = tt-param.ano-ref) then do:
             assign v_num_erro = v_num_erro + 1.
             {utp/ut-liter.i N∆o_encontra-se_integrado_funcion†rio mpe L}
             create tt_erro.
             assign tt_erro.log_erro = yes
                    tt_erro.num_erro = v_num_erro
                    tt_erro.des_erro = trim(return-value) + " " + string(bfunc-ponto.cdn_funcionario,"99999999") + ". ".
             next.
          END.
       end.
       else do:
          if bfunc-ponto.dat_desligto_func <> ? and
             bfunc-ponto.dat_desligto_func < date(tt-param.mes-ref,01,tt-param.ano-ref) then 
             next.
       end.

       if not can-find(first tt_categ where tt_categ.cdn_categ_sal = bfunc-ponto.cdn_categ_sal) then
          next.

       IF SUBSTR(bfunc-ponto.cod_livre_1,029,01) = "S" AND
          NOT CAN-FIND(FIRST det_calend_func WHERE
                             det_calend_func.cdn_empresa      = bfunc-ponto.cdn_empresa AND
                             det_calend_func.cdn_estab        = bfunc-ponto.cdn_estab AND
                             det_calend_func.cdn_funcionario  = bfunc-ponto.cdn_funcionario AND
                             det_calend_func.dat_refer_calend = DATE(tt-param.mes-ref,01,tt-param.ano-ref)) THEN DO:

          assign v_cdn_estab_mens  = bfunc-ponto.cdn_estab       
                 v_cdn_func_mens   = bfunc-ponto.cdn_funcionario 
                 v_mes_men         = tt-param.mes-ref           
                 v_ano_men         = tt-param.ano-ref.

          disp v_cdn_estab_mens
               v_cdn_func_mens
               v_mes_men
               v_ano_men
               with frame f-erro-calen-func.
          down with frame f-erro-calen-func.
          
          assign v_num_erro = v_num_erro + 1.
          {utp/ut-liter.i Calendario_Funcionario_Inexistente mpe L}
          create tt_erro.
          assign tt_erro.num_erro = v_num_erro
                 tt_erro.des_erro = trim(return-value)
                 tt_erro.log_erro = yes.
          next.
       END.
       
       /**** Cleiton ****/
       if v_log_folha_educnal then 
        if not int(substring(string(bfunc-ponto.cdn_funcionario,"99999999"),7,2)) >= tt-param.i-ctr-ini   or 
           not int(substring(string(bfunc-ponto.cdn_funcionario,"99999999"),7,2)) <= tt-param.i-ctr-fim  then
           next.                      

       /** FO 1392.772 - Engenharia Transferencia x Ponto Eletronico **/
       if substring(bfunc-ponto.cod_livre_1,47,1) = "S" then do:
          /** Integra informaá‰es do Funcion†rio Origem **/
          assign l_log_error = no.
          run pi-integra-func-origem (output l_log_error).
          if l_log_error then
             next.
       end.
       /******** Chamada EPC - Evandro **********/
       for each tt-epc where tt-epc.cod-event = "CIENTE":
           delete tt-epc. 
       end.
       create tt-epc.
       assign tt-epc.cod-event = "CIENTE"
              tt-epc.val-parameter = string(bfunc-ponto.cdn_empresa)     + ";" +
                                     string(bfunc-ponto.cdn_estab)       + ";" +
                                     string(bfunc-ponto.cdn_funcionario) + ";" +
                                     string(tt-param.ano-ref)            + ";" +
                                     string(tt-param.mes-ref)            + ";".
       {include/i-epc201.i "CIENTE"}
       
       IF RETURN-VALUE = "N-OK" THEN
          NEXT.

       find bcatponto no-lock where
            bcatponto.cdn_clas_func = bfunc-ponto.cdn_clas_func and
            bcatponto.cdn_empresa   = bfunc-ponto.cdn_empresa   and
            bcatponto.cdn_estab     = bfunc-ponto.cdn_estab     and
            bcatponto.cdn_categ_sal = bfunc-ponto.cdn_categ_sal no-error.
       if avail bcatponto then do:

           find first funcionario of bfunc-ponto no-lock no-error.
           find first event_integr no-lock where
               event_integr.cdn_empresa    = funcionario.cdn_empresa   and
               event_integr.cdn_estab      = funcionario.cdn_estab     and
               event_integr.cdn_categ_sal  = funcionario.cdn_categ_sal and
               event_integr.idi_tip_func   = funcionario.idi_tip_func  no-error.
           if avail event_integr then do:
               assign i-ev-trbdiu       = event_integr.cdn_event_hrs_diurno
                      i-ev-trbnot       = event_integr.cdn_event_hrs_notur
                      i-ev-dsrdiu       = event_integr.cdn_event_dsr_diurno
                      i-ev-dsrnot       = event_integr.cdn_event_dsr_notur
                      i-ev-dsrdiuper    = event_integr.cdn_event_desc_dsr_diurno
                      i-ev-dsrnotper    = event_integr.cdn_event_desc_dsr_notur
                      i-ev-desc-hrs-diu = event_integr.cdn_event_falta_diurno
                      i-ev-desc-hrs-not = event_integr.cdn_event_falta_notur
                      i-ev-hrs-dif-diu  = event_integr.cdn_event_difer_diurno 
                      i-ev-hrs-dif-not  = event_integr.cdn_event_difer_notur
                      i-ev-ferdiuper    = i-ev-dsrdiuper
                      i-ev-fernotper    = i-ev-dsrnotper
                     
                      l-adc-evnot = no. /* cris */

               find first event_fp no-lock where
                          event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento AND 
                          event_fp.cdn_event_fp = i-ev-trbnot no-error.
               if event_fp.cdn_efp_suplem_hrs_notur > "" /*0*/ then
                   assign i-ev-supnot = event_fp.cdn_efp_suplem_hrs_notur
                          l-sup-evnot = yes.
               else 
                   assign i-ev-supnot = ""
                          l-sup-evnot = no.

               if event_fp.idi_tip_adc_notur <> 3 then  /* cris turno m¢vel */
                   assign l-adc-evnot = yes.

               if l-sup-evnot = yes then do:
                  if not can-find(first event_fp no-lock where
                                  event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento AND
                                  event_fp.cdn_event_fp = i-ev-supnot) then do:
                      assign v_num_erro = v_num_erro + 1.
                      {utp/ut-liter.i N∆o_encontrado_Evento_de_Suplementaá∆o_Noturna_= mpe L}
                      assign v_des_erro1 = trim(return-value) + " " + string(i-ev-supnot).
                      create tt_erro.
                      assign tt_erro.num_erro = v_num_erro
                             tt_erro.des_erro = v_des_erro1
                             tt_erro.log_erro = yes.
                  end.
               end.
           end.
           else do:
               assign v_num_erro = v_num_erro + 1.
               {utp/ut-liter.i Eventos_Integracao_nao_cadastrados_para_o_Estab. mpe L}
               assign v_des_erro1 = trim(return-value) + " " + string(funcionario.cdn_estab) + " ".
               {utp/ut-liter.i Categoria mpe L}
               assign v_des_erro1 = v_des_erro1 + " " + trim(return-value) + " " + string(funcionario.cdn_categ_sal) + " ".
               {utp/ut-liter.i e_Tipo_Func. mpe L}
               assign v_des_erro1 = v_des_erro1 + " " + trim(return-value) + " " + {database/intm/i01tm075.i 04 funcionario.idi_tip_func}.

               create tt_erro.
               assign tt_erro.num_erro = v_num_erro
                      tt_erro.des_erro = v_des_erro1
                      tt_erro.log_erro = yes.
               next.
           end.

          find bcontrole-catponto of bcatponto no-lock where
               bcontrole-catponto.num_mes_primei_calc_realzdo = tt-param.mes-ref and
               bcontrole-catponto.num_ano_primei_calc_ptoelet = tt-param.ano-ref no-error.
          if avail bcontrole-catponto then do:
             if can-find(first movto_ptoelet of bfunc-ponto where
                         movto_ptoelet.num_ano_refer_fp  = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                         movto_ptoelet.num_mes_refer_fp  = bcontrole-catponto.num_mes_primei_calc_realzdo and
                         movto_ptoelet.idi_tip_fp_calcul = 1 and
                         movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
                         movto_ptoelet.idi_tip_movto_ptoelet = 1 and
                         movto_ptoelet.val_livre_1 = 0) then do:
                assign v_num_erro = v_num_erro + 1.
                {utp/ut-liter.i Per°odo_Ponto_do_Funcionario mpe L}
                assign v_des_erro1 = trim(return-value) + " " + string(bfunc-ponto.cdn_funcionario) + " ".
                {utp/ut-liter.i j†_integrado mpe L}
                assign v_des_erro1 = v_des_erro1 + " " + trim(return-value).
                create tt_erro.
                assign tt_erro.num_erro = v_num_erro
                       tt_erro.des_erro = v_des_erro1
                       tt_erro.log_erro = yes.
                next.
             end.
             /*** Funcionario com transferencia nao deve ser integrado Evandro ***/
             IF bfunc-ponto.dat_desligto >= DATE(bcontrole-catponto.num_mes_primei_calc_realzdo,01,bcontrole-catponto.num_ano_primei_calc_ptoelet) THEN DO:
                 FOR EACH sit_afast_func fields(dat_inic_sit_afast
                                                cdn_sit_afast_func) of bfunc-ponto NO-LOCK where   
                     sit_afast_func.dat_inic_sit_afast = bfunc-ponto.dat_desligto:

                     if can-find(first sit_afast no-lock where
                                 sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func and
                                 sit_afast.idi_signif_sit     = 4) then do:
                         assign v_num_erro = v_num_erro + 1.
                         {utp/ut-liter.i Funcion†rio_transferido_! mpe L}
                         assign v_des_erro1 = trim(return-value).
                         create tt_erro.
                         assign tt_erro.num_erro  = v_num_erro
                                tt_erro.des_erro  = v_des_erro1
                                tt_erro.log_erro  = yes.
                         leave.
                     end.
                 END.
             END.

             if tt-param.i-ind-selec = 3 then do:
                if v_log_utiliz_folha_compl then do:
                   if bcatponto.cdn_estab = "" then 
                      find funcionario of bcatponto no-lock no-error.
                   if not can-find(last movto_ptoelet no-lock where
                                   movto_ptoelet.cdn_empresa = funcionario.cdn_empresa and
                                   movto_ptoelet.cdn_estab   = funcionario.cdn_estab   and
                                   movto_ptoelet.cdn_funcionario   = funcionario.cdn_funcionario               and
                                   movto_ptoelet.num_mes_refer_fp  = bcontrole-catponto.num_mes_primei_calc_realzdo and
                                   movto_ptoelet.num_ano_refer_fp  = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                                   movto_ptoelet.idi_tip_fp_calcul           = 1 and
                                   movto_ptoelet.num_parc_calc_movto_ptoelet = 9 and
                                   movto_ptoelet.idi_tip_movto_ptoelet = 1) then do:
                      if can-find(first func_calc_compl of funcionario no-lock) then do:
                         if can-find(first movto_calcul_func of funcionario where
                                     movto_calcul_func.num_ano_refer_fp         = bcontrole-catponto.num_ano_primei_calc_ptoelet and
                                     movto_calcul_func.num_mes_refer_fp         = bcontrole-catponto.num_mes_primei_calc_realzdo and
                                     movto_calcul_func.idi_tip_fp               = 1                                        and
                                     movto_calcul_func.qti_parc_habilit_calc_fp = 2                                        and
                                     movto_calcul_func.num_seq_movto_calcul_fp  = 0) then do: /** funcionario com calculo de folha complementar efetuado **/
                             assign v_num_erro = v_num_erro + 1.
                             run utp/ut-msgs.p (input "msg", input 19397, input "").
                             create tt_erro.
                             assign tt_erro.num_erro = v_num_erro
                                    tt_erro.des_erro = trim(return-value) + " " + 
                                                       string(bfunc-ponto.cdn_funcionario,">>>>>>>9")
                                    tt_erro.log_erro = yes.
                             next.
                         end.
                      end. 
                   end.   
                end.   
             end.

             empty temp-table tt-calendar no-error.

             /* carregar a tt-calendar com todos os calendarios dos turnos que
                o funcionario possar estar lotado nos meses do periodo de ponto */
             find first bcontrole-funcponto exclusive-lock where
                        bcontrole-funcponto.cdn_empresa                 = bfunc-ponto.cdn_empresa  and
                        bcontrole-funcponto.cdn_estab                   = bfunc-ponto.cdn_estab       and                      
                        bcontrole-funcponto.cdn_funcionario             = bfunc-ponto.cdn_funcionario and
                        bcontrole-funcponto.num_ano_primei_calc_ptoelet = tt-param.ano-ref and
                        bcontrole-funcponto.num_mes_primei_calc_realzdo = tt-param.mes-ref no-error.
             if avail bcontrole-funcponto then do:
                assign v_dat_aux_ini = date(month(bcontrole-funcponto.dat_inic_period_apurac_pto_mes),
                                            01,YEAR(bcontrole-funcponto.dat_inic_period_apurac_pto_mes)).

                RUN prghur/fpp/fpapi002.p (INPUT MONTH(bcontrole-funcponto.dat_term_period_apurac_pto_mes),
                                           INPUT YEAR(bcontrole-funcponto.dat_term_period_apurac_pto_mes),
                                           INPUT-OUTPUT v_dat_aux_fim).
                /* verifica se o funcionario possui calendario - gabriela */
                RUN pi_procura_calend_func(INPUT v_dat_aux_ini,
                                           INPUT v_dat_aux_fim).
             end.
             ELSE
                 ASSIGN v_dat_aux_ini = dat-ini-mes
                        v_dat_aux_fim = dat-fim-mes.

             FIND FIRST tt-calendar NO-ERROR.

             IF NOT AVAIL tt-calendar THEN
                /* se nao tem calendario por funcionario, ler o calendario da localidade */
                RUN pi_grava_tt_calendar (INPUT v_dat_aux_ini,
                                          INPUT v_dat_aux_fim).

             FIND funcionario OF bfunc-ponto NO-LOCK NO-ERROR.
             IF AVAIL funcionario THEN
                IF funcionario.dat_admis_transf_func = DATE(tt-param.mes-ref,01,tt-param.ano-ref) AND
                   CAN-FIND(FIRST tt-calendar) AND
                   NOT CAN-FIND(tt-calendar WHERE
                                tt-calendar.dat_refer_calend = bcontrole-funcponto.dat_inic_period_apurac_pto_mes) THEN DO:
                   ASSIGN i-mes-ant-transf = IF MONTH(funcionario.dat_admis_transf_func) = 1
                                             THEN 12
                                             ELSE MONTH(funcionario.dat_admis_transf_func) - 1
                          i-ano-ant-transf = IF MONTH(funcionario.dat_admis_transf_func) = 1
                                             THEN YEAR(funcionario.dat_admis_transf_func) - 1
                                             ELSE YEAR(funcionario.dat_admis_transf_func).

                   RUN pi_grava_tt_calendar (INPUT DATE(i-mes-ant-transf,01,i-ano-ant-transf),
                                             INPUT funcionario.dat_admis_transf_func).
                END.

             RUN pi-monta-tt-calend.
             if avail bturnotrb and avail bturmatrb then do:

                {prghur/pep/pe4000.i}

             end.
          end. /* bcontrole cat */
          else do:
             assign v_num_erro = v_num_erro + 1.
             {utp/ut-liter.i Habilitaá∆o_Ponto_Inexistente mpe L}
             create tt_erro.
             assign tt_erro.num_erro = v_num_erro
                    tt_erro.des_erro = trim(return-value)
                    tt_erro.log_erro = yes.
             next.
          end.
       end. /* catponto */
    end. /* bfunc-ponto */
END.

PROCEDURE pi-cria-movto:
    
   create movto_ptoelet.
   assign movto_ptoelet.cdn_empresa       = bfunc-ponto.cdn_empresa
          movto_ptoelet.cdn_estab         = bfunc-ponto.cdn_estab
          movto_ptoelet.cdn_funcionario   = bfunc-ponto.cdn_funcionario
          movto_ptoelet.num_ano_refer_fp  = bcontrole-catponto.num_ano_primei_calc_ptoelet
          movto_ptoelet.num_mes_refer_fp  = bcontrole-catponto.num_mes_primei_calc_realzdo
          movto_ptoelet.idi_tip_fp_calcul = 1
          movto_ptoelet.num_parc_calc_movto_ptoelet = 9
          movto_ptoelet.cdn_efp           = i-ev-codigo
          movto_ptoelet.qtd_movto_ptoelet = i-qtd-hrs
          movto_ptoelet.idi_tip_movto_ptoelet = 1.
   assign v_num_erro = v_num_erro + 1.
   create tt_erro.
   assign tt_erro.num_erro = v_num_erro
          tt_erro.des_erro = ""
          tt_erro.log_erro = no.

end.

PROCEDURE pi_procura_calend_func.
DEF INPUT PARAMETER p_dat_aux_ini AS DATE FORMAT "99/99/9999" NO-UNDO.
DEF INPUT PARAMETER p_dat_aux_fim AS DATE FORMAT "99/99/9999" NO-UNDO.

IF bfunc-ponto.dat_desligto_func = p_dat_aux_fim OR
   bfunc-ponto.dat_desligto_func = (p_dat_aux_fim - 1) THEN DO:
   RUN prghur/fpp/fpapi002.p (INPUT v_num_mes_seg,
                              INPUT v_num_ano_seg,
                              INPUT-OUTPUT p_dat_aux_fim).   
END.

ASSIGN v_log_calend_func = NO
       v_log_criou       = NO
       v_dat_ini_aux     = p_dat_aux_ini
       v_dat_fim_aux     = p_dat_aux_fim.

ASSIGN v_num_mes_ant = IF (MONTH(p_dat_aux_ini) - 1) = 0
                       THEN 12
                       ELSE MONTH(p_dat_aux_ini) - 1
       v_num_ano_ant = IF (MONTH(p_dat_aux_ini) - 1) = 0
                       THEN YEAR(p_dat_aux_ini) - 1
                       ELSE YEAR(p_dat_aux_ini).


IF SUBSTR(bfunc-ponto.cod_livre_1,029,01) = "S" THEN DO:

   /*  aqui3 */
   FOR EACH bdet_calend_func EXCLUSIVE-LOCK WHERE
       bdet_calend_func.cdn_empresa       = bfunc-ponto.cdn_empresa AND
       bdet_calend_func.cdn_estab         = bfunc-ponto.cdn_estab AND
       bdet_calend_func.cdn_funcionario   = bfunc-ponto.cdn_funcionario AND
       bdet_calend_func.dat_refer_calend >= DATE(v_num_mes_ant,01,v_num_ano_ant) AND
       bdet_calend_func.dat_refer_calend <= p_dat_aux_fim:

 /*    DEF INPUT PARAMETER p_dat_ini AS DATE NO-UNDO.
       DEF INPUT PARAMETER p_dat_fim AS DATE NO-UNDO. */

       ASSIGN /*p_dat_ini         = v_dat_aux_ini - 2
               p_dat_fim         = v_dat_aux_fim + 2 */
              v_dat_fim_localid = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_dat_fim_emprest = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_dat_fim_lotac   = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
              v_log_emprest     = no
              v_cod_pais        = " "
              v_cdn_localid     = 0
              v_cdn_turno       = 0
              v_cdn_turma       = 0
              v_cdn_jorn        = 0.

       {prghur/pep/pe9994.i bdet_calend_func.dat_refer_calend} 

       IF v_cdn_turno <> bdet_calend_func.cdn_turno_trab and
          v_cdn_turno > 0 THEN 
          ASSIGN bdet_calend_func.cdn_turno_trab = v_cdn_turno.
       IF v_cdn_turma <> bdet_calend_func.cdn_turma_trab AND
          v_cdn_turma > 0  THEN
          ASSIGN bdet_calend_func.cdn_turma_trab = v_cdn_turma.
   END.
    
   FOR EACH det_calend_func NO-LOCK WHERE
            det_calend_func.cdn_empresa       = bfunc-ponto.cdn_empresa AND
            det_calend_func.cdn_estab         = bfunc-ponto.cdn_estab AND
            det_calend_func.cdn_funcionario   = bfunc-ponto.cdn_funcionario AND
            det_calend_func.dat_refer_calend >= DATE(v_num_mes_ant,01,v_num_ano_ant) AND
            det_calend_func.dat_refer_calend <= p_dat_aux_fim and
       not can-find(first tt-calendar NO-LOCK WHERE
                    tt-calendar.cdn_turno_trab   = det_calend_func.cdn_turno_trab AND
                    tt-calendar.cdn_turma_trab   = det_calend_func.cdn_turma_trab AND
                    tt-calendar.cod_pais         = det_calend_func.cod_pais AND
                    tt-calendar.cdn_localidade   = det_calend_func.cdn_localidade AND 
                    tt-calendar.dat_refer_calend = det_calend_func.dat_refer_calend):
       ASSIGN v_log_criou = YES.
       CREATE tt-calendar.
       ASSIGN tt-calendar.cdn_jorn_trab            = det_calend_func.cdn_jorn_trab 
              tt-calendar.cdn_jorn_trab_origin     = det_calend_func.cdn_jorn_trab_origin 
              tt-calendar.cdn_localidade           = det_calend_func.cdn_localidade
              tt-calendar.cdn_turma_trab           = det_calend_func.cdn_turma_trab
              tt-calendar.cdn_turno_trab           = det_calend_func.cdn_turno_trab
              tt-calendar.cod_pais                 = det_calend_func.cod_pais
              tt-calendar.cod_tip_dia              = det_calend_func.cod_tip_dia
              tt-calendar.dat_inic_dia_trab        = det_calend_func.dat_inic_dia_trab
              tt-calendar.dat_refer_calend         = det_calend_func.dat_refer_calend
              tt-calendar.dat_term_dia_trab        = det_calend_func.dat_term_dia_trab
              tt-calendar.idi_period_interv_refei  = det_calend_func.idi_period_interv_refei
              tt-calendar.idi_sit_dia_trab         = det_calend_func.idi_sit_dia_trab
              tt-calendar.num_dia_escal_calend     = det_calend_func.num_dia_escal_calend
              tt-calendar.num_horar_inic_dia_trab  = det_calend_func.num_horar_inic_dia_trab
              tt-calendar.num_horar_term_dia_trab  = det_calend_func.num_horar_term_dia_trab
              tt-calendar.num_hora_fim_jorn_trab   = det_calend_func.num_hora_fim_jorn_trab
              tt-calendar.num_hora_inic_jorn_trab  = det_calend_func.num_hora_inic_jorn_trab
              tt-calendar.num_livre_1              = det_calend_func.num_livre_1
              tt-calendar.num_period_escal         = det_calend_func.num_period_escal
              tt-calendar.num_sema_dia_calend      = det_calend_func.num_sema_dia_calend
              tt-calendar.qti_dias_trabdo_sema     = det_calend_func.qti_dias_trabdo_sema
              tt-calendar.qti_dsr_sema             = det_calend_func.qti_dsr_sema
              tt-calendar.qti_fer_sema             = det_calend_func.qti_fer_sema
              tt-calendar.qti_hrs_compcao          = det_calend_func.qti_hrs_compcao
              tt-calendar.qti_hrs_compcao_sema     = det_calend_func.qti_hrs_compcao_sema
              tt-calendar.qti_hrs_compens_func     = det_calend_func.qti_hrs_compens_func
              tt-calendar.qti_hrs_compens_sema     = det_calend_func.qti_hrs_compens_sema
              tt-calendar.qti_hrs_dsr_diurno_sema  = det_calend_func.qti_hrs_dsr_diurno_sema
              tt-calendar.qti_hrs_dsr_notur_sema   = det_calend_func.qti_hrs_dsr_notur_sema
              tt-calendar.qti_hrs_dsr_sema         = det_calend_func.qti_hrs_dsr_sema
              tt-calendar.qti_hrs_fer_diurno_sema  = det_calend_func.qti_hrs_fer_diurno_sema
              tt-calendar.qti_hrs_fer_notur_sema   = det_calend_func.qti_hrs_fer_notur_sema
              tt-calendar.qti_hrs_fer_sema         = det_calend_func.qti_hrs_fer_sema
              tt-calendar.qti_padr_hrs_diurno      = det_calend_func.qti_padr_hrs_diurno
              tt-calendar.qti_padr_hrs_diurno_sema = det_calend_func.qti_padr_hrs_diurno_sema
              tt-calendar.qti_padr_hrs_notur       = det_calend_func.qti_padr_hrs_notur
              tt-calendar.qti_padr_hrs_notur_sema  = det_calend_func.qti_padr_hrs_notur_sema.
   END. /* for each det_calend_func */
   /* verificar se tem emprestimo e criar a tt-calendar */
   IF v_log_criou THEN DO:
      ASSIGN v_log_calend_func = YES.
      RUN pi_verifica_emprest.
   END.
END.
END. /* procedure */

PROCEDURE pi_verifica_emprest.
DEF VAR v_dat_proc     AS DATE NO-UNDO.
DEF VAR v_num_dias_mes AS INT  NO-UNDO.
DEF VAR v_num_mes      AS INT FORMAT 99 NO-UNDO.
DEF VAR v_num_ano      AS INT FORMAT 9999  NO-UNDO.

DEF BUFFER bemprest_turno_turma_trab FOR emprest_turno_turma_trab.

ASSIGN v_dat_fim_localid = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
       v_dat_fim_emprest = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
       v_dat_fim_lotac   = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
       v_log_emprest     = no
       v_cod_pais        = " "
       v_cdn_localid     = 0
       v_cdn_turno       = 0
       v_cdn_turma       = 0
       v_cdn_jorn        = 0 .

FOR EACH bemprest_turno_turma_trab fields(cdn_empresa
                                          cdn_estab
                                          cdn_funcionario
                                          dat_inic_alter_horar_turma
                                          dat_fim_alter_horar_turma) NO-LOCK WHERE
         bemprest_turno_turma_trab.cdn_empresa                 = bfunc-ponto.cdn_empresa and
         bemprest_turno_turma_trab.cdn_estab                   = bfunc-ponto.cdn_estab AND
         bemprest_turno_turma_trab.cdn_funcionario             = bfunc-ponto.cdn_funcionario AND
         bemprest_turno_turma_trab.dat_inic_alter_horar_turma <= v_dat_fim_aux and
         bemprest_turno_turma_trab.dat_fim_alter_horar_turma  >= v_dat_ini_aux :
    assign v_dat_ini_emprest_aux = if bemprest_turno_turma_trab.dat_inic_alter_horar_turma < v_dat_ini_aux
                                   then v_dat_ini_aux
                                   else bemprest_turno_turma_trab.dat_inic_alter_horar_turma
           v_dat_fim_emprest_aux = if bemprest_turno_turma_trab.dat_fim_alter_horar_turma < v_dat_fim_aux
                                   then bemprest_turno_turma_trab.dat_fim_alter_horar_turma
                                   else v_dat_fim_aux.
    DO v_dat_proc = v_dat_ini_emprest_aux TO v_dat_fim_emprest_aux: 
       ASSIGN v_dat_aux_proc = v_dat_proc
              v_num_mes      = MONTH(v_dat_proc)
              v_num_ano      = YEAR(v_dat_proc).

       if v_num_mes = 2 then do:
          if (v_num_ano modulo 4) = 0 then
              assign v_num_dias_mes = 29.
          else 
              assign v_num_dias_mes = 28.   
       end.        
       else 
          assign v_num_dias_mes = if v_num_mes = 4 or
                                     v_num_mes = 6 or
                                     v_num_mes = 9 or
                                     v_num_mes = 11
                                  then 30
                                  else 31.    

       {prghur/pep/pe9994.i v_dat_proc}

       if not can-find(first tt-calendar NO-LOCK WHERE
                       tt-calendar.cdn_turno_trab   = v_cdn_turno AND
                       tt-calendar.cdn_turma_trab   = v_cdn_turma AND
                       tt-calendar.cod_pais         = v_cod_pais AND
                       tt-calendar.cdn_localidade   = v_cdn_localid AND 
                       tt-calendar.dat_refer_calend = v_dat_proc) then do:
          FOR EACH det_calend_turma_localid NO-LOCK WHERE
                   det_calend_turma_localid.cdn_turno_trab    = v_cdn_turno AND
                   det_calend_turma_localid.cdn_turma_trab    = v_cdn_turma AND
                   det_calend_turma_localid.cdn_localidade    = v_cdn_localid AND
                   det_calend_turma_localid.cod_pais          = v_cod_pais AND
                   det_calend_turma_localid.dat_refer_calend >= date(v_num_mes,01,v_num_ano) AND
                   det_calend_turma_localid.dat_refer_calend <= DATE(v_num_mes,v_num_dias_mes,v_num_ano) :

              IF NOT CAN-FIND(tt-calendar NO-LOCK WHERE
                              tt-calendar.cdn_turno_trab   = v_cdn_turno AND
                              tt-calendar.cdn_turma_trab   = v_cdn_turma AND
                              tt-calendar.cod_pais         = v_cod_pais AND
                              tt-calendar.cdn_localidade   = v_cdn_localid AND 
                              tt-calendar.dat_refer_calend = det_calend_turma_localid.dat_refer_calend) THEN DO:
                 CREATE tt-calendar.
                 ASSIGN tt-calendar.cdn_jorn_trab            = det_calend_turma_localid.cdn_jorn_trab
                        tt-calendar.cdn_jorn_trab_origin     = det_calend_turma_localid.cdn_jorn_trab_origin
                        tt-calendar.cdn_localidade           = det_calend_turma_localid.cdn_localidade
                        tt-calendar.cdn_turma_trab           = det_calend_turma_localid.cdn_turma_trab
                        tt-calendar.cdn_turno_trab           = det_calend_turma_localid.cdn_turno_trab
                        tt-calendar.cod_pais                 = det_calend_turma_localid.cod_pais
                        tt-calendar.cod_tip_dia              = det_calend_turma_localid.cod_tip_dia
                        tt-calendar.dat_inic_dia_trab        = det_calend_turma_localid.dat_inic_dia_trab
                        tt-calendar.dat_refer_calend         = det_calend_turma_localid.dat_refer_calend
                        tt-calendar.dat_term_dia_trab        = det_calend_turma_localid.dat_term_dia_trab
                        tt-calendar.idi_period_interv_refei  = det_calend_turma_localid.idi_period_interv_refei
                        tt-calendar.idi_sit_dia_trab         = det_calend_turma_localid.idi_sit_dia_trab
                        tt-calendar.num_dia_escal_calend     = det_calend_turma_localid.num_dia_escal_calend
                        tt-calendar.num_horar_inic_dia_trab  = det_calend_turma_localid.num_horar_inic_dia_trab
                        tt-calendar.num_horar_term_dia_trab  = det_calend_turma_localid.num_horar_term_dia_trab
                        tt-calendar.num_hora_fim_jorn_trab   = det_calend_turma_localid.num_hora_fim_jorn_trab
                        tt-calendar.num_hora_inic_jorn_trab  = det_calend_turma_localid.num_hora_inic_jorn_trab
                        tt-calendar.num_livre_1              = det_calend_turma_localid.num_livre_1
                        tt-calendar.num_period_escal         = det_calend_turma_localid.num_period_escal
                        tt-calendar.num_sema_dia_calend      = det_calend_turma_localid.num_sema_dia_calend
                        tt-calendar.qti_dias_trabdo_sema     = det_calend_turma_localid.qti_dias_trabdo_sema
                        tt-calendar.qti_dsr_sema             = det_calend_turma_localid.qti_dsr_sema
                        tt-calendar.qti_fer_sema             = det_calend_turma_localid.qti_fer_sema
                        tt-calendar.qti_hrs_compcao          = det_calend_turma_localid.qti_hrs_compcao
                        tt-calendar.qti_hrs_compcao_sema     = det_calend_turma_localid.qti_hrs_compcao_sema
                        tt-calendar.qti_hrs_compens_func     = det_calend_turma_localid.qti_hrs_compens_func
                        tt-calendar.qti_hrs_compens_sema     = det_calend_turma_localid.qti_hrs_compens_sema
                        tt-calendar.qti_hrs_dsr_diurno_sema  = det_calend_turma_localid.qti_hrs_dsr_diurno_sema
                        tt-calendar.qti_hrs_dsr_notur_sema   = det_calend_turma_localid.qti_hrs_dsr_notur_sema
                        tt-calendar.qti_hrs_dsr_sema         = det_calend_turma_localid.qti_hrs_dsr_sema
                        tt-calendar.qti_hrs_fer_diurno_sema  = det_calend_turma_localid.qti_hrs_fer_diurno_sema
                        tt-calendar.qti_hrs_fer_notur_sema   = det_calend_turma_localid.qti_hrs_fer_notur_sema
                        tt-calendar.qti_hrs_fer_sema         = det_calend_turma_localid.qti_hrs_fer_sema
                        tt-calendar.qti_padr_hrs_diurno      = det_calend_turma_localid.qti_padr_hrs_diurno
                        tt-calendar.qti_padr_hrs_diurno_sema = det_calend_turma_localid.qti_padr_hrs_diurno_sema
                        tt-calendar.qti_padr_hrs_notur       = det_calend_turma_localid.qti_padr_hrs_notur
                        tt-calendar.qti_padr_hrs_notur_sema  = det_calend_turma_localid.qti_padr_hrs_notur_sema.
              END. /* if not can-find */
          END. /* for each */
       END. /* if not avail */
    END. /* do */
END.

END.

PROCEDURE pi_grava_tt_calendar.
DEF INPUT PARAMETER p_dat_ini AS DATE NO-UNDO.
DEF INPUT PARAMETER p_dat_fim AS DATE NO-UNDO.

DEF VAR v_dat_ant_aux     AS DATE FORMAT "99/99/9999" NO-UNDO.
DEF VAR v_dat_aux         AS DATE FORMAT "99/99/9999" NO-UNDO.
DEF VAR v_num_mes         AS INT                      NO-UNDO.
DEF VAR v_num_ano         AS INT                      NO-UNDO.
DEF VAR v_num_dias_mes    AS INT                      NO-UNDO.
DEF VAR v_log_posic_orig  AS LOG INIT NO              NO-UNDO.
DEF VAR v_row_bfunc_ponto AS ROWID                    NO-UNDO.
DEF VAR v_log_calend_dest AS LOG INIT NO              NO-UNDO.


IF bfunc-ponto.dat_desligto_func = p_dat_fim OR
   bfunc-ponto.dat_desligto_func = (p_dat_fim - 1) then
   RUN prghur/fpp/fpapi002.p (INPUT v_num_mes_seg,
                              INPUT v_num_ano_seg,
                              INPUT-OUTPUT p_dat_fim).   

/* aqui */
ASSIGN p_dat_ini         = p_dat_ini - 2
       p_dat_fim         = p_dat_fim + 2
       v_dat_fim_localid = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
       v_dat_fim_emprest = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
       v_dat_fim_lotac   = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
       v_log_emprest     = no
       v_cod_pais        = " "
       v_cdn_localid     = 0
       v_cdn_turno       = 0
       v_cdn_turma       = 0
       v_cdn_jorn        = 0
       v_dat_ant_aux     = date(month(p_dat_ini),01,YEAR(p_dat_ini))
       v_cdn_emp_orig    = ""
       v_cdn_estab_orig  = ""
       v_cdn_func_orig   = {prghur/dop/eng005.i &VAR="0"}
       v_row_bfunc_ponto = ?
       v_log_calend_dest = NO
       v_log_posic_orig  = NO.


if bfunc-ponto.dat_desligto_func < p_dat_fim then
   assign p_dat_fim = bfunc-ponto.dat_desligto_func + 2.

if bfunc-ponto.dat_admis_func > p_dat_ini then
   assign p_dat_ini = bfunc-ponto.dat_admis_func - 2.

FIND funcionario OF bfunc-ponto NO-LOCK NO-ERROR.
IF AVAIL funcionario THEN
   IF funcionario.dat_admis_transf_func = DATE(tt-param.mes-ref,01,tt-param.ano-ref) THEN DO:
      for each bsit_afast_func fields(dat_term_proces_sit_afast
                                      dat_inic_proces_sit_afast
                                      cdn_sit_afast_func
                                      cdn_empres_orig
                                      cdn_estab_orig
                                      cdn_func_orig) of bfunc-ponto exclusive-lock where 
               bsit_afast_func.dat_term_proces_sit_afast >= bcontrole-catponto.dat_inic_period_apurac_pto_mes AND 
               bsit_afast_func.dat_inic_proces_sit_afast <= dat-fim-mes,
          first bsit_afast no-lock where
                bsit_afast.cdn_sit_afast_func = bsit_afast_func.cdn_sit_afast_func and
                bsit_afast.idi_signif_sit = 3:
          assign v_cdn_emp_orig    = bsit_afast_func.cdn_empres_orig
                 v_cdn_estab_orig  = bsit_afast_func.cdn_estab_orig
                 v_cdn_func_orig   = bsit_afast_func.cdn_func_orig
                 v_row_bfunc_ponto = rowid(bfunc-ponto).
      END.
   END.

/*SOCOCO*/
assign v_log_alterou = no
       v_log_period_pto_difer = NO.

for each tt-epc exclusive-lock where tt-epc.cod-event = "period_pto_difer":U :
    delete tt-epc.                                                            
end.                                                                          
create tt-epc.                                                                
assign tt-epc.cod-event = "period_pto_difer":U.                            

{include/i-epc201.i "period_pto_difer"}                                       
if return-value = "period_pto_difer-OK":U THEN DO:
   ASSIGN v_log_period_pto_difer = YES. 
   IF bfunc-ponto.dat_admis_func > p_dat_fim THEN
   ASSIGN p_dat_fim = bfunc-ponto.dat_admis_func + 1.
END.
/*SOCOCO*/

DO v_dat_aux = p_dat_ini TO p_dat_fim:

   /*SOCOCO*/
    IF v_log_period_pto_difer = YES and
       month(v_dat_aux) = month(p_dat_fim) THEN DO:
       FIND param_empres_rh NO-LOCK WHERE
            param_empres_rh.cdn_empresa = v_cdn_empres_usuar NO-ERROR.
       IF param_empres_rh.num_mes_refer_calc_efetd = 12 THEN
          assign p_dat_fim = date(1,1,param_empres_rh.num_ano_refer_calc_efetd + 1) - 1
                 v_log_alterou = yes.
       ELSE
          assign p_dat_fim = date(param_empres_rh.num_mes_refer_calc_efetd + 1,1,param_empres_rh.num_ano_refer_calc_efetd) - 1
                 v_log_alterou = yes.
    END.
    /*SOCOCO*/

   ASSIGN v_dat_aux_proc = v_dat_aux
          v_num_mes      = MONTH(v_dat_aux)
          v_num_ano      = YEAR(v_dat_aux).

   if v_num_mes = 2 then do:
      if (v_num_ano modulo 4) = 0 then
         assign v_num_dias_mes = 29.
      else 
         assign v_num_dias_mes = 28.   
   end.        
   else 
      assign v_num_dias_mes = if v_num_mes = 4 or
                                 v_num_mes = 6 or
                                 v_num_mes = 9 or
                                 v_num_mes = 11
                              then 30
                              else 31.    

   IF DATE(tt-param.mes-ref,01,tt-param.ano-ref) = funcionario.dat_admis_transf_func AND
      v_dat_aux <= funcionario.dat_admis_transf_func AND 
      v_log_calend_dest = NO THEN DO:
      IF v_log_posic_orig = NO THEN DO:
         FIND FIRST bfunc-ponto NO-LOCK WHERE 
                    bfunc-ponto.cdn_empresa     = v_cdn_emp_orig   AND 
                    bfunc-ponto.cdn_estab       = v_cdn_estab_orig AND 
                    bfunc-ponto.cdn_funcionario = v_cdn_func_orig  NO-ERROR.
         IF AVAIL bfunc-ponto THEN 
            ASSIGN v_log_posic_orig = YES.
         ELSE 
            FIND FIRST bfunc-ponto NO-LOCK WHERE 
                       rowid(bfunc-ponto) = v_row_bfunc_ponto NO-ERROR.
      END.
      IF v_dat_aux = funcionario.dat_admis_transf_func and
         v_log_calend_dest = NO THEN DO:
         FIND FIRST bfunc-ponto NO-LOCK WHERE
                    rowid(bfunc-ponto) = v_row_bfunc_ponto NO-ERROR.
         
         ASSIGN v_log_calend_dest = YES 
                v_dat_aux         = p_dat_ini
                v_dat_fim_localid = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                v_dat_fim_emprest = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                v_dat_fim_lotac   = &IF "{&ems_dbtype}":U = "MSS":U &THEN date(01,01,1800) &ELSE date(01,01,0001) &ENDIF 
                v_log_emprest     = NO.
      END.
   END.

   {prghur/pep/pe9994.i v_dat_aux} 

   if v_cdn_turno = 0 then
      assign v_cdn_turno = bfunc-ponto.cdn_turno_trab.

   if v_cdn_turma = 0 then
      assign v_cdn_turma = bfunc-ponto.cdn_turma_trab.

   if v_cod_pais = "" then
      assign v_cod_pais = bfunc-ponto.cod_pais.

   if v_cdn_localid = 0 then
      assign v_cdn_localid = bfunc-ponto.cdn_localidade.

   if not can-find(first tt-calendar NO-LOCK WHERE
                   tt-calendar.cdn_turno_trab   = v_cdn_turno AND
                   tt-calendar.cdn_turma_trab   = v_cdn_turma AND
                   tt-calendar.cod_pais         = v_cod_pais AND
                   tt-calendar.cdn_localidade   = v_cdn_localid AND 
                   tt-calendar.dat_refer_calend = v_dat_aux) then do:
      FOR EACH det_calend_turma_localid NO-LOCK WHERE
               det_calend_turma_localid.cdn_turno_trab    = v_cdn_turno AND
               det_calend_turma_localid.cdn_turma_trab    = v_cdn_turma AND
               det_calend_turma_localid.cdn_localidade    = v_cdn_localid AND
               det_calend_turma_localid.cod_pais          = v_cod_pais AND
               det_calend_turma_localid.dat_refer_calend >= v_dat_ant_aux AND
               det_calend_turma_localid.dat_refer_calend <= DATE(v_num_mes,v_num_dias_mes,v_num_ano):
          IF NOT CAN-FIND ( tt-calendar NO-LOCK WHERE
                            tt-calendar.cdn_turno_trab   = v_cdn_turno AND
                            tt-calendar.cdn_turma_trab   = v_cdn_turma AND
                            tt-calendar.cod_pais         = v_cod_pais AND
                            tt-calendar.cdn_localidade   = v_cdn_localid AND 
                            tt-calendar.dat_refer_calend = det_calend_turma_localid.dat_refer_calend) THEN DO:
             CREATE tt-calendar.
             ASSIGN tt-calendar.cdn_jorn_trab            = det_calend_turma_localid.cdn_jorn_trab 
                    tt-calendar.cdn_jorn_trab_origin     = det_calend_turma_localid.cdn_jorn_trab_origin 
                    tt-calendar.cdn_localidade           = det_calend_turma_localid.cdn_localidade
                    tt-calendar.cdn_turma_trab           = det_calend_turma_localid.cdn_turma_trab
                    tt-calendar.cdn_turno_trab           = det_calend_turma_localid.cdn_turno_trab
                    tt-calendar.cod_pais                 = det_calend_turma_localid.cod_pais
                    tt-calendar.cod_tip_dia              = det_calend_turma_localid.cod_tip_dia
                    tt-calendar.dat_inic_dia_trab        = det_calend_turma_localid.dat_inic_dia_trab
                    tt-calendar.dat_refer_calend         = det_calend_turma_localid.dat_refer_calend
                    tt-calendar.dat_term_dia_trab        = det_calend_turma_localid.dat_term_dia_trab
                    tt-calendar.idi_period_interv_refei  = det_calend_turma_localid.idi_period_interv_refei
                    tt-calendar.idi_sit_dia_trab         = det_calend_turma_localid.idi_sit_dia_trab
                    tt-calendar.num_dia_escal_calend     = det_calend_turma_localid.num_dia_escal_calend
                    tt-calendar.num_horar_inic_dia_trab  = det_calend_turma_localid.num_horar_inic_dia_trab
                    tt-calendar.num_horar_term_dia_trab  = det_calend_turma_localid.num_horar_term_dia_trab
                    tt-calendar.num_hora_fim_jorn_trab   = det_calend_turma_localid.num_hora_fim_jorn_trab
                    tt-calendar.num_hora_inic_jorn_trab  = det_calend_turma_localid.num_hora_inic_jorn_trab
                    tt-calendar.num_livre_1              = det_calend_turma_localid.num_livre_1
                    tt-calendar.num_period_escal         = det_calend_turma_localid.num_period_escal
                    tt-calendar.num_sema_dia_calend      = det_calend_turma_localid.num_sema_dia_calend
                    tt-calendar.qti_dias_trabdo_sema     = det_calend_turma_localid.qti_dias_trabdo_sema
                    tt-calendar.qti_dsr_sema             = det_calend_turma_localid.qti_dsr_sema
                    tt-calendar.qti_fer_sema             = det_calend_turma_localid.qti_fer_sema
                    tt-calendar.qti_hrs_compcao          = det_calend_turma_localid.qti_hrs_compcao
                    tt-calendar.qti_hrs_compcao_sema     = det_calend_turma_localid.qti_hrs_compcao_sema
                    tt-calendar.qti_hrs_compens_func     = det_calend_turma_localid.qti_hrs_compens_func
                    tt-calendar.qti_hrs_compens_sema     = det_calend_turma_localid.qti_hrs_compens_sema
                    tt-calendar.qti_hrs_dsr_diurno_sema  = det_calend_turma_localid.qti_hrs_dsr_diurno_sema
                    tt-calendar.qti_hrs_dsr_notur_sema   = det_calend_turma_localid.qti_hrs_dsr_notur_sema
                    tt-calendar.qti_hrs_dsr_sema         = det_calend_turma_localid.qti_hrs_dsr_sema
                    tt-calendar.qti_hrs_fer_diurno_sema  = det_calend_turma_localid.qti_hrs_fer_diurno_sema
                    tt-calendar.qti_hrs_fer_notur_sema   = det_calend_turma_localid.qti_hrs_fer_notur_sema
                    tt-calendar.qti_hrs_fer_sema         = det_calend_turma_localid.qti_hrs_fer_sema
                    tt-calendar.qti_padr_hrs_diurno      = det_calend_turma_localid.qti_padr_hrs_diurno
                    tt-calendar.qti_padr_hrs_diurno_sema = det_calend_turma_localid.qti_padr_hrs_diurno_sema
                    tt-calendar.qti_padr_hrs_notur       = det_calend_turma_localid.qti_padr_hrs_notur
                    tt-calendar.qti_padr_hrs_notur_sema  = det_calend_turma_localid.qti_padr_hrs_notur_sema.
          END. 
      END.
   END.
   if not can-find(first tt-calendar NO-LOCK WHERE
                   tt-calendar.cdn_turno_trab   = v_cdn_turno AND
                   tt-calendar.cdn_turma_trab   = v_cdn_turma AND
                   tt-calendar.cod_pais         = v_cod_pais AND
                   tt-calendar.cdn_localidade   = v_cdn_localid AND 
                   tt-calendar.dat_refer_calend = v_dat_aux) then do:
      if v_cdn_turno_mens   <> v_cdn_turno      or
         v_cdn_turma_mens   <> v_cdn_turma      or
         v_cdn_localid_mens <> v_cdn_localid    or 
         v_mes_mens         <> month(v_dat_aux) or
         v_ano_mens         <> year(v_dat_aux)  then do:

         assign v_cdn_turno_mens   = v_cdn_turno
                v_cdn_turma_mens   = v_cdn_turma 
                v_cdn_localid_mens = v_cdn_localid
                v_mes_mens         = month(v_dat_aux)
                v_ano_mens         = year(v_dat_aux).

         disp v_cdn_turno
              v_cdn_turma
              v_cdn_localid
              v_cod_pais
              v_mes_mens
              v_ano_mens
              with frame f-erro-calen.
         down with frame f-erro-calen.
      end.
   end.
END.
END. /* procedure */

procedure pi-integra-func-origem:

   define output parameter p_log_error as logical no-undo.

   def var v_num_hrs_neg               as int format "9999999999" no-undo.
   def var v_num_hrs_pos               as int format "9999999999" no-undo.

   assign p_log_error = no.

   /** bfunc-ponto = Funcionario Destino **/
   /** b-funciona  = Funcionario Origem  **/

   find first sit_afast_func of bfunc-ponto no-lock
        where (can-find (first sit_afast
                         where sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func
                           and (sit_afast.idi_signif_sit    = 3
                            or  sit_afast.idi_signif_sit    = 11))) no-error.

   if avail sit_afast_func then do:
      /** Pesquisa funcionario Origem **/
      find first b-funciona no-lock
           where b-funciona.cdn_empresa     = sit_afast_func.cdn_empres_orig
             and b-funciona.cdn_estab       = sit_afast_func.cdn_estab_orig
             and b-funciona.cdn_funcionario = sit_afast_func.cdn_func_orig no-error.

      if avail b-funciona then do:
         /** Verifica se o Ponto est† calculado na Origem **/
         run pi-verifica-pto-calcul-origem (input-output p_log_error).

         if p_log_error then
            return.
         
         assign v_cod_mes_ano = string(month(b-funciona.dat_desligto_func),"99") + string(year(b-funciona.dat_desligto_func),"9999").

         for each bco_hrs_compens_func of bfunc-ponto exclusive-lock where
                  bco_hrs_compens_func.idi_hrs_posit = 7 or
                  bco_hrs_compens_func.idi_hrs_posit = 8 :
             delete bco_hrs_compens_func.
         end.

         for each bco_hrs_compens_func of b-funciona exclusive-lock where
                  bco_hrs_compens_func.cod_mes_ano_refer_fp = " ":
            for each b-bco_hrs_compens_func fields(cdn_empresa
                                                   cdn_estab
                                                   cdn_funcionario
                                                   cdn_tip_compcao_hrs
                                                   dat_atualiz_bco_hora
                                                   num_horar_inic_mpe) exclusive-lock where
                     b-bco_hrs_compens_func.cdn_empresa          = bfunc-ponto.cdn_empresa     and
                     b-bco_hrs_compens_func.cdn_estab            = bfunc-ponto.cdn_estab       and
                     b-bco_hrs_compens_func.cdn_funcionario      = bfunc-ponto.cdn_funcionario and
                     b-bco_hrs_compens_func.cdn_tip_compcao_hrs  = bco_hrs_compens_func.cdn_tip_compcao_hrs  and
                     b-bco_hrs_compens_func.dat_atualiz_bco_hora = bco_hrs_compens_func.dat_atualiz_bco_hora and
                     b-bco_hrs_compens_func.num_horar_inic_mpe   = bco_hrs_compens_func.num_horar_inic_mpe:
                delete b-bco_hrs_compens_func.
            end.

            create b-bco_hrs_compens_func.
            assign b-bco_hrs_compens_func.cdn_empresa     = bfunc-ponto.cdn_empresa
                   b-bco_hrs_compens_func.cdn_estab       = bfunc-ponto.cdn_estab
                   b-bco_hrs_compens_func.cdn_funcionario = bfunc-ponto.cdn_funcionario.
            buffer-copy bco_hrs_compens_func except
                        bco_hrs_compens_func.cdn_empresa
                        bco_hrs_compens_func.cdn_estab
                        bco_hrs_compens_func.cdn_funcionario to b-bco_hrs_compens_func.
            if bco_hrs_compens_func.idi_hrs_posit = 1 or bco_hrs_compens_func.idi_hrs_posit = 5 then
               assign bco_hrs_compens_func.idi_hrs_posit = 7
                      bco_hrs_compens_func.cod_mes_ano_refer_fp = v_cod_mes_ano.
            if bco_hrs_compens_func.idi_hrs_posit = 2 or bco_hrs_compens_func.idi_hrs_posit = 6 then
               assign bco_hrs_compens_func.idi_hrs_posit = 8
                      bco_hrs_compens_func.cod_mes_ano_refer_fp = v_cod_mes_ano.
         end. /* bco */

         assign v_num_hrs_pos = 0
                v_num_hrs_neg = 0.
         for each bco_hrs_compens_func fields(cod_mes_ano_refer_fp
                                              idi_tratam_lancto_bco_hrs
                                              cdn_tip_compcao_hrs
                                              dat_atualiz_bco_hora
                                              idi_hrs_posit
                                              qti_hrs_marcac_ptoelet) of b-funciona no-lock
            where bco_hrs_compens_func.cod_mes_ano_refer_fp      = ""
              and bco_hrs_compens_func.idi_tratam_lancto_bco_hrs = 1
              and bco_hrs_compens_func.cdn_tip_compcao_hrs       = 1
              and bco_hrs_compens_func.dat_atualiz_bco_hora     <= b-funciona.dat_desligto_func:
        
            if bco_hrs_compens_func.idi_hrs_posit = 1 then 
               assign v_num_hrs_pos = v_num_hrs_pos + bco_hrs_compens_func.qti_hrs_marcac_ptoelet.
            else do:
               if bco_hrs_compens_func.idi_hrs_posit = 2 then 
                  assign v_num_hrs_neg = v_num_hrs_neg + bco_hrs_compens_func.qti_hrs_marcac_ptoelet.
            end.
         end. /* bco */
        
         find first bcontrole-funcponto-orig exclusive-lock
              where bcontrole-funcponto-orig.cdn_empresa                 = b-funciona.cdn_empresa
                and bcontrole-funcponto-orig.cdn_estab                   = b-funciona.cdn_estab
                and bcontrole-funcponto-orig.cdn_funcionario             = b-funciona.cdn_funcionario
                and bcontrole-funcponto-orig.num_ano_primei_calc_ptoelet = tt-param.ano-ref
                and bcontrole-funcponto-orig.num_mes_primei_calc_realzdo = tt-param.mes-ref no-error.
         
         if avail bcontrole-funcponto-orig then
            assign bcontrole-funcponto-orig.num_livre_1 = v_num_hrs_pos
                   bcontrole-funcponto-orig.num_livre_2 = v_num_hrs_neg.

         for each movto_mpe_refeit exclusive-lock of b-funciona where
                  movto_mpe_refeit.dat_refer_movto_mpe_refei = ?:

            for each b-movto_mpe_refeit fields(cdn_empresa
                                               cdn_estab
                                               cdn_funcionario
                                               dat_proces_mpe) exclusive-lock
               where b-movto_mpe_refeit.cdn_empresa     = bfunc-ponto.cdn_empresa
                 and b-movto_mpe_refeit.cdn_estab       = bfunc-ponto.cdn_estab
                 and b-movto_mpe_refeit.cdn_funcionario = bfunc-ponto.cdn_funcionario
                 and b-movto_mpe_refeit.dat_proces_mpe  = movto_mpe_refeit.dat_proces_mpe:
                delete b-movto_mpe_refeit.
            end.

            create b-movto_mpe_refeit.
            assign b-movto_mpe_refeit.cdn_empresa     = bfunc-ponto.cdn_empresa
                   b-movto_mpe_refeit.cdn_estab       = bfunc-ponto.cdn_estab
                   b-movto_mpe_refeit.cdn_funcionario = bfunc-ponto.cdn_funcionario.
            buffer-copy movto_mpe_refeit except
                        movto_mpe_refeit.cdn_empresa
                        movto_mpe_refeit.cdn_estab
                        movto_mpe_refeit.cdn_funcionario to b-movto_mpe_refeit.
            delete movto_mpe_refeit.
         end. /* movto refei */
         
         /** FO 1392.772 - Engenharia Transferància x Ponto Eletrìnico **/
         for each efp_par_marcac_ptoelet exclusive-lock of b-funciona where
                  efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet = 1 and
                  efp_par_marcac_ptoelet.num_mes_ano_refer_fp = " ":

            for each b-efp_par_marcac fields(cdn_empresa
                                             cdn_estab
                                             cdn_funcionario
                                             dat_proces_mpe) exclusive-lock
               where b-efp_par_marcac.cdn_empresa     = bfunc-ponto.cdn_empresa
                 and b-efp_par_marcac.cdn_estab       = bfunc-ponto.cdn_estab
                 and b-efp_par_marcac.cdn_funcionario = bfunc-ponto.cdn_funcionario
                 and b-efp_par_marcac.dat_proces_mpe  = efp_par_marcac.dat_proces_mpe:
                delete b-efp_par_marcac.
            end.

            create b-efp_par_marcac.
            assign b-efp_par_marcac.cdn_empresa     = bfunc-ponto.cdn_empresa
                   b-efp_par_marcac.cdn_estab       = bfunc-ponto.cdn_estab
                   b-efp_par_marcac.cdn_funcionario = bfunc-ponto.cdn_funcionario.
            buffer-copy efp_par_marcac_ptoelet except
                        efp_par_marcac_ptoelet.cdn_empresa
                        efp_par_marcac_ptoelet.cdn_estab
                        efp_par_marcac_ptoelet.cdn_funcionario to b-efp_par_marcac.
            assign efp_par_marcac_ptoelet.num_mes_ano_refer_fp = string(month(b-funciona.dat_desligto_func),"99") + string(year(b-funciona.dat_desligto_func),"9999").
         end.
      end.
   end.
end.

procedure pi-verifica-pto-calcul-origem:
   define input-output parameter p_log_error as logical no-undo.

   find first bcontrole-funcponto-orig no-lock
        where bcontrole-funcponto-orig.cdn_empresa                 = b-funciona.cdn_empresa
          and bcontrole-funcponto-orig.cdn_estab                   = b-funciona.cdn_estab
          and bcontrole-funcponto-orig.cdn_funcionario             = b-funciona.cdn_funcionario
          and bcontrole-funcponto-orig.num_ano_primei_calc_ptoelet = tt-param.ano-ref
          and bcontrole-funcponto-orig.num_mes_primei_calc_realzdo = tt-param.mes-ref no-error.
   
   if avail bcontrole-funcponto-orig then do:
      assign v_dat_ini_per_orig = bcontrole-funcponto-orig.dat_inic_period_apurac_pto_mes
             v_dat_fim_per_orig = bcontrole-funcponto-orig.dat_term_period_apurac_pto_mes.
   
      if b-funciona.dat_admis_func > bcontrole-funcponto-orig.dat_inic_period_apurac_pto_mes then
         assign v_dat_ini_per_orig = b-funciona.dat_admis_func.
      if b-funciona.cdn_estab <> "" then do:
         find funcionario of b-funciona no-lock no-error.
         if avail funcionario and funcionario.dat_admis_transf_func > bcontrole-funcponto-orig.dat_inic_period_apurac_pto_mes then
            assign v_dat_ini_per_orig = funcionario.dat_admis_transf_func.
      end.
   
      if b-funciona.dat_desligto_func < bcontrole-funcponto-orig.dat_term_period_apurac_pto_mes then do:
         find habilit_rescis of b-funciona no-lock no-error.
         if available habilit_rescis and habilit_rescis.dat_inic_aviso_previo = b-funciona.dat_desligto_func and
            habilit_rescis.idi_tip_aviso_previo <> 3 then
            assign v_dat_fim_per_orig = b-funciona.dat_desligto_func - 1.
         else do:
            assign v_log_transf = no.
            for each sit_afast_func fields(dat_inic_sit_afast
                                           cdn_sit_afast_func) of b-funciona no-lock where
                     sit_afast_func.dat_inic_sit_afast = b-funciona.dat_desligto_func:
               find sit_afast no-lock where
                    sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func no-error.
               if avail sit_afast and sit_afast.idi_signif_sit = 4 then do:
                  assign v_dat_fim_per_orig = b-funciona.dat_desligto_func - 1
                         v_log_transf = yes.
                  leave.
               end.
            end.
            if v_log_transf = no then
               assign v_dat_fim_per_orig = b-funciona.dat_desligto_func.
         end. /* else */
      end. /* dt-desligto */
   
      assign v_log_nao_calc = no.
      do v_dat_ref_orig = v_dat_ini_per_orig to v_dat_fim_per_orig:
         if not can-find(first par_marcac_ptoelet of b-funciona
                         where par_marcac_ptoelet.dat_proces_mpe = v_dat_ref_orig) then do:
            assign v_log_nao_calc = yes.
            leave.
         end.
      end.

      if v_log_nao_calc = yes then do:
          assign v_num_erro = v_num_erro + 1.
          {utp/ut-liter.i Per°odo_Ponto_do_funcion†rio_Origem mpe L}
          assign v_des_erro1 = trim(return-value) + " " + string(b-funciona.cdn_funcionario).
          {utp/ut-liter.i estabelecimento mpe L}
          assign v_des_erro1 = v_des_erro1 + ", " + trim(return-value) + " " + string(b-funciona.cdn_estab).
          {utp/ut-liter.i n∆o_calculado mpe L}
          assign v_des_erro1 = v_des_erro1 + " " + trim(return-value).
          create tt_erro.
          assign tt_erro.num_erro = v_num_erro
                 tt_erro.des_erro = v_des_erro1
                 tt_erro.log_erro = yes
                 p_log_error      = yes.
      end.
   end.

end procedure.
