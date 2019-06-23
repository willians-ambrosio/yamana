DEFINE NEW GLOBAL SHARED TEMP-TABLE tt_funcionario 
       FIELDS cdn_empresa              LIKE funcionario.cdn_empresa
       FIELDS cdn_estab                LIKE funcionario.cdn_estab
       FIELDS cdn_funcionario          LIKE funcionario.cdn_funcionario
    INDEX funcionario IS PRIMARY UNIQUE cdn_empresa       
                                        cdn_estab         
                                        cdn_funcionario.

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt_funcionario_horas 
       FIELDS cdn_empresa              LIKE funcionario.cdn_empresa
       FIELDS cdn_estab                LIKE funcionario.cdn_estab
       FIELDS cdn_funcionario          LIKE funcionario.cdn_funcionario
       FIELDS cdn_clas_func            LIKE funcionario.cdn_clas_func
       FIELDS cdn_categ_sal            LIKE funcionario.cdn_categ_sal
       FIELDS dat_marcac_ptoelet       LIKE efp_par_marcac_ptoelet.dat_marcac_ptoelet 
       FIELDS qti_hrs_marcac_ptoelet   LIKE efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet
       FIELDS log_considera            AS   LOGICAL
       FIELDS cdn_efp                  LIKE efp_par_marcac_ptoelet.cdn_efp
       FIELDS des_motivo               AS   CHARACTER FORMAT "x(500)"
    INDEX data IS PRIMARY UNIQUE cdn_empresa       
                                 cdn_estab         
                                 cdn_funcionario   
                                 dat_marcac_ptoelet.

DEFINE NEW GLOBAL SHARED VARIABLE i_num_ano_refer_fp LIKE efp_par_marcac_ptoelet.num_ano_refer_fp  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i_num_mes_refer_fp LIKE efp_par_marcac_ptoelet.num_mes_refer_fp  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dat_proces_mpe_ini LIKE efp_par_marcac_ptoelet.dat_proces_mpe    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dat_proces_mpe_fim LIKE efp_par_marcac_ptoelet.dat_proces_mpe    NO-UNDO.


DEFINE VARIABLE d-data      LIKE efp_par_marcac_ptoelet.dat_marcac_ptoelet       NO-UNDO. 
DEFINE VARIABLE d-qt-horas  LIKE efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet   NO-UNDO. 
DEFINE VARIABLE l-entrou    AS   LOGICAL                                         NO-UNDO.
DEFINE VARIABLE c-arquivo   AS   CHARACTER                                       NO-UNDO.

DEFINE VARIABLE c-weekday   AS   CHARACTER INITIAL ["Domingo,Segunda-Feira,Teráa-Feira,Quarta-Feira,Quinta-Feira,Sexta-Feira,S†bado"] NO-UNDO.
DEFINE VARIABLE c-descricao AS   CHARACTER FORMAT "x(200)" NO-UNDO.


/* Include i-epc200.i: Definiªío Temp-Table tt-epc */
{include/i-epc200.i1}

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.


IF p-ind-event = "Gradiente" THEN DO:
   EMPTY TEMP-TABLE tt_funcionario.
   EMPTY TEMP-TABLE tt_funcionario_horas.
END.

IF p-ind-event = "Instrutor_a" THEN DO:
   FOR EACH tt-epc 
            WHERE tt-epc.cod-event = p-ind-event
            NO-LOCK:
      IF NOT CAN-FIND(FIRST tt_funcionario
                      WHERE tt_funcionario.cdn_empresa     = ENTRY(1,tt-epc.val-parameter,";")  AND
                            tt_funcionario.cdn_estab       = ENTRY(2,tt-epc.val-parameter,";")  AND        
                            tt_funcionario.cdn_funcionario = INTEGER(ENTRY(3,tt-epc.val-parameter,";")) 
                      NO-LOCK) THEN DO:
         CREATE tt_funcionario.
         ASSIGN tt_funcionario.cdn_empresa     = ENTRY(1,tt-epc.val-parameter,";")
                tt_funcionario.cdn_estab       = ENTRY(2,tt-epc.val-parameter,";")
                tt_funcionario.cdn_funcionario = INTEGER(ENTRY(3,tt-epc.val-parameter,";"))
             NO-ERROR.
      END.
   END.
END.

IF p-ind-event = "mes_tradic" THEN DO:
   FOR EACH tt-epc 
            WHERE tt-epc.cod-event = p-ind-event
            NO-LOCK:
      ASSIGN i_num_ano_refer_fp = INTEGER(SUBSTRING(tt-epc.val-parameter,9,4))
             i_num_mes_refer_fp = INTEGER(SUBSTRING(tt-epc.val-parameter,7,2)).
   END.
END.

IF p-ind-event = "antes-calculo" THEN DO:
   EMPTY TEMP-TABLE tt_funcionario.
   EMPTY TEMP-TABLE tt_funcionario_horas.

   FOR EACH tt-epc 
            WHERE tt-epc.cod-event = p-ind-event
            NO-LOCK:
      ASSIGN dat_proces_mpe_ini = DATE(ENTRY(1,tt-epc.val-parameter,";"))
             dat_proces_mpe_fim = DATE(ENTRY(2,tt-epc.val-parameter,";")).
   END.
END.

/* MESSAGE "p-ind-event " p-ind-event                                                                                    */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                */
/* FOR EACH movto_ptoelet WHERE                                                                                          */
/*          movto_ptoelet.cdn_empresa                 = "800"           AND                                              */
/*          movto_ptoelet.cdn_estab                   = "801"           AND                                              */
/*          movto_ptoelet.cdn_funcionario             = 11915           AND                                              */
/*          movto_ptoelet.num_ano_refer_fp            = 2019            AND                                              */
/*          movto_ptoelet.num_mes_refer_fp           >= 01              AND                                              */
/*          movto_ptoelet.num_mes_refer_fp           <= 02              AND                                              */
/*          movto_ptoelet.cdn_efp                     = "003"           NO-LOCK:                                         */
/*                                                                                                                       */
/*    MESSAGE                                                                                                            */
/*         "p-ind-event " p-ind-event SKIP                                                                               */
/*         "movto_ptoelet.cdn_empresa                          "  movto_ptoelet.cdn_empresa                         skip */
/*         "movto_ptoelet.cdn_estab                            "  movto_ptoelet.cdn_estab                           skip */
/*         "movto_ptoelet.cdn_funcionario                      "  movto_ptoelet.cdn_funcionario                     skip */
/*         "movto_ptoelet.num_ano_refer_fp                     "  movto_ptoelet.num_ano_refer_fp                    skip */
/*         "movto_ptoelet.num_mes_refer_fp                     "  movto_ptoelet.num_mes_refer_fp                    skip */
/*         "movto_ptoelet.idi_tip_fp_calcul                    "  movto_ptoelet.idi_tip_fp_calcul                   skip */
/*         "movto_ptoelet.num_parc_calc_movto_ptoelet          "  movto_ptoelet.num_parc_calc_movto_ptoelet         skip */
/*         "movto_ptoelet.cdn_efp                              "  movto_ptoelet.cdn_efp                             skip */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                             */
/* END.                                                                                                                  */

IF p-ind-event = "Instrutor_b" THEN 
DO: 
   blk:
   FOR EACH tt_funcionario NO-LOCK,
      FIRST funcionario OF tt_funcionario WHERE 
           (funcionario.dat_desligto_func = ?                          OR
            funcionario.dat_desligto_func > dat_proces_mpe_ini)        NO-LOCK,
      FIRST es_categ_ptoelet WHERE                                                                                                    
            es_categ_ptoelet.cdn_clas_func = funcionario.cdn_clas_func AND
            es_categ_ptoelet.cdn_empresa   = funcionario.cdn_empresa   AND
            es_categ_ptoelet.cdn_estab     = funcionario.cdn_estab     AND
            es_categ_ptoelet.cdn_categ_sal = funcionario.cdn_categ_sal NO-LOCK:

      DO d-data = dat_proces_mpe_ini TO dat_proces_mpe_fim:
         CASE WEEKDAY(d-data):
              WHEN 1 THEN ASSIGN d-qt-horas = es_categ_ptoelet.qtd_domingo * 3600.
              WHEN 2 THEN ASSIGN d-qt-horas = es_categ_ptoelet.qtd_segunda * 3600.
              WHEN 3 THEN ASSIGN d-qt-horas = es_categ_ptoelet.qtd_terca   * 3600.
              WHEN 4 THEN ASSIGN d-qt-horas = es_categ_ptoelet.qtd_quarta  * 3600.
              WHEN 5 THEN ASSIGN d-qt-horas = es_categ_ptoelet.qtd_quinta  * 3600.
              WHEN 6 THEN ASSIGN d-qt-horas = es_categ_ptoelet.qtd_sexta   * 3600.
              WHEN 7 THEN ASSIGN d-qt-horas = es_categ_ptoelet.qtd_sabado  * 3600.
         END CASE.
     
         CREATE tt_funcionario_horas.
         ASSIGN tt_funcionario_horas.cdn_empresa            = funcionario.cdn_empresa
                tt_funcionario_horas.cdn_estab              = funcionario.cdn_estab
                tt_funcionario_horas.cdn_funcionario        = funcionario.cdn_funcionario
                tt_funcionario_horas.cdn_clas_func          = funcionario.cdn_clas_func
                tt_funcionario_horas.cdn_categ_sal          = funcionario.cdn_categ_sal
                tt_funcionario_horas.dat_marcac_ptoelet     = d-data
                tt_funcionario_horas.qti_hrs_marcac_ptoelet = d-qt-horas
                tt_funcionario_horas.cdn_efp                = es_categ_ptoelet.cdn_efp.

         IF d-qt-horas = 0 THEN
            ASSIGN tt_funcionario_horas.des_motivo    = "Dia n∆o considerado para o funcion†rio: N∆o trabalhado (" + STRING(ENTRY(WEEKDAY(d-data),c-weekday)) + ")" 
                   tt_funcionario_horas.log_considera = NO.   



         IF es_categ_ptoelet.calc_pto_turno_dif = YES                                                           AND
            CAN-FIND(FIRST det_calend_turma_localid WHERE
                           det_calend_turma_localid.cdn_turma_trab    = funcionario.cdn_turma_trab              AND 
                           det_calend_turma_localid.cdn_turno_trab    = funcionario.cdn_turno_trab              AND 
                           det_calend_turma_localid.cod_pais          = funcionario.cod_pais                    AND 
                           det_calend_turma_localid.cdn_localidade    = funcionario.cdn_localidade              AND
                           det_calend_turma_localid.dat_refer_calend  = tt_funcionario_horas.dat_marcac_ptoelet AND 
                           det_calend_turma_localid.idi_sit_dia_trab  = 1                                       NO-LOCK) THEN
            ASSIGN tt_funcionario_horas.log_considera = YES
                   tt_funcionario_horas.des_motivo    = "".
      END.
   END. /* for each tt_funcionario */

   FOR EACH tt_funcionario_horas
            WHERE tt_funcionario_horas.qti_hrs_marcac_ptoelet <> 0
            EXCLUSIVE-LOCK:
      /* Verifica se o usuario marcou ponto no dia */
      IF CAN-FIND (FIRST marcac_ptoelet
                   WHERE marcac_ptoelet.cdn_empresa     = tt_funcionario_horas.cdn_empresa      AND
                         marcac_ptoelet.cdn_estab       = tt_funcionario_horas.cdn_estab        AND
                         marcac_ptoelet.cdn_funcionario = tt_funcionario_horas.cdn_funcionario  AND
                         marcac_ptoelet.dat_proces_mpe  = tt_funcionario_horas.dat_marcac_ptoelet
                   NO-LOCK ) THEN DO:
         ASSIGN l-entrou = NO.

         FOR EACH sit_afast_func
                  WHERE sit_afast_func.cdn_empresa         = tt_funcionario_horas.cdn_empresa        AND
                        sit_afast_func.cdn_estab           = tt_funcionario_horas.cdn_estab          AND
                        sit_afast_func.cdn_funcionario     = tt_funcionario_horas.cdn_funcionario    AND
                        sit_afast_func.dat_inic_sit_afast >= tt_funcionario_horas.dat_marcac_ptoelet AND
                        sit_afast_func.dat_inic_sit_afast <= tt_funcionario_horas.dat_marcac_ptoelet NO-LOCK:
            IF CAN-FIND(FIRST es_categ_ptoelet_sit
                        WHERE es_categ_ptoelet_sit.cdn_clas_func      = tt_funcionario_horas.cdn_clas_func      AND
                              es_categ_ptoelet_sit.cdn_empresa        = tt_funcionario_horas.cdn_empresa        AND
                              es_categ_ptoelet_sit.cdn_estab          = tt_funcionario_horas.cdn_estab          AND
                              es_categ_ptoelet_sit.cdn_categ_sal      = tt_funcionario_horas.cdn_categ_sal      AND
                              es_categ_ptoelet_sit.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func       NO-LOCK) THEN
               ASSIGN tt_funcionario_horas.log_considera = YES.            
            ELSE
               RUN pi-situacao.

            ASSIGN l-entrou = YES.
         END.

         IF l-entrou = NO THEN
            ASSIGN tt_funcionario_horas.log_considera = YES.
      END.
      ELSE DO:
         /* Verifica se existe situaá∆o para o funcionario nessa data que permita receber as horas */
         FOR EACH sit_afast_func
                  WHERE sit_afast_func.cdn_empresa         = tt_funcionario_horas.cdn_empresa        AND
                        sit_afast_func.cdn_estab           = tt_funcionario_horas.cdn_estab          AND
                        sit_afast_func.cdn_funcionario     = tt_funcionario_horas.cdn_funcionario    AND
                        sit_afast_func.dat_inic_sit_afast >= tt_funcionario_horas.dat_marcac_ptoelet AND
                        sit_afast_func.dat_inic_sit_afast <= tt_funcionario_horas.dat_marcac_ptoelet
                  NO-LOCK:
            IF CAN-FIND(FIRST es_categ_ptoelet_sit
                        WHERE es_categ_ptoelet_sit.cdn_clas_func      = tt_funcionario_horas.cdn_clas_func      AND
                              es_categ_ptoelet_sit.cdn_empresa        = tt_funcionario_horas.cdn_empresa        AND
                              es_categ_ptoelet_sit.cdn_estab          = tt_funcionario_horas.cdn_estab          AND
                              es_categ_ptoelet_sit.cdn_categ_sal      = tt_funcionario_horas.cdn_categ_sal      AND
                              es_categ_ptoelet_sit.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func
                        NO-LOCK)  THEN DO:
               ASSIGN tt_funcionario_horas.log_considera = YES.
            END.
            ELSE
               RUN pi-situacao.
         END.
      END.

      IF tt_funcionario_horas.log_considera THEN DO:
         RUN pi-cria-evento.
      END.
      ELSE DO:
         IF tt_funcionario_horas.des_motivo    = "" THEN DO: 
            ASSIGN tt_funcionario_horas.des_motivo    = "Dia n∆o considerado para o funcion†rio: N∆o trabalhado (" + STRING(ENTRY(WEEKDAY(tt_funcionario_horas.dat_marcac_ptoelet),c-weekday)) + ")".
         END.
      END.

   END.

   ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "pe2200-upc_" + REPLACE (STRING(TODAY,"99/99/9999"),"/","") + REPLACE (STRING(TIME,"hh:mm:ss"),":","") + ".csv".

   OUTPUT TO VALUE (c-arquivo) NO-CONVERT NO-MAP.

   PUT "Empresa;Estabelecimento;Funcionario;Classe;Categoria;Data;Quantidade Horas;Considera?;Motivo" SKIP.

   blk_log:
   FOR EACH tt_funcionario_horas 
            NO-LOCK:
      PUT tt_funcionario_horas.cdn_empresa               ";"
          tt_funcionario_horas.cdn_estab                 ";"
          tt_funcionario_horas.cdn_funcionario           ";"
          tt_funcionario_horas.cdn_clas_func             ";"
          tt_funcionario_horas.cdn_categ_sal             ";"
          tt_funcionario_horas.dat_marcac_ptoelet        ";"
          string(tt_funcionario_horas.qti_hrs_marcac_ptoelet,"hh:mm:ss")    ";"
          tt_funcionario_horas.log_considera             ";" 
          tt_funcionario_horas.des_motivo                SKIP .
   END.

   OUTPUT CLOSE.
END.

PROCEDURE pi-cria-evento:
    FIND FIRST efp_par_marcac_ptoelet
         WHERE efp_par_marcac_ptoelet.cdn_empresa         = tt_funcionario_horas.cdn_empresa         AND   
               efp_par_marcac_ptoelet.cdn_estab           = tt_funcionario_horas.cdn_estab           AND   
               efp_par_marcac_ptoelet.cdn_funcionario     = tt_funcionario_horas.cdn_funcionario     AND   
               efp_par_marcac_ptoelet.dat_proces_mpe      = tt_funcionario_horas.dat_marcac_ptoelet  AND   
               efp_par_marcac_ptoelet.dat_marcac_ptoelet  = tt_funcionario_horas.dat_marcac_ptoelet  AND   
               efp_par_marcac_ptoelet.cdn_efp             = tt_funcionario_horas.cdn_efp 
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE(efp_par_marcac_ptoelet) THEN
       DELETE efp_par_marcac_ptoelet.

   CREATE efp_par_marcac_ptoelet.
   ASSIGN efp_par_marcac_ptoelet.cdn_empresa                    = tt_funcionario_horas.cdn_empresa
          efp_par_marcac_ptoelet.cdn_estab                      = tt_funcionario_horas.cdn_estab
          efp_par_marcac_ptoelet.cdn_funcionario                = tt_funcionario_horas.cdn_funcionario
          efp_par_marcac_ptoelet.dat_proces_mpe                 = tt_funcionario_horas.dat_marcac_ptoelet
          efp_par_marcac_ptoelet.dat_marcac_ptoelet             = tt_funcionario_horas.dat_marcac_ptoelet
          efp_par_marcac_ptoelet.num_horar_inic_proces_mpe      = 0
          efp_par_marcac_ptoelet.num_horar_fim_proces_mpe       = 0
          efp_par_marcac_ptoelet.cdn_efp                        = tt_funcionario_horas.cdn_efp
          efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet         = tt_funcionario_horas.qti_hrs_marcac_ptoelet
          efp_par_marcac_ptoelet.idi_sit_mpe                    = 1
          efp_par_marcac_ptoelet.cdn_sit_afast_func             = 0
          efp_par_marcac_ptoelet.idi_tip_ocor_ptoelet           = 3
          efp_par_marcac_ptoelet.num_mes_ano_refer_fp           = STRING(i_num_ano_refer_fp,"9999") + STRING(i_num_mes_refer_fp,"99")
          efp_par_marcac_ptoelet.log_marcac_ptoelet_tratada     = NO
/*           efp_par_marcac_ptoelet.log_hrs_diurno                 = NO */
          efp_par_marcac_ptoelet.log_hrs_diurno                 = YES
          efp_par_marcac_ptoelet.cod_usuar_ult_atualiz          = ""
          efp_par_marcac_ptoelet.dat_ult_atualiz                = TODAY
          efp_par_marcac_ptoelet.hra_ult_atualiz                = STRING(TIME,"HH:MM:SS")
          efp_par_marcac_ptoelet.num_ano_refer_fp               = i_num_ano_refer_fp
          efp_par_marcac_ptoelet.num_mes_refer_fp               = i_num_mes_refer_fp
          efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet_orig    = tt_funcionario_horas.qti_hrs_marcac_ptoelet
          efp_par_marcac_ptoelet.cod_tip_dia                    = "1"
          efp_par_marcac_ptoelet.cdn_sindicato                  = 1
          efp_par_marcac_ptoelet.log_integr_gerac_mpe           = NO
          efp_par_marcac_ptoelet.cdn_obs_func                   = 0
   NO-ERROR.


END PROCEDURE.


PROCEDURE pi-situacao:
   FIND FIRST sit_afast OF sit_afast_func
        NO-LOCK NO-ERROR.

   ASSIGN c-descricao = IF AVAILABLE(sit_afast) THEN sit_afast.des_sit_afast ELSE "".

   ASSIGN tt_funcionario_horas.des_motivo    = "Situaá∆o : " + STRING(c-descricao,"x(200)").
          tt_funcionario_horas.log_considera = NO.
END PROCEDURE.
