/*******************************************************************************
** Programa: pe4000-upc
** Autor...: Serginho - DKP
** Data....: 10/2017
** OBS.....: UPC utilizada pelo programa pe4000rp.p
** Objetivo: C lculo de valor adicional ao ponto eletr“nico
*******************************************************************************
** Data             Autor            Projeto Altera‡Æo                   
** 19/04/2018       DPC/DKP          Yamana HCM - Ponto in itineri
**
**
******************************************************************************/
DEFINE NEW GLOBAL SHARED TEMP-TABLE tt_funcionario_pe4000 
       FIELDS cdn_empresa              LIKE funcionario.cdn_empresa
       FIELDS cdn_estab                LIKE funcionario.cdn_estab
       FIELDS cdn_funcionario          LIKE funcionario.cdn_funcionario
       FIELDS dat_proces_mpe_ini       LIKE efp_par_marcac_ptoelet.dat_proces_mpe
       FIELDS dat_proces_mpe_fim       LIKE efp_par_marcac_ptoelet.dat_proces_mpe
       FIELDS qtd_movto_ptoelet        LIKE movto_ptoelet.qtd_movto_ptoelet
       FIELDS cdn_efp                  LIKE efp_par_marcac_ptoelet.cdn_efp
    INDEX funcionario IS PRIMARY UNIQUE cdn_empresa       
                                        cdn_estab         
                                        cdn_funcionario.

DEFINE TEMP-TABLE tt_movto_ptoelet LIKE movto_ptoelet.

DEFINE VARIABLE c-arquivo   AS   CHARACTER                                       NO-UNDO.
DEFINE VARIABLE d_qti_hrs_marcac_ptoelet LIKE movto_ptoelet.qtd_movto_ptoelet NO-UNDO.

/* Include i-epc200.i: Defini»’o Temp-Table tt-epc */
{include/i-epc200.i1}

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

IF p-ind-event = "INTEGRA" THEN DO:
   EMPTY TEMP-TABLE tt_funcionario_pe4000.
END.

IF p-ind-event = "Instrutor_a" THEN DO:
   FOR EACH tt-epc 
            WHERE tt-epc.cod-event = p-ind-event
            NO-LOCK:
      IF NOT CAN-FIND(FIRST tt_funcionario_pe4000
                      WHERE tt_funcionario_pe4000.cdn_empresa     = ENTRY(1,tt-epc.val-parameter,";")  AND
                            tt_funcionario_pe4000.cdn_estab       = ENTRY(2,tt-epc.val-parameter,";")  AND        
                            tt_funcionario_pe4000.cdn_funcionario = INTEGER(ENTRY(3,tt-epc.val-parameter,";")) 
                      NO-LOCK) THEN DO:
         CREATE tt_funcionario_pe4000.
         ASSIGN tt_funcionario_pe4000.cdn_empresa        =         ENTRY(1,tt-epc.val-parameter,";")
                tt_funcionario_pe4000.cdn_estab          =         ENTRY(2,tt-epc.val-parameter,";")
                tt_funcionario_pe4000.cdn_funcionario    = INTEGER(ENTRY(3,tt-epc.val-parameter,";"))
                tt_funcionario_pe4000.dat_proces_mpe_ini =    DATE(ENTRY(4,tt-epc.val-parameter,";"))
                tt_funcionario_pe4000.dat_proces_mpe_fim =    DATE(ENTRY(5,tt-epc.val-parameter,";"))
             NO-ERROR.
      END.
   END.
END.

IF p-ind-event = "Instrutor_b" THEN DO: 

   blk:
   FOR EACH tt_funcionario_pe4000
            NO-LOCK:
      FOR EACH funcionario OF tt_funcionario_pe4000
               NO-LOCK,
               FIRST clas_func OF funcionario
               NO-LOCK,
               FIRST es_categ_ptoelet OF clas_func
               NO-LOCK,
               FIRST categ_sal OF funcionario
               NO-LOCK:

         ASSIGN d_qti_hrs_marcac_ptoelet = 0.

         /* Verifica os lan‡amentos do funcionario */
         blk2:
         FOR EACH efp_par_marcac_ptoelet OF funcionario
                  WHERE efp_par_marcac_ptoelet.dat_proces_mpe >= tt_funcionario_pe4000.dat_proces_mpe_ini AND
                        efp_par_marcac_ptoelet.dat_proces_mpe <= tt_funcionario_pe4000.dat_proces_mpe_fim   AND
                        efp_par_marcac_ptoelet.cdn_efp         = es_categ_ptoelet.cdn_efp
                  NO-LOCK:

          
            ASSIGN d_qti_hrs_marcac_ptoelet = d_qti_hrs_marcac_ptoelet + (efp_par_marcac_ptoelet.qti_hrs_marcac_ptoelet / 3600).
         END.

         ASSIGN tt_funcionario_pe4000.qtd_movto_ptoelet = d_qti_hrs_marcac_ptoelet
                tt_funcionario_pe4000.cdn_efp           = es_categ_ptoelet.cdn_efp.

         /* Verificar se esta valida‡Æo nÆo acumula com a pr¢xima... es_categ_ptoelet...*/
         IF tt_funcionario_pe4000.qtd_movto_ptoelet <> 0 THEN
            RUN pi-gera-movimento.

         /* Para os funcionarios que nÆo usam ponto eletronico cria as qtds de acrescimo - ajuda de custo */
         IF es_categ_ptoelet.calc_pto_turno_dif THEN
             RUN pi-gera-movto-dif.

      END.
   END.

   ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "pe4000-upc_" + REPLACE (STRING(TODAY,"99/99/9999"),"/","") + REPLACE (STRING(TIME,"hh:mm:ss"),":","") + ".csv".

   OUTPUT TO VALUE (c-arquivo) NO-CONVERT NO-MAP.

   PUT "Empresa;Estabelecimento;Funcionario;Data Inicial;Data Final;Evento;Quantidade" SKIP.

   blk_log:
   FOR EACH tt_funcionario_pe4000 
            NO-LOCK:
      PUT tt_funcionario_pe4000.cdn_empresa         ";"
          tt_funcionario_pe4000.cdn_estab           ";"
          tt_funcionario_pe4000.cdn_funcionario     ";"
          tt_funcionario_pe4000.dat_proces_mpe_ini  ";"
          tt_funcionario_pe4000.dat_proces_mpe_fim  ";"
          tt_funcionario_pe4000.cdn_efp             ";"
          tt_funcionario_pe4000.qtd_movto_ptoelet   ";" SKIP.
   END.

   OUTPUT CLOSE.
END.

PROCEDURE pi-gera-movimento:
   EMPTY TEMP-TABLE tt_movto_ptoelet.

   CREATE tt_movto_ptoelet.
   ASSIGN tt_movto_ptoelet.cdn_empresa                 = tt_funcionario_pe4000.cdn_empresa 
          tt_movto_ptoelet.cdn_estab                   = tt_funcionario_pe4000.cdn_estab                  
          tt_movto_ptoelet.cdn_funcionario             = tt_funcionario_pe4000.cdn_funcionario            
          tt_movto_ptoelet.num_ano_refer_fp            = categ_sal.num_livre_2           
          tt_movto_ptoelet.num_mes_refer_fp            = categ_sal.num_livre_1           
          tt_movto_ptoelet.idi_tip_fp_calcul           = 1 /*Normal*/
          tt_movto_ptoelet.num_parc_calc_movto_ptoelet = 9
          tt_movto_ptoelet.cdn_efp                     = es_categ_ptoelet.cdn_efp                    
          tt_movto_ptoelet.dat_proces_mpe              = DATE (categ_sal.num_livre_1,1,categ_sal.num_livre_2)             
          tt_movto_ptoelet.qtd_movto_ptoelet           = tt_funcionario_pe4000.qtd_movto_ptoelet          
          tt_movto_ptoelet.val_movto_ptoelet           = 0          
          tt_movto_ptoelet.idi_tip_movto_ptoelet       = 1 /*Horas*/
          tt_movto_ptoelet.qti_hrs_marcac_ptoelet      = tt_funcionario_pe4000.qtd_movto_ptoelet     
          tt_movto_ptoelet.cod_usuar_ult_atualiz       = ""      
          tt_movto_ptoelet.dat_ult_atualiz             = TODAY
          tt_movto_ptoelet.hra_ult_atualiz             = STRING(TIME,"hh:mm:ss").

   IF NOT CAN-FIND(FIRST movto_ptoelet
                   WHERE movto_ptoelet.cdn_empresa                 = tt_movto_ptoelet.cdn_empresa                 AND 
                         movto_ptoelet.cdn_estab                   = tt_movto_ptoelet.cdn_estab                   AND 
                         movto_ptoelet.cdn_funcionario             = tt_movto_ptoelet.cdn_funcionario             AND 
                         movto_ptoelet.num_ano_refer_fp            = tt_movto_ptoelet.num_ano_refer_fp            AND 
                         movto_ptoelet.num_mes_refer_fp            = tt_movto_ptoelet.num_mes_refer_fp            AND 
                         movto_ptoelet.idi_tip_fp_calcul           = tt_movto_ptoelet.idi_tip_fp_calcul           AND 
                         movto_ptoelet.num_parc_calc_movto_ptoelet = tt_movto_ptoelet.num_parc_calc_movto_ptoelet AND 
                         movto_ptoelet.cdn_efp                     = tt_movto_ptoelet.cdn_efp                     
                   NO-LOCK) THEN DO:
       CREATE movto_ptoelet.
       BUFFER-COPY tt_movto_ptoelet TO movto_ptoelet NO-ERROR.
   END.

END PROCEDURE.

/* 19/04/18 - DKP - In itinere */
PROCEDURE pi-gera-movto-dif:
   
   DEFINE VARIABLE c-dtini    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE c-dtfim    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE d-qt-horas AS DECIMAL     NO-UNDO.

   EMPTY TEMP-TABLE tt_movto_ptoelet.

   FOR EACH det_calend_turma_localid NO-LOCK 
        WHERE det_calend_turma_localid.cdn_turma_trab    = funcionario.cdn_turma_trab   
         AND  det_calend_turma_localid.cdn_turno_trab    = funcionario.cdn_turno_trab
         AND  det_calend_turma_localid.cod_pais          = funcionario.cod_pais
         AND  det_calend_turma_localid.cdn_localidade    = funcionario.cdn_localidade
         AND  det_calend_turma_localid.dat_refer_calend >= tt_funcionario_pe4000.dat_proces_mpe_ini
         AND  det_calend_turma_localid.dat_refer_calend <= tt_funcionario_pe4000.dat_proces_mpe_fim, 
         EACH jorn_trab  NO-LOCK 
        WHERE jorn_trab.cdn_jorn_trab = det_calend_turma_localid.cdn_jorn_trab
          AND jorn_trab.cod_tip_dia = "TR": /*Trabalhado */
        
            /* somente considera situa‡Æo trabalhando. Licen‡a e f‚rias sÆo ignoradas */
            IF det_calend_turma_localid.idi_sit_dia_trab <> 1 THEN NEXT. 

            /* Calcula qtd de horas do turno de trabalho para funcion rio */
            /* C lculo efetuado com base no per¡odo de trabalho do calend rio e nos parƒmetros de percentuais extras informados no pe0180*/
             /*
            ASSIGN c-dtini    = STRING(DAY(det_calend_turma_localid.dat_inic_dia_trab)) + "-" + STRING(MONTH(det_calend_turma_localid.dat_inic_dia_trab)) + '-' + STRING(YEAR(det_calend_turma_localid.dat_inic_dia_trab)) + " " + STRING(det_calend_turma_localid.num_horar_inic_dia_trab, "HH:MM:SS")
                   c-dtfim    = STRING(DAY(det_calend_turma_localid.dat_term_dia_trab)) + '-' + STRING(MONTH(det_calend_turma_localid.dat_term_dia_trab)) + '-' + STRING(YEAR(det_calend_turma_localid.dat_term_dia_trab)) + " " + STRING(det_calend_turma_localid.num_horar_term_dia_trab, "HH:MM:SS")
                   d-qt-horas = INTERVAL(DATETIME(c-dtfim),DATETIME(c-dtini),"hours") + 0.30. /* adiciona 30 minutos por dia de trabalho */
                   */
             ASSIGN d-qt-horas = d-qt-horas + 0.50. /* adiciona 30 minutos por dia de trabalho */

   END. /*det_calend_turma_localid*/

   CREATE tt_movto_ptoelet.
   ASSIGN tt_movto_ptoelet.cdn_empresa                 = tt_funcionario_pe4000.cdn_empresa 
          tt_movto_ptoelet.cdn_estab                   = tt_funcionario_pe4000.cdn_estab                  
          tt_movto_ptoelet.cdn_funcionario             = tt_funcionario_pe4000.cdn_funcionario            
          tt_movto_ptoelet.num_ano_refer_fp            = categ_sal.num_livre_2           
          tt_movto_ptoelet.num_mes_refer_fp            = categ_sal.num_livre_1           
          tt_movto_ptoelet.idi_tip_fp_calcul           = 1 /*Normal*/
          tt_movto_ptoelet.num_parc_calc_movto_ptoelet = 9
          tt_movto_ptoelet.cdn_efp                     = es_categ_ptoelet.cdn_efp                    
          tt_movto_ptoelet.dat_proces_mpe              = DATE (categ_sal.num_livre_1,1,categ_sal.num_livre_2)             
          tt_movto_ptoelet.qtd_movto_ptoelet           = d-qt-horas
          tt_movto_ptoelet.val_movto_ptoelet           = 0          
          tt_movto_ptoelet.idi_tip_movto_ptoelet       = 1 /*Horas*/
          tt_movto_ptoelet.qti_hrs_marcac_ptoelet      = d-qt-horas     
          tt_movto_ptoelet.cod_usuar_ult_atualiz       = ""      
          tt_movto_ptoelet.dat_ult_atualiz             = TODAY
          tt_movto_ptoelet.hra_ult_atualiz             = STRING(TIME,"hh:mm:ss").

   IF NOT CAN-FIND(FIRST movto_ptoelet
                   WHERE movto_ptoelet.cdn_empresa                 = tt_movto_ptoelet.cdn_empresa                 AND 
                         movto_ptoelet.cdn_estab                   = tt_movto_ptoelet.cdn_estab                   AND 
                         movto_ptoelet.cdn_funcionario             = tt_movto_ptoelet.cdn_funcionario             AND 
                         movto_ptoelet.num_ano_refer_fp            = tt_movto_ptoelet.num_ano_refer_fp            AND 
                         movto_ptoelet.num_mes_refer_fp            = tt_movto_ptoelet.num_mes_refer_fp            AND 
                         movto_ptoelet.idi_tip_fp_calcul           = tt_movto_ptoelet.idi_tip_fp_calcul           AND 
                         movto_ptoelet.num_parc_calc_movto_ptoelet = tt_movto_ptoelet.num_parc_calc_movto_ptoelet AND 
                         movto_ptoelet.cdn_efp                     = tt_movto_ptoelet.cdn_efp                     
                   NO-LOCK) THEN DO:
       CREATE movto_ptoelet.
       BUFFER-COPY tt_movto_ptoelet TO movto_ptoelet NO-ERROR.
   END.

END PROCEDURE.
