/****************************************************************************************** 
** 	   Programa: APYA510RP.P
**   	  Autor: Sergio Luiz Neto da Silveira - DSC
** 	 Fornecedor: DKP
**         Data: 17/0702018
** Change/Chamado: 
**      Objetivo: Relatorio Intercompany
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
/************************************* INCLUDES PADRAO **************************************/
/* include de controle de versío */
{include/i-prgvrs.i APYA510RP 12.1.17.028}
{include/i-rpvar.i}
{prgfin/apl/apya510.i}
/********************************************************************************************/
DEF BUFFER ext_movto_operac_financ FOR ems5_esp.ext_movto_operac_financ.
DEFINE VARIABLE d-valor-dolar-pagto  AS DECIMAL     NO-UNDO.
/********************************* DEFINICAO DE TEMP-TABLES *********************************/
/* defini¬∆o das temp-tables para recebimento de par§metros */
define temp-table tt-param no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    FIELD cod_emp_ini               AS CHAR
    FIELD cod_emp_fim               AS CHAR
    FIELD cod_tip_produt_financ_ini as character
    FIELD cod_tip_produt_financ_fim as character
    FIELD cod_produt_financ_ini     as character
    FIELD cod_produt_financ_fim     as character
    FIELD cod_admdra_apf_ini        as character
    FIELD cod_admdra_apf_fim        as character
    FIELD cod_operac_financ_ini     as character
    FIELD cod_operac_financ_fim     as character
    FIELD data_movimento_ini        AS DATE
    FIELD data_movimento_fim        AS DATE
    FIELD cod_banco_ini             AS CHAR
    FIELD cod_banco_fim             AS CHAR.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.

DEFINE STREAM st-excel.
/********************************************************************************************/


/********************************* DEFINICAO DE PARAMETROS **********************************/
/* recebimento de par§metros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST tt-param.
/********************************************************************************************/

/********************************** DEFINICAO DE VARIAVEIS **********************************/
define variable h-acomp        as handle    no-undo.
DEFINE VARIABLE i-cont AS INTEGER NO-UNDO.

DEFINE VARIABLE c-modelo          AS CHAR FORMAT "X(256)"              NO-UNDO.
DEFINE VARIABLE excelappl         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE workbooks         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE worksheets        AS COM-HANDLE                        NO-UNDO.

DEF VAR c_cambio AS CHAR NO-UNDO.

DEFINE BUFFER banco FOR ems5.banco. 

/* DPC - Criaá∆o de Tempor†ria para gravar os dados do processamento do relat¢rio antes de abrir o excel */
DEF TEMP-TABLE tt-dados
    FIELD tipo_produto       AS CHAR
    FIELD cod_produt_finan   AS CHAR
    FIELD cod_operac_financ  AS CHAR
    FIELD DESC_produt_financ AS CHAR
    FIELD emp_ext       AS CHAR
    FIELD cod_atu_plan  AS CHAR
    FIELD cod_emp       AS CHAR 
    FIELD cod_emp_mod   AS CHAR
    /* Dados contrato m∆e */
    FIELD mae_cod_contr  AS CHAR
    FIELD mae_moeda_ctr  AS CHAR
    FIELD mae_vl_contrat AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mae_vl_sl_ini  AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mae_sl_contrat AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mae_dt_ini     AS DATE
    FIELD mae_dt-fim     AS DATE
    /* Dados contrato filho */
    FIELD fi_inst_financ   AS CHAR
    FIELD fi_contr_entrada AS CHAR
    FIELD fi_rof           AS CHAR
    FIELD fi_dt_ini_f      AS DATE
    FIELD fi_dt_fim_f      AS DATE
    FIELD moeda            AS CHAR
    FIELD dt_tx_oper       AS DATE
    FIELD fi_tx_oper       AS DEC FORMAT "->>9.9999"
    FIELD valor_operacao   AS DEC
    /* Movto emprÇstimos */
    FIELD me_sl_principal AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_sl_juros     AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_sl_ir        AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_novos_emp    AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_juros_prov   AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_dif_cambial  AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_prov_irrf    AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_pg_principal AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_pg_juro      AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD me_sl_final     AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    /* movto emprÇstimos */
    FIELD mv_sl_emp       AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mv_novo_emp     AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mv_juros_prov   AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mv_dif_cambial  AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mv_prv_irrf     AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mv_pg_principal AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mv_pg_juros     AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD mv_sl_final     AS DEC FORMAT "->>>,>>>,>>>,>>9.99"
    /* Conta Cont†bil */
    FIELD cc_principal     AS CHAR
    FIELD cc_desc          AS CHAR
    FIELD cc_var_cambial   AS CHAR
    FIELD cc_desc_var_camb AS CHAR
    FIELD cc_juros         AS CHAR
    FIELD cc_desc_juros    AS CHAR

    FIELD rowid-movto      AS ROWID.

/********************************************************************************************/
/************************************ DEFINICAO DE FUNCOES **********************************/

FUNCTION fcUltDiaMes RETURN DATE (INPUT da-data  AS DATE):

    DEFINE VARIABLE d-data-ult AS DATE  NO-UNDO.

    ASSIGN d-data-ult = DATE(MONTH(da-data),28,YEAR(da-data)) + 4.

    ASSIGN d-data-ult = DATE(MONTH(d-data-ult),1,YEAR(d-data-ult)) - 1.

    RETURN d-data-ult.

END FUNCTION.

FUNCTION fcDolar RETURN DECIMAL (INPUT ip-valor AS DECIMAL,
                                 INPUT ip-data  AS DATE):
   DEFINE VARIABLE de-valor-conv AS DECIMAL NO-UNDO.

   RUN cdp/cd0812.p (0,1,ip-valor,ip-data,OUTPUT de-valor-conv).

   RETURN de-valor-conv.

END FUNCTION.

FUNCTION fcValorDolarData RETURN DECIMAL (INPUT ip-data  AS DATE):
   DEFINE VARIABLE de-valor-conv AS DECIMAL NO-UNDO.

   RUN cdp/cd0812.p (1,0,1,ip-data,OUTPUT de-valor-conv).

   RETURN de-valor-conv.

END FUNCTION.

/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
/************************************* BLOCO PRINCIPAL **************************************/
run utp/ut-acomp.p PERSISTENT SET h-acomp. 
                                           
{utp/ut-liter.i Imprimindo *}              

{include/i-rpcab.i}
{include/i-rpout.i}

run pi-inicializar IN h-acomp (INPUT RETURN-VALUE). 
/* include padrío para output de relat˜rios */
RUN pi-dados.

RUN pi-excel-abrir.
RUN pi-impressao.
RUN pi-excel-fechar.

/* fechamento do output do relat¢rio */
RUN pi-finalizar IN h-acomp. 

{include/i-rpclo.i}
RETURN "OK":U.
/********************************************************************************************/
DEFINE VARIABLE d-dolar-ini   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE d-dolar-fim   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE d-dolar-medio AS DECIMAL     NO-UNDO FORMAT ">>9.99999".


/********************************* DEFINICAO DE procedureS **********************************/
PROCEDURE pi-dados:
    
   DEFINE VARIABLE d-valor-real     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE d-dolar-medio-alt AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE d-valor-dolar AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE v_perc_impto  AS DECIMAL   NO-UNDO FORMAT ">>9.99999".
   
   DEFINE VARIABLE c_cod_tip_produt_financ       LIKE operac_financ.cod_tip_produt_financ NO-UNDO.
   DEFINE VARIABLE v_val_movto_apl_pagamento     LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_pgto-ini      LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_juros         LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_juros-ini     LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_variacao      LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_variacao-ini  LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_liberacao     LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_saldo         LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_novos_emp     LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_saldo_inicial LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   
   DEFINE VARIABLE v_val_juros_operac_financ         LIKE operac_financ.val_juros_operac_financ NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_impto_operac_financ         LIKE operac_financ.val_impto_operac_financ NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_impto_operac_financ_imposto LIKE operac_financ.val_impto_operac_financ NO-UNDO EXTENT 3.
   DEFINE VARIABLE v_val_sdo_dat                     LIKE operac_financ.val_operac_financ NO-UNDO.
   DEFINE VARIABLE v_val_sdo_princ_operac_financ     LIKE operac_financ.val_operac_financ NO-UNDO.
   DEFINE VARIABLE v_val_sdo_dat_moeda               LIKE operac_financ.val_operac_financ NO-UNDO.
   DEFINE VARIABLE v_val_sdo_princ_operac_financ_moeda LIKE operac_financ.val_operac_financ NO-UNDO.
   DEFINE VARIABLE v_val_movto_apl_saldo_inicial-tot LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_novos_emp-tot     LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_juros_operac_financ-tot     LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_variacao-tot      LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_impto_operac_financ-tot     LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_pagamento-tot     LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_movto_apl_juros-tot         LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_val_provisao_juros              LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 3.
   DEFINE VARIABLE v_val_impto_sdo-ini               LIKE movto_operac_financ.val_movto_apl   NO-UNDO EXTENT 2.
   DEFINE VARIABLE v_vl_tx_juros_operac              LIKE indic_econ_operac_financ.val_tax_juros_operac_financ NO-UNDO.
   DEFINE VARIABLE v_dt_tx_juros_operac              AS DATE    NO-UNDO.
   

   FIND FIRST param_cenar_ctbl_apl NO-LOCK NO-ERROR.

   /* Ser† usado apenas qdo n∆o existir o dolar mÇdio informado na cotaá∆o do contrato */
   RUN prgfin\apl\dolar-medio.p(INPUT DATE(tt-param.data_movimento_ini),
                                INPUT DATE(tt-param.data_movimento_fim),
                                OUTPUT d-dolar-medio-alt).

   FIND FIRST param_cenar_ctbl_apl WHERE 
              param_cenar_ctbl_apl.cod_cenar_ctbl = "CONTSOC" NO-LOCK NO-ERROR.

   /* Calcula os valores de dolar que ser∆o utilizados */
     
    /* Verificar esta l¢gica - convers∆o de moeda */
    FIND FIRST cotac_parid WHERE 
               cotac_parid.cod_indic_econ_base  = 'real'  AND
               cotac_parid.cod_indic_econ_idx   = 'dolar' AND
               cotac_parid.dat_cotac_indic_econ = tt-param.data_movimento_ini NO-LOCK NO-ERROR.
    IF AVAIL cotac_parid THEN
        ASSIGN d-dolar-ini = cotac_parid.val_cotac_indic_econ.
     FIND FIRST cotac_parid WHERE 
               cotac_parid.cod_indic_econ_base  = 'real'  AND
               cotac_parid.cod_indic_econ_idx   = 'dolar' AND
               cotac_parid.dat_cotac_indic_econ = tt-param.data_movimento_fim  NO-LOCK NO-ERROR.
    IF AVAIL cotac_parid THEN
        ASSIGN d-dolar-fim = cotac_parid.val_cotac_indic_econ.

    blk:
    FOR EACH operac_financ NO-LOCK 
           WHERE operac_financ.cod_banco                   >= tt-param.cod_banco_ini 
             AND operac_financ.cod_banco                   <= tt-param.cod_banco_fim 
             AND operac_financ.cod_produt_financ           >= tt-param.cod_produt_financ_ini
             AND operac_financ.cod_produt_financ           <= tt-param.cod_produt_financ_fim
             AND operac_financ.cod_operac_financ           >= tt-param.cod_operac_financ_ini  
             AND operac_financ.cod_operac_financ           <= tt-param.cod_operac_financ_fim
             AND operac_financ.cod_empresa                 >= tt-param.cod_emp_ini
             AND operac_financ.cod_empresa                 <= tt-param.cod_emp_fim
             AND (operac_financ.ind_sit_operac_financ_apl   = "ATIVA"
              OR operac_financ.ind_sit_operac_financ_apl    = "ENCERRADA"), 
        EACH produt_financ OF operac_financ NO-LOCK 
            WHERE produt_financ.cod_tip_produt_financ >= tt-param.cod_tip_produt_financ_ini 
              AND produt_financ.cod_tip_produt_financ <= tt-param.cod_tip_produt_financ_fim,
        FIRST banco OF operac_financ NO-LOCK:

          RUN pi-acompanhar IN h-acomp ("Operaá∆o: " + operac_financ.cod_operac_financ). 

          ASSIGN v_val_movto_apl_pagamento     = 0
                 v_val_movto_apl_pgto-ini      = 0
                 v_val_movto_apl_juros         = 0
                 v_val_movto_apl_juros-ini     = 0
                 v_val_movto_apl_variacao      = 0
                 v_val_movto_apl_variacao-ini  = 0
                 v_val_movto_apl_liberacao     = 0
                 v_val_movto_apl_saldo         = 0
                 v_val_movto_apl_novos_emp     = 0
                 v_val_movto_apl_saldo_inicial = 0
                 v_val_juros_operac_financ           = 0  
                 v_val_impto_operac_financ           = 0
                 v_val_impto_operac_financ_imposto   = 0
                 v_val_movto_apl_saldo_inicial-tot   = 0
                 v_val_movto_apl_novos_emp-tot       = 0
                 v_val_juros_operac_financ-tot       = 0
                 v_val_movto_apl_variacao-tot        = 0
                 v_val_impto_operac_financ-tot       = 0
                 v_val_movto_apl_pagamento-tot       = 0
                 v_val_movto_apl_juros-tot           = 0
                 v_val_provisao_juros                = 0
                 v_val_juros_sdo-ini                 = 0
                 v_impto_ir                          = 0
                 v_vl_tx_juros_operac                = 0
                 v_dt_tx_juros_operac                = ?
                 v_val_sdo_dat                       = 0                 
                 v_val_sdo_princ_operac_financ       = 0
                 v_val_sdo_princ_operac_financ_moeda = 0
                 v_val_sdo_dat_moeda                 = 0
                 v_val_impto_sdo-ini                 = 0.

          /* Saldo Inicial - Na moeda do contrato*/
          RUN prgfin/apl/apl757zb.py (Input 1,
                                      Input (tt-param.data_movimento_ini),
                                      Input operac_financ.num_id_operac_financ,
                                      output v_val_sdo_dat,
                                      output v_val_sdo_princ_operac_financ,
                                      Input "CONTSOC") /*prg_api_recompor_saldo_emprestimo*/.

          /* Saldo inicial em reais */
          RUN prgfin/apl/apl767za.py (Input 1,
                                      Input (tt-param.data_movimento_ini  ),
                                      Input operac_financ.num_id_operac_financ,
                                      Input "REAL",
                                      output v_val_sdo_dat_moeda,
                                      output table tt_log_erro,
                                      Input "CONTSOC",
                                      output v_val_sdo_princ_operac_financ_moeda) /*prg_api_recompor_saldo_emprestimo_moeda*/.
                  
          FIND FIRST es_operac_financ OF operac_financ NO-LOCK NO-ERROR.
          
          FIND FIRST contrat_apf NO-LOCK 
              WHERE contrat_apf.cod_empresa      = operac_financ.cod_empresa 
                AND contrat_apf.cod_contrat_apf  = es_operac_financ.cod_contrat_apf NO-ERROR.
          IF AVAIL contrat_apf AND (contrat_apf.cod_admdra_apf < tt-param.cod_admdra_apf_ini OR contrat_apf.cod_admdra_apf > tt-param.cod_admdra_apf_fim) THEN NEXT.
                                                    
          FIND FIRST admdra_apf NO-LOCK
                 WHERE admdra_apf.cod_admdra_apf = contrat_apf.cod_admdra_apf NO-ERROR.    
          
          FIND LAST es_operac_financ_rof OF es_operac_financ NO-LOCK NO-ERROR.
    
           /* Valor para Taxa da Operaá∆o - pesquisa a taxa v†lida dentro do per°odo selecionado */

          FOR EACH indic_econ_operac_financ OF operac_financ NO-LOCK:
              
              IF indic_econ_operac_financ.dat_inic_valid > tt-param.data_movimento_fim THEN NEXT.
              IF indic_econ_operac_financ.dat_fim_valid < tt-param.data_movimento_ini  THEN NEXT.  

              ASSIGN v_vl_tx_juros_operac = indic_econ_operac_financ.val_tax_juros_operac_financ
                     v_dt_tx_juros_operac = indic_econ_operac_financ.dat_fim_valid.
          END.
          
          ASSIGN c_cambio = "".
          FOR EACH es_operac_financ_cambio NO-LOCK                                                              
                     WHERE es_operac_financ_cambio.cod_banco          = es_operac_financ.cod_banco                 
                     AND   es_operac_financ_cambio.cod_produt_financ  = es_operac_financ.cod_produt_financ         
                     AND   es_operac_financ_cambio.cod_operac_financ  = es_operac_financ.cod_operac_financ         
                     AND   es_operac_financ_cambio.cod_contrat_apf    = es_operac_financ.cod_contrat_apf :
             IF NOT es_operac_financ.log_atualizado THEN NEXT. /* Apenas para operaá‰es atualizadas - Em digitaá∆o n∆o entrar† */
             ASSIGN c_cambio = (IF c_cambio = "" THEN es_operac_financ_cambio.cod_cambio_operacao ELSE c_cambio + "/" + es_operac_financ_cambio.cod_cambio_operacao).
      
          END.
      
          /* Aditivos - A data do contrato m∆e refletir† a data do aditivo */
          FIND LAST aditivo_contrat_apf NO-LOCK OF contrat_apf USE-INDEX idx1 NO-ERROR.
      
          /* Considera a movimentaá∆o da data depois do inicio */
          /* O Saldo inicial j† considera toda a movimentaá∆o atÇ a data de in°cio, assim as movimentaá‰es n∆o precisam ser consideradas movamente */
          FOR EACH movto_operac_financ NO-LOCK
             WHERE movto_operac_financ.num_id_operac_financ = operac_financ.num_id_operac_financ
               AND movto_operac_financ.dat_transacao <= tt-param.data_movimento_fim:

              FIND contrato_cotac NO-LOCK 
                  WHERE contrato_cotac.mo-codigo = 1 
                  AND   contrato_cotac.data-periodo = DATE(MONTH(movto_operac_financ.dat_transacao),1,YEAR(movto_operac_financ.dat_transacao)) NO-ERROR.
              IF AVAIL contrato_cotac
                   THEN ASSIGN d-dolar-medio = contrato_cotac.dolar-medio.
              ELSE ASSIGN d-dolar-medio = d-dolar-medio-alt.

              ASSIGN d-valor-real  = 0
                     d-valor-dolar = 0.
      
              /* Tratativa valores em reais e dolar - Calcula o dolar do dia do movimento */ 
              IF movto_operac_financ.cod_indic_econ = "Real"                                                                                                                                                                           
                  THEN ASSIGN d-valor-real  = (IF operac_financ.cod_Indic_econ = "Dolar" THEN movto_operac_financ.val_movto_indic_econ_movto ELSE movto_operac_financ.val_movto_apl)                                                   
                              d-valor-dolar = (IF operac_financ.cod_Indic_econ = "Dolar" THEN movto_operac_financ.val_movto_apl ELSE ROUND(movto_operac_financ.val_movto_apl / fcValorDolarData(movto_operac_financ.dat_transacao),6)).
                        
              ELSE DO:

                  ASSIGN d-valor-dolar = movto_operac_financ.val_movto_indic_econ_movto
                         d-valor-real  = 0.

                  /* Busca na contabilizaá∆o os valores em reais */
                  FOR EACH aprop_ctbl_apl OF  movto_operac_financ NO-LOCK: 

                      /* Ir† considerar apenas os movimentos de crÇdito */
                      IF aprop_ctbl_apl.ind_natur_lancto_ctbl = "DB" THEN NEXT. 
    
                      FOR EACH val_aprop_ctbl_apl OF aprop_ctbl_apl NO-LOCK:
    
                           ASSIGN d-valor-real = d-valor-real + val_aprop_ctbl_apl.val_aprop_ctbl.
    
                      END.
                  END. 
                  IF d-valor-real = 0  THEN
                     ASSIGN d-valor-real = ROUND(d-valor-dolar * fcValorDolarData(movto_operac_financ.dat_transacao),6).
              END.
      
              /* C†lculo de Juros que ser† somado no saldo inicial - em reais - Considerado todo o juros do contrato desde o in°cio atÇ a data fim do relat¢rio */
              IF movto_operac_financ.dat_transacao <= tt-param.data_movimento_ini THEN DO:
                  IF (movto_operac_financ.ind_tip_trans_apl = "Juros" OR 
                      movto_operac_financ.ind_tip_trans_apl = "Acerto Valor a Maior" OR 
                      movto_operac_financ.ind_tip_trans_apl = "Correcao") THEN
                     ASSIGN v_val_juros_sdo-ini[1] = ROUND(v_val_juros_sdo-ini[1] + d-valor-real ,6)
                            v_val_juros_sdo-ini[2] = ROUND(v_val_juros_sdo-ini[2] + d-valor-dolar,6).
                  
                  IF (movto_operac_financ.ind_tip_trans_apl = "Estorno Juros" OR 
                      movto_operac_financ.ind_tip_trans_apl = "Acerto Valor a Menor") THEN 
                             ASSIGN v_val_juros_sdo-ini[1] = ROUND(v_val_juros_sdo-ini[1] - d-valor-real,6)
                                    v_val_juros_sdo-ini[2] = ROUND(v_val_juros_sdo-ini[2] - d-valor-dolar,6).

                  /* Imposto para o saldo Inicial - Considerado apenas para relat¢rios depois de 31/12/2017*/
                  IF movto_operac_financ.ind_tip_trans_apl = "Imposto" THEN 
                     ASSIGN v_val_impto_sdo-ini[1] = ROUND(v_val_impto_sdo-ini[1] + d-valor-real ,6)
                            v_val_impto_sdo-ini[2] = ROUND(v_val_impto_sdo-ini[2] + d-valor-dolar,6).
                                 
                  IF movto_operac_financ.ind_tip_trans_apl = "Estorno Imposto" THEN 
                     ASSIGN v_val_impto_sdo-ini[1] = ROUND(v_val_impto_sdo-ini[1] - d-valor-real,6)
                            v_val_impto_sdo-ini[2] = ROUND(v_val_impto_sdo-ini[2] - d-valor-dolar,6).
              END.
      
              /*Imposto - Movimentaá∆o data inicial atÇ a data fim do relat¢rio */
              IF movto_operac_financ.dat_transacao > tt-param.data_movimento_ini THEN DO:

                  /* provis∆o de irrf */
                  IF movto_operac_financ.ind_tip_trans_apl = "Imposto" THEN
                      ASSIGN v_val_impto_operac_financ_imposto[1] = v_val_impto_operac_financ_imposto[1] + d-valor-real
                             v_val_impto_operac_financ_imposto[3] = v_val_impto_operac_financ_imposto[3] + ROUND(d-valor-real / d-dolar-medio,6)
                             v_val_impto_operac_financ_imposto[2] = v_val_impto_operac_financ_imposto[2] + d-valor-dolar.
                     
                  IF movto_operac_financ.ind_tip_trans_apl = "Estorno Imposto" THEN
                      ASSIGN v_val_impto_operac_financ_imposto[1] = v_val_impto_operac_financ_imposto[1] - d-valor-real 
                             v_val_impto_operac_financ_imposto[2] = v_val_impto_operac_financ_imposto[2] - d-valor-dolar.

                  /* Provisao de IRRF - Valor sobre correá∆o de valor - 15% sobre o valor do acerto de valor */
                  IF produt_financ.log_incid_impto_apl AND movto_operac_financ.ind_tip_trans_apl = "Acerto Valor a Maior"
                       THEN  ASSIGN v_val_impto_operac_financ_imposto[1] = v_val_impto_operac_financ_imposto[1] + ROUND(d-valor-real * 0.15,6)
                                    v_val_impto_operac_financ_imposto[3] = v_val_impto_operac_financ_imposto[3] + ROUND((d-valor-real / d-dolar-medio) * 0.15,6)
                                    v_val_impto_operac_financ_imposto[2] = v_val_impto_operac_financ_imposto[2] + ROUND(d-valor-dolar * 0.15,6).
                  IF produt_financ.log_incid_impto_apl AND movto_operac_financ.ind_tip_trans_apl = "Acerto Valor a Menor" THEN 
                             ASSIGN v_val_impto_operac_financ_imposto[1] = v_val_impto_operac_financ_imposto[1] - ROUND(d-valor-real * 0.15,6)
                                    v_val_impto_operac_financ_imposto[3] = v_val_impto_operac_financ_imposto[3] - ROUND((d-valor-real / d-dolar-medio) * 0.15,6)
                                    v_val_impto_operac_financ_imposto[2] = v_val_impto_operac_financ_imposto[2] - ROUND(d-valor-dolar * 0.15,6).      

                  /* Juros Provisionado */
                  /* Em dolar - utiliza o dolar mÇdio */
                  IF (movto_operac_financ.ind_tip_trans_apl = "Juros" OR
                      movto_operac_financ.ind_tip_trans_apl = "Acerto Valor a Maior" OR
                      movto_operac_financ.ind_tip_trans_apl = "Correá∆o")
                  THEN ASSIGN v_val_provisao_juros[1] = v_val_provisao_juros[1] + d-valor-real
                              v_val_provisao_juros[3] = v_val_provisao_juros[3] + ROUND(d-valor-real / d-dolar-medio,6)  /* Valor usado para contrato em reais - converte pela dolar mÇdio da data da transaá∆o*/
                              v_val_provisao_juros[2] = v_val_provisao_juros[2] + d-valor-dolar.

                  IF (movto_operac_financ.ind_tip_trans_apl = "Estorno Juros" OR
                      movto_operac_financ.ind_tip_trans_apl = "Acerto Valor a Menor") THEN
                         ASSIGN v_val_provisao_juros[1] = v_val_provisao_juros[1] - d-valor-real
                                v_val_provisao_juros[3] = v_val_provisao_juros[3] - ROUND(d-valor-real / d-dolar-medio,6)  
                                v_val_provisao_juros[2] = v_val_provisao_juros[2] - d-valor-dolar.

                  /* Novos EmprÇstimos */
                  IF movto_operac_financ.ind_tip_trans_apl = "Liberaá∆o" THEN
                      ASSIGN v_val_movto_apl_novos_emp[1] = v_val_movto_apl_novos_emp[1] + d-valor-real
                             v_val_movto_apl_novos_emp[2] = v_val_movto_apl_novos_emp[2] + d-valor-dolar.
                  
                  /* Variaá∆o Cambial - Para contratos em reais n∆o ir† mostrar a variaá∆o cambial */                                                                                                                                                                                                                     
                  IF movto_operac_financ.ind_tip_trans_apl = "Variaá∆o Cambial" THEN 
                      ASSIGN  v_val_movto_apl_variacao[1] = v_val_movto_apl_variacao[1] + d-valor-real                                     
                              v_val_movto_apl_variacao[2] = v_val_movto_apl_variacao[2] + d-valor-dolar.

                  /* Variaá∆o Cambial Juros - Desconta o IR da variaá∆o qdo a operaá∆o tiver incidància de imposto */
                  IF movto_operac_financ.ind_tip_trans_apl = "Var Cambial Juros"  THEN 
                      ASSIGN  v_val_movto_apl_variacao[1] = v_val_movto_apl_variacao[1] + (d-valor-real - (IF produt_financ.log_incid_impto_apl THEN (d-valor-real * 0.15) ELSE 0))                                    
                              v_val_movto_apl_variacao[2] = v_val_movto_apl_variacao[2] + (d-valor-dolar - (IF produt_financ.log_incid_impto_apl THEN (d-valor-dolar * 0.15) ELSE 0)).

              END.
          END.  /* FOR EACH movto_operac_financ */

          /* Juros Pagos - Real e Dolar */
          FOR EACH parc_operac_financ OF operac_financ NO-LOCK 
                  WHERE parc_operac_financ.ind_sit_parc_pagto = "quitada"
                    AND parc_operac_financ.dat_pagto <= tt-param.data_movimento_fim:
                  
            ASSIGN d-valor-dolar = 0 
                   d-valor-real  = 0.

            FIND FIRST movto_operac_financ NO-LOCK OF parc_operac_financ NO-ERROR.
            IF AVAIL movto_operac_financ THEN DO:

               /* Tratativa valores em reais e dolar - Calcula o dolar do dia do movimento */ 
               IF movto_operac_financ.cod_indic_econ = "Real"                                                                                                                                                                           
                  THEN ASSIGN d-valor-real  = (IF operac_financ.cod_Indic_econ = "Dolar" THEN movto_operac_financ.val_movto_indic_econ_movto ELSE movto_operac_financ.val_movto_apl)                                                   
                              d-valor-dolar = (IF operac_financ.cod_Indic_econ = "Dolar" THEN movto_operac_financ.val_movto_apl ELSE ROUND(movto_operac_financ.val_movto_apl / fcValorDolarData(movto_operac_financ.dat_transacao),6)).
                                       
               ELSE DO:

                  ASSIGN d-valor-dolar = movto_operac_financ.val_movto_indic_econ_movto.

                  /* Busca na contabilizaá∆o os valores em reais */
                  FOR EACH aprop_ctbl_apl OF  movto_operac_financ NO-LOCK: 

                      /* Ir† considerar apenas os movimentos de crÇdito */
                      IF aprop_ctbl_apl.ind_natur_lancto_ctbl = "DB" THEN NEXT. 
    
                      FOR EACH val_aprop_ctbl_apl OF aprop_ctbl_apl NO-LOCK:
    
                           ASSIGN d-valor-real = d-valor-real + val_aprop_ctbl_apl.val_aprop_ctbl.
    
                      END.
                  END. 
               END.
            END.

            /* Dolar contratado no pagamento */
            ASSIGN d-valor-dolar-pagto = 0.
            FIND FIRST ext_movto_operac_financ NO-LOCK WHERE 
                           ext_movto_operac_financ.num_id_operac_financ  = movto_operac_financ.num_id_Operac_financ AND
                           ext_movto_operac_financ.num_seq_operac_financ = movto_operac_financ.num_seq_movto_operac_financ NO-ERROR.
            IF AVAIL ext_movto_operac_financ THEN 
               ASSIGN d-valor-dolar-pagto = ROUND(movto_operac_financ.val_movto_apl / ext_movto_operac_financ.cotacao,6).
             /* Dolar do pagamento */

            IF parc_operac_financ.dat_pagto > tt-param.data_movimento_ini THEN DO:

                /* Valores de pagamento de Juros */
                IF parc_operac_financ.ind_tip_parc_emprest = "Juros" THEN
                          ASSIGN  v_val_movto_apl_juros[1] =  v_val_movto_apl_juros[1] + d-valor-real
                                  v_val_movto_apl_juros[2] =  v_val_movto_apl_juros[2] + (IF d-valor-dolar-pagto = 0 THEN d-valor-dolar ELSE d-valor-dolar-pagto).
                
                /* Parcelas do pagamento do principal */
                IF parc_operac_financ.ind_tip_parc_emprest = "Pagamento" 
                     THEN ASSIGN v_val_movto_apl_pagamento[1] = v_val_movto_apl_pagamento[1] + d-valor-real
                                 v_val_movto_apl_pagamento[2] = v_val_movto_apl_pagamento[2] + (IF d-valor-dolar-pagto = 0 THEN d-valor-dolar ELSE d-valor-dolar-pagto).
            END.
            ELSE DO:
                /* Valores de pagamento - Desconta do juros do saldo inicial os juros pagos*/
                IF parc_operac_financ.ind_tip_parc_emprest = "Juros" 
                  THEN ASSIGN v_val_juros_sdo-ini[1] = ROUND(v_val_juros_sdo-ini[1] - d-valor-real ,6)  
                              v_val_juros_sdo-ini[2] = ROUND(v_val_juros_sdo-ini[2] - (IF d-valor-dolar-pagto = 0 THEN d-valor-dolar ELSE d-valor-dolar-pagto),6). 

            END.
          END. /* FOR EACH parc_operac_financ */

          /* Calcula o IR apenas qdo o produto possui imposto */
   
          ASSIGN v_perc_impto = 0.
          IF produt_financ.log_incid_impto_apl THEN DO:
              FIND es_produt_financ NO-LOCK OF produt_financ NO-ERROR.
              FIND classif_impto NO-LOCK WHERE
                   classif_impto.cod_imposto = es_produt_financ.cod_imposto NO-ERROR.
              IF AVAIL classif_impto
                  THEN ASSIGN v_perc_impto = classif_impto.val_aliq_impto / 100.
              ELSE ASSIGN v_perc_impto = 0.


              /* Operaá‰es muito antigas e em dolar n∆o possuem todas as contabilizaá‰es para o c†lculo do valor em reais, 
                 por isso Ç efetuada a convers∆o e c†lculo do valor do IR manualmente **/
              IF operac_financ.cod_Indic_econ = "Dolar" AND operac_financ.dat_operac_financ < 12/31/2017 THEN
                ASSIGN v_val_impto_sdo-ini[1] = ROUND(v_val_juros_sdo-ini[2] * d-dolar-ini * 0.15,6)  
                       v_val_impto_sdo-ini[2] = ROUND(v_val_juros_sdo-ini[2] * 0.15,6).
              ELSE DO:

                  /* Para o relat¢rio na data de 31/12/2017 ir† calcular o valor do imposto e subtrair do saldo inicial */
                  /* Apenas para os produtos que possuem imposto!!!! */
                  /* Em Teste IF tt-param.data_movimento_ini = 12/31/2017 THEN DO: */
    
                      /* Calcula o Juros do Saldo Inicial a partir dos retornos do Relat¢rio  e ignora o que foi calculado nos movimentos */
                      IF operac_financ.cod_indic_econ = "REAIS" THEN
                          ASSIGN v_val_impto_sdo-ini[1] = ROUND((v_val_sdo_dat - v_val_sdo_princ_operac_financ) * 0.15,6) 
                                 v_val_impto_sdo-ini[2] = ROUND((v_val_sdo_dat_moeda - v_val_sdo_princ_operac_financ_moeda) * 0.15,6).   
                      ELSE ASSIGN v_val_impto_sdo-ini[1] = ROUND((v_val_sdo_dat_moeda - v_val_sdo_princ_operac_financ_moeda) * 0.15,6) 
                                 v_val_impto_sdo-ini[2] = ROUND((v_val_sdo_dat - v_val_sdo_princ_operac_financ) * 0.15,6). 
                  /*END.*/
              END.

              /* Desconta 15% de IR dos Juros */
              ASSIGN v_val_movto_apl_juros[1] = v_val_movto_apl_juros[1] - ROUND(v_val_movto_apl_juros[1] * 0.15,6)
                     v_val_movto_apl_juros[2] = v_val_movto_apl_juros[2] - ROUND(v_val_movto_apl_juros[2] * 0.15,6).
          END.


          /* Saldo Inicial e Imposto do Saldo Inicial */ 
          ASSIGN v_val_movto_apl_saldo_inicial[1] = (IF operac_financ.cod_indic_econ = "REAL" THEN v_val_sdo_dat ELSE v_val_sdo_dat_moeda)
                 v_val_movto_apl_saldo_inicial[2] = (IF operac_financ.cod_indic_econ = "REAL" THEN v_val_sdo_dat_moeda ELSE v_val_sdo_dat).
            
          /* Grava dados da Tempor†ria */ 
          /* Cadastro */
          CREATE tt-dados.
          ASSIGN tt-dados.tipo_produto       = produt_financ.cod_tip_produt_financ
                 tt-dados.cod_produt_finan   = produt_financ.cod_produt_financ
                 tt-dados.desc_produt_financ = produt_financ.des_produt_financ 
                 tt-dados.emp_ext            = admdra_apf.cod_admdra_apf  WHEN AVAIL admdra_apf
                 tt-dados.cod_atu_plan       = admdra_apf.nom_razao_social WHEN AVAIL admdra_apf
                 tt-dados.cod_emp            = operac_financ.cod_empresa
                 tt-dados.cod_operac_financ  = operac_financ.cod_operac_financ 
                 /*contrato mae*/
                 tt-dados.mae_cod_contr    = contrat_apf.cod_contrat_apf WHEN AVAIL contrat_apf
                 tt-dados.mae_moeda_ctr    = contrat_apf.cod_indic_econ WHEN AVAIL contrat_apf
                 tt-dados.mae_vl_contrat   = contrat_apf.val_lim_cr_contrat_total + contrat_apf.val_aditivo WHEN AVAIL contrat_apf 
                 tt-dados.mae_vl_sl_ini    = contrat_apf.val_lim_cr_contrat_apf WHEN AVAIL contrat_apf  
                 tt-dados.mae_sl_contrat   = ((contrat_apf.val_lim_cr_contrat_apf + contrat_apf.val_aditivo) - contrat_apf.val_operac_financ) WHEN AVAIL contrat_apf
                 tt-dados.mae_dt_ini       = contrat_apf.dat_inic_valid WHEN AVAIL contrat_apf  
                 tt-dados.mae_dt-fim       = (IF AVAIL aditivo_contrat_apf THEN aditivo_contrat_apf.dat_fim_valid ELSE (IF AVAIL contrat_apf THEN contrat_apf.dat_fim_valid ELSE ?))
                 /* Dados operaá‰es financeiras - Contratos Filhos */
                 tt-dados.fi_inst_financ   = operac_financ.cod_banco + " - " + banco.nom_banco
                 tt-dados.fi_contr_entrada = c_cambio
                 tt-dados.fi_rof           = es_operac_financ_rof.cod_rof_operacao WHEN AVAIL es_operac_financ_rof
                 tt-dados.fi_dt_ini_f      = operac_financ.dat_operac_financ  
                 tt-dados.fi_dt_fim_f      = operac_financ.dat_vencto_operac_financ 
                 tt-dados.moeda            = operac_financ.cod_indic_econ
                 tt-dados.dt_tx_oper       = v_dt_tx_juros_operac 
                 tt-dados.fi_tx_oper       = v_vl_tx_juros_operac
                 tt-dados.valor_operacao   = operac_financ.val_operac_financ 
                  /* Valores controlados em reais */
                 tt-dados.me_sl_principal  = (IF v_val_movto_apl_saldo_inicial[1] > 0 THEN (v_val_movto_apl_saldo_inicial[1] - v_val_impto_sdo-ini[1]) ELSE v_val_movto_apl_saldo_inicial[1])
                 tt-dados.me_sl_juros      = v_val_juros_sdo-ini[1]   /* Saldo Inicial Ç Principal + Juros - IR, entretando na conta o juros n∆o entra, pois o Totvs j† retorna o valor com o juros considerado */            
                 tt-dados.me_sl_ir         = v_val_impto_sdo-ini[1]
                 tt-dados.me_novos_emp     = v_val_movto_apl_novos_emp[1]         
                 tt-dados.me_juros_prov    = v_val_provisao_juros[1]         
                 tt-dados.me_dif_cambial   = v_val_movto_apl_variacao[1]
                 tt-dados.me_prov_irrf     = v_val_impto_operac_financ_imposto[1] * -1 
                 tt-dados.me_pg_principal  = (v_val_movto_apl_pagamento[1]  * -1)        
                 tt-dados.me_pg_juro       = (v_val_movto_apl_juros[1] * -1).

          ASSIGN tt-dados.me_sl_final      = (tt-dados.me_sl_principal 
                                            + tt-dados.me_novos_emp
                                            + tt-dados.me_juros_prov
                                            + tt-dados.me_dif_cambial
                                            + tt-dados.me_prov_irrf
                                            + tt-dados.me_pg_principal
                                            + tt-dados.me_pg_juro).
                                         
          /* Valores controlados em outra moeda - Quando o contrato Ç em real */ 
          IF operac_financ.cod_indic_econ = "REAL" THEN DO:
              ASSIGN tt-dados.mv_sl_emp         = /* Dolar dt inicial */ (IF v_val_movto_apl_saldo_inicial[1] > 0 THEN ROUND((v_val_movto_apl_saldo_inicial[1] - v_val_impto_sdo-ini[1])  / d-dolar-ini,6) ELSE 0)
                     tt-dados.mv_novo_emp       = /* Dolar do movimento */ v_val_movto_apl_novos_emp[2] 
                     tt-dados.mv_juros_prov     = /* Dolar medio */ v_val_provisao_juros[3]
                     tt-dados.mv_prv_irrf       = v_val_impto_operac_financ_imposto[3] * -1
                     tt-dados.mv_pg_principal   = /* Cotaá∆o do dia do movimento */ v_val_movto_apl_pagamento[2] * -1
                     tt-dados.mv_pg_juros       = /* Dolar da data do movimento */ v_val_movto_apl_juros[2] * -1.
    
              /* Saldo Final e Variaá∆o Cambial dos valores */
                   
              ASSIGN tt-dados.mv_sl_final     = ROUND(tt-dados.me_sl_final / d-dolar-fim,6).
              ASSIGN tt-dados.mv_dif_cambial  = tt-dados.mv_sl_final - (tt-dados.mv_sl_emp + tt-dados.mv_novo_emp + tt-dados.mv_juros_prov + tt-dados.mv_prv_irrf + /* esse valores est∆o negativos*/ tt-dados.mv_pg_principal + tt-dados.mv_pg_juros).
          END.
          ELSE DO:
               ASSIGN tt-dados.mv_sl_emp         = (IF v_val_movto_apl_saldo_inicial[2] > 0 THEN (v_val_movto_apl_saldo_inicial[2] - v_val_impto_sdo-ini[2]) ELSE 0)
                      tt-dados.mv_novo_emp       = v_val_movto_apl_novos_emp[2]
                      tt-dados.mv_juros_prov     = v_val_provisao_juros[2] 
                      tt-dados.mv_dif_cambial    = (IF operac_financ.cod_indic_econ <> "Real" THEN 0 ELSE v_val_movto_apl_variacao[2]) 
                      tt-dados.mv_prv_irrf       = v_val_impto_operac_financ_imposto[2] * -1
                      tt-dados.mv_pg_principal   = v_val_movto_apl_pagamento[2] * -1
                      tt-dados.mv_pg_juros       =  v_val_movto_apl_juros[2] * -1.

                      tt-dados.mv_sl_final       =  ROUND(tt-dados.mv_sl_emp  
                                                          + tt-dados.mv_novo_emp
                                                          + tt-dados.mv_juros_prov 
                                                          + tt-dados.mv_dif_cambial
                                                          + tt-dados.mv_prv_irrf 
                                                          + tt-dados.mv_pg_principal
                                                          + tt-dados.mv_pg_juros,6).
          END.
        /* Contabilizaá∆o */ /* DPC - Qdo tiver mais de uma - como ficar†? */
        FOR EACH cta_produt_financ OF produt_financ NO-LOCK,
            FIRST cta_ctbl OF cta_produt_financ NO-LOCK: 

            IF cta_produt_financ.dat_fim_valid <  tt-param.data_movimento_ini THEN NEXT.
            IF cta_produt_financ.dat_inic_valid > tt-param.data_movimento_fim THEN NEXT.
    
            CASE cta_produt_financ.ind_finalid_ctb:
                WHEN "Saldo Curto Prazo" THEN
                     ASSIGN tt-dados.cc_principal = cta_ctbl.cod_cta_ctbl   
                            tt-dados.cc_desc      = cta_ctbl.des_tit_ctbl.
    
                WHEN  "Variaá∆o Cambial" THEN
                    ASSIGN tt-dados.cc_var_cambial   = cta_ctbl.cod_cta_ctbl  
                           tt-dados.cc_desc_var_camb = cta_ctbl.des_tit_ctbl. 
    
                WHEN "Juros" THEN
                    ASSIGN tt-dados.cc_juros      = cta_ctbl.cod_cta_ctbl  
                           tt-dados.cc_desc_juros = cta_ctbl.des_tit_ctbl. 
    
    
            END CASE.
        END.
    END. /* operac_financ */
END PROCEDURE.              

PROCEDURE pi-impressao:
   DEFINE VARIABLE i-linha AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-col   AS INTEGER NO-UNDO.

   ASSIGN i-linha = 5
          i-col   = 1.

   FOR EACH tt-dados:

      /* Se os valores estiverem zerados n∆o lista */
      IF tt-dados.me_sl_principal  = 0 AND
         tt-dados.me_novos_emp        = 0 AND
         tt-dados.me_juros_prov       = 0 AND
         tt-dados.me_dif_cambial      = 0 AND
         tt-dados.me_prov_irrf        = 0 AND
         tt-dados.me_pg_principal     = 0 AND
         tt-dados.me_pg_juro          = 0 AND
         tt-dados.me_sl_final         = 0 THEN NEXT.

       RUN pi-acompanhar IN h-acomp ("Excel - Operaá∆o: " + tt-dados.cod_operac_financ). 
      
      /* Cadastro */
      ASSIGN excelappl:cells(i-linha,i-col)      = tt-dados.tipo_produto        /* A */
             excelappl:cells(i-linha,i-col + 2)  = tt-dados.cod_produt_finan    /* C */
             excelappl:cells(i-linha,i-col + 3)  = tt-dados.DESC_produt_financ  /* D */
             excelappl:cells(i-linha,i-col + 5)  = tt-dados.emp_ext             /* F */
             excelappl:cells(i-linha,i-col + 6)  = tt-dados.cod_atu_plan        /* G */
             excelappl:cells(i-linha,i-col + 8)  = tt-dados.cod_emp             /* I */
             excelappl:cells(i-linha,i-col + 9)  = tt-dados.cod_operac_financ   /* J */

             /*contrato mae*/
             excelappl:cells(i-linha,12) = tt-dados.mae_cod_contr 
             excelappl:cells(i-linha,13) = tt-dados.mae_moeda_ctr  
             excelappl:cells(i-linha,14) = tt-dados.mae_vl_contrat 
             excelappl:cells(i-linha,15) = tt-dados.mae_sl_contrat
             excelappl:cells(i-linha,16) = tt-dados.mae_dt_ini     
             excelappl:cells(i-linha,17) = tt-dados.mae_dt-fim 

             /****** Operaá∆o Financeira - Contratos Filho********/
             excelappl:cells(i-linha,19) = tt-dados.fi_inst_financ   
             excelappl:cells(i-linha,20) = tt-dados.fi_contr_entrada 
             excelappl:cells(i-linha,21) = tt-dados.fi_rof           
             excelappl:cells(i-linha,22) = tt-dados.fi_dt_ini_f      
             excelappl:cells(i-linha,23) = tt-dados.fi_dt_fim_f
             excelappl:cells(i-linha,24) = tt-dados.moeda
             excelappl:cells(i-linha,25) = tt-dados.dt_tx_oper 
             excelappl:cells(i-linha,26) = tt-dados.fi_tx_oper
             excelappl:cells(i-linha,27) = tt-dados.valor_operacao

             /* Valores controlados em reais */
             excelappl:cells(i-linha,29)  = tt-dados.me_sl_principal
             excelappl:cells(i-linha,30)  = tt-dados.me_novos_emp           
             excelappl:cells(i-linha,31)  = tt-dados.me_juros_prov          
             excelappl:cells(i-linha,32)  = tt-dados.me_dif_cambial         
             excelappl:cells(i-linha,33)  = tt-dados.me_prov_irrf           
             excelappl:cells(i-linha,34)  = tt-dados.me_pg_principal        
             excelappl:cells(i-linha,35)  = tt-dados.me_pg_juro             
             excelappl:cells(i-linha,36)  = tt-dados.me_sl_final
 
             /* Valores controlados em d¢lar */
             excelappl:cells(i-linha,38)  = tt-dados.mv_sl_emp 
             excelappl:cells(i-linha,39)  = tt-dados.mv_novo_emp                          
             excelappl:cells(i-linha,40)  = tt-dados.mv_juros_prov   
             excelappl:cells(i-linha,41)  = tt-dados.mv_dif_cambial
             excelappl:cells(i-linha,42)  = tt-dados.mv_prv_irrf     
             excelappl:cells(i-linha,43)  = tt-dados.mv_pg_principal        
             excelappl:cells(i-linha,44)  = tt-dados.mv_pg_juros           
             excelappl:cells(i-linha,45)  = tt-dados.mv_sl_final     

             excelappl:cells(i-linha,47)  = tt-dados.cc_principal       
             excelappl:cells(i-linha,48)  = tt-dados.cc_desc            
             excelappl:cells(i-linha,49)  = tt-dados.cc_var_cambial     
             excelappl:cells(i-linha,50)  = tt-dados.cc_desc_var_camb   
             excelappl:cells(i-linha,51)  = tt-dados.cc_juros           
             excelappl:cells(i-linha,52)  = tt-dados.cc_desc_juros

             excelappl:cells(i-linha,53)  = tt-dados.me_sl_juros
             excelappl:cells(i-linha,54)  = tt-dados.me_sl_ir.

            
       ASSIGN i-linha = i-linha + 1.
   END.
END PROCEDURE.

PROCEDURE pi-excel-abrir :
   ASSIGN c-modelo = SEARCH('prgfin\apl\apya510.xltx').

   CREATE "excel.application" excelappl.
   excelappl:VISIBLE = FALSE.
   excelappl:workbooks:ADD(c-modelo).
   excelappl:worksheets:ITEM(1):SELECT.
   excelappl:worksheets:ITEM(1):NAME = "APYA510-" + REPLACE(STRING(TODAY,'99/99/9999'),"/","").
 
   ASSIGN excelappl:cells(2,01) = "Per°odo:"
          excelappl:cells(2,03) = STRING(tt-param.data_movimento_ini,"99/99/9999") + " a " + STRING(tt-param.data_movimento_fim,"99/99/9999")
          excelappl:cells(2,38) = "Taxas: Dolar Inicial: " + STRING(d-dolar-ini) + " Dolar Fechamento: " + STRING(d-dolar-fim) + " Dolar MÇdio: " + STRING(d-dolar-medio)
          excelappl:cells(4,29) = "Saldo em "       + STRING(tt-param.data_movimento_ini,"99/99/9999") 
          excelappl:cells(4,36) = "Saldo Final em " + STRING(tt-param.data_movimento_fim,"99/99/9999") 
          excelappl:cells(4,38) = "Saldo em "       + STRING(tt-param.data_movimento_ini,"99/99/9999") 
          excelappl:cells(4,45) = "Saldo Final em " + STRING(tt-param.data_movimento_fim,"99/99/9999").
          

END PROCEDURE.

PROCEDURE pi-excel-fechar :
   excelAppl:APPLICATION:DisplayAlerts = FALSE.

   excelAppl:Cells:SELECT.
   excelAppl:Cells:EntireColumn:AutoFit.
   excelappl:VISIBLE = TRUE.
    
   RELEASE OBJECT worksheets NO-ERROR.
   RELEASE OBJECT workbooks  NO-ERROR.
   RELEASE OBJECT excelappl  NO-ERROR.

END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/
