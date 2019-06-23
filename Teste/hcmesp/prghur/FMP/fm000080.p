/*----------------------------------------------------------------------------*
  Programa...: FM000080.P    
  Data.......: 18/08/03 
  Analista...: Aline Leal de Campos
  Objetivo...: Calular Hora Extra - Atestado M‚dico (Evento 80)
               
  Altera‡äes:  
               
 *----------------------------------------------------------------------------*/

def shared var i-un-retorno        as dec                                  no-undo.
def shared var i-vl-retorno        as dec                                  no-undo.
def shared var i-bs-retorno        as dec                                  no-undo.
def shared var i-sal-mes           as dec                                  no-undo.
def shared var l-fr5100            as log                                  no-undo.
/*def shared var i-un-evt            as dec  extent 999                      no-undo.*/

def shared buffer bparam_empres_rh      for param_empres_rh.
def shared buffer bcateg                for categ_sal.
def shared buffer bfunciona             for funcionario.
def shared buffer bturno_trab           for turno_trab.

DEF VAR i_dias_afast AS INT  NO-UNDO.
DEF VAR da_fim_folha AS DATE NO-UNDO.
DEF VAR da_ini_folha AS DATE NO-UNDO.
DEF VAR da_aux       AS DATE NO-UNDO.

DEF VAR i            AS INT  NO-UNDO.
DEF VAR i_mes        AS INT  NO-UNDO.
DEF VAR i_ano        AS INT  NO-UNDO.

DEF VAR d_vl_calc LIKE adc_ferias_13o.val_adc_val_ferias NO-UNDO.


ASSIGN i-vl-retorno = 0.
   
FIND param_empres_rh WHERE 
     param_empres_rh.cdn_empresa = bfunciona.cdn_empresa 
     NO-LOCK NO-ERROR.
IF NOT AVAIL param_empres_rh THEN RETURN.

FIND categ_sal WHERE
     categ_sal.cdn_empresa   = bfunciona.cdn_empresa   AND
     categ_sal.cdn_estab     = bfunciona.cdn_estab     AND
     categ_sal.cdn_categ_sal = bfunciona.cdn_categ_sal
     NO-LOCK NO-ERROR.

/* dia de inicio e termino do calculo da folha */
ASSIGN da_fim_folha = DATE(param_empres_rh.num_mes_refer_calc_efetd, categ_sal.num_dia_fim_period_pto, param_empres_rh.num_ano_refer_calc_efetd)
       da_ini_folha = da_fim_folha - DAY(da_fim_folha)
       da_ini_folha = DATE(MONTH(da_ini_folha), categ_sal.num_dia_inic_period_pto, YEAR(da_ini_folha)).

/* Verifica se houve afastamento por doen‡a */
i_dias_afast = 0.
FOR EACH sit_afast_func WHERE
    sit_afast_func.cdn_empresa         = bfunciona.cdn_empresa     AND
    sit_afast_func.cdn_estab           = bfunciona.cdn_estab       AND
    sit_afast_func.cdn_funcionario     = bfunciona.cdn_funcionario AND
    sit_afast_func.cdn_sit_afast_func  = 10                        AND /* Situacao 10 - Aux¡lio Doen‡a */
    sit_afast_func.dat_inic_sit_afast >= da_ini_folha              AND
    sit_afast_func.dat_term_sit_afast <= da_fim_folha
    NO-LOCK:

    i_dias_afast = i_dias_afast + sit_afast_func.qti_dias_sit_func.

END. /* FOR EACH sit_afast_func */

IF i_dias_afast > 0 THEN DO:
    /* Calcula media de adicionais recebidos nos ultimos 12 meses */
    da_aux = da_fim_folha.
    DO i = 1 TO 12:
        ASSIGN da_aux = da_aux - DAY(da_aux)
               i_mes = MONTH(da_aux)
               i_ano = YEAR(da_aux).

        IF DATE(i_mes,DAY(da_aux),i_ano) < bfunciona.dat_admis_func THEN
            LEAVE.

        FIND adc_ferias_13o WHERE
             adc_ferias_13o.cdn_empresa                = bfunciona.cdn_empresa                          AND
             adc_ferias_13o.cdn_estab                  = bfunciona.cdn_estab                            AND
             adc_ferias_13o.cdn_funcionario            = bfunciona.cdn_funcionario                      AND
             adc_ferias_13o.num_ano_mes_adc_ferias_13o = INT(STRING(i_ano,'9999') + STRING(i_mes,'99')) AND
             adc_ferias_13o.cdn_tip_adc_ferias_13o     = 0
             NO-LOCK NO-ERROR.

        IF AVAIL adc_ferias_13o THEN
            d_vl_calc = d_vl_calc + adc_ferias_13o.val_adc_val_ferias.

    END.
    i = i - 1.

    IF i > 0 THEN
        ASSIGN d_vl_calc = d_vl_calc / i                    /* media dos adicionais nos ultimos 12 meses */
               d_vl_calc = (d_vl_calc / 30) * i_dias_afast. /* valor dos adicionais por dia ( div 30) * dias afastados */

END. /* IF i_dias_afast > 0 */

ASSIGN  i-vl-retorno =  d_vl_calc.

RETURN.
