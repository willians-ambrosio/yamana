/*----------------------------------------------------------------------------*
  Programa...: FM000400.P    
  Data.......: 18/08/03 
  Analista...: Aline Leal de Campos
  Objetivo...: Calular Pagamento do INSS - Acidente de Trabalho (Evento 400)
               
  Altera‡äes:  
               
 *----------------------------------------------------------------------------*/

def shared var i-un-retorno        as dec                                  no-undo.
def shared var i-vl-retorno        as dec                                  no-undo.
def shared var i-bs-retorno        as dec                                  no-undo.
def shared var i-sal-mes           as dec                                  no-undo.
def shared var l-fr5100            as log                                  no-undo.
/*def shared var i-un-evt            as dec  extent 999                      no-undo.*/

/*def shared buffer bparam_empres_rh      for param_empres_rh.*/
def shared buffer bcateg                for categ_sal.
def shared buffer bfunciona             for funcionario.
def shared buffer bturno_trab           for turno_trab.

DEF VAR da_fim_folha AS DATE NO-UNDO.
DEF VAR da_ini_folha AS DATE NO-UNDO.
DEF VAR da_aux       AS DATE NO-UNDO.

DEF VAR i            AS INT  NO-UNDO.
DEF VAR i_aux        AS INT  NO-UNDO.
DEF VAR i_mes        AS INT  NO-UNDO.
DEF VAR i_ano        AS INT  NO-UNDO.
DEF VAR i_qtd_mes    AS INT  NO-UNDO.

DEF VAR i_qtd_hrs  LIKE movto_calcul_func.qtd_hrs_demonst_efp[1] NO-UNDO.
DEF VAR d_vl_calc  LIKE movto_calcul_func.val_calcul_efp[1]      NO-UNDO.
DEF VAR de_vl_base LIKE movto_calcul_func.val_base_calc_fp[1]    NO-UNDO.

DEF TEMP-TABLE tt_sal
    FIELD vl_base LIKE  movto_calcul_func.val_base_calc_fp[1]
    INDEX ind_vl vl_base DESCENDING.


ASSIGN i-vl-retorno = 0.
   
FIND param_empres_rh WHERE 
     param_empres_rh.cdn_empresa = bfunciona.cdn_empresa 
     NO-LOCK NO-ERROR.
IF NOT AVAIL param_empres_rh THEN RETURN.


/* dia de inicio e termino do calculo da folha */
ASSIGN i_ano = param_empres_rh.num_ano_refer_calc_efetd
       i_mes = param_empres_rh.num_mes_refer_calc_efetd.

MESSAGE 'Calculo da Folha' i_ano i_mes
    VIEW-AS ALERT-BOX.


ASSIGN d_vl_calc = 0
       i_qtd_hrs = 0.

/* Evento 181 - Quantidade de Horas pagas pelo INSS */
FOR EACH movto_calcul_func WHERE
    movto_calcul_func.cdn_empresa              = bfunciona.cdn_empresa     AND
    movto_calcul_func.cdn_estab                = bfunciona.cdn_estab       AND
    movto_calcul_func.cdn_funcionario          = bfunciona.cdn_funcionario AND
    movto_calcul_func.num_ano_refer_fp         = i_ano                     AND
    movto_calcul_func.num_mes_refer_fp         = i_mes                     AND
    movto_calcul_func.idi_tip_fp               = 1                         AND  /* 1 - Normal */
    movto_calcul_func.qti_parc_habilit_calc_fp = 9                              /* Parcela 9 */
    NO-LOCK:

    DO i_aux = 1 TO 30:
        IF movto_calcul_func.cdn_event[i_aux] = "181" THEN DO:
            i_qtd_hrs = movto_calcul_func.qtd_hrs_demonst_efp[i_aux].
            LEAVE.
        END.
    END.
END. /* FOR EACH movto_calcul_func */

MESSAGE 'Funcion rio' bfunciona.cdn_funcionario SKIP
        'Evento 181' i_qtd_hrs
    VIEW-AS ALERT-BOX.

/* Acha os 12 maiores sal rios dos £ltimos 18 meses */
ASSIGN da_aux    = DATE(i_mes,28,i_ano) + 4
       da_aux    = da_aux - DAY(da_aux)      /* ultimo dia do mˆs do calculo da folha */
       i_qtd_mes = 0.

/* Cria temp-table para guardar os 12 maiores sal rios */
DO i_aux = 1 TO 12:
    CREATE tt_sal.
    ASSIGN tt_sal.vl_base = 0.
END.

DO i = 1 TO 18:
    ASSIGN da_aux     = da_aux - DAY(da_aux)
           de_vl_base = 0.
    
    FOR EACH movto_calcul_func WHERE
        movto_calcul_func.cdn_empresa              = bfunciona.cdn_empresa     AND
        movto_calcul_func.cdn_estab                = bfunciona.cdn_estab       AND
        movto_calcul_func.cdn_funcionario          = bfunciona.cdn_funcionario AND
        movto_calcul_func.num_ano_refer_fp         = YEAR(da_aux)              AND
        movto_calcul_func.num_mes_refer_fp         = MONTH(da_aux)             AND
        movto_calcul_func.idi_tip_fp               = 1                         AND  /* 1 - Normal */
        movto_calcul_func.qti_parc_habilit_calc_fp = 9                              /* Parcela 9 */
        NO-LOCK:

        DO i_aux = 1 TO 30:
            IF movto_calcul_func.cdn_event[i_aux] = "511" OR movto_calcul_func.cdn_event[i_aux] = "512" THEN DO:
                de_vl_base = movto_calcul_func.val_base_calc_fp[i_aux].
            END.
        END.
    END. /* FOR EACH movto_calcul_func */

    MESSAGE 'Funcion rio' bfunciona.cdn_funcionario SKIP
            'Salario em' da_aux '-' de_vl_base
        VIEW-AS ALERT-BOX.
            

    IF de_vl_base > 0 THEN DO:
        /* acha o menor salario */
        FIND LAST tt_sal USE-INDEX ind_vl NO-ERROR.
        IF de_vl_base > tt_sal.vl_base THEN
            tt_sal.vl_base = de_vl_base.
    END.

    IF da_aux >= bfunciona.dat_admis_func THEN
        i_qtd_mes = i_qtd_mes + 1.

END. /* DO i = 1 TO 18 */

/* calcula a media dos 12 salarios selecionados */
i_aux = i_qtd_mes.
FOR EACH tt_sal:
    IF i_aux <= 0 THEN LEAVE.
    ASSIGN d_vl_calc = d_vl_calc + tt_sal.vl_base
           i_aux     = i_aux - 1.
END.

MESSAGE 'Funcion rio' bfunciona.cdn_funcionario SKIP
        'M‚dia Sal rio' d_vl_calc SKIP
        'Meses' i_qtd_mes
    VIEW-AS ALERT-BOX.

/* Media dos ultimos 12 meses */
d_vl_calc = d_vl_calc / (IF i_qtd_mes > 12 THEN 12 ELSE i_qtd_mes).

FIND turno_trab WHERE
     turno_trab.cdn_turno_trab = bfunciona.cdn_turno_trab
     NO-LOCK NO-ERROR.

ASSIGN d_vl_calc = d_vl_calc / turno_trab.qtd_hrs_padr_mes_rh  /* Valor m‚dio por hora de acordo com horas padrao trabalhadas no mes */
       d_vl_calc = d_vl_calc * i_qtd_hrs.                      /* valor m‚dio * quantidade de horas do evento 181 */ 

MESSAGE 'Funcion rio' bfunciona.cdn_funcionario SKIP
        'Evento 400' d_vl_calc
    VIEW-AS ALERT-BOX.

ASSIGN  i-vl-retorno =  d_vl_calc.

RETURN.
