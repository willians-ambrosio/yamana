/******************************************************************
**
** Programa: tw_cotac_parid_U01
**
** Objetivo: Trigger de Cota‡Æo de Moeda
**
**    Autor: Willians Moreira Ambrosio - DKP
**
**     Data: Jan/2019
**
**   Versao: 12.01.21.000 - Bloqueio de altera‡Æo da cota‡Æo de moeda
**                          Bloqueio de valor com varia‡Æo acima/menor 5%
******************************************************************/
{include/i-prgvrs.i tw_cotac_parid_U01 12.01.21.000}
/* ----------------------------------------------------------- */
Def Parameter Buffer p_table     For cotac_parid.
Def Parameter Buffer p_old_table For cotac_parid.
/* ----------------------------------------------------------- */
DEFINE VARIABLE c_des_msg      As CHARACTER NO-UNDO.
DEFINE VARIABLE de_val_liq     As DECIMAL   NO-UNDO.
DEFINE VARIABLE i-contador     AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-alt-prog     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cAuxMsg        AS CHARACTER NO-UNDO.

DEFINE VARIABLE de-valor-orig  LIKE cotac_parid.val_cotac_indic_econ   NO-UNDO.
DEFINE VARIABLE de-valor-atual LIKE cotac_parid.val_cotac_indic_econ   NO-UNDO.
DEFINE VARIABLE de-variacao    LIKE cotac_parid.val_cotac_indic_econ   NO-UNDO.
/* ----------------------------------------------------------- */
ASSIGN i-contador = 1
       l-alt-prog = NO.
/* ----------------------------------------------------------- */
DO WHILE PROGRAM-NAME(i-contador) <> ?:

    IF INDEX(PROGRAM-NAME(i-contador), "utb025aa.p":U)           <> 0 AND 
       INDEX(PROGRAM-NAME(i-contador), "pi_atualiza_cotacoes":U) <> 0 THEN
    DO:
        ASSIGN l-alt-prog = YES.
        LEAVE.
    END.
    ASSIGN i-contador = i-contador + 1.
END.
/* ----------------------------------------------------------- */
IF l-alt-prog AND p_table.cod_indic_econ_base = "Real" THEN
DO:
   ASSIGN de-valor-orig  = p_old_table.val_cotac_indic_econ
          de-valor-atual = p_table.val_cotac_indic_econ
          de-variacao    = (de-valor-orig / 100) * 5
          cAuxMsg        = "".
                                                                                                                                                   
   IF p_old_table.val_cotac_indic_econ <> p_table.val_cotac_indic_econ THEN
   DO:
       IF p_table.cod_indic_econ_base = "REAL" AND
          p_table.cod_indic_econ_idx  = "UFIR" THEN         
       DO:
          run utp/ut-msgs.p (input "show",
                             input 17006,
                             input "Altera‡Æo NÆo Permitida!~~Altera‡Æo de cota‡Æo de moeda entre Real/UFIR, nÆo permitida.").       
          RETURN "NOK".
       END.
       ELSE
       DO:
          IF ((de-valor-atual - de-valor-orig ) > de-variacao OR 
              (de-valor-orig  - de-valor-atual) > de-variacao) THEN
          DO:
              run utp/ut-msgs.p (input "show",
                                 input 15825,
                                 input "Aten‡Æo!~~Varia‡Æo da cota‡Æo, esta superior a 5%.").             
          END.
       END.
   END.
END.

RETURN "OK".

