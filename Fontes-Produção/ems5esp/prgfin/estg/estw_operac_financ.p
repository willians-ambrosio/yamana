/******************************************************************************/
/*  Empresa ...........: DSC PRAXIS                                           */
/*  Autor..............: Sergio Luiz Neto da Silveira                         */
/*  Data ..............: 06/09/2016                                           */
/*  Programa ..........: trigger/tw-operac_financ.p                           */
/*  Objetivo ..........: Trigger de WRITE da tabela operac_financ             */
/******************************************************************************/

/*****************************************************************************/
/*     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS                  */
/*****************************************************************************/

{include/i-prgvrs.i tw-operac_financ 12.1.17.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table     FOR operac_financ.
DEFINE PARAMETER BUFFER p-old-table FOR operac_financ.


/* --- MAIN BLOCK --- */
IF NEW(p-table) THEN DO:
   FIND FIRST es_produt_financ
        WHERE es_produt_financ.cod_produt_financ = p-table.cod_produt_financ
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(es_produt_financ) THEN
      RETURN "OK":U.

   /* Valida contrato m∆e */
   IF es_produt_financ.log_contrato THEN DO:

        FIND FIRST es_operac_financ
           WHERE es_operac_financ.cod_banco = p-table.cod_banco AND
                 es_operac_financ.cod_produt_financ = p-table.cod_produt_financ AND
                 es_operac_financ.cod_operac_financ = p-table.cod_operac_financ NO-LOCK NO-ERROR.
        IF NOT AVAIL es_operac_financ THEN DO:
    
          {utp/ut-table.i mgesp es_operac_financ 1}
          RUN utp/ut-msgs.p (INPUT "show",
                             INPUT 2,
                             INPUT RETURN-VALUE).
          RETURN "NOK":U.
       END.
       ELSE IF TRIM(es_operac_financ.cod_contrat_apf) = ""  THEN DO:

           MESSAGE "Contrato m∆e n∆o foi informado!"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN "NOK":U.
       END.
   END. /* contrato ma~e */


   RETURN "OK":U.
END.
ELSE DO:
   IF (p-table.ind_sit_operac_financ_apl <> p-old-table.ind_sit_operac_financ_apl) THEN DO:
       IF p-table.ind_sit_operac_financ_apl = "Ativa" THEN DO:
          FOR EACH es_operac_financ OF p-table EXCLUSIVE-LOCK:

             ASSIGN es_operac_financ.log_atualiza = YES.

             FIND contrat_apf NO-LOCK WHERE
                  contrat_apf.cod_contrat_apf = es_operac_financ.cod_contrat_apf NO-ERROR.
             IF AVAIL contrat_apf THEN
                RUN prgfin/apl/apya598.p (INPUT ROWID(contrat_apf)).
          END.
       END.
   END.
END.

RETURN "OK".
