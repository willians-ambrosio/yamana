/****************************************************************************************** 
** 	   Programa: apya598.p
**   	  Autor: 
** 	 Fornecedor: DKP
**         Data: 
** Change/Chamado: 
**      Objetivo: Abrir o arquivo anexo do contrato 
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
DEFINE INPUT  PARAMETER ipr-rowid-contrat AS ROWID       NO-UNDO.

DEFINE VARIABLE de-total-aditivo   LIKE aditivo_contrat_apf.val_lim_cr_aditivo_contrat_apf.
DEFINE VARIABLE de-total-operacao  LIKE operac_financ.val_operac_financ.
DEFINE VARIABLE d-cotacao          AS DECIMAL     NO-UNDO.

DEF BUFFER cotacao FOR ems2cadme.cotacao.
DEF BUFFER b-contrat_apf FOR contrat_apf.

FIND b-contrat_apf WHERE 
        ROWID(b-contrat_apf) = ipr-rowid-contrat NO-ERROR.
IF NOT AVAIL b-contrat_apf THEN RETURN 'nok'.

/* Total aditivo */
FOR EACH aditivo_contrat_apf OF b-contrat_apf NO-LOCK:

   ASSIGN de-total-aditivo = de-total-aditivo + aditivo_contrat_apf.val_lim_cr_aditivo_contrat_apf.

END.

/* Soma valor das opera‡äes */
FOR EACH es_operac_financ
         WHERE es_operac_financ.cod_contrat_apf = b-contrat_apf.cod_contrat_apf
           AND es_operac_financ.log_atualizado  = YES:

    /* DPC */
    ASSIGN es_operac_financ.val_operac_financ_dolar = 0.
   /* Atualiza o valor da opera‡Æo financeira */
   IF es_operac_financ.val_operac_financ_dolar = 0 THEN DO:

       FIND operac_financ OF es_operac_financ NO-LOCK NO-ERROR.

       /* Converte para a moeda do Contrato */
       IF operac_financ.cod_indic_econ <> b-contrat_apf.cod_indic_econ 
           THEN ASSIGN es_operac_financ.val_operac_financ_dolar = ROUND(operac_financ.val_operac_financ / es_operac_financ.vl-cotac-saldo,4).
       ELSE es_operac_financ.val_operac_financ_dolar = operac_financ.val_operac_financ.

   END.
   ASSIGN de-total-operacao = de-total-operacao + es_operac_financ.val_operac_financ_dolar.
   
END.
ASSIGN b-contrat_apf.val_operac_financ = de-total-operacao
       b-contrat_apf.val_aditivos      = de-total-aditivo.
