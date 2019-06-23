/****************************************************************************************** 
** 	       Programa: cn04001rp-e00.p
**   	      Autor: Daniela Campos
**   	 Fornecedor: DKP
**    	 Data: 17/11/2018
** Change/Chamado: REQ06
**       Objetivo: Efetua chamadas das epcïs re1005rp-e02.p
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**

****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: p-ind-event
** PAR¶METROS DE SAÖDA: tt-epc
** CADASTRADO NO FONTE TOTVS: RE1005RP
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{utp/ut-glob.i}
{include/i-prgvrs.i CN0401 12.06.00.000}
{include/i-epc200.i} 

DEFINE INPUT PARAM p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/* MESSAGE p-ind-event                                    */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.             */
/* FOR EACH tt-epc:                                       */
/*   MESSAGE                                              */
/*     "tt-epc.cod-event      " tt-epc.cod-event     skip */
/*     "tt-epc.cod-parameter  " tt-epc.cod-parameter skip */
/*     "tt-epc.val-parameter  " tt-epc.val-parameter skip */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/* END.                                                   */

{utp/utapi019.i}

DEF TEMP-TABLE RowErrors
    FIELD ErrorSequence     AS Integer              
    FIELD ErrorNumber       AS Integer              
    FIELD ErrorDescription  AS Character        
    FIELD ErrorParameters   AS Character        
    FIELD ErrorType         AS CHARACTER
    FIELD ErrorHelp         AS CHAR 
    FIELD ErrorSubType      AS CHAR.  


DEF NEW GLOBAL SHARED VAR cn0401-log-email AS LOG.

DEF BUFFER bf-contrato-for FOR contrato-for.
    
IF p-ind-event = "antes-mostrar-contrato" THEN DO:

    FIND FIRST tt-epc WHERE
               tt-epc.cod-event     = p-ind-event AND        
               tt-epc.cod-parameter = "contrato-rowid" NO-ERROR.
    IF NOT AVAIL tt-epc THEN RETURN "OK".

    FIND bf-contrato-for NO-LOCK WHERE
         ROWID(bf-contrato-for) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
    IF AVAIL bf-contrato-for AND  bf-contrato-for.ind-sit-contrat = 1 /* NÆo emitido */ THEN DO:
    
        /* Rotina para enviar e-mail qdo tiver garantia para alertar a c‚lula de contratos */
        FIND ext-contrato-for OF bf-contrato-for NO-LOCK NO-ERROR.
        IF AVAIL ext-contrato-for AND ext-contrato-for.ind_tip_gar > 0 AND cn0401-log-email = NO THEN DO:

            RUN pi-sendemail.

        END.
    END.
END.

PROCEDURE pi-sendemail:

   DEFINE VARIABLE hShowMsg AS HANDLE NO-UNDO.

   /* Busca parƒmetro de e-mail da C‚lula de contrato */
   DEFINE VARIABLE c-arq-anexo AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE c-gar AS CHARACTER   NO-UNDO INITIAL "Cau‡Æo,Seguro,Carta Fian‡a".
   DEFINE VARIABLE i-seq AS INTEGER     NO-UNDO.

   FIND FIRST param_email NO-LOCK NO-ERROR.

   FOR EACH es_param_emp NO-LOCK 
       WHERE es_param_emp.cod_empresa    = v_cod_empres_usuar
       AND   es_param_emp.cod_prog_dtsul = "CN0401"
       AND   es_param_emp.cod_referencia BEGINS "email_contrato":

        /* Verifica a existencia da planilha em excel gerada pela procedure pi-report-excel*/
        /* Ser  enviado pelo anexo para o usuario */

        EMPTY TEMP-TABLE tt-envio2.
        EMPTY TEMP-TABLE tt-mensagem.

        CREATE tt-envio2.
        ASSIGN tt-envio2.versao-integracao = 1
               tt-envio2.exchange          = param_email.log_servid_exchange
               tt-envio2.servidor          = param_email.cod_servid_e_mail
               tt-envio2.porta             = param_email.num_porta
               tt-envio2.destino           = es_param_emp.cod_parametro
               tt-envio2.assunto           = "Comunicado de emissÆo de contrato com garantia"
               tt-envio2.remetente         = "suprimentos@yamana.com"
               tt-envio2.copia             = ""
               tt-envio2.mensagem          = "Prezados(a/as)," +  CHR(13) + CHR(13) 
                                             + "Informo que o contrato " + STRING(bf-contrato-for.nr-contrato) + " foi emitido nesta data e possui a garantia " 
                                             + STRING(ENTRY(ext-contrato-for.ind_tip_gar,c-gar))  + "." 
                                             + CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) + "Suprimentos Yamana"   
               tt-envio2.importancia       = 1
               tt-envio2.log-enviada       = NO 
               tt-envio2.log-lida          = NO 
               tt-envio2.acomp             = NO.

        CREATE tt-mensagem.
        ASSIGN tt-mensagem.seq-mensagem = 1
               tt-mensagem.mensagem     = "Prezados(a/as)," +  CHR(13) + CHR(13)                                                                           
                                          + "Informo que o contrato " + STRING(bf-contrato-for.nr-contrato) + " foi emitido nesta data e possui a garantia " 
                                          + STRING(ENTRY(ext-contrato-for.ind_tip_gar,c-gar))  + "."                                                       
                                          + CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) + "Suprimentos Yamana".                              

        RUN utp/utapi019.p PERSISTENT SET h-utapi019.

        RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                      INPUT  TABLE tt-mensagem,
                                      OUTPUT TABLE tt-erros).
        FOR EACH tt-erros:

            ASSIGN i-seq = i-seq + 1.

            CREATE RowErrors.
            ASSIGN RowErrors.ErrorSequence     = i-seq
                   RowErrors.ErrorNumber       = tt-erros.cod-erro
                   RowErrors.ErrorDescription  = tt-erros.desc-erro
                   RowErrors.ErrorParameters   = tt-erros.desc-arq 
                   RowErrors.ErrorType         = "ERRO"
                   RowErrors.ErrorHelp         =  "Contrato: " + STRING(bf-contrato-for.nr-contrato) + 
                                                  " Fornec: " + STRING(bf-contrato-for.cod-emitente) + 
                                                  " E-mail: " + es_param_emp.cod_parametro + "."
                   RowErrors.ErrorSubType      = "ERRO".

        END.

        IF CAN-FIND(FIRST RowErrors) THEN DO:
            {method/showmessage.i1}
            {method/ShowMessage.i2 &Modal="YES"}
        END.    

        ASSIGN cn0401-log-email = YES.
        DELETE PROCEDURE h-utapi019.     
   END.
END.
    
