/****************************************************************************************** 
** 	   Programa: movto_cta_corren.i
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 25/09/2018
** Change/Chamado: 
**      Objetivo: Gera arquivo de Remessa de transf. entre contas para banco via van Accesstage
**                Segmentos A e B layout 240 posiá‰es - Banco do Brasil
**                Usado nos fontes add_movto_cta_corren_u00.p e mov_movto_cta_corren_u00.p
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: Rowid da tabela de movimentaá∆o da conta corrente
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
    DEF BUFFER es_param_cta_corren  FOR ems5_esp.es_param_cta_corren.
    DEF BUFFER bf-movto_cta_corren  FOR movto_cta_corren.
    DEF BUFFER bf1-movto_cta_corren FOR movto_cta_corren.
    DEF BUFFER bf-cta_corren        FOR cta_corren.

       
    FIND bf-movto_cta_corren NO-LOCK WHERE
        RECID(bf-movto_cta_corren) = p-row-table NO-ERROR.
    IF NOT AVAIL bf-movto_cta_corren THEN RETURN "OK".
 
    /* Somente movimentos de sa°da */
    IF bf-movto_cta_corren.ind_fluxo_movto_cta_corren = "SAI" THEN DO: 

        FIND tip_trans_cx NO-LOCK WHERE
             tip_trans_cx.cod_tip_trans_cx = bf-movto_cta_corren.cod_tip_trans_cx NO-ERROR.
        IF NOT AVAIL tip_trans_cx THEN RETURN "OK".

        IF tip_trans_cx.ind_espec_tip_trans_cx <> "transf" THEN RETURN "OK". /* Somente executa para movimentos de transfer~encia entre contas */
        
        /* Verifica se o arquivo foi gerado */
        FIND FIRST es_rem_movto_cta_corren NO-LOCK WHERE
                   es_rem_movto_cta_corren.num_id_movto_cta_corren = bf-movto_cta_corren.num_id_movto_cta_corren NO-ERROR.
        IF AVAIL es_rem_movto_cta_corren THEN DO:
            
            IF es_rem_movto_cta_corren.arq_gerado THEN DO:
    
                MESSAGE "Arquivo de remessa j† foi enviado para o banco!" SKIP
                        "Faáa um movimento de estorno!"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                RETURN ERROR.
    
            END.
    
            RETURN "OK".
        END.
        IF NOT AVAIL es_rem_movto_cta_corren OR NOT es_rem_movto_cta_corren.arq_gerado THEN DO:
            /* Verifica se a conta corrente est† dentro dos parÉmetros para gerar o arquivo de remessa */
            FIND FIRST es_param_van_dir NO-LOCK 
                 WHERE es_param_van_dir.identificador  = "01" /* Remessa */
                   AND es_param_van_dir.cod_cta_corren =  bf-movto_cta_corren.cod_cta_corren  NO-ERROR.
            IF NOT AVAIL es_param_van_dir THEN RETURN "OK". /* nem todas as contas tem o psdid e ir∆o gerar edi para van de pagamentos */
            
            IF SEARCH(es_param_van_dir.progTransf) = ? THEN DO:
        
                MESSAGE "Programa EDI Transferencia n∆o encontrado!"   SKIP   
                        "VAN001-W01 PSDID: " es_param_van_dir.psdid SKIP
                        "ID:" es_param_van_dir.identificador            SKIP
                        "Conta Corrente: " bf-movto_cta_corren.cod_cta_corren SKIP
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "NOK".
            END.
        
            RUN VALUE(es_param_van_dir.progTransf) (INPUT ROWID(bf-movto_cta_corren),
                                                    INPUT es_param_van_dir.dir_remessa).
            IF RETURN-VALUE <> "OK"
                 THEN DO:
        
                MESSAGE RETURN-VALUE
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
                RETURN ERROR.
            END.

            CREATE es_rem_movto_cta_corren.
            ASSIGN es_rem_movto_cta_corren.num_id_movto_cta_corren = bf-movto_cta_corren.num_id_movto_cta_corren
                   es_rem_movto_cta_corren.DtHrGer = DATETIME(TODAY).
        END.
    END.
    ELSE DO:
        /* Procura movimento relacionado e tambÇm n∆o deixa alterar */         
        FIND bf1-movto_cta_corren NO-LOCK WHERE
             bf1-movto_cta_corren.num_id_movto_cta_corren = bf-movto_cta_corren.num_id_movto_cta_transf NO-ERROR.
        IF AVAIL bf1-movto_cta_corren THEN DO:

            FIND tip_trans_cx NO-LOCK WHERE
                 tip_trans_cx.cod_tip_trans_cx = bf-movto_cta_corren.cod_tip_trans_cx NO-ERROR.
            IF NOT AVAIL tip_trans_cx THEN RETURN "OK".

            IF tip_trans_cx.ind_espec_tip_trans_cx <> "transf" THEN RETURN "OK". /* Somente executa para movimentos de transfer~encia entre contas */

            /* Verifica se o arquivo foi gerado */
            FIND FIRST es_rem_movto_cta_corren NO-LOCK WHERE
                       es_rem_movto_cta_corren.num_id_movto_cta_corren = bf1-movto_cta_corren.num_id_movto_cta_corren NO-ERROR.
            IF AVAIL es_rem_movto_cta_corren THEN DO:
                
                IF es_rem_movto_cta_corren.arq_gerado THEN DO:
        
                    MESSAGE "Arquivo de remessa j† foi enviado para o banco!" SKIP
                            "Faáa um movimento de estorno!"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
                    RETURN ERROR.
                END.
        
                RETURN "OK".
            END.
        END.

        RETURN "OK".
    END.
    
  
    
