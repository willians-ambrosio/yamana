/****************************************************************************************** 
** 	   Programa: movto_cta_corren.i
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 25/09/2018
** Change/Chamado: 
**      Objetivo: Gera arquivo de Remessa de transf. entre contas para banco via van Accesstage
**                Segmentos A e B layout 240 posi‡äes - Banco do Brasil
**                Usado nos fontes add_movto_cta_corren_u00.p e mov_movto_cta_corren_u00.p
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: Rowid da tabela de movimenta‡Æo da conta corrente
** PAR¶METROS DE SAÖDA: N/A
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

    /* Verifica se o arquivo foi gerado */
    FIND FIRST es_rem_movto_cta_corren NO-LOCK WHERE
               es_rem_movto_cta_corren.num_id_movto_cta_corren = bf-movto_cta_corren.num_id_movto_cta_corren NO-ERROR.
    IF AVAIL es_rem_movto_cta_corren THEN DO:
            
        IF es_rem_movto_cta_corren.arq_gerado THEN DO:

            MESSAGE "Arquivo de remessa j  foi enviado para o banco!" SKIP
                    "Fa‡a um movimento de estorno!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN ERROR.

        END.
    END.    
 
    /* Somente movimentos de sa¡da */
    IF (bf-movto_cta_corren.ind_fluxo_movto_cta_corren = "SAI" AND bf-movto_cta_corren.cod_tip_trans_cx = "20.3") OR 
        AVAIL es_rem_movto_cta_corren THEN DO: 

        FIND tip_trans_cx NO-LOCK WHERE
             tip_trans_cx.cod_tip_trans_cx = bf-movto_cta_corren.cod_tip_trans_cx NO-ERROR.
        IF NOT AVAIL tip_trans_cx THEN RETURN "OK".

        IF tip_trans_cx.ind_espec_tip_trans_cx <> "transf" THEN RETURN "OK". /* Somente executa para movimentos de transfer~encia entre contas */
        
        /* Verifica se o arquivo foi gerado */
        FIND FIRST es_rem_movto_cta_corren NO-LOCK WHERE
                   es_rem_movto_cta_corren.num_id_movto_cta_corren = bf-movto_cta_corren.num_id_movto_cta_corren NO-ERROR.
        IF AVAIL es_rem_movto_cta_corren THEN DO:
            
            IF es_rem_movto_cta_corren.arq_gerado THEN DO:
    
                MESSAGE "Arquivo de remessa j  foi enviado para o banco!" SKIP
                        "Fa‡a um movimento de estorno!"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                RETURN ERROR.
    
            END.
    
            RETURN "OK".
        END.

        IF NOT AVAIL es_rem_movto_cta_corren OR NOT es_rem_movto_cta_corren.arq_gerado THEN DO:

            DEFINE VARIABLE c-parceiro AS CHARACTER   NO-UNDO.

            FIND cta_corren NO-LOCK WHERE
                 cta_corren.cod_cta_corren = bf-movto_cta_corren.cod_cta_corren NO-ERROR.

            ASSIGN c-parceiro = (IF cta_corren.cod_banco = "1" THEN "388" ELSE cta_corren.cod_banco) +  STRING(i-ep-codigo-usuario).

                FIND localiz_arq_edi NO-LOCK WHERE
                     localiz_arq_edi.ind_finalid_localiz_arq_edi = "exportacao" AND 
                     localiz_arq_edi.cdn_parcei_edi              = int(c-parceiro) AND 
                     localiz_arq_edi.cdn_trans_edi               = 1000 NO-ERROR.
                IF NOT AVAIL localiz_arq_edi THEN RETURN "OK".


            /* Verifica qual ‚ o banco e executa o programa que gera a remessa */
            CASE cta_corren.cod_banco:
                /* Ler diret¢rio do EDI */ 

                WHEN "237" THEN RUN van\van004.p (INPUT ROWID(bf-movto_cta_corren),    
                                                  INPUT localiz_arq_edi.des_dir_edi). /* Bradesco */ 
                    
                WHEN "1" THEN RUN van\van003.p (INPUT ROWID(bf-movto_cta_corren),  
                                                INPUT localiz_arq_edi.des_dir_edi). /* Brasil */
                    
                WHEN "745" THEN RUN van\van005.p (INPUT ROWID(bf-movto_cta_corren),  
                                                  INPUT localiz_arq_edi.des_dir_edi). /* Citi */

            END CASE.
           
            CREATE es_rem_movto_cta_corren.
            ASSIGN es_rem_movto_cta_corren.num_id_movto_cta_corren = bf-movto_cta_corren.num_id_movto_cta_corren
                   es_rem_movto_cta_corren.DtHrGer = DATETIME(TODAY)
                   es_rem_movto_cta_corren.arq_gerado = YES.
        END.
    END.
    ELSE DO:
        /* Procura movimento relacionado e tamb‚m nÆo deixa alterar */         
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
        
                    MESSAGE "Arquivo de remessa j  foi enviado para o banco!" SKIP
                            "Fa‡a um movimento de estorno!"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
                    RETURN ERROR.
                END.
        
                RETURN "OK".
            END.
        END.

        RETURN "OK".
    END.
    
  
    
