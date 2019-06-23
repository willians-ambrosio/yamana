/****************************************************************************************** 
**         Programa: apb001-i01.i
**            Autor: Vando Ribeiro
**       Fornecedor: DKP
**       Data: 05/11/2018
** Change/Chamado: REQ04
**    Objetivo: Atualizar a tabela temporaria de pendencia de aprova‡Æo de titulos.
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**

****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

FIND FIRST esp_estab_aprov NO-LOCK
     WHERE esp_estab_aprov.cod_usuario = c-seg-usuario
       AND esp_estab_aprov.cod-estabel = esp_pend_lote_ap.cod_estab NO-ERROR.
IF AVAIL esp_estab_aprov THEN
DO:
    CREATE tt_esp_pend_lote_ap.
    BUFFER-COPY esp_pend_lote_ap TO tt_esp_pend_lote_ap NO-ERROR.

    FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.
    IF AVAIL item_lote_impl_ap THEN
    ASSIGN
          tt_esp_pend_lote_ap.cdn_fornecedor    = item_lote_impl_ap.cdn_fornecedor   
          tt_esp_pend_lote_ap.cod_espec_docto   = item_lote_impl_ap.cod_espec_docto  
          tt_esp_pend_lote_ap.cod_ser_docto     = item_lote_impl_ap.cod_ser_docto    
          tt_esp_pend_lote_ap.cod_tit_ap        = item_lote_impl_ap.cod_tit_ap       
          tt_esp_pend_lote_ap.cod_parcela       = item_lote_impl_ap.cod_parcela      
          tt_esp_pend_lote_ap.dat_emis_docto    = item_lote_impl_ap.dat_emis_docto   
          tt_esp_pend_lote_ap.dat_vencto_tit_ap = item_lote_impl_ap.dat_vencto_tit_ap
          tt_esp_pend_lote_ap.val_tit_ap        = item_lote_impl_ap.val_tit_ap
          tt_esp_pend_lote_ap.des_text_histor   = item_lote_impl_ap.des_text_histor.      

END.
