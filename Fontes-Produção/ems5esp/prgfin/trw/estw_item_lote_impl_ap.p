/****************************************************************************************** 
** 	   Programa: estw_item_lote_impl_ap.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 01/11/2018
** Change/Chamado: REQ04
**      Objetivo: Atualiza Pendˆncia de Aproca‡Æo do Lote APB
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: item_lote_impl_ap
******************************************************************************************/
{include/i-prgvrs.i estw_item_lote_impl_ap 12.1.17.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table      FOR item_lote_impl_ap.
DEFINE PARAMETER BUFFER p-old-table  FOR item_lote_impl_ap.

IF p-table.ind_origin_tit_ap = "APB" AND NOT 
    CAPS(v_cod_usuar_corren) BEGINS "RPW" AND NOT
    CAPS(USERID("ems5")) BEGINS "RPW" THEN
DO:
    FIND esp_pend_lote_ap OF p-table NO-LOCK NO-ERROR.
    /*
    FIND FIRST esp_pend_lote_ap EXCLUSIVE-LOCK
         WHERE esp_pend_lote_ap.cod_estab      = p-table.cod_estab        
           and esp_pend_lote_ap.cod_refer      = p-table.cod_refer              
           and esp_pend_lote_ap.num_seq_refer  = p-table.num_seq_refer NO-ERROR.         
    */        
    IF NOT AVAIL esp_pend_lote_ap THEN
    DO:
        CREATE esp_pend_lote_ap.
        ASSIGN
            esp_pend_lote_ap.cod_estab            = p-table.cod_estab    
            esp_pend_lote_ap.cod_refer            = p-table.cod_refer    
            esp_pend_lote_ap.num_seq_refer        = p-table.num_seq_refer.
    END.
    
    FIND CURRENT esp_pend_lote_ap EXCLUSIVE-LOCK.
    ASSIGN esp_pend_lote_ap.cod_usuario  = c-seg-usuario
           esp_pend_lote_ap.dt_digitacao = NOW.
    FIND CURRENT esp_pend_lote_ap NO-LOCK.
END.
