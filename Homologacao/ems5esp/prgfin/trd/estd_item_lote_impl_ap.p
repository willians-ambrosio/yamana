/****************************************************************************************** 
** 	   Programa: estd_item_lote_impl_ap.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 01/11/2018
** Change/Chamado: REQ04
**      Objetivo: Elimina Pendˆncia de Aprova‡Æo do Lote APB
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

DEFINE PARAMETER BUFFER p-table      FOR item_lote_impl_ap.

FIND esp_pend_lote_ap OF p-table EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL esp_pend_lote_ap THEN
    DELETE esp_pend_lote_ap.


