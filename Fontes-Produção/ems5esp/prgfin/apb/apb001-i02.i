/****************************************************************************************** 
**         Programa: apb001-i02.i
**            Autor: Vando Ribeiro
**       Fornecedor: DKP
**       Data: 05/11/2018
** Change/Chamado: REQ04
**    Objetivo: Atualizar a tabela temporaria de pendencia de aprova‡Æo de movtos cmg.
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
    
CREATE tt_esp_pend_movto_cta_corren.
BUFFER-COPY esp_pend_movto_cta_corren TO tt_esp_pend_movto_cta_corren NO-ERROR.
FIND movto_cta_corren OF esp_pend_movto_cta_corren NO-LOCK NO-ERROR.
IF AVAIL movto_cta_corren THEN
ASSIGN
    tt_esp_pend_movto_cta_corren.val_movto_cta_corren        = movto_cta_corren.val_movto_cta_corren          
    tt_esp_pend_movto_cta_corren.dat_transacao               = movto_cta_corren.dat_transacao                 
    tt_esp_pend_movto_cta_corren.des_histor_movto_cta_corren = movto_cta_corren.des_histor_movto_cta_corren   
    tt_esp_pend_movto_cta_corren.cod_usuar_ult_atualiz       = movto_cta_corren.cod_usuar_ult_atualiz.

