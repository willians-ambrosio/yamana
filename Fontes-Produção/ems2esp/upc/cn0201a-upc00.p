/****************************************************************************************** 
** 	   Programa: cn0201a-upc00.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: 
**      Objetivo: Chamada das diversas upcïs utilizadas pelo cadastro de contratos 
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: cn0201a
** CADASTRADO NA TABELA: N/A
******************************************************************************************/



/*****************************************************************************
 ** PROGRAMA..: CN0201-U00.P
 ** OBJETIVO..: UPC NA MANUTENCAO DE CONTRATOS - CN0201
 ** AUTOR.....: DSC
 ** CLIENTE...: YAMANA
 ** VERSAO....: 2.00.00.001 - 21/05/2009 - Luiz CRUZ.
 ** ALTERA€åES:
 ******************************************************************************/

/* *** DEFINICAO DE PARAMETROS *** */
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.


/*******************************************************************************
** Chamada do Programa: cn0201a-upc
**            Autor...: Log¡stica (log339640)
**            Data....: 07/2008
**            OBS.....: UPC utilizada pelo programa cn0201a.w
**            Objetivo: 
*******************************************************************************/

    run upc/cn0201a-upc.p(input p-ind-event, 
                          input p-ind-object,
                          input p-wgh-object,
                          input p-wgh-frame, 
                          input p-cod-table, 
                          input p-row-table).   

    IF RETURN-VALUE = 'NOK'  THEN RETURN-VALUE.
        

/*******************************************************************************
** Chamada do Programa: cn0201a-upc01.P
**            Autor...: Luiz CRUZ/DSC
**            Data....: 05/2009
**            OBS.....: Desabilita campo COMPRADOR e carrega o codigo 
**                      do usuario logado.
**            Objetivo: 
*******************************************************************************/

   /* include comentada para teste de materiais
    essa inclide nÆo esta na nossa especifica‡Æo e est  faltando includes que a datasul nÆo possui */
    
    run upc/cn0201a-upc01.P(input p-ind-event,
                            input p-ind-object,
                            input p-wgh-object,
                            input p-wgh-frame,
                            input p-cod-table,
                            input p-row-table).
                           
    /* DPC- Projeto tesouraria REQ06 - Cria aba de parƒmetros de garantia de contratos */
    run upc/cn0201a-upc02.P(input p-ind-event, 
                            input p-ind-object,
                            input p-wgh-object,
                            input p-wgh-frame, 
                            input p-cod-table, 
                            input p-row-table).
