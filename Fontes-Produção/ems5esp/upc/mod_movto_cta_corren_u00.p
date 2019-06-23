/****************************************************************************************** 
** 	   Programa: mod_movto_cta_corren_u00.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 25/09/2018
** Change/Chamado: 
**      Objetivo: Gerar o arquivo de envio de remessa do EDI para transferˆncia entre contas da mesma
                  empresa
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: add_movto_cta_corren
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID         NO-UNDO.

/*
MESSAGE "p-ind-evento.....: " p-ind-event         SKIP
        "p-ind-objeto.....: " p-ind-object        SKIP
        "Handle do Objeto.: " p-wgh-object        SKIP
        "Frame............: " p-wgh-frame         SKIP
        "Nome da tabela...: " p-cod-table         SKIP
        "Rowid da tabela..: " STRING(p-row-table) SKIP
    VIEW-AS ALERT-BOX TITLE "bas-espec-docto".
*/

IF p-ind-event = "VALIDATE"  THEN DO:

   {upc\movto_cta_corren.i}
    
END.
