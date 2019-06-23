/****************************************************************************************** 
** 	   Programa: bas_extrat_cta_corren_import_u00.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 05/10/2018
** Change/Chamado: 
**      Objetivo: 
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: bas_extrat_cta_corren_import 
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.

/*
    MESSAGE "p-ind-evento.....: " p-ind-event         SKIP
            "p-ind-objeto.....: " p-ind-object        SKIP
            "Handle do Objeto.: " p-wgh-object        SKIP
            "Frame............: " p-wgh-frame         SKIP
            "Nome da tabela...: " p-cod-table         SKIP
            "Rowid da tabela..: " STRING(p-row-table) SKIP
        VIEW-AS ALERT-BOX TITLE "bas-espec-docto".
*/

IF p-ind-event = "INITIALIZE" 
    THEN DO: 

/*        DEFINE VARIABLE h-van006 AS HANDLE      NO-UNDO. */
/*                                                         */
/*        RUN van\van006.p PERSISTENT SET h-van006.        */
/*                                                         */
/*        RUN pi-remessa IN h-van006.                      */
/*        RUN pi-listamsg IN h-van006.                     */

END.
               
