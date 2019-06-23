/****************************************************************************************** 
** 	   Programa: estw_bord_ap.p
**   	  Autor: Felipe Vieria
** 	 Fornecedor: DKP
**         Data: 02/10/2018
** Change/Chamado: 
**      Objetivo: Executa o procedimento van/van006.p ao alt
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: item_bord_ap
******************************************************************************************/

{include/i-prgvrs.i estw_bord_ap 12.1.17.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table     FOR bord_ap.
DEFINE PARAMETER BUFFER p-old-table FOR bord_ap.
DEFINE VARIABLE h-van006 AS HANDLE  NO-UNDO.

/* --- MAIN BLOCK --- */
IF NOT NEW(p-table) THEN DO:     

   IF p-table.ind_sit_bord_ap = "Enviado ao Banco" THEN DO:
       
       /*
       RUN van\van006.p PERSISTENT SET h-van006.

       RUN pi-remessa IN h-van006.
       RUN pi-listamsg IN h-van006.
         */

   END.

    
/*   RETURN ERROR. */
END.

RETURN "OK".

