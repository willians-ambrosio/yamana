/****************************************************************************************** 
** 	   Programa: estw_item_bord_ap.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 30/07/2018
** Change/Chamado: 
**      Objetivo: Valida se ‚ t¡tulo de imposto pela esp‚cie e solicita o c¢digo do tributo
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

{include/i-prgvrs.i estw_item_bord_ap 12.1.17.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table     FOR movto_operac_financ.
DEFINE PARAMETER BUFFER p-old-table FOR movto_operac_financ.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fill-tx-contrat              AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE taxa_contratada AS DECIMAL        NO-UNDO.


/* DEFINE BUFFER bf-item_bord_ap FOR item_bord_ap. */

/* --- MAIN BLOCK --- */
IF NEW(p-table) THEN DO: 

   IF VALID-HANDLE(wh-fill-tx-contrat) THEN DO:

       taxa_contratada = DECIMAL(wh-fill-tx-contrat:SCREEN-VALUE).

       IF taxa_contratada <> 0 THEN DO:

         FIND FIRST ext_movto_operac_financ 
              WHERE ext_movto_operac_financ.num_id_operac_financ  = p-table.num_id_operac_financ   
                AND ext_movto_operac_financ.num_seq_operac_financ = p-table.num_seq_movto_operac_financ 
                NO-ERROR.
         IF NOT AVAIL(ext_movto_operac_financ) THEN DO:

            CREATE ext_movto_operac_financ.
            ASSIGN ext_movto_operac_financ.num_id_operac_financ  = p-table.num_id_operac_financ        
                   ext_movto_operac_financ.num_seq_operac_financ = p-table.num_seq_movto_operac_financ. 

         END.

         ASSIGN ext_movto_operac_financ.cotacao         = taxa_contratada.

       END.
       ELSE DO:
           
           MESSAGE "O valor da taxa contratada nÆo pode ser zero!"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

           RETURN "NOK".
       END.

   END.

END.

    
RETURN "OK".

