/****************************************************************************************** 
** 	   Programa: estd_item_bord_ap.p
**   	  Autor: Felipe Vieira
** 	 Fornecedor: DKP
**         Data: 10/08/2018
** Change/Chamado: 
**      Objetivo: Apaga o registro na tabela ext_espec_docto
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

{include/i-prgvrs.i estd_item_bord_ap 12.1.17.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table     FOR item_bord_ap.

/* --- MAIN BLOCK --- */
FIND FIRST ext_item_bord_ap EXCLUSIVE-LOCK
     WHERE ext_item_bord_ap.cod_estab_bord      = p-table.cod_estab_bord      
       AND ext_item_bord_ap.num_id_item_bord_ap = p-table.num_id_item_bord_ap NO-ERROR.
IF AVAIL(ext_item_bord_ap) THEN
    DELETE ext_item_bord_ap.




