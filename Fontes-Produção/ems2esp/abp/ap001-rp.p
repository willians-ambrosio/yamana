/****************************************************************************************** 
** 	   Programa: ap001-rp.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 21/05/2018
** Change/Chamado: 
**      Objetivo: Importa‡Æo de t¡tulos de antecipaa‡Æo e reembolso de despesa com origem no Concur
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{apb\ap001-rp.i} /* Defini‡äes das tempor rias e espec¡ficas usadas pela API */
{apb\ap001-rp1.i NEW} /* Defini‡äes espec¡ficos para a customiza‡Æo */
{apb\ap001-rp2.i} /* DEfini‡äes espec¡ficas tempor rias API Antecipa‡Æo */

/* Defini‡äes template */
{include/i-prgvrs.i AP001-RP 2.06.00.001}  /*** 010010 ***/
{include/i_dbinst.i}
{utp/ut-glob.i}

{utp/utapi019.i}

{apb\ap001-rp3.i} /* Cont‚m todo o c¢digo fonte e processamento do arquivo utilizado nos programa ap001-rp.p e ap002-rp.p */

