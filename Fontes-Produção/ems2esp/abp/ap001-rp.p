/****************************************************************************************** 
** 	   Programa: ap001-rp.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 21/05/2018
** Change/Chamado: 
**      Objetivo: Importa��o de t�tulos de antecipaa��o e reembolso de despesa com origem no Concur
**
******************************** CONTROLE DE ALTERA��ES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri��o da Altera��o
**
**
****************************** INFORMA��ES ADICIONAIS ************************************
** PAR�METROS DE ENTRADA: N/A
** PAR�METROS DE SA�DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{apb\ap001-rp.i} /* Defini��es das tempor�rias e espec�ficas usadas pela API */
{apb\ap001-rp1.i NEW} /* Defini��es espec�ficos para a customiza��o */
{apb\ap001-rp2.i} /* DEfini��es espec�ficas tempor�rias API Antecipa��o */

/* Defini��es template */
{include/i-prgvrs.i AP001-RP 2.06.00.001}  /*** 010010 ***/
{include/i_dbinst.i}
{utp/ut-glob.i}

{utp/utapi019.i}

{apb\ap001-rp3.i} /* Cont�m todo o c�digo fonte e processamento do arquivo utilizado nos programa ap001-rp.p e ap002-rp.p */

