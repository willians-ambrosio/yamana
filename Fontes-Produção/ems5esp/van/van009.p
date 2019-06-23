/****************************************************************************************** 
** 	   Programa: van009.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 25/09/2018
** Change/Chamado: 
**      Objetivo: Conectar banco do ems5 antes de executar o programa de retorno do HCM
**                Executar a chamada ao programa van009-w01.w
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: Rowid da tabela de movimenta‡Æo da conta corrente
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
   
    {van\van006.i} /* Parametros para conexao de banco */

    RUN pi-conecta.

    RUN van\van009-w01.w.

    RUN pi-discon.

    
