/****************************************************************************************** 
** 	   Programa: van009.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 25/09/2018
** Change/Chamado: 
**      Objetivo: Conectar banco do ems5 antes de executar o programa de retorno do HCM
**                Executar a chamada ao programa van009-w01.w
******************************** CONTROLE DE ALTERA��ES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri��o da Altera��o
**
**
****************************** INFORMA��ES ADICIONAIS ************************************
** PAR�METROS DE ENTRADA: Rowid da tabela de movimenta��o da conta corrente
** PAR�METROS DE SA�DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
   
    {van\van006.i} /* Parametros para conexao de banco */

    RUN pi-conecta.

    RUN van\van009-w01.w.

    RUN pi-discon.

    
