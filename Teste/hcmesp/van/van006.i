/****************************************************************************************** 
** 	   Programa: van006.i
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 25/09/2018
** Change/Chamado: 
**      Objetivo: Conectar banco do ems5 antes de executar o programa de retorno do HCM
**                Executar a chamada ao programa van009-w01.w
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: 
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

    
PROCEDURE pi-conecta:
    /* Conecta bancos do 5 */
    DEFINE VARIABLE c_conect AS CHARACTER   NO-UNDO.

    FIND FIRST param_conex_base_dados WHERE
               param_conex_base_dados.cdn_base_dados_conex = 02 AND 
               param_conex_base_dados.nom_fisic_base_dados = 'ems5' NO-ERROR.
    IF NOT AVAIL PARAM_conex_base_dados THEN RETURN "Parametro de conex∆o com o EMS5 n∆o encontrado!".
            
    ASSIGN c_conect =   " -db " + param_conex_base_dados.nom_fisic_base_dados 
                      + " -ld " + param_conex_base_dados.nom_logic_base_dados
                      + " -H "  + param_conex_base_dados.nom_maquina
                      + " -S "  + param_conex_base_dados.des_serv_conex 
                      + " -N "  + param_conex_base_dados.des_protoc_conex_base_dados.
    
    IF NOT CONNECTED(param_conex_base_dados.nom_logic_base_dados) 
        THEN CONNECT VALUE(c_conect).
    
    IF NOT CONNECTED(param_conex_base_dados.nom_fisic_base_dados) THEN DO:
        RUN utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Banco Movimento n∆o Conectado~~N∆o ser† poss°vel mostrar as informaá‰es relativas a conta e centro de custo ").
        RETURN "NOK":U.
    END.    

END PROCEDURE.

PROCEDURE pi-discon:

    FIND FIRST param_conex_base_dados WHERE
               param_conex_base_dados.cdn_base_dados_conex = 02 AND 
               param_conex_base_dados.nom_fisic_base_dados = 'ems5' NO-ERROR.
    IF CONNECTED(param_conex_base_dados.nom_logic_base_dados) THEN 
           DISCONNECT VALUE(param_conex_base_dados.nom_logic_base_dados).

END PROCEDURE.
