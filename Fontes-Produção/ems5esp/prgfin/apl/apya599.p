/****************************************************************************************** 
** 	   Programa: apya599.p
**   	  Autor: 
** 	 Fornecedor: DKP
**         Data: 
** Change/Chamado: 
**      Objetivo: Abrir o arquivo anexo do contrato 
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAMETER ipc-arquivo AS CHARACTER NO-UNDO.
DEFINE VARIABLE objAppWScript AS COM-HANDLE  NO-UNDO.

IF SEARCH(ipc-arquivo) <> ? THEN DO:

    CREATE "Wscript.Shell":U objAppWScript.
    objAppWScript:Run(QUOTER(ipc-arquivo)).
    RELEASE OBJECT objAppWScript NO-ERROR.

END.
ELSE 
    MESSAGE "Arquivo n∆o encontrado ou usu†rio sem permiss∆o para acesso."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

