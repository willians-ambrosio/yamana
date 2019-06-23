/****************************************************************************************** 
** 	   Programa: apl007ea-u05.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: display das informaá‰es do contrato m∆e na tela da operaá∆o financeira 
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

def new global shared var wh-fill    AS widget-handle no-undo.
def new global shared var wh-fill-2  AS widget-handle no-undo.

find contrat_apf where contrat_apf.cod_contrat_apf = wh-fill:SCREEN-VALUE no-lock no-error.
IF AVAILABLE(contrat_apf) THEN DO:
   ASSIGN wh-fill:SCREEN-VALUE   = contrat_apf.cod_contrat_apf  
          wh-fill-2:SCREEN-VALUE = contrat_apf.des_contrat_apf.
END.
ELSE ASSIGN wh-fill:SCREEN-VALUE   = ""
            wh-fill-2:SCREEN-VALUE = "".   

    
    
