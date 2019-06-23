/****************************************************************************************** 
** 	   Programa: apya597.p
**   	  Autor: Daniela Campos 
** 	 Fornecedor: DKP
**         Data: 17/0702018
** Change/Chamado: 
**      Objetivo: Retornar o diret¢rio padr∆o dos arquivos.
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
DEFINE INPUT  PARAMETER ipc-tipo    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opc-dirbase AS CHARACTER   NO-UNDO.


FIND FIRST es_param_apl NO-LOCK NO-ERROR.
IF AVAIL es_param_apl THEN DO:

    CASE ipc-tipo:

        WHEN "mae" THEN
            ASSIGN opc-dirbase = es_param_apl.DIR_contrato_mae.
        WHEN "aditivo" 
            THEN ASSIGN opc-dirbase = es_param_apl.DIR_aditivo.
        WHEN "filho" 
            THEN ASSIGN opc-dirbase = es_param_apl.DIR_contrato_filho.
        WHEN "ROF" 
            THEN ASSIGN opc-dirbase = es_param_apl.DIR_rof.
    END CASE.
END.
ELSE ASSIGN opc-dirbase = SESSION:TEMP-DIRECTORY.
