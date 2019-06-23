/****************************************************************************************** 
** 	   Programa: estd_movto_cta_corren.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 04/10/2018
** Change/Chamado: 
**      Objetivo: Bloqueia a exclus∆o de movimentos de transferància enviados ao banco 
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 02/11/2018   Vndo Ribeiro    Grupo DKP   Emilina Pendància de Aprovaáao de Movto CC
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: movto_cta_corren
******************************************************************************************/

{include/i-prgvrs.i estd_movto_cta_corren 12.1.17.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table FOR movto_cta_corren.

DEF BUFFER bf1x-movto_cta_corren FOR movto_cta_corren.
   
/* --- MAIN BLOCK --- */
/* Verifica se o arquivo foi gerado */
DEFINE VARIABLE l-ok AS LOGICAL NO-UNDO.

ASSIGN l-ok = YES.

RUN pi-valida (INPUT p-table.num_id_movto_cta_corren).

IF l-ok THEN DO:
     /* Procura movimento relacionado e tambÇm n∆o deixa alterar */         
    FIND bf1x-movto_cta_corren NO-LOCK WHERE
         bf1x-movto_cta_corren.num_id_movto_cta_corren = p-table.num_id_movto_cta_transf NO-ERROR.
    IF AVAIL bf1x-movto_cta_corren THEN 
        RUN pi-valida (INPUT bf1x-movto_cta_corren.num_id_movto_cta_corren).
END.
IF NOT l-ok
     THEN RETURN ERROR.

/*Emilina Pendància de Aprovaáao de Movto CC*/
FIND esp_pend_movto_cta_corren OF p-table EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL esp_pend_movto_cta_corren THEN
    DELETE esp_pend_movto_cta_corren.

RETURN "OK".
    
PROCEDURE pi-valida:

    DEFINE INPUT  PARAMETER ipi-idmovto AS INTEGER     NO-UNDO.

    /* Verifica se o arquivo foi gerado */
    FIND FIRST es_rem_movto_cta_corren EXCLUSIVE-LOCK WHERE
               es_rem_movto_cta_corren.num_id_movto_cta_corren = ipi-idmovto NO-ERROR.
    IF AVAIL es_rem_movto_cta_corren THEN DO:
        
        IF es_rem_movto_cta_corren.arq_gerado THEN DO:
    
            MESSAGE "Arquivo de remessa j† foi enviado para o banco!" SKIP
                    "Faáa um movimento de estorno!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
            ASSIGN l-ok = no. 
        END.
        IF l-ok
             THEN DELETE es_rem_movto_cta.
    END.
END.

