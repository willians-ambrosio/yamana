/****************************************************************************************** 
** 	   Programa: estw_movto_cta_corren.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 02/11/2018
** Change/Chamado: 
**      Objetivo: Atualiza Pendˆncia de Aprova‡ao Movto CC
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: movto_cta_corren
******************************************************************************************/

{include/i-prgvrs.i estw_movto_cta_corren 12.1.17.000}
    
{utp/ut-glob.i} 
{utp/utapi019.i}

DEFINE PARAMETER BUFFER p-table     FOR movto_cta_corren.
DEFINE PARAMETER BUFFER p-old-table FOR movto_cta_corren.

DEFINE VARIABLE c-mess-top  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess-base AS CHARACTER   NO-UNDO.

IF p-table.cod_modul_dtsul = "CMG" AND p-table.cod_tip_trans_cx <> "20.3" THEN
DO:
    RUN utp/utapi019.p PERSISTENT SET h-utapi019.
    {prgfin\apb\apb001-i04.i}

    FIND esp_pend_movto_cta_corren OF p-table NO-LOCK NO-ERROR.
    /*
    FIND FIRST esp_pend_movto_cta_corren NO-LOCK
         WHERE esp_pend_movto_cta_corren.cod_cta_corren           = p-table.cod_cta_corren              
           and esp_pend_movto_cta_corren.dat_movto_cta_corren     = p-table.dat_movto_cta_corren              
           and esp_pend_movto_cta_corren.num_seq_movto_cta_corren = p-table.num_seq_movto_cta_corren NO-ERROR.
    */
    IF NOT AVAIL esp_pend_movto_cta_corren THEN
    DO:
        CREATE esp_pend_movto_cta_corren.
        ASSIGN 
            esp_pend_movto_cta_corren.cod_cta_corren           = p-table.cod_cta_corren          
            esp_pend_movto_cta_corren.dat_movto_cta_corren     = p-table.dat_movto_cta_corren    
            esp_pend_movto_cta_corren.num_seq_movto_cta_corren = p-table.num_seq_movto_cta_corren.

    END.
    FIND CURRENT esp_pend_movto_cta_corren EXCLUSIVE-LOCK.
    ASSIGN esp_pend_movto_cta_corren.cod_usuario  = c-seg-usuario
           esp_pend_movto_cta_corren.dt_digitacao = NOW.
    FIND CURRENT esp_pend_movto_cta_corren NO-LOCK.

    c-mess = "<tr>"
     + "<td>" + p-table.cod_cta_corren +                                    "</td>"
     + "<td>" + STRING(p-table.dat_movto_cta_corren, "99/99/9999") +        "</td>"
     + "<td>" + STRING(p-table.num_seq_movto_cta_corren, ">>>9") +          "</td>"
     + "<td>" + p-table.ind_fluxo_movto_cta_corren +                        "</td>"
     + "<td>" + STRING(p-table.val_movto_cta_corren, "->>>>,>>>,>>9.99") +  "</td>"
     + "<td>" + STRING(p-table.dat_transacao , "99/99/9999") +              "</td>"
     + "<td>" + p-table.des_histor_movto_cta_corren +                       "</td>"
     + "<td>" + c-seg-usuario +                                             "</td>"
     + "<td>" + STRING(NOW, "99/99/9999 HH:MM") +                           "</td>"
     + "</tr>".

    FIND FIRST param_email NO-LOCK NO-ERROR.
    FOR EACH esp_aprovador NO-LOCK
       WHERE esp_aprovador.aprov_caixa_bancos
         AND esp_aprovador.nivel_aprovador = 1:

         EMPTY TEMP-TABLE tt-envio2.
         EMPTY TEMP-TABLE tt-mensagem.
         EMPTY TEMP-TABLE tt-erros.

        CREATE tt-envio2.
            ASSIGN tt-envio2.versao-integracao = 1
                   tt-envio2.exchange          = param_email.log_servid_exchange
                   tt-envio2.servidor          = param_email.cod_servid_e_mail
                   tt-envio2.porta             = param_email.num_porta
                   tt-envio2.destino           = esp_aprovador.email
                   tt-envio2.assunto           = "Pendˆncia de Movimentos para Aprova‡Æo"
                   tt-envio2.remetente         = "SustencaoYamana@yamana.com"
                   tt-envio2.copia             = ""
                   tt-envio2.importancia       = 1
                   tt-envio2.log-enviada       = NO
                   tt-envio2.log-lida          = NO
                   tt-envio2.acomp             = NO
                   tt-envio2.formato           = "html".

            CREATE tt-mensagem.                                       
            ASSIGN tt-mensagem.seq-mensagem = 1                         
                   tt-mensagem.mensagem     = c-mess-top + c-mess + c-mess-base. 

            RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                           INPUT  TABLE tt-mensagem,
                                           OUTPUT TABLE tt-erros).
        IF TEMP-TABLE tt-erros:HAS-RECORDS THEN
        DO:
            OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erro-email-cmg-" + STRING(TODAY, "99999999") + "-" + STRING(ETIME, "99999999") + ".txt").
            FOR EACH tt-erros:
                DISP tt-erros WITH SCROLLABLE.
            END.
            OUTPUT CLOSE.
        END.
    END.
    DELETE PROCEDURE h-utapi019.

    RUN utp/ut-msgs.p ("show",31396,"Bloqueio de Movimento~~Movimento gerou pendˆncia e foi enviado para aprova‡Æo. S¢ ser  possivel contabilizar ap¢s a aprova‡Æo.").


END.
RETURN "OK".

