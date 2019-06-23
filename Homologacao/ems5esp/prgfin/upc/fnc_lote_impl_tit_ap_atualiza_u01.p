/****************************************************************************************** 
** 	   Programa: fnc_lote_impl_tit_ap_atualiza_u01.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 01/11/2018
** Change/Chamado: REQ04
**      Objetivo: Elimina Pendància de Aprocaá∆o do Lote APB
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: p-ind-event
** PAR∂METROS DE SA÷DA: l-error e tt-epc
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{include\i-epc200.i1}

DEFINE INPUT        PARAMETER p-ind-event AS CHARACTER   NO-UNDO.
DEFINE OUTPUT       PARAMETER l-erro      AS LOGICAL     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-epc.

DEFINE NEW GLOBAL SHARED VARIABLE l-prg-aprov-movto  AS LOGICAL NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE v_cod_usuar_corren AS CHARACTER FORMAT "x(12)":U LABEL "Usu†rio Corrente" COLUMN-LABEL  "Usu†rio Corrente" NO-UNDO.

IF p-ind-event = "item_lote_impl_ap" AND NOT l-prg-aprov-movto THEN
DO:
    ASSIGN l-erro = YES.
    FOR EACH tt-epc WHERE
             tt-epc.cod-event = p-ind-event:

        IF tt-epc.cod-parameter <> "Recid" THEN NEXT.

        FIND item_lote_impl_ap NO-LOCK WHERE
            RECID(item_lote_impl_ap) = INT(tt-epc.val-parameter) NO-ERROR.

        IF NOT AVAIL item_lote_impl_ap THEN NEXT.
        
        IF item_lote_impl_ap.ind_origin_tit_ap <> "APB" OR
           CAPS(v_cod_usuar_corren) BEGINS "RPW" OR CAPS(USERID("ems5")) BEGINS "RPW" THEN
        DO:
            ASSIGN l-erro = NO.
            LEAVE.
        END.
        
        FIND FIRST esp_pend_lote_ap OF item_lote_impl_ap NO-LOCK NO-ERROR.
        IF NOT AVAIL esp_pend_lote_ap THEN
        DO:
            FIND FIRST esp_hist_aprov_tit OF item_lote_impl_ap NO-LOCK NO-ERROR.
            IF NOT AVAIL esp_hist_aprov_tit THEN
            DO:
                RUN utp/ut-msgs.p ("show",17006,"Lote n∆o Pode Ser Atualizado~~Lote n∆o pode ser atualizado porque n∆o consta no controle de aprovaá∆o.").
                ASSIGN l-erro = YES.
            END.
            ELSE
            DO:
                IF esp_hist_aprov_tit.dt_reprovacao <> ? THEN
                DO:
                    RUN utp/ut-msgs.p ('show',17006,'Lote n∆o Pode Ser Atualizado~~Lote n∆o pode ser atualizado porque foi reprovado pelo motivo "' + TRIM(esp_hist_aprov_tit.mot-reprov) + '" Vocà deve corrigir ou excluir.').
                    ASSIGN l-erro = YES.
                END.
                IF esp_hist_aprov_tit.dt_aprovacao1 <> ?  AND
                   esp_hist_aprov_tit.dt_aprovacao2 <> ? THEN
                    ASSIGN l-erro = NO. /*ser† atualizado*/
            END.
        END.
        ELSE
        DO:
            RUN utp/ut-msgs.p ("show",17006,"Lote n∆o Pode Ser Atualizado~~Lote n∆o pode ser atualizado porque existe pendància de aprovaá∆o.").
            ASSIGN l-erro = YES.
        END.

    END.
END.
