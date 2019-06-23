/****************************************************************************************** 
** 	   Programa: dlg_histor_tit_u01.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: REQ04
**      Objetivo: Validar hist¢rico de titulos manuais
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: p-ind-event, p-ind-object, p-wgh-object, p-wgh-frame, p-cod-table e p-row-table
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-des_text_histor_padr AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-des_text_histor_padr AS CHARACTER NO-UNDO.

/*
MESSAGE "add_item_lote_impl_tit_ap_u02.p" SKIP
        "p-ind-evento.....: " p-ind-event         SKIP
        "p-ind-objeto.....: " p-ind-object        SKIP
        "Handle do Objeto.: " p-wgh-object        SKIP
        "Frame............: " p-wgh-frame         SKIP
        "Nome da tabela...: " p-cod-table         SKIP
        "Rowid da tabela..: " STRING(p-row-table) SKIP
    VIEW-AS ALERT-BOX TITLE "bas-espec-docto".
*/

IF p-ind-event = "INITIALIZE" AND p-cod-table = "histor_tit_movto_ap" THEN DO:
    c-des_text_histor_padr = "".
    RUN piBuscaWidget(INPUT "des_text_histor_padr",
                      INPUT  p-wgh-frame,
                      OUTPUT h-des_text_histor_padr).
END.

IF p-ind-event = "ASSIGN" AND p-cod-table = "histor_tit_movto_ap" THEN DO:
    IF VALID-HANDLE(h-des_text_histor_padr) THEN
    c-des_text_histor_padr = h-des_text_histor_padr:SCREEN-VALUE.

END.


PROCEDURE piBuscaWidget:
    DEF INPUT  PARAM pNome     AS CHAR.
    DEF INPUT  PARAM pFrame    AS WIDGET-HANDLE.
    DEF OUTPUT PARAM pObject   AS WIDGET-HANDLE.

    DEF VAR hFrame             AS WIDGET-HANDLE.
    DEF VAR whObjeto           AS WIDGET-HANDLE.

    ASSIGN hFrame = pFrame:FIRST-CHILD.

    DO WHILE VALID-HANDLE(hFrame):

        IF hFrame:TYPE <> "field-group" THEN
        DO:

            IF hFrame:Type = "frame" THEN
            DO:
                RUN piBuscaWidget(INPUT  pNome,
                                  INPUT  hFrame,
                                  OUTPUT whObjeto).
                IF whObjeto <> ? THEN
                DO:
                    ASSIGN pObject = whObjeto.
                    LEAVE.
                END.
            END.
            /*
            MESSAGE hFrame:NAME SKIP
                    hFrame:TYPE
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */

            IF    hFrame:NAME = pNome THEN
            DO:


                ASSIGN pObject = hFrame.
                LEAVE.
            END.

            ASSIGN hFrame = hFrame:NEXT-SIBLING.
        END.
        ELSE ASSIGN hFrame = hFrame:FIRST-CHILD.
    END.
END PROCEDURE.
