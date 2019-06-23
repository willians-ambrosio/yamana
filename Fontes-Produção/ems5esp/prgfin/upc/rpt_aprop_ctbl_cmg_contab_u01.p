/****************************************************************************************** 
** 	   Programa: rpt_aprop_ctbl_cmg_contab_u01.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: 
**      Objetivo: NÆo Permite execu‡Æo do demonstrativo Contabil se existir movimentos pendentes de aprova‡Æo
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID     NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-bt_exec             AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-aux-bt_exec         AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-v_dat_transacao_ini AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-v_dat_transacao_fim AS HANDLE NO-UNDO.
DEFINE VARIABLE l-permite-ctb AS LOGICAL     NO-UNDO.

/*
MESSAGE "p-ind-evento.....: " p-ind-event         SKIP
        "p-ind-objeto.....: " p-ind-object        SKIP
        "Handle do Objeto.: " p-wgh-object        SKIP
        "Frame............: " p-wgh-frame         SKIP
        "Nome da tabela...: " p-cod-table         SKIP
        "Rowid da tabela..: " STRING(p-row-table) SKIP
    VIEW-AS ALERT-BOX TITLE "bas-espec-docto".
*/

IF p-ind-event  = "NOVA_SELECAO" THEN DO:

    RUN piBuscaWidget(INPUT "v_dat_transacao_ini",
                      INPUT  p-wgh-frame,
                      OUTPUT h-v_dat_transacao_ini).

    RUN piBuscaWidget(INPUT "v_dat_transacao_fim",
                      INPUT  p-wgh-frame,
                     OUTPUT h-v_dat_transacao_fim).
END.

IF p-ind-event  = "INITIALIZE"    THEN DO:

    RUN piBuscaWidget(INPUT "bt_exec",
                      INPUT  p-wgh-frame,
                      OUTPUT h-bt_exec).
    IF VALID-HANDLE(h-bt_exec) THEN DO:
        CREATE BUTTON h-aux-bt_exec
            ASSIGN  ROW         = h-bt_exec:ROW  
                    COLUMN      = h-bt_exec:COLUMN
                    WIDTH       = h-bt_exec:WIDTH
                    HEIGHT      = h-bt_exec:HEIGHT
                    LABEL       = h-bt_exec:LABEL
                    FRAME       = h-bt_exec:FRAME
                    FLAT-BUTTON = h-bt_exec:FLAT-BUTTON
                    TOOLTIP     = h-bt_exec:TOOLTIP
                    HELP        = h-bt_exec:HELP
                    NAME        = "h-aux-bt_exec"
                    SENSITIVE   = YES /*h-bt_exec:SENSITIVE  */
                    VISIBLE     = h-bt_exec:VISIBLE
                    CONVERT-3D-COLOR = h-bt_exec:convert-3D-COLOR.

        ON "CHOOSE" OF h-aux-bt_exec PERSISTENT RUN prgfin/upc/rpt_aprop_ctbl_cmg_contab_u01.p (INPUT "botao-exec",
                                                                     INPUT p-ind-object,   
                                                                     INPUT p-wgh-object,   
                                                                     INPUT p-wgh-frame,    
                                                                     INPUT p-cod-table,    
                                                                     INPUT p-row-table).   
        h-aux-bt_exec:LOAD-IMAGE-UP(h-bt_exec:IMAGE-UP).
        h-aux-bt_exec:LOAD-IMAGE-INSENSITIVE(h-bt_exec:IMAGE-INSENSITIVE).
        h-aux-bt_exec:MOVE-TO-TOP().
        ASSIGN h-bt_exec:TAB-STOP  = NO
               h-bt_exec:SENSITIVE = NO.

    END.
END.

IF p-ind-event  = "botao-exec" THEN
DO:
    l-permite-ctb = YES.
    FOR FIRST esp_pend_movto_cta_corren NO-LOCK
        WHERE esp_pend_movto_cta_corren.dt_reprovacao = ?
          AND esp_pend_movto_cta_corren.dat_movto_cta_corren >= DATE(h-v_dat_transacao_ini:SCREEN-VALUE)
          AND esp_pend_movto_cta_corren.dat_movto_cta_corren <= DATE(h-v_dat_transacao_fim:SCREEN-VALUE):

        l-permite-ctb = NO.
        RUN utp/ut-msgs.p ("show",17006,"Execu‡Æo nÆo Permitida~~Execu‡Æo nÆo permitida porque existem movimentos pendentes de aprova‡Æo.").
    END.
    IF l-permite-ctb THEN
        APPLY "CHOOSE" TO h-bt_exec.
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
