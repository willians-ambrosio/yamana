/****************************************************************************************** 
** 	   Programa: bas_item_lote_impl_ap_u01.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: REQ04
**      Objetivo: Envia e-mail de pendˆcia de atualiza‡Æo de t¡tulos.

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
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID     NO-UNDO.
/*
MESSAGE "add_item_lote_impl_tit_ap_u01.p" SKIP
        "p-ind-evento.....: " p-ind-event         SKIP
        "p-ind-objeto.....: " p-ind-object        SKIP
        "Handle do Objeto.: " p-wgh-object        SKIP
        "Frame............: " p-wgh-frame         SKIP
        "Nome da tabela...: " p-cod-table         SKIP
        "Rowid da tabela..: " STRING(p-row-table) SKIP
    VIEW-AS ALERT-BOX TITLE "bas-espec-docto".
*/

DEF NEW GLOBAL SHARED VAR h-bt_ok_base              AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-aux-bt_ok_base          AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cod_estab               AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cod_refer               AS HANDLE NO-UNDO.

DEFINE VARIABLE c-mess-top  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess-base AS CHARACTER   NO-UNDO.

IF p-ind-event  = "INITIALIZE"    THEN DO:
    RUN piBuscaWidget(INPUT "cod_estab",
                      INPUT  p-wgh-frame,
                      OUTPUT h-cod_estab).
    RUN piBuscaWidget(INPUT "cod_refer",
                      INPUT  p-wgh-frame,
                      OUTPUT h-cod_refer).

    RUN piBuscaWidget(INPUT "bt_ok",
                      INPUT  p-wgh-frame,
                      OUTPUT h-bt_ok_base).
    
    IF VALID-HANDLE(h-bt_ok_base) THEN DO:
         CREATE BUTTON h-aux-bt_ok_base
             ASSIGN  ROW         = h-bt_ok_base:ROW  
                     COLUMN      = h-bt_ok_base:COLUMN
                     WIDTH       = h-bt_ok_base:WIDTH
                     HEIGHT      = h-bt_ok_base:HEIGHT
                     LABEL       = h-bt_ok_base:LABEL
                     FRAME       = h-bt_ok_base:FRAME
                     FLAT-BUTTON = h-bt_ok_base:FLAT-BUTTON
                     TOOLTIP     = h-bt_ok_base:TOOLTIP
                     HELP        = h-bt_ok_base:HELP
                     NAME        = "h-aux-bt_ok_base"
                     SENSITIVE   = YES /*h-bt_ok_base:SENSITIVE  */
                     VISIBLE     = h-bt_ok_base:VISIBLE
                     CONVERT-3D-COLOR = h-bt_ok_base:convert-3D-COLOR.
    
         ON "CHOOSE" OF h-aux-bt_ok_base PERSISTENT RUN prgfin/upc/bas_item_lote_impl_ap_u01.p (INPUT "b-ok",
                                                                                               /*INPUT p-ind-event,*/
                                                                                               INPUT p-ind-object,   
                                                                                               INPUT p-wgh-object,   
                                                                                               INPUT p-wgh-frame,    
                                                                                               INPUT p-cod-table,    
                                                                                               INPUT p-row-table).   
         h-aux-bt_ok_base:LOAD-IMAGE-UP(h-bt_ok_base:IMAGE-UP).
         h-aux-bt_ok_base:LOAD-IMAGE-INSENSITIVE(h-bt_ok_base:IMAGE-INSENSITIVE).
         h-aux-bt_ok_base:MOVE-TO-TOP().
         ASSIGN h-bt_ok_base:TAB-STOP  = NO
                h-bt_ok_base:SENSITIVE = NO.
    
    END. /*IF VALID-HANDLE(h-bt_ok_base) THEN DO:*/
END. /*IF p-ind-event  = "AFTER-INITIALIZE" AND*/

IF p-ind-event = "b-ok" THEN DO:

    IF p-ind-event = "b-ok" THEN 
        APPLY "CHOOSE" TO h-bt_ok_base.

    IF RETURN-VALUE = "OK" THEN
    DO:
        {utp/utapi019.i}
        RUN utp/utapi019.p PERSISTENT SET h-utapi019.
        {prgfin\apb\apb001-i03.i}
        c-mess = "".
        FOR EACH item_lote_impl_ap NO-LOCK
           WHERE item_lote_impl_ap.cod_estab = h-cod_estab:SCREEN-VALUE
             AND item_lote_impl_ap.cod_refer = h-cod_refer:SCREEN-VALUE:

            FIND ems5.fornecedor OF item_lote_impl_ap NO-LOCK NO-ERROR.

            FIND FIRST esp_pend_lote_ap OF item_lote_impl_ap NO-LOCK
                 WHERE (esp_pend_lote_ap.dt_aprovacao1 = ?
                    OR esp_pend_lote_ap.dt_aprovacao2 = ?)
                   AND esp_pend_lote_ap.dt_reprovacao = ? NO-ERROR.
            IF AVAIL esp_pend_lote_ap THEN

            c-mess = c-mess + "<tr>"                                                                                                     
                + "<td>" + item_lote_impl_ap.cod_empresa +                                           "</td>"
                + "<td>" + item_lote_impl_ap.cod_estab +                                             "</td>"
                + "<td>" + STRING(item_lote_impl_ap.cdn_fornecedor) + " - " + fornecedor.nom_abrev + "</td>"
                + "<td>" + item_lote_impl_ap.cod_tit_ap +                                            "</td>"
                + "<td>" + item_lote_impl_ap.cod_ser_docto +                                         "</td>"
                + "<td>" + item_lote_impl_ap.cod_espec_docto +                                       "</td>"
                + "<td>" + item_lote_impl_ap.cod_parcela +                                           "</td>"
                + "<td>" + STRING(item_lote_impl_ap.val_tit_ap, "->>>>,>>>,>>9.99") +                "</td>"
                + "<td>" + STRING(item_lote_impl_ap.dat_emis_docto, "99/99/9999") +                  "</td>"
                + "<td>" + STRING(item_lote_impl_ap.dat_vencto_tit_ap, "99/99/9999") +               "</td>"
                + "<td>" + item_lote_impl_ap.des_text_histor +                                       "</td>"
                + "<td>" + esp_pend_lote_ap.cod_usuario +                                            "</td>"
                + "<td>" + STRING(esp_pend_lote_ap.dt_digitacao, "99/99/9999 HH:MM") +                     "</td>"
                + "</tr>".
        END.

        IF c-mess <> "" THEN
        DO:
            FIND FIRST param_email NO-LOCK NO-ERROR.
            FOR EACH esp_aprovador NO-LOCK
               WHERE esp_aprovador.aprov_contas_pagar
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
                           tt-envio2.assunto           = "Pendˆncia de T¡tulos para Aprova‡Æo"
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
                    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erro-email-apb-" + STRING(TODAY, "99999999") + "-" + STRING(ETIME, "99999999") + ".txt").
                    FOR EACH tt-erros:
                        DISP tt-erros WITH SCROLLABLE.
                    END.
                    OUTPUT CLOSE.
                END.
            END.
        END.
        DELETE PROCEDURE h-utapi019.
    END.

    RETURN RETURN-VALUE.
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
