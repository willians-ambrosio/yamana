/****************************************************************************************** 
** 	   Programa: add_item_lote_impl_tit_ap_u01.p
**   	  Autor: Vando Ribeiro
** 	 Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: REQ04
**      Objetivo: Valida Hist¢rico
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: p-ind-event, p-ind-object, p-wgh-object, p-wgh-frame, p-cod-table e p-row-table
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: add_item_lote_impl_tit_ap
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

DEF NEW GLOBAL SHARED VAR h-bt_ok              AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-aux-bt_ok          AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt_sav             AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-aux-bt_sav         AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cod_espec_docto-ap AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-des_text_histor_padr AS CHARACTER NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-tit-v_cod_estab_tit_ap_cor AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tit-v_cod_fornec_infor  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tit-cod_ser_docto       AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tit-cod_tit_ap          AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tit-val_tit_ap          AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tit-dat_emis_docto      AS HANDLE NO-UNDO.

DEF VAR c-confirma-titulos AS LOGICAL NO-UNDO.

DEF BUFFER b-item_lote_impl_ap FOR item_lote_impl_ap.
DEFINE BUFFER b_tit_ap FOR tit_ap.

DEFINE TEMP-TABLE tt_tit_ap_par
    FIELD cod_estab         LIKE tit_ap.cod_estab                
    FIELD cdn_fornecedor    LIKE tit_ap.cdn_fornecedor
    FIELD cod_espec_docto   LIKE tit_ap.cod_espec_docto          
    FIELD cod_ser_docto     LIKE tit_ap.cod_ser_docto            
    FIELD cod_tit_ap        LIKE tit_ap.cod_tit_ap               
    FIELD val_origin_tit_ap LIKE tit_ap.val_origin_tit_ap
    FIELD dat_emis_docto    LIKE tit_ap.dat_emis_docto.

IF p-ind-event  = "INITIALIZE"    THEN DO:

   RUN piBuscaWidget(INPUT "bt_ok",
                     INPUT  p-wgh-frame,
                     OUTPUT h-bt_ok).

   RUN piBuscaWidget(INPUT "bt_sav",
                     INPUT  p-wgh-frame,
                     OUTPUT h-bt_sav).

   RUN piBuscaWidget(INPUT "cod_espec_docto",
                     INPUT  p-wgh-frame,
                     OUTPUT h-cod_espec_docto-ap).

   RUN piBuscaWidget(INPUT "v_cod_estab_tit_ap_cor",
                     INPUT  p-wgh-frame,
                     OUTPUT h-tit-v_cod_estab_tit_ap_cor).
   RUN piBuscaWidget(INPUT "v_cod_fornec_infor",
                     INPUT  p-wgh-frame,
                     OUTPUT h-tit-v_cod_fornec_infor).
   RUN piBuscaWidget(INPUT "cod_ser_docto",
                     INPUT  p-wgh-frame,
                     OUTPUT h-tit-cod_ser_docto).
   RUN piBuscaWidget(INPUT "cod_tit_ap",
                     INPUT  p-wgh-frame,
                     OUTPUT h-tit-cod_tit_ap).
   RUN piBuscaWidget(INPUT "val_tit_ap",
                     INPUT  p-wgh-frame,
                     OUTPUT h-tit-val_tit_ap).
   RUN piBuscaWidget(INPUT "dat_emis_docto",
                     INPUT  p-wgh-frame,
                     OUTPUT h-tit-dat_emis_docto).

   IF VALID-HANDLE(h-bt_ok) THEN DO:
        CREATE BUTTON h-aux-bt_ok
            ASSIGN  ROW         = h-bt_ok:ROW  
                    COLUMN      = h-bt_ok:COLUMN
                    WIDTH       = h-bt_ok:WIDTH
                    HEIGHT      = h-bt_ok:HEIGHT
                    LABEL       = "OK*"
                    FRAME       = h-bt_ok:FRAME
                    FLAT-BUTTON = h-bt_ok:FLAT-BUTTON
                    TOOLTIP     = h-bt_ok:TOOLTIP
                    HELP        = h-bt_ok:HELP
                    NAME        = "h-aux-bt_ok"
                    SENSITIVE   = YES /*h-bt_ok:SENSITIVE  */
                    VISIBLE     = h-bt_ok:VISIBLE
                    CONVERT-3D-COLOR = h-bt_ok:convert-3D-COLOR.

        ON "CHOOSE" OF h-aux-bt_ok PERSISTENT RUN prgfin/upc/add_item_lote_impl_tit_ap_u01.p (INPUT "b-ok",
                                                                                              /*INPUT p-ind-event,*/
                                                                                              INPUT p-ind-object,   
                                                                                              INPUT p-wgh-object,   
                                                                                              INPUT p-wgh-frame,    
                                                                                              INPUT p-cod-table,    
                                                                                              INPUT p-row-table).   
        h-aux-bt_ok:LOAD-IMAGE-UP(h-bt_ok:IMAGE-UP).
        h-aux-bt_ok:LOAD-IMAGE-INSENSITIVE(h-bt_ok:IMAGE-INSENSITIVE).
        h-aux-bt_ok:MOVE-TO-TOP().
        ASSIGN h-bt_ok:TAB-STOP  = NO
               h-bt_ok:SENSITIVE = NO.

   END. /*IF VALID-HANDLE(h-bt_ok) THEN DO:*/

    IF VALID-HANDLE(h-bt_sav) THEN DO:
        CREATE BUTTON h-aux-bt_sav
            ASSIGN  ROW         = h-bt_sav:ROW  
                    COLUMN      = h-bt_sav:COLUMN
                    WIDTH       = h-bt_sav:WIDTH
                    HEIGHT      = h-bt_sav:HEIGHT
                    LABEL       = "SALVAR*"
                    FRAME       = h-bt_sav:FRAME
                    FLAT-BUTTON = h-bt_sav:FLAT-BUTTON
                    TOOLTIP     = h-bt_sav:TOOLTIP
                    HELP        = h-bt_sav:HELP
                    NAME        = "h-aux-bt_sav"
                    SENSITIVE   = YES /*h-bt_sav:SENSITIVE*/
                    VISIBLE     = h-bt_sav:VISIBLE
                    CONVERT-3D-COLOR = h-bt_sav:convert-3D-COLOR.

        ON "CHOOSE" OF h-aux-bt_sav PERSISTENT RUN prgfin/upc/add_item_lote_impl_tit_ap_u01.p (INPUT "b-sav",
                                                                                               /*INPUT p-ind-event, */
                                                                                               INPUT p-ind-object,
                                                                                               INPUT p-wgh-object,
                                                                                               INPUT p-wgh-frame, 
                                                                                               INPUT p-cod-table, 
                                                                                               INPUT p-row-table).

        h-aux-bt_sav:LOAD-IMAGE-UP(h-bt_sav:IMAGE-UP).                                    
        h-aux-bt_sav:LOAD-IMAGE-INSENSITIVE(h-bt_sav:IMAGE-INSENSITIVE).
        h-aux-bt_sav:MOVE-TO-TOP().
        ASSIGN h-bt_sav:TAB-STOP  = NO
               h-bt_sav:SENSITIVE = NO.

    END. /*IF VALID-HANDLE(h-bt_sav) THEN DO:*/

END. /*IF p-ind-event  = "AFTER-INITIALIZE" AND*/

IF p-ind-event  = "DISPLAY"    THEN DO:
    FIND item_lote_impl_ap WHERE
         RECID(item_lote_impl_ap) = p-row-table NO-LOCK.
    IF AVAIL item_lote_impl_ap THEN
        c-des_text_histor_padr = item_lote_impl_ap.des_text_histor.

END.

IF p-ind-event = "b-ok" OR p-ind-event = "b-sav"   THEN DO:

    IF VALID-HANDLE(h-tit-v_cod_estab_tit_ap_cor) AND
       VALID-HANDLE(h-tit-v_cod_fornec_infor)     AND
       VALID-HANDLE(h-tit-cod_ser_docto)          AND
       VALID-HANDLE(h-tit-cod_tit_ap)             AND
       VALID-HANDLE(h-tit-val_tit_ap)             AND
       VALID-HANDLE(h-tit-dat_emis_docto)         AND
       VALID-HANDLE(h-cod_espec_docto-ap ) THEN
    DO:
        /*
        MESSAGE 
           "Estab: "  h-tit-v_cod_estab_tit_ap_cor:SCREEN-VALUE skip
           "Fornecedor: "  h-tit-v_cod_fornec_infor:SCREEN-VALUE     skip
           "EspÇcie: "  h-cod_espec_docto-ap:SCREEN-VALUE         SKIP
           "SÇrie: "  h-tit-cod_ser_docto:SCREEN-VALUE          skip
           "T°tulo: "  h-tit-cod_tit_ap:SCREEN-VALUE             skip
           "Valor:"  h-tit-val_tit_ap:SCREEN-VALUE             skip
           "Data Emiss∆o: "  h-tit-dat_emis_docto:SCREEN-VALUE         skip
           " "  VIEW-AS ALERT-BOX INFO BUTTONS OK.
           
        FIND FIRST b_tit_ap NO-LOCK
             WHERE b_tit_ap.cod_estab            = h-tit-v_cod_estab_tit_ap_cor:SCREEN-VALUE
               AND b_tit_ap.cdn_fornecedor       = INTEGER(h-tit-v_cod_fornec_infor:SCREEN-VALUE)
               AND YEAR(b_tit_ap.dat_emis_docto) = YEAR(DATE(h-tit-dat_emis_docto:SCREEN-VALUE))
               AND b_tit_ap.ind_origin_tit_ap    = "APB" 
               AND b_tit_ap.log_tit_ap_estordo   = NO
               AND b_tit_ap.val_origin_tit_ap    = DECIMAL(h-tit-val_tit_ap:SCREEN-VALUE)
               /*AND b_tit_ap.cod_tit_ap           <> h-tit-cod_tit_ap:SCREEN-VALUE */ NO-ERROR.
           
           
        */

        EMPTY TEMP-TABLE tt_tit_ap_par.
        
        FOR EACH b_tit_ap NO-LOCK
           WHERE b_tit_ap.cod_estab            = h-tit-v_cod_estab_tit_ap_cor:SCREEN-VALUE
             AND b_tit_ap.cdn_fornecedor       = INTEGER(h-tit-v_cod_fornec_infor:SCREEN-VALUE)
             AND YEAR(b_tit_ap.dat_emis_docto) = YEAR(DATE(h-tit-dat_emis_docto:SCREEN-VALUE))
             AND b_tit_ap.ind_origin_tit_ap    = "APB" 
             AND b_tit_ap.log_tit_ap_estordo   = NO
             AND b_tit_ap.val_origin_tit_ap    = DECIMAL(h-tit-val_tit_ap:SCREEN-VALUE):
            
            CREATE tt_tit_ap_par.
            ASSIGN
                tt_tit_ap_par.cod_estab          = b_tit_ap.cod_estab        
                tt_tit_ap_par.cdn_fornecedor     = b_tit_ap.cdn_fornecedor   
                tt_tit_ap_par.cod_espec_docto    = b_tit_ap.cod_espec_docto  
                tt_tit_ap_par.cod_ser_docto      = b_tit_ap.cod_ser_docto    
                tt_tit_ap_par.cod_tit_ap         = b_tit_ap.cod_tit_ap       
                tt_tit_ap_par.val_origin_tit_ap  = b_tit_ap.val_origin_tit_ap
                tt_tit_ap_par.dat_emis_docto     = b_tit_ap.dat_emis_docto.
        END.

        FOR EACH b-item_lote_impl_ap NO-LOCK
           WHERE b-item_lote_impl_ap.cod_estab            = h-tit-v_cod_estab_tit_ap_cor:SCREEN-VALUE
             AND b-item_lote_impl_ap.cdn_fornecedor       = INTEGER(h-tit-v_cod_fornec_infor:SCREEN-VALUE)
             AND YEAR(b-item_lote_impl_ap.dat_emis_docto) = YEAR(DATE(h-tit-dat_emis_docto:SCREEN-VALUE))
             AND b-item_lote_impl_ap.val_tit_ap           = DECIMAL(h-tit-val_tit_ap:SCREEN-VALUE)
             AND RECID(b-item_lote_impl_ap)              <> p-row-table:
                     
            CREATE tt_tit_ap_par.
            ASSIGN
                tt_tit_ap_par.cod_estab          = b-item_lote_impl_ap.cod_estab        
                tt_tit_ap_par.cdn_fornecedor     = b-item_lote_impl_ap.cdn_fornecedor   
                tt_tit_ap_par.cod_espec_docto    = b-item_lote_impl_ap.cod_espec_docto  
                tt_tit_ap_par.cod_ser_docto      = b-item_lote_impl_ap.cod_ser_docto    
                tt_tit_ap_par.cod_tit_ap         = b-item_lote_impl_ap.cod_tit_ap       
                tt_tit_ap_par.val_origin_tit_ap  = b-item_lote_impl_ap.val_tit_ap
                tt_tit_ap_par.dat_emis_docto     = b-item_lote_impl_ap.dat_emis_docto.
        END.

        IF TEMP-TABLE tt_tit_ap_par:HAS-RECORDS THEN DO:
            RUN prgfin\upc\add_item_lote_impl_tit_ap_u02.w (INPUT TABLE tt_tit_ap_par,
                                                            OUTPUT c-confirma-titulos).

            IF NOT c-confirma-titulos THEN
            RETURN "NOK".
        END.

        /*
        IF AVAIL b_tit_ap THEN
        DO:
            RUN utp/ut-msgs.p (INPUT "show":U, INPUT 27100, 
                               INPUT  "Confirmaá∆o~~Existe um t°tulo cadastrado para este fornecedor com os dados parecidos." + CHR(13) +
                               "Estab: "        +  b_tit_ap.cod_estab                                      + chr(13) +
                               "Fornecedor: "   +  STRING(b_tit_ap.cdn_fornecedor)                         + chr(13) +
                               "EspÇcie: "      +  b_tit_ap.cod_espec_docto                                + chr(13) +
                               "SÇrie: "        +  b_tit_ap.cod_ser_docto                                  + chr(13) +
                               "T°tulo: "       +  b_tit_ap.cod_tit_ap                                     + chr(13) +
                               "Valor:"         +  STRING(b_tit_ap.val_origin_tit_ap, "->>>>,>>>,>>9.99")  + chr(13) +
                               "Data Emiss∆o: " +  STRING(b_tit_ap.dat_emis_docto, "99/99/9999")           + chr(13) +
                               "Confirma a inclus∆o/alteraá∆o do t°tulo?").
            IF RETURN-VALUE <> "YES" AND RETURN-VALUE <> "OK" THEN
                RETURN "NOK".
        end.                               
        */
        
    END.

    IF VALID-HANDLE(h-cod_espec_docto-ap) AND h-cod_espec_docto-ap:SCREEN-VALUE <> "AN" THEN
    DO:
        IF c-des_text_histor_padr = "" THEN DO:
            RUN utp/ut-msgs.p ("show",17006,"Hist¢rico Inv†lido~~Deve ser informada uma justificativa no campo hist¢rico.").
            RETURN "NOK".
        END.

        IF p-ind-event = "b-ok" THEN 
            APPLY "CHOOSE" TO h-bt_ok.
        ELSE
            APPLY "CHOOSE" TO h-bt_sav. 

        IF RETURN-VALUE = "OK" THEN
            c-des_text_histor_padr = "".

        RETURN RETURN-VALUE.
    END.
    ELSE DO:
        c-des_text_histor_padr = "".
        IF p-ind-event = "b-ok" THEN 
            APPLY "CHOOSE" TO h-bt_ok.
        ELSE
            APPLY "CHOOSE" TO h-bt_sav. 

        RETURN RETURN-VALUE.
    END.
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
